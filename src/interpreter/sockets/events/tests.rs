use lpc_rs_utils::config::ConfigBuilder;

use super::*;
use crate::interpreter::sockets::tests::{Fixture, callback, endpoint};

fn fixture(count: usize, input: usize, accepts: usize) -> Fixture {
    Fixture::from_config(
        ConfigBuilder::default()
            .max_sockets(count)
            .max_pending_socket_input(input)
            .max_pending_socket_accepts(accepts)
            .build()
            .unwrap(),
    )
}

fn settle(f: &mut Fixture, handle: SocketHandle, succeeded: bool) -> SocketEvent {
    let txn = f.begin();
    let lease = f.access(&txn).lease(handle).unwrap().unwrap();
    assert!(f.access(&txn).acknowledge(&lease, succeeded).unwrap());
    f.commit(&txn);
    lease.event
}

fn stream(f: &mut Fixture) -> SocketHandle {
    let handle = f.connect();
    f.open(handle);
    assert!(matches!(
        settle(f, handle, true).data,
        SocketEventData::Ready { .. }
    ));
    handle
}

fn listener(f: &mut Fixture) -> SocketHandle {
    let txn = f.begin();
    let handle = f
        .access(&txn)
        .listen(&f.owner, endpoint(0), callback(&f.owner))
        .unwrap();
    f.commit(&txn);
    f.commands.recv().unwrap();
    let txn = f.begin();
    assert!(f.access(&txn).ready(handle, endpoint(1000), None).unwrap());
    f.commit(&txn);
    handle
}

fn reserve(f: &mut Fixture, listener: SocketHandle) -> SocketAcceptPermit {
    let txn = f.begin();
    let SocketAdmission::Admitted(permit) = f.access(&txn).reserve_accept(listener).unwrap() else {
        panic!("accept slot unavailable");
    };
    f.commit(&txn);
    permit
}

fn accept(f: &mut Fixture, listener: SocketHandle) -> SocketHandle {
    let permit = reserve(f, listener);
    let txn = f.begin();
    assert_eq!(
        f.access(&txn)
            .accepted(permit, endpoint(1000), endpoint(2000))
            .unwrap(),
        SocketAdmission::Admitted(permit.child)
    );
    f.commit(&txn);
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::OpenAccepted {
            handle: permit.child
        }]
    );
    permit.child
}

#[test]
fn input_remains_charged_and_unchanged_until_acknowledgement_commits() {
    let mut f = fixture(2, 4, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    let bytes = Bytes::from_static(b"a\0bc");
    assert!(f.access(&txn).read(handle, 1, bytes.clone()).unwrap());
    f.commit(&txn);
    let aborted = f.begin();
    let lease = f.access(&aborted).lease(handle).unwrap().unwrap();
    assert_eq!(lease.event.data, SocketEventData::Read(bytes));
    assert!(f.access(&aborted).acknowledge(&lease, true).unwrap());
    assert_eq!(f.access(&aborted).input_capacity(handle).unwrap(), Some(4));
    let retry = f.begin();
    assert_eq!(f.access(&retry).input_capacity(handle).unwrap(), Some(0));
    let same = f.access(&retry).lease(handle).unwrap().unwrap();
    assert_eq!(lease.event, same.event);
    assert!(f.access(&retry).acknowledge(&lease, true).is_err());
    assert!(f.access(&retry).acknowledge(&same, true).unwrap());
    assert!(!f.access(&retry).acknowledge(&same, true).unwrap());
    f.commit(&retry);
    assert!(f.access(&f.begin()).lease(handle).unwrap().is_none());
    assert_eq!(
        f.access(&f.begin()).input_capacity(handle).unwrap(),
        Some(4)
    );
    assert!(f.commands.is_empty());
}

#[test]
fn read_sequence_and_byte_budget_reject_duplicates_gaps_and_overflow() {
    let mut f = fixture(2, 3, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    let access = f.access(&txn);
    assert!(access.read(handle, 2, Bytes::from_static(b"x")).is_err());
    assert!(access.read(handle, 1, Bytes::new()).is_err());
    assert!(access.read(handle, 1, Bytes::from_static(b"1234")).is_err());
    assert!(access.read(handle, 1, Bytes::from_static(b"12")).unwrap());
    assert!(!access.read(handle, 1, Bytes::from_static(b"12")).unwrap());
    assert!(access.read(handle, 2, Bytes::from_static(b"34")).is_err());
    assert!(access.read(handle, 2, Bytes::from_static(b"3")).unwrap());
    f.commit(&txn);
    assert_eq!(
        settle(&mut f, handle, false).data,
        SocketEventData::Read(Bytes::from_static(b"12"))
    );
    assert_eq!(
        f.access(&f.begin()).input_capacity(handle).unwrap(),
        Some(2)
    );
    assert_eq!(
        settle(&mut f, handle, true).data,
        SocketEventData::Read(Bytes::from_static(b"3"))
    );
    assert_eq!(
        f.access(&f.begin()).input_capacity(handle).unwrap(),
        Some(3)
    );
}

#[test]
fn callback_conflict_retains_the_same_event_without_releasing_credits() {
    let mut f = fixture(2, 2, 1);
    let handle = stream(&mut f);
    let cell = VarId::new();
    let txn = f.begin();
    txn.with(|txn| txn.write(cell, 1.into()));
    f.access(&txn)
        .read(handle, 1, Bytes::from_static(b"12"))
        .unwrap();
    f.commit(&txn);
    let attempt = f.begin();
    let lease = f.access(&attempt).lease(handle).unwrap().unwrap();
    attempt.with(|txn| {
        txn.read(cell);
        txn.write(cell, 2.into());
    });
    f.access(&attempt).acknowledge(&lease, true).unwrap();
    let other = f.begin();
    other.with(|txn| txn.write(cell, 3.into()));
    f.commit(&other);
    assert!(
        f.committer
            .commit(attempt.with(Transaction::take_changeset))
            .is_err()
    );
    let retry = f.begin();
    assert_eq!(
        f.access(&retry).lease(handle).unwrap().unwrap().event,
        lease.event
    );
    assert_eq!(f.access(&retry).input_capacity(handle).unwrap(), Some(0));
    assert_eq!(settle(&mut f, handle, true).id, lease.event.id);
}

#[test]
fn transfer_invalidates_old_delivery_and_retargets_its_unconsumed_head() {
    let mut f = fixture(2, 2, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    f.access(&txn)
        .read(handle, 1, Bytes::from_static(b"hi"))
        .unwrap();
    f.commit(&txn);
    let attempt = f.begin();
    let lease = f.access(&attempt).lease(handle).unwrap().unwrap();
    f.access(&attempt).acknowledge(&lease, true).unwrap();
    let recipient = Arc::new(Process::default());
    let ptr = callback(&recipient);
    let transfer = f.begin();
    f.access(&transfer)
        .transfer(handle, &f.owner, &recipient, ptr.clone())
        .unwrap();
    f.commit(&transfer);
    assert!(
        f.committer
            .commit(attempt.with(Transaction::take_changeset))
            .is_err()
    );
    let retry = f.begin();
    let next = f.access(&retry).lease(handle).unwrap().unwrap();
    assert_eq!(next.event, lease.event);
    assert!(Arc::ptr_eq(&next.owner, &recipient));
    assert!(Arc::ptr_eq(&next.callback, &ptr));
    f.access(&retry).acknowledge(&next, true).unwrap();
    f.commit(&retry);
}

#[test]
fn callback_can_transfer_its_own_stream_and_acknowledge_in_one_transaction() {
    let mut f = fixture(2, 2, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    f.access(&txn)
        .read(handle, 1, Bytes::from_static(b"hi"))
        .unwrap();
    f.commit(&txn);
    let recipient = Arc::new(Process::default());
    let txn = f.begin();
    let lease = f.access(&txn).lease(handle).unwrap().unwrap();
    f.access(&txn)
        .transfer(handle, &f.owner, &recipient, callback(&recipient))
        .unwrap();
    assert!(f.access(&txn).acknowledge(&lease, true).unwrap());
    f.commit(&txn);
    assert_eq!(
        f.access(&f.begin()).input_capacity(handle).unwrap(),
        Some(2)
    );
    assert!(
        f.access(&f.begin())
            .query(Some(&f.owner))
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        f.access(&f.begin()).query(Some(&recipient)).unwrap().len(),
        1
    );
}

#[test]
fn eof_follows_input_once_and_preserves_the_write_side() {
    let mut f = fixture(2, 2, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    let access = f.access(&txn);
    access.read(handle, 1, Bytes::from_static(b"hi")).unwrap();
    assert!(access.eof(handle, 2).unwrap());
    assert!(!access.eof(handle, 2).unwrap());
    assert!(!access.read(handle, 3, Bytes::from_static(b"x")).unwrap());
    assert_eq!(access.input_capacity(handle).unwrap(), None);
    assert!(access.info(handle).unwrap().read_eof);
    assert!(
        access
            .write(&f.owner, handle, Bytes::from_static(b"reply"))
            .unwrap()
    );
    f.commit(&txn);
    assert!(matches!(
        settle(&mut f, handle, true).data,
        SocketEventData::Read(_)
    ));
    assert_eq!(settle(&mut f, handle, true).data, SocketEventData::Eof);
    assert!(f.access(&f.begin()).lease(handle).unwrap().is_none());
    assert_eq!(
        f.access(&f.begin()).info(handle).unwrap().state,
        SocketState::Open
    );
}

#[test]
fn reads_and_acknowledgements_on_different_streams_do_not_conflict() {
    let mut f = fixture(2, 2, 1);
    let a = stream(&mut f);
    let b = stream(&mut f);
    let first = f.begin();
    let second = f.begin();
    f.access(&first)
        .read(a, 1, Bytes::from_static(b"a"))
        .unwrap();
    f.access(&second)
        .read(b, 1, Bytes::from_static(b"b"))
        .unwrap();
    f.commit(&first);
    f.commit(&second);
    let first = f.begin();
    let second = f.begin();
    for (txn, handle) in [(&first, a), (&second, b)] {
        let lease = f.access(txn).lease(handle).unwrap().unwrap();
        f.access(txn).acknowledge(&lease, true).unwrap();
    }
    f.commit(&first);
    f.commit(&second);
}

#[test]
fn accept_reservations_count_against_vm_quota_before_becoming_visible() {
    let mut f = fixture(2, 2, 2);
    let parent = listener(&mut f);
    let abandoned = f.begin();
    let SocketAdmission::Admitted(_) = f.access(&abandoned).reserve_accept(parent).unwrap() else {
        panic!()
    };
    let permit = reserve(&mut f, parent);
    let txn = f.begin();
    let access = f.access(&txn);
    assert!(access.info(permit.child).is_none());
    assert_eq!(access.query(None).unwrap().len(), 1);
    assert!(
        access
            .connect(&f.owner, "host".into(), 9, callback(&f.owner))
            .is_err()
    );
    assert_eq!(
        access.reserve_accept(parent).unwrap(),
        SocketAdmission::Full
    );
    access.close(&f.owner, parent).unwrap();
    f.commit(&txn);
    let txn = f.begin();
    assert_eq!(
        f.access(&txn)
            .accepted(permit, endpoint(1), endpoint(2))
            .unwrap(),
        SocketAdmission::Stopped
    );
    assert!(
        f.access(&txn)
            .connect(&f.owner, "host".into(), 9, callback(&f.owner))
            .is_ok()
    );
}

#[test]
fn concurrent_listener_reservations_cannot_overbook_vm_quota() {
    let mut f = fixture(3, 2, 2);
    let a = listener(&mut f);
    let b = listener(&mut f);
    let first = f.begin();
    let second = f.begin();
    assert!(matches!(
        f.access(&first).reserve_accept(a).unwrap(),
        SocketAdmission::Admitted(_)
    ));
    assert!(matches!(
        f.access(&second).reserve_accept(b).unwrap(),
        SocketAdmission::Admitted(_)
    ));
    f.commit(&first);
    assert!(
        f.committer
            .commit(second.with(Transaction::take_changeset))
            .is_err()
    );
    assert_eq!(
        f.access(&f.begin()).reserve_accept(b).unwrap(),
        SocketAdmission::Full
    );
}

#[test]
fn ready_precedes_accept_and_child_io_stays_paused_until_acknowledgement_commits() {
    let mut f = fixture(3, 2, 1);
    let parent = listener(&mut f);
    let child = accept(&mut f, parent);
    assert!(matches!(
        settle(&mut f, parent, true).data,
        SocketEventData::Ready { .. }
    ));
    let txn = f.begin();
    let access = f.access(&txn);
    let lease = access.lease(parent).unwrap().unwrap();
    assert_eq!(lease.event.data, SocketEventData::Accept(child));
    assert_eq!(access.input_capacity(child).unwrap(), None);
    assert!(!access.read(child, 1, Bytes::from_static(b"x")).unwrap());
    assert!(access.lease(child).unwrap().is_none());
    assert_eq!(
        access.reserve_accept(parent).unwrap(),
        SocketAdmission::Full
    );
    assert!(
        access
            .write(&f.owner, child, Bytes::from_static(b"reply"))
            .unwrap()
    );
    assert!(access.acknowledge(&lease, true).unwrap());
    assert_eq!(f.access(&f.begin()).input_capacity(child).unwrap(), None);
    assert!(f.commands.is_empty());
    f.commit(&txn);
    assert!(
        matches!(&f.commands.recv().unwrap()[..], [SocketCommand::Write { handle, .. }, SocketCommand::Activate { handle: activated }] if *handle == child && *activated == child)
    );
    assert_eq!(f.access(&f.begin()).input_capacity(child).unwrap(), Some(2));
    assert!(matches!(
        f.access(&f.begin()).reserve_accept(parent).unwrap(),
        SocketAdmission::Admitted(_)
    ));
}

#[test]
fn failed_accept_retires_child_and_returns_listener_and_vm_capacity() {
    let mut f = fixture(2, 2, 1);
    let parent = listener(&mut f);
    settle(&mut f, parent, true);
    let child = accept(&mut f, parent);
    assert_eq!(
        settle(&mut f, parent, false).data,
        SocketEventData::Accept(child)
    );
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Retire { handle: child }]
    );
    assert!(f.access(&f.begin()).info(child).is_none());
    let txn = f.begin();
    assert_eq!(
        f.access(&txn).reserve_accept(parent).unwrap(),
        SocketAdmission::Full
    );
    assert!(f.access(&txn).disposed(child));
    f.commit(&txn);
    let next = reserve(&mut f, parent);
    assert!(next.child.get() > child.get());
}

#[test]
fn listener_close_discards_undelivered_children_even_after_their_transfer() {
    let mut f = fixture(4, 2, 2);
    let parent = listener(&mut f);
    settle(&mut f, parent, true);
    let child = accept(&mut f, parent);
    let recipient = Arc::new(Process::default());
    let txn = f.begin();
    f.access(&txn)
        .transfer(child, &f.owner, &recipient, callback(&recipient))
        .unwrap();
    f.commit(&txn);
    let pending = reserve(&mut f, parent);
    let stale = f.begin();
    let lease = f.access(&stale).lease(parent).unwrap().unwrap();
    f.access(&stale).acknowledge(&lease, true).unwrap();
    let txn = f.begin();
    f.access(&txn).close(&f.owner, parent).unwrap();
    f.commit(&txn);
    assert!(
        f.committer
            .commit(stale.with(Transaction::take_changeset))
            .is_err()
    );
    let access = f.access(&f.begin());
    assert!(access.info(child).is_none());
    assert!(access.info(pending.child).is_none());
    assert!(access.query(Some(&recipient)).unwrap().is_empty());
    assert!(access.lease(parent).unwrap().is_none());
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[
            SocketCommand::Retire { handle: child },
            SocketCommand::Close {
                handle: parent,
                drain: false
            }
        ]
    );
}

#[test]
fn delivered_child_is_independent_of_listener_retirement() {
    let mut f = fixture(3, 2, 1);
    let parent = listener(&mut f);
    settle(&mut f, parent, true);
    let child = accept(&mut f, parent);
    settle(&mut f, parent, true);
    f.commands.recv().unwrap();
    let txn = f.begin();
    f.access(&txn).close(&f.owner, parent).unwrap();
    f.commit(&txn);
    assert_eq!(f.access(&f.begin()).input_capacity(child).unwrap(), Some(2));
    let txn = f.begin();
    assert!(
        f.access(&txn)
            .read(child, 1, Bytes::from_static(b"ok"))
            .unwrap()
    );
    f.commit(&txn);
    assert_eq!(
        settle(&mut f, child, true).data,
        SocketEventData::Read(Bytes::from_static(b"ok"))
    );
}

#[test]
fn closed_event_is_final_and_failed_delivery_still_retires_metadata() {
    let mut f = fixture(1, 2, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    let access = f.access(&txn);
    access.read(handle, 1, Bytes::from_static(b"hi")).unwrap();
    access.closed(handle);
    assert!(!access.eof(handle, 2).unwrap());
    assert_eq!(access.info(handle).unwrap().state, SocketState::Closed);
    assert!(
        access
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_err()
    );
    f.commit(&txn);
    assert!(matches!(
        settle(&mut f, handle, true).data,
        SocketEventData::Read(_)
    ));
    assert!(matches!(
        settle(&mut f, handle, false).data,
        SocketEventData::Closed { .. }
    ));
    assert!(f.access(&f.begin()).info(handle).is_none());
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Retire { handle }]
    );
    let txn = f.begin();
    assert!(f.access(&txn).disposed(handle));
    f.commit(&txn);
    f.connect();
}

#[test]
fn drained_is_a_bounded_wakeup_and_is_suppressed_while_closing() {
    let mut f = fixture(1, 2, 1);
    let handle = stream(&mut f);
    for sequence in 1..=3 {
        let txn = f.begin();
        f.access(&txn)
            .write(&f.owner, handle, Bytes::from_static(b"a"))
            .unwrap();
        f.commit(&txn);
        f.commands.recv().unwrap();
        let txn = f.begin();
        f.access(&txn).complete_write(handle, sequence).unwrap();
        f.commit(&txn);
    }
    assert_eq!(settle(&mut f, handle, true).data, SocketEventData::Drained);
    assert!(f.access(&f.begin()).lease(handle).unwrap().is_none());
    let txn = f.begin();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"a"))
        .unwrap();
    f.access(&txn).close(&f.owner, handle).unwrap();
    f.commit(&txn);
    f.commands.recv().unwrap();
    let txn = f.begin();
    f.access(&txn).complete_write(handle, 4).unwrap();
    f.commit(&txn);
    assert!(f.access(&f.begin()).lease(handle).unwrap().is_none());
}

#[test]
fn close_cancels_pending_drained_without_dropping_reads_and_aborted_close_preserves_it() {
    let mut f = fixture(1, 2, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"a"))
        .unwrap();
    f.access(&txn).complete_write(handle, 1).unwrap();
    f.access(&txn)
        .read(handle, 1, Bytes::from_static(b"x"))
        .unwrap();
    f.commit(&txn);
    f.commands.recv().unwrap();
    let aborted = f.begin();
    f.access(&aborted).close(&f.owner, handle).unwrap();
    assert_eq!(
        f.access(&aborted)
            .lease(handle)
            .unwrap()
            .unwrap()
            .event()
            .data,
        SocketEventData::Read(Bytes::from_static(b"x"))
    );
    drop(aborted);
    let stale = f.begin();
    let lease = f.access(&stale).lease(handle).unwrap().unwrap();
    assert_eq!(lease.event().data, SocketEventData::Drained);
    f.access(&stale).acknowledge(&lease, true).unwrap();
    let txn = f.begin();
    f.access(&txn).close(&f.owner, handle).unwrap();
    f.commit(&txn);
    assert!(
        f.committer
            .commit(stale.with(Transaction::take_changeset))
            .is_err()
    );
    assert_eq!(
        settle(&mut f, handle, true).data,
        SocketEventData::Read(Bytes::from_static(b"x"))
    );
    assert!(f.access(&f.begin()).lease(handle).unwrap().is_none());
}

#[test]
fn terminal_failure_suppresses_an_already_queued_drained_wakeup() {
    let mut f = fixture(1, 2, 1);
    let handle = stream(&mut f);
    let txn = f.begin();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"a"))
        .unwrap();
    f.access(&txn).complete_write(handle, 1).unwrap();
    f.access(&txn).closed(handle);
    f.commit(&txn);
    assert!(matches!(
        settle(&mut f, handle, true).data,
        SocketEventData::Closed { .. }
    ));
}

#[test]
fn gc_retains_input_and_unpublished_accept_reservations() {
    let mut f = fixture(3, 2, 1);
    let handle = stream(&mut f);
    let parent = listener(&mut f);
    let permit = reserve(&mut f, parent);
    let txn = f.begin();
    f.access(&txn)
        .read(handle, 1, Bytes::from_static(b"hi"))
        .unwrap();
    f.commit(&txn);
    f.gc();
    assert_eq!(
        settle(&mut f, handle, true).data,
        SocketEventData::Read(Bytes::from_static(b"hi"))
    );
    let txn = f.begin();
    assert_eq!(
        f.access(&txn)
            .accepted(permit, endpoint(1), endpoint(2))
            .unwrap(),
        SocketAdmission::Admitted(permit.child)
    );
    f.commit(&txn);
    f.gc();
    assert!(f.access(&f.begin()).info(permit.child).is_some());
    assert!(matches!(
        settle(&mut f, parent, true).data,
        SocketEventData::Ready { .. }
    ));
    assert_eq!(
        settle(&mut f, parent, true).data,
        SocketEventData::Accept(permit.child)
    );
}

#[test]
fn child_terminal_delivery_is_gated_by_its_pending_accept() {
    let mut f = fixture(2, 2, 1);
    let parent = listener(&mut f);
    settle(&mut f, parent, true);
    let child = accept(&mut f, parent);
    let txn = f.begin();
    f.access(&txn).close(&f.owner, child).unwrap();
    f.access(&txn).closed(child);
    f.commit(&txn);
    f.commands.recv().unwrap();
    assert!(f.access(&f.begin()).lease(child).unwrap().is_none());
    assert_eq!(
        settle(&mut f, parent, true).data,
        SocketEventData::Accept(child)
    );
    assert!(f.commands.is_empty());
    assert!(matches!(
        settle(&mut f, child, false).data,
        SocketEventData::Closed { .. }
    ));
    assert!(f.access(&f.begin()).info(child).is_none());
}

#[test]
fn owner_retirement_discards_pending_accepts_and_invalidates_an_old_delivery() {
    let mut f = fixture(3, 2, 2);
    let parent = listener(&mut f);
    settle(&mut f, parent, true);
    let child = accept(&mut f, parent);
    let recipient = Arc::new(Process::default());
    let txn = f.begin();
    f.access(&txn)
        .transfer(child, &f.owner, &recipient, callback(&recipient))
        .unwrap();
    f.commit(&txn);
    let pending = reserve(&mut f, parent);
    let attempt = f.begin();
    let lease = f.access(&attempt).lease(parent).unwrap().unwrap();
    f.access(&attempt).acknowledge(&lease, true).unwrap();
    let retire = f.begin();
    retire.with(|txn| f.sockets.retire_object(txn, &f.owner));
    f.commit(&retire);
    assert!(
        f.committer
            .commit(attempt.with(Transaction::take_changeset))
            .is_err()
    );
    let access = f.access(&f.begin());
    assert!(access.query(None).unwrap().is_empty());
    assert!(access.query(Some(&recipient)).unwrap().is_empty());
    assert!(access.info(pending.child).is_none());
    f.gc();
    assert_eq!(f.commands.try_iter().count(), 1);
    f.connect();
}
