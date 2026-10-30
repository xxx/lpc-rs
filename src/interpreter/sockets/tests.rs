use lpc_rs_utils::config::ConfigBuilder;
use thin_vec::thin_vec;
use ustr::ustr;

use super::*;
use crate::interpreter::{
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
    stm::{CommitProtocol, Committer, Effect, WorldRoot},
};

pub(super) struct Fixture {
    pub(super) sockets: Sockets,
    pub(super) committer: Committer,
    pub(super) commands: flume::Receiver<SocketCommands>,
    pub(super) owner: Arc<Process>,
}

impl Fixture {
    fn new(count: usize, bytes: usize, writes: usize) -> Self {
        let config = ConfigBuilder::default()
            .max_sockets(count)
            .max_pending_socket_output(bytes)
            .max_pending_socket_writes(writes)
            .build()
            .unwrap();
        Self::from_config(config)
    }

    pub(super) fn from_config(config: Config) -> Self {
        let sockets = Sockets::new(&config);
        let commands = sockets.take_commands().unwrap();
        Self {
            sockets,
            commands,
            committer: Committer::new(),
            owner: Arc::default(),
        }
    }

    pub(super) fn begin(&self) -> TxnHandle {
        TxnHandle::new(Transaction::new(self.committer.snapshot_clone()))
    }

    pub(super) fn access<'a>(&'a self, txn: &TxnHandle) -> SocketAccess<'a> {
        SocketAccess {
            sockets: &self.sockets,
            txn: txn.clone(),
        }
    }

    pub(super) fn commit(&mut self, txn: &TxnHandle) {
        self.committer
            .commit(txn.with(Transaction::take_changeset))
            .unwrap();
    }

    pub(super) fn connect(&mut self) -> SocketHandle {
        let txn = self.begin();
        let handle = self
            .access(&txn)
            .connect(&self.owner, "localhost".into(), 9, callback(&self.owner))
            .unwrap();
        self.commit(&txn);
        assert!(
            matches!(&self.commands.try_recv().unwrap()[..], [SocketCommand::Open { handle: found, .. }] if *found == handle)
        );
        handle
    }

    pub(super) fn open(&mut self, handle: SocketHandle) {
        let txn = self.begin();
        assert!(
            self.access(&txn)
                .ready(handle, endpoint(1000), Some(endpoint(9)))
                .unwrap()
        );
        self.commit(&txn);
    }

    pub(super) fn gc(&mut self) {
        let (tx, _) = flume::unbounded();
        let (reply, rx) = flume::bounded(1);
        self.committer.process(
            CommitProtocol::GcPass {
                roots: self
                    .sockets
                    .root()
                    .map(WorldRoot::Var)
                    .into_iter()
                    .collect(),
                reply,
            },
            &tx,
        );
        rx.recv().unwrap().unwrap();
    }
}

pub(super) fn endpoint(port: u16) -> SocketAddr {
    SocketAddr::from(([127, 0, 0, 1], port))
}

pub(super) fn callback(owner: &Arc<Process>) -> Arc<FunctionPtr> {
    Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(owner))
            .address(FunctionAddress::Efun(ustr("dump")))
            .build()
            .unwrap(),
    )
}

#[test]
fn unused_sockets_allocate_no_cells_or_channels() {
    let sockets = Sockets::new(&Config::default());
    assert!(sockets.root().is_none());
    assert!(sockets.queue.get().is_none());
    let owner = Process::default();
    let mut txn = Transaction::new(Committer::new().snapshot_clone());
    sockets.retire_object(&mut txn, &owner);
    assert!(txn.is_clean());
    assert!(owner.socket_index.get().is_none());
    assert!(sockets.queue.get().is_none());
}

#[test]
fn aborted_creation_publishes_nothing_and_never_reuses_its_handle() {
    let mut f = Fixture::new(1, 8, 2);
    let txn = f.begin();
    let handle = f
        .access(&txn)
        .connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
        .unwrap();
    assert!(f.access(&txn).info(handle).is_some());
    assert!(f.access(&f.begin()).info(handle).is_none());
    drop(txn);
    assert!(f.commands.is_empty());
    let next = f.connect();
    assert!(next.get() > handle.get());
    assert!(f.access(&f.begin()).info(handle).is_none());
}

#[test]
fn concurrent_creations_cannot_overbook_the_vm() {
    let mut f = Fixture::new(1, 8, 2);
    let first = f.begin();
    let second = f.begin();
    let handle = f
        .access(&first)
        .connect(&f.owner, "one".into(), 1, callback(&f.owner))
        .unwrap();
    f.access(&second)
        .connect(&f.owner, "two".into(), 2, callback(&f.owner))
        .unwrap();
    f.commit(&first);
    assert!(
        f.committer
            .commit(second.with(Transaction::take_changeset))
            .is_err()
    );
    let retry = f.begin();
    assert!(
        f.access(&retry)
            .connect(&f.owner, "two".into(), 2, callback(&f.owner))
            .is_err()
    );
    assert_eq!(f.access(&retry).query(None).unwrap().len(), 1);
    assert!(
        matches!(&f.commands.recv().unwrap()[..], [SocketCommand::Open { handle: found, .. }] if *found == handle)
    );
    assert!(f.commands.is_empty());
}

#[test]
fn empty_queries_and_missing_handle_reads_track_first_creation() {
    for selection in 0..3 {
        let mut f = Fixture::new(1, 4, 2);
        let empty = f.begin();
        match selection {
            0 => assert!(f.access(&empty).query(None).unwrap().is_empty()),
            1 => assert!(f.access(&empty).query(Some(&f.owner)).unwrap().is_empty()),
            _ => assert!(
                f.access(&empty)
                    .info(SocketHandle::try_from(1).unwrap())
                    .is_none()
            ),
        }
        f.connect();
        assert!(
            f.committer
                .commit(empty.with(Transaction::take_changeset))
                .is_err()
        );
        assert_eq!(f.access(&f.begin()).query(None).unwrap().len(), 1);
    }
}

#[test]
fn whole_buffer_admission_preserves_binary_bytes_and_both_limits() {
    let mut f = Fixture::new(2, 5, 2);
    let handle = f.connect();
    let txn = f.begin();
    let access = f.access(&txn);
    let data = Bytes::from_static(b"a\0b");
    assert!(access.write(&f.owner, handle, data.clone()).unwrap());
    assert!(
        !access
            .write(&f.owner, handle, Bytes::from_static(b"xyz"))
            .unwrap()
    );
    assert!(
        access
            .write(&f.owner, handle, Bytes::from_static(b"c"))
            .unwrap()
    );
    assert!(
        !access
            .write(&f.owner, handle, Bytes::from_static(b"d"))
            .unwrap()
    );
    assert!(access.write(&f.owner, handle, Bytes::new()).unwrap());
    assert!(
        access
            .write(&f.owner, handle, Bytes::from_static(b"123456"))
            .is_err()
    );
    assert_eq!(access.info(handle).unwrap().output_pending, 4);
    f.commit(&txn);
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[
            SocketCommand::Write {
                handle,
                sequence: 1,
                data
            },
            SocketCommand::Write {
                handle,
                sequence: 2,
                data: Bytes::from_static(b"c")
            },
        ]
    );
}

#[test]
fn conflicted_write_emits_nothing_and_retry_observes_the_winners_budget() {
    let mut f = Fixture::new(2, 4, 2);
    let handle = f.connect();
    let first = f.begin();
    let second = f.begin();
    for txn in [&first, &second] {
        assert!(
            f.access(txn)
                .write(&f.owner, handle, Bytes::from_static(b"abc"))
                .unwrap()
        );
    }
    f.commit(&first);
    assert!(
        f.committer
            .commit(second.with(Transaction::take_changeset))
            .is_err()
    );
    let retry = f.begin();
    assert!(
        !f.access(&retry)
            .write(&f.owner, handle, Bytes::from_static(b"abc"))
            .unwrap()
    );
    f.commit(&retry);
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Write {
            handle,
            sequence: 1,
            data: Bytes::from_static(b"abc"),
        }]
    );
    assert!(f.commands.is_empty());
}

#[test]
fn writes_on_independent_sockets_commit_from_the_same_snapshot() {
    let mut f = Fixture::new(2, 4, 2);
    let a = f.connect();
    let b = f.connect();
    let first = f.begin();
    let second = f.begin();
    f.access(&first)
        .write(&f.owner, a, Bytes::from_static(b"a"))
        .unwrap();
    f.access(&second)
        .write(&f.owner, b, Bytes::from_static(b"b"))
        .unwrap();
    f.commit(&first);
    f.commit(&second);
    assert_eq!(f.commands.len(), 2);
}

#[test]
fn drain_completion_is_ordered_idempotent_and_restores_admission() {
    let mut f = Fixture::new(2, 4, 2);
    let handle = f.connect();
    let txn = f.begin();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"ab"))
        .unwrap();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"cd"))
        .unwrap();
    f.commit(&txn);
    f.commands.recv().unwrap();
    let txn = f.begin();
    let access = f.access(&txn);
    assert!(access.complete_write(handle, 2).is_err());
    assert_eq!(access.info(handle).unwrap().output_pending, 4);
    assert!(access.complete_write(handle, 1).unwrap());
    assert!(!access.complete_write(handle, 1).unwrap());
    assert_eq!(access.info(handle).unwrap().output_pending, 2);
    assert!(
        access
            .write(&f.owner, handle, Bytes::from_static(b"ef"))
            .unwrap()
    );
    f.commit(&txn);
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Write {
            handle,
            sequence: 3,
            data: Bytes::from_static(b"ef"),
        }]
    );
}

#[test]
fn aborting_completion_does_not_release_the_committed_budget() {
    let mut f = Fixture::new(1, 1, 1);
    let handle = f.connect();
    let txn = f.begin();
    f.access(&txn)
        .write(&f.owner, handle, Bytes::from_static(b"a"))
        .unwrap();
    f.commit(&txn);
    f.commands.recv().unwrap();
    let aborted = f.begin();
    f.access(&aborted).complete_write(handle, 1).unwrap();
    drop(aborted);
    assert!(
        !f.access(&f.begin())
            .write(&f.owner, handle, Bytes::from_static(b"b"))
            .unwrap()
    );
    assert!(f.commands.is_empty());
}

#[test]
fn ownership_change_and_output_completion_do_not_conflict() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    f.open(handle);
    let recipient = Arc::new(Process::default());
    let seed = f.begin();
    f.access(&seed)
        .write(&f.owner, handle, Bytes::from_static(b"abc"))
        .unwrap();
    f.commit(&seed);
    f.commands.recv().unwrap();
    let handover = f.begin();
    let drain = f.begin();
    f.access(&handover)
        .transfer(handle, &f.owner, &recipient, callback(&recipient))
        .unwrap();
    f.access(&drain).complete_write(handle, 1).unwrap();
    f.commit(&drain);
    f.commit(&handover);
    let inspect = f.begin();
    assert_eq!(f.access(&inspect).info(handle).unwrap().output_pending, 0);
    assert!(f.access(&inspect).query(Some(&f.owner)).unwrap().is_empty());
    assert_eq!(f.access(&inspect).query(Some(&recipient)).unwrap().len(), 1);
}

#[test]
fn transfer_invalidates_an_old_owners_pending_write() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    f.open(handle);
    let recipient = Arc::new(Process::default());
    let write = f.begin();
    let transfer = f.begin();
    f.access(&write)
        .write(&f.owner, handle, Bytes::from_static(b"old"))
        .unwrap();
    f.access(&transfer)
        .transfer(handle, &f.owner, &recipient, callback(&recipient))
        .unwrap();
    f.commit(&transfer);
    assert!(
        f.committer
            .commit(write.with(Transaction::take_changeset))
            .is_err()
    );
    assert!(
        f.access(&f.begin())
            .write(&f.owner, handle, Bytes::from_static(b"old"))
            .is_err()
    );
    assert!(
        f.access(&f.begin())
            .write(&recipient, handle, Bytes::from_static(b"new"))
            .unwrap()
    );
    assert!(f.commands.is_empty());
}

#[test]
fn aborted_transfer_preserves_owner_registration_and_inventories() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    f.open(handle);
    let recipient = Arc::new(Process::default());
    let txn = f.begin();
    let (_, before) = txn.with(|t| f.sockets.record(t, handle)).unwrap();
    f.access(&txn)
        .transfer(handle, &f.owner, &recipient, callback(&recipient))
        .unwrap();
    drop(txn);
    let inspect = f.begin();
    let (_, after) = inspect.with(|t| f.sockets.record(t, handle)).unwrap();
    assert_eq!(before, after);
    assert_eq!(f.access(&inspect).query(Some(&f.owner)).unwrap().len(), 1);
    assert!(
        f.access(&inspect)
            .query(Some(&recipient))
            .unwrap()
            .is_empty()
    );
}

#[test]
fn close_cancels_setup_and_is_idempotent_within_the_transaction() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    let txn = f.begin();
    let access = f.access(&txn);
    access
        .write(&f.owner, handle, Bytes::from_static(b"abc"))
        .unwrap();
    access.close(&f.owner, handle).unwrap();
    access.close(&f.owner, handle).unwrap();
    assert!(access.write(&f.owner, handle, Bytes::new()).is_err());
    assert!(
        !access
            .ready(handle, endpoint(1000), Some(endpoint(9)))
            .unwrap()
    );
    f.commit(&txn);
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[
            SocketCommand::Write {
                handle,
                sequence: 1,
                data: Bytes::from_static(b"abc")
            },
            SocketCommand::Close {
                handle,
                drain: false
            },
        ]
    );
}

#[test]
fn retirement_cannot_reuse_quota_while_its_commands_and_buffers_are_pending() {
    let mut f = Fixture::new(1, 8, 1);
    let txn = f.begin();
    let handle = f
        .access(&txn)
        .connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
        .unwrap();
    assert!(
        f.access(&txn)
            .write(&f.owner, handle, Bytes::copy_from_slice(b"12345678"))
            .unwrap()
    );
    txn.with(|t| f.sockets.retire_object(t, &f.owner));
    assert!(f.access(&txn).info(handle).is_none());
    assert!(
        f.access(&txn)
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_err()
    );
    f.commit(&txn);
    for _ in 0..32 {
        assert!(
            f.access(&f.begin())
                .connect(&f.owner, "other".into(), 9, callback(&f.owner))
                .is_err()
        );
    }
    let batch = f.commands.recv().unwrap();
    assert!(
        matches!(&batch[..], [SocketCommand::Open { .. }, SocketCommand::Write { data, .. }, SocketCommand::Retire { handle: retired }] if data.len() == 8 && *retired == handle)
    );
    drop(batch);
    let txn = f.begin();
    assert!(f.access(&txn).disposed(handle));
    f.commit(&txn);
    assert!(
        f.access(&f.begin())
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_ok()
    );
}

#[test]
fn aborted_disposal_keeps_a_retired_sockets_quota_charge() {
    let mut f = Fixture::new(1, 8, 1);
    let handle = f.connect();
    let txn = f.begin();
    txn.with(|t| f.sockets.retire_object(t, &f.owner));
    f.commit(&txn);
    f.commands.recv().unwrap();
    let aborted = f.begin();
    assert!(f.access(&aborted).disposed(handle));
    drop(aborted);
    assert!(
        f.access(&f.begin())
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_err()
    );
    let txn = f.begin();
    assert!(f.access(&txn).disposed(handle));
    assert!(!f.access(&txn).disposed(handle));
    f.commit(&txn);
    assert!(
        f.access(&f.begin())
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_ok()
    );
}

#[test]
fn late_completions_and_repeated_retirement_cannot_revive_a_retired_socket() {
    let mut f = Fixture::new(1, 8, 1);
    let handle = f.connect();
    let txn = f.begin();
    assert!(!f.access(&txn).disposed(handle));
    assert!(
        f.access(&txn)
            .write(&f.owner, handle, Bytes::from_static(b"x"))
            .unwrap()
    );
    txn.with(|t| f.sockets.retire_object(t, &f.owner));
    txn.with(|t| f.sockets.retire_object(t, &f.owner));
    assert!(!f.access(&txn).complete_write(handle, 1).unwrap());
    assert!(
        !f.access(&txn)
            .ready(handle, endpoint(1), Some(endpoint(2)))
            .unwrap()
    );
    assert!(f.access(&txn).lease(handle).unwrap().is_none());
    f.commit(&txn);
    assert!(matches!(
        &f.commands.recv().unwrap()[..],
        [SocketCommand::Write { .. }, SocketCommand::Retire { .. }]
    ));
}

#[test]
fn established_close_drains_and_invalidates_a_concurrent_write() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    f.open(handle);
    let closing = f.begin();
    let writing = f.begin();
    f.access(&closing).close(&f.owner, handle).unwrap();
    f.access(&writing)
        .write(&f.owner, handle, Bytes::from_static(b"late"))
        .unwrap();
    f.commit(&closing);
    assert!(
        f.committer
            .commit(writing.with(Transaction::take_changeset))
            .is_err()
    );
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Close {
            handle,
            drain: true
        }]
    );
    assert!(f.commands.is_empty());
}

#[test]
fn terminal_records_hold_quota_until_dispatch_cleanup() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    let txn = f.begin();
    f.access(&txn).closed(handle);
    f.commit(&txn);
    let txn = f.begin();
    assert!(
        f.access(&txn)
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_err()
    );
    assert_eq!(
        f.access(&txn).info(handle).unwrap().state,
        SocketState::Closed
    );
    f.access(&txn).finish_closed(handle).unwrap();
    assert!(f.access(&txn).info(handle).is_none());
    assert!(
        f.access(&txn)
            .connect(&f.owner, "other".into(), 9, callback(&f.owner))
            .is_err()
    );
    f.commit(&txn);
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Retire { handle }]
    );
    let txn = f.begin();
    assert!(f.access(&txn).disposed(handle));
    let next = f
        .access(&txn)
        .connect(&f.owner, "other".into(), 9, callback(&f.owner))
        .unwrap();
    f.commit(&txn);
    assert_ne!(next, handle);
    assert_eq!(f.commands.recv().unwrap().len(), 1);
}

#[test]
fn owner_retirement_rolls_back_or_discards_output_after_commit() {
    let mut f = Fixture::new(2, 4, 2);
    let handle = f.connect();
    let aborted = f.begin();
    aborted.with(|t| f.sockets.retire_object(t, &f.owner));
    assert!(f.access(&aborted).info(handle).is_none());
    drop(aborted);
    assert!(f.access(&f.begin()).info(handle).is_some());
    assert!(f.commands.is_empty());
    let txn = f.begin();
    txn.with(|t| f.sockets.retire_object(t, &f.owner));
    f.commit(&txn);
    assert!(f.access(&f.begin()).info(handle).is_none());
    assert_eq!(
        &f.commands.recv().unwrap()[..],
        &[SocketCommand::Retire { handle }]
    );
}

#[test]
fn socket_commands_publish_before_reply_even_when_its_receiver_is_gone() {
    let mut f = Fixture::new(1, 4, 2);
    let txn = f.begin();
    let handle = f
        .access(&txn)
        .connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
        .unwrap();
    let (tx, _) = flume::unbounded();
    let (reply, rx) = flume::bounded(1);
    drop(rx);
    f.committer.process(
        CommitProtocol::Commit {
            changeset: txn.with(Transaction::take_changeset),
            releases_base: false,
            reply,
        },
        &tx,
    );
    assert_eq!(f.commands.recv().unwrap().len(), 1);
    assert!(f.access(&f.begin()).info(handle).is_some());
    assert!(f.commands.is_empty());
}

#[test]
fn delayed_effect_delivery_cannot_reorder_committed_writes() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    let first = f.begin();
    first.with(|t| t.record_effect(Effect::DebugLog("first".into())));
    f.access(&first)
        .write(&f.owner, handle, Bytes::from_static(b"a"))
        .unwrap();
    f.commit(&first);
    let second = f.begin();
    second.with(|t| t.record_effect(Effect::DebugLog("second".into())));
    f.access(&second)
        .write(&f.owner, handle, Bytes::from_static(b"b"))
        .unwrap();
    f.commit(&second);
    let second_effects = second.with(Transaction::take_effects);
    let first_effects = first.with(Transaction::take_effects);
    assert_eq!(second_effects.len(), 1);
    assert_eq!(first_effects.len(), 1);
    for (sequence, data) in [(1, b"a".as_slice()), (2, b"b".as_slice())] {
        assert!(
            matches!(&f.commands.recv().unwrap()[..], [SocketCommand::Write { sequence: actual, data: bytes, .. }] if *actual == sequence && bytes.as_ref() == data)
        );
    }
}

#[test]
fn registry_roots_keep_callback_captures_until_terminal_cleanup() {
    let mut f = Fixture::new(1, 4, 2);
    let txn = f.begin();
    let capture = VarId::new();
    txn.with(|t| t.write(capture, 42.into()));
    let ptr = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&f.owner))
            .address(FunctionAddress::Efun(ustr("dump")))
            .upvalue_ptrs(thin_vec![capture])
            .build()
            .unwrap(),
    );
    let handle = f
        .access(&txn)
        .connect(&f.owner, "localhost".into(), 9, ptr)
        .unwrap();
    f.commit(&txn);
    f.commands.recv().unwrap();
    f.gc();
    assert!(f.committer.snapshot_clone().read(capture).is_some());
    assert!(f.access(&f.begin()).info(handle).is_some());
    let txn = f.begin();
    f.access(&txn).closed(handle);
    f.access(&txn).finish_closed(handle).unwrap();
    f.commit(&txn);
    f.gc();
    assert!(f.committer.snapshot_clone().read(capture).is_none());
}

#[test]
fn listener_and_stale_handle_validation_publish_no_invalid_commands() {
    let mut f = Fixture::new(1, 4, 2);
    let txn = f.begin();
    let access = f.access(&txn);
    assert!(SocketHandle::try_from(0).is_err());
    assert!(SocketHandle::try_from(-1).is_err());
    assert!(
        access
            .connect(&f.owner, "localhost".into(), 0, callback(&f.owner))
            .is_err()
    );
    let handle = access
        .listen(&f.owner, endpoint(0), callback(&f.owner))
        .unwrap();
    assert!(access.ready(handle, endpoint(1000), None).unwrap());
    assert!(
        access
            .write(&f.owner, handle, Bytes::from_static(b"a"))
            .is_err()
    );
    assert!(
        access
            .transfer(handle, &f.owner, &Arc::default(), callback(&f.owner))
            .is_err()
    );
    assert!(access.close(&Arc::default(), handle).is_err());
    assert!(
        access
            .close(&f.owner, SocketHandle::try_from(100).unwrap())
            .is_err()
    );
    assert!(access.finish_closed(handle).is_err());
    f.commit(&txn);
    assert!(
        matches!(&f.commands.recv().unwrap()[..], [SocketCommand::Open { target: SocketTarget::Listen(address), .. }] if address.port() == 0)
    );
    assert!(f.commands.is_empty());
}

#[test]
fn handle_exhaustion_and_transport_failure_leave_no_world_changes() {
    let f = Fixture::new(1, 4, 2);
    f.sockets.next_handle.store(i64::MAX, Ordering::Relaxed);
    let txn = f.begin();
    assert!(
        f.access(&txn)
            .connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
            .is_err()
    );
    assert!(txn.with(|t| t.is_clean()));
    assert!(f.commands.is_empty());
    drop(f.commands);
    let access = SocketAccess {
        sockets: &f.sockets,
        txn: txn.clone(),
    };
    assert!(
        access
            .connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
            .is_err()
    );
    assert!(txn.with(|t| t.is_clean()));
}

#[tokio::test]
async fn native_destruction_and_gc_follow_the_socket_registry() {
    use crate::interpreter::{
        call_stack::CallStack,
        efun::{Efun, efun_context::EfunContext},
        stm::{commit_changeset, start_txn},
        vm::global_state::GlobalState,
    };

    let (tx, _) = tokio::sync::mpsc::channel(16);
    let global = Arc::new(GlobalState::new(Config::default(), tx));
    let owner = Arc::new(Process::default());
    crate::interpreter::object_space::ObjectSpace::insert_process_physical(
        &global.object_space,
        owner.clone(),
    );
    let commands = global.sockets.take_commands().unwrap();
    let capture = VarId::new();
    let live = start_txn(&global.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    txn.with(|t| t.write(capture, 42.into()));
    let ptr = Arc::new(
        FunctionPtrBuilder::default()
            .owner(Arc::downgrade(&owner))
            .address(FunctionAddress::Efun(ustr("dump")))
            .upvalue_ptrs(thin_vec![capture])
            .build()
            .unwrap(),
    );
    let mut context = TaskContext::new(global.clone(), owner.clone(), None);
    context.txn = txn.clone();
    let handle = SocketAccess::new(&context)
        .connect(&owner, "localhost".into(), 9, ptr)
        .unwrap();
    commit_changeset(&global.committer_tx, txn.with(Transaction::take_changeset))
        .await
        .unwrap()
        .unwrap();
    drop(live);
    commands.recv().unwrap();
    global.gc().await.unwrap().unwrap();

    let live = start_txn(&global.committer_tx).await.unwrap();
    context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    assert!(context.txn.with(|t| t.read(capture)).is_some());
    assert!(SocketAccess::new(&context).info(handle).is_some());
    let mut stack = CallStack::<10>::default();
    EfunContext::new(&mut stack, &context, Efun::destruct).remove_process(owner.clone());
    assert!(SocketAccess::new(&context).info(handle).is_none());
    assert!(commands.is_empty());
    drop(live);

    let live = start_txn(&global.committer_tx).await.unwrap();
    context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    assert!(SocketAccess::new(&context).info(handle).is_some());
    assert!(
        SocketAccess::new(&context)
            .write(&owner, handle, Bytes::new())
            .unwrap()
    );
    EfunContext::new(&mut stack, &context, Efun::destruct).remove_process(owner.clone());
    commit_changeset(
        &global.committer_tx,
        context.txn.with(Transaction::take_changeset),
    )
    .await
    .unwrap()
    .unwrap();
    drop(live);
    assert_eq!(
        &commands.recv().unwrap()[..],
        &[SocketCommand::Retire { handle }]
    );
    global.gc().await.unwrap().unwrap();

    let live = start_txn(&global.committer_tx).await.unwrap();
    context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    assert!(context.txn.with(|t| t.read(capture)).is_none());
    assert!(SocketAccess::new(&context).info(handle).is_none());
    assert!(commands.is_empty());
    drop(live);
}

#[tokio::test]
async fn caught_initializer_failure_retires_the_failed_owners_sockets() {
    use crate::{
        interpreter::{
            stm::{commit_changeset, start_txn},
            vm::global_state::GlobalState,
        },
        util::process_builder::compile_process_from_code,
    };
    let (tx, _) = tokio::sync::mpsc::channel(16);
    let global = Arc::new(GlobalState::new(Config::default(), tx));
    let (owner, _) = compile_process_from_code(
        &global.object_space,
        "/failed.c",
        "void create() { throw(\"failed initializer\"); }",
        None,
        None,
    )
    .await
    .unwrap();
    let live = start_txn(&global.committer_tx).await.unwrap();
    let mut context = TaskContext::new(global.clone(), Arc::new(Process::default()), None);
    context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    let commands = global.sockets.take_commands().unwrap();
    let handle = SocketAccess::new(&context)
        .connect(&owner, "localhost".into(), 9, callback(&owner))
        .unwrap();
    assert!(context.insert_and_initialize(None, &owner).await.is_err());
    assert!(SocketAccess::new(&context).info(handle).is_none());
    assert!(SocketAccess::new(&context).query(None).unwrap().is_empty());
    assert!(commands.is_empty());
    commit_changeset(
        &global.committer_tx,
        context.txn.with(Transaction::take_changeset),
    )
    .await
    .unwrap()
    .unwrap();
    drop(live);
    assert_eq!(
        &commands.recv().unwrap()[..],
        &[
            SocketCommand::Open {
                handle,
                target: SocketTarget::Connect {
                    host: "localhost".into(),
                    port: 9
                }
            },
            SocketCommand::Retire { handle },
        ]
    );
}

#[test]
fn committed_record_snapshots_distinguish_lifecycle_and_callback_changes() {
    let mut f = Fixture::new(1, 4, 2);
    let handle = f.connect();
    let snapshot = |f: &Fixture| {
        f.begin()
            .with(|txn| f.sockets.record(txn, handle))
            .unwrap()
            .1
    };
    let connecting = snapshot(&f);
    assert_eq!(connecting, connecting.clone());
    f.open(handle);
    let open = snapshot(&f);
    assert_ne!(connecting, open);
    assert_eq!(open, snapshot(&f));

    let txn = f.begin();
    f.access(&txn)
        .transfer(handle, &f.owner, &f.owner, callback(&f.owner))
        .unwrap();
    f.commit(&txn);
    let replaced_callback = snapshot(&f);
    assert_ne!(open, replaced_callback);
    assert_eq!(replaced_callback, snapshot(&f));

    let txn = f.begin();
    f.access(&txn).close(&f.owner, handle).unwrap();
    f.commit(&txn);
    let closing = snapshot(&f);
    assert_ne!(replaced_callback, closing);
    assert_eq!(closing, snapshot(&f));
}

#[test]
fn mismatched_ready_endpoints_leave_setup_retryable_and_publish_nothing() {
    for listening in [false, true] {
        let mut f = Fixture::new(1, 4, 2);
        let txn = f.begin();
        let access = f.access(&txn);
        let handle = if listening {
            access.listen(&f.owner, endpoint(0), callback(&f.owner))
        } else {
            access.connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
        }
        .unwrap();
        f.commit(&txn);
        assert!(
            matches!(&f.commands.try_recv().unwrap()[..], [SocketCommand::Open { handle: h, .. }] if *h == handle)
        );

        let txn = f.begin();
        let access = f.access(&txn);
        let remote = (!listening).then_some(endpoint(9));
        let wrong_remote = listening.then_some(endpoint(9));
        assert!(access.ready(handle, endpoint(1000), wrong_remote).is_err());
        assert!(txn.with(|t| t.is_clean()));
        assert!(access.lease(handle).unwrap().is_none());
        let info = access.info(handle).unwrap();
        assert_eq!(
            info.state,
            if listening {
                SocketState::Binding
            } else {
                SocketState::Connecting
            }
        );
        assert_eq!(info.local, None);
        assert_eq!(info.remote, None);
        assert!(access.ready(handle, endpoint(1000), remote).unwrap());
        f.commit(&txn);
        assert!(f.commands.is_empty());
        let txn = f.begin();
        let lease = f.access(&txn).lease(handle).unwrap().unwrap();
        assert_eq!(
            lease.event().data,
            SocketEventData::Ready {
                local: endpoint(1000),
                remote
            }
        );
    }
}

#[test]
fn destroyed_owners_cannot_create_socket_registrations() {
    let mut f = Fixture::new(1, 4, 2);
    let txn = f.begin();
    let cell = *f.owner.cell.get_or_init(VarId::new);
    txn.with(|t| t.remove_object(cell, f.owner.clone()));
    assert!(!f.owner.is_live(&txn));
    let access = f.access(&txn);
    assert!(
        access
            .connect(&f.owner, "localhost".into(), 9, callback(&f.owner))
            .is_err()
    );
    assert!(
        access
            .listen(&f.owner, endpoint(0), callback(&f.owner))
            .is_err()
    );
    assert!(access.query(None).unwrap().is_empty());
    f.commit(&txn);
    assert!(f.commands.is_empty());
    assert!(f.access(&f.begin()).query(None).unwrap().is_empty());
}
