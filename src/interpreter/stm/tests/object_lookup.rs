use std::sync::Arc;

use lpc_rs_core::lpc_path::LpcPath;

use crate::interpreter::{
    lpc_ref::LpcRef,
    object_space::ObjectSpace,
    process::Process,
    program::ProgramBuilder,
    stm::{
        Changeset, Committer, Conflict, ObjectChange, Transaction, TxnHandle, VarId,
        txn_find_object, txn_insert_process, txn_undo_insert,
    },
    task_context::ObjectLookup,
};

fn bootstrap_fixture() -> (Committer, Arc<ObjectSpace>, Arc<Process>) {
    let mut committer = Committer::new();
    let space = Arc::new(ObjectSpace::default());
    let process = Arc::new(Process::new(
        ProgramBuilder::default()
            .filename(LpcPath::from("/victim.c"))
            .build()
            .unwrap(),
    ));
    ObjectSpace::insert_process_physical(&space, process.clone());
    let mut placement = Changeset::new(committer.current_version());
    placement
        .object_changes
        .push(ObjectChange::new(&process, true));
    committer.commit(placement).unwrap();
    (committer, space, process)
}

#[test]
fn bootstrap_lookup_preserves_snapshots_across_committed_removal_and_flush() {
    let (mut committer, space, victim) = bootstrap_fixture();
    let old = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    let removal = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_undo_insert(&removal, &space, &victim);
    assert!(matches!(
        txn_find_object(&removal, &space, &"/victim".into()),
        ObjectLookup::Removed
    ));
    committer
        .commit(removal.with(|t| t.take_changeset()))
        .unwrap();
    let fresh = TxnHandle::new(Transaction::new(committer.snapshot_clone()));

    for flush in [false, true] {
        if flush {
            space.apply_remove("/victim", &victim);
        } else {
            assert!(space.lookup("/victim").is_some());
        }
        assert!(matches!(
            txn_find_object(&old, &space, &"/victim.c".into()),
            ObjectLookup::Found(found) if Arc::ptr_eq(&found, &victim)
        ));
        assert!(matches!(
            txn_find_object(&fresh, &space, &"/victim".into()),
            ObjectLookup::NotCreated
        ));
    }
    assert!(matches!(
        committer.commit(old.with(|t| t.take_changeset())),
        Err(Conflict::ReadInvalidated { .. })
    ));
}

#[test]
fn bootstrap_lookup_reads_conflict_with_removal_after_an_aborted_attempt() {
    let (mut committer, space, victim) = bootstrap_fixture();
    let aborted = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_undo_insert(&aborted, &space, &victim);
    drop(aborted);
    let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    assert!(matches!(
        txn_find_object(&reader, &space, &"/victim".into()),
        ObjectLookup::Found(found) if Arc::ptr_eq(&found, &victim)
    ));
    let removal = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_undo_insert(&removal, &space, &victim);
    committer
        .commit(removal.with(|t| t.take_changeset()))
        .unwrap();

    assert!(matches!(
        committer.commit(reader.with(|t| t.take_changeset())),
        Err(Conflict::ReadInvalidated { cell, .. }) if cell == *victim.cell.get().unwrap()
    ));
}

#[test]
fn bootstrap_lookup_uses_pending_and_committed_replacements_before_fallback() {
    let (mut committer, space, victim) = bootstrap_fixture();
    let old = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    let replacement = Arc::new(Process::new(victim.initial_program().clone()));
    let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_undo_insert(&writer, &space, &victim);
    txn_insert_process(&writer, &space, &replacement);
    assert!(matches!(
        txn_find_object(&writer, &space, &"/victim".into()),
        ObjectLookup::Found(found) if Arc::ptr_eq(&found, &replacement)
    ));
    committer
        .commit(writer.with(|t| t.take_changeset()))
        .unwrap();
    let fresh = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    for flush in [false, true] {
        if flush {
            space.apply_remove("/victim", &victim);
            space.apply_insert("/victim", replacement.clone());
        }
        assert!(matches!(
            txn_find_object(&old, &space, &"/victim".into()),
            ObjectLookup::Found(found) if Arc::ptr_eq(&found, &victim)
        ));
        assert!(matches!(
            txn_find_object(&fresh, &space, &"/victim".into()),
            ObjectLookup::Found(found) if Arc::ptr_eq(&found, &replacement)
        ));
    }
    txn_undo_insert(&fresh, &space, &replacement);
    assert!(matches!(
        txn_find_object(&fresh, &space, &"/victim".into()),
        ObjectLookup::Removed
    ));
}

#[test]
fn bootstrap_lookup_does_not_add_dependencies_on_unrelated_writes() {
    let (mut committer, space, victim) = bootstrap_fixture();
    let aborted = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_undo_insert(&aborted, &space, &victim);
    drop(aborted);
    let reader = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    assert!(matches!(
        txn_find_object(&reader, &space, &"/victim".into()),
        ObjectLookup::Found(found) if Arc::ptr_eq(&found, &victim)
    ));
    let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    writer.with(|t| t.write(VarId::new(), LpcRef::from(42)));
    let other = Arc::new(Process::new(
        ProgramBuilder::default()
            .filename(LpcPath::from("/other.c"))
            .build()
            .unwrap(),
    ));
    txn_insert_process(&writer, &space, &other);
    committer
        .commit(writer.with(|t| t.take_changeset()))
        .unwrap();

    assert!(
        committer
            .commit(reader.with(|t| t.take_changeset()))
            .is_ok()
    );
}

#[test]
fn lookup_does_not_expose_aborted_or_future_transactional_insertions() {
    let (mut committer, space, _) = bootstrap_fixture();
    let process = Arc::new(Process::new(
        ProgramBuilder::default()
            .filename(LpcPath::from("/other.c"))
            .build()
            .unwrap(),
    ));
    let aborted = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_insert_process(&aborted, &space, &process);
    drop(aborted);
    let old = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    assert!(matches!(
        txn_find_object(&old, &space, &"/other".into()),
        ObjectLookup::NotCreated
    ));
    let writer = TxnHandle::new(Transaction::new(committer.snapshot_clone()));
    txn_insert_process(&writer, &space, &process);
    committer
        .commit(writer.with(|t| t.take_changeset()))
        .unwrap();
    space.apply_insert("/other", process);

    assert!(matches!(
        txn_find_object(&old, &space, &"/other".into()),
        ObjectLookup::NotCreated
    ));
}
