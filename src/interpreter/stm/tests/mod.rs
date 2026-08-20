//! Test-only support for the STM module: shared commit-protocol drivers
//! ([`helpers`]) plus the tests themselves.

mod helpers;
mod soak;

// Re-export the shared drivers so sibling test modules can import them
// with a single `use stm::tests::*`.
use std::sync::Arc;

pub(crate) use helpers::*;
use imbl::OrdMap;
use indexmap::IndexMap;

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::{
        Transaction, VarId, Version, WorldValue,
        changeset::Changeset,
        committer::{CommitProtocol, Committer},
        snapshot::Snapshot,
    },
};

#[test]
fn multiple_variables_are_isolated() {
    let var_id = VarId::new();
    let mut map = OrdMap::new();
    map.insert(var_id, WorldValue::ref_of(LpcRef::from(123)));

    let snapshot = Snapshot::new(Version::new(), map);
    let mut transaction = Transaction::new(snapshot);
    let var_id2 = VarId::new();
    transaction.write(var_id2, LpcRef::from("foo"));
    assert_eq!(transaction.read(var_id), Some(LpcRef::from(123)));
    assert_eq!(transaction.read(var_id2), Some(LpcRef::from("foo")));
}

#[test]
fn read_sees_previous_writes_before_falling_back_to_state() {
    let var_id = VarId::new();
    let mut map = OrdMap::new();
    map.insert(var_id, WorldValue::ref_of(LpcRef::from(123)));

    let snapshot = Snapshot::new(Version::new(), map);
    let mut transaction = Transaction::new(snapshot);
    assert_eq!(transaction.read(var_id), Some(LpcRef::from(123)));

    let value = LpcRef::from(42.42);
    transaction.write(var_id, value.clone());

    assert_eq!(transaction.read(var_id), Some(value));
}

#[test]
fn a_payload_var_roundtrips_through_the_committed_world() {
    let (tx, _rx) = flume::unbounded();
    let mut committer = Committer::new();
    let payload = VarId::new();
    let contents = Arc::new(LpcArray::new([LpcRef::from(10), LpcRef::from(20)]));

    // A payload var is a fresh VarId, so the blind write always commits.
    let mut seed = Changeset::new(committer.current_version());
    seed.write(payload, WorldValue::Array(contents.clone()));
    committer.commit(seed).expect("payload seed should commit");

    let (reply_tx, reply_rx) = flume::bounded(1);
    committer.process(
        CommitProtocol::Query {
            var_id: payload,
            reply: reply_tx,
        },
        &tx,
    );
    assert_eq!(
        reply_rx.recv().expect("no reply"),
        WorldValue::Array(contents)
    );
}

#[test]
fn a_second_writer_of_a_payload_var_conflicts() {
    let mut committer = Committer::new();
    let payload = VarId::new();

    let mut base = Changeset::new(committer.current_version());
    base.write(
        payload,
        WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(1)]))),
    );
    committer.commit(base).expect("base should commit");
    let base_version = committer.current_version();

    // Both changesets read the payload var from the same base; the first
    // commit invalidates the second by the conflict rule.
    let mut a = Changeset::new(base_version);
    a.track_read(payload);
    a.write(
        payload,
        WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(2)]))),
    );

    let mut b = Changeset::new(base_version);
    b.track_read(payload);
    b.write(
        payload,
        WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(3)]))),
    );

    committer.commit(a).expect("first commit should win");
    assert!(committer.commit(b).is_err());
}

#[test]
fn a_reader_holding_a_snapshot_retains_the_old_payload() {
    let mut committer = Committer::new();
    let payload = VarId::new();
    let old_contents = Arc::new(LpcArray::new([LpcRef::from(10), LpcRef::from(20)]));

    let mut seed = Changeset::new(committer.current_version());
    seed.write(payload, WorldValue::Array(old_contents.clone()));
    committer.commit(seed).expect("seed should commit");

    // The structural-sharing clone keeps the old contents' Arc alive.
    let reader = committer.snapshot_clone();
    assert_eq!(Arc::strong_count(&old_contents), 2);

    let mut advance = Changeset::new(committer.current_version());
    advance.track_read(payload);
    advance.write(
        payload,
        WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(99)]))),
    );
    committer.commit(advance).expect("advance should commit");

    assert_eq!(
        reader.read(payload),
        Some(WorldValue::Array(old_contents.clone()))
    );
    assert_eq!(Arc::strong_count(&old_contents), 2);
}

#[test]
fn querying_an_absent_var_reads_back_null() {
    let (tx, _rx) = flume::unbounded();
    let mut committer = Committer::new();

    let (reply_tx, reply_rx) = flume::bounded(1);
    committer.process(
        CommitProtocol::Query {
            var_id: VarId::new(),
            reply: reply_tx,
        },
        &tx,
    );
    assert_eq!(reply_rx.recv().expect("no reply"), WorldValue::null());
}

#[test]
fn a_mapping_payload_var_roundtrips_too() {
    let (tx, _rx) = flume::unbounded();
    let mut committer = Committer::new();
    let payload = VarId::new();
    let mut mapping = IndexMap::new();
    mapping.insert(LpcRef::from("a"), LpcRef::from(1));
    let contents = Arc::new(LpcMapping::new(mapping));

    let mut seed = Changeset::new(committer.current_version());
    seed.write(payload, WorldValue::Mapping(contents.clone()));
    committer.commit(seed).expect("mapping seed should commit");

    let (reply_tx, reply_rx) = flume::bounded(1);
    committer.process(
        CommitProtocol::Query {
            var_id: payload,
            reply: reply_tx,
        },
        &tx,
    );
    assert_eq!(
        reply_rx.recv().expect("no reply"),
        WorldValue::Mapping(contents)
    );
}
