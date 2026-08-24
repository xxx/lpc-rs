//! Test-only support for the STM module: shared commit-protocol drivers
//! ([`helpers`]) plus the tests themselves.

mod conflict_probes;
mod helpers;
mod soak;

// Re-export the shared drivers so sibling test modules can import them
// with a single `use stm::tests::*`.
use std::sync::Arc;

pub(crate) use helpers::*;
use imbl::HashMap;
use indexmap::IndexMap;

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::{
        MergeOp, Transaction, VarId, Version, WorldValue, changeset::Changeset,
        committer::Committer, snapshot::Snapshot,
    },
};

#[test]
fn multiple_variables_are_isolated() {
    let var_id = VarId::new();
    let mut map = HashMap::new();
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
    let mut map = HashMap::new();
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
    let mut committer = Committer::new();
    let payload = VarId::new();
    let contents = Arc::new(LpcArray::new([LpcRef::from(10), LpcRef::from(20)]));

    // A payload var is a fresh VarId, so the blind write always commits.
    let mut seed = Changeset::new(committer.current_version());
    seed.write(payload, WorldValue::Array(contents.clone()));
    committer.commit(seed).expect("payload seed should commit");

    assert_eq!(committer.committed(payload), WorldValue::Array(contents));
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
    let committer = Committer::new();

    assert_eq!(committer.committed(VarId::new()), WorldValue::null());
}

#[test]
fn a_mapping_payload_var_roundtrips_too() {
    let mut committer = Committer::new();
    let payload = VarId::new();
    let mut mapping = IndexMap::new();
    mapping.insert(LpcRef::from("a"), LpcRef::from(1));
    let contents = Arc::new(LpcMapping::new(mapping));

    let mut seed = Changeset::new(committer.current_version());
    seed.write(payload, WorldValue::Mapping(contents.clone()));
    committer.commit(seed).expect("mapping seed should commit");

    assert_eq!(committer.committed(payload), WorldValue::Mapping(contents));
}

#[test]
fn a_read_of_a_merged_var_materializes_tracked_and_folded() {
    let var_id = VarId::new();
    let mut map = HashMap::new();
    map.insert(var_id, WorldValue::ref_of(LpcRef::from(5)));
    let mut transaction = Transaction::new(Snapshot::new(Version::new(), map));

    transaction.merge(var_id, MergeOp::IntAdd(2));
    assert_eq!(transaction.read(var_id), Some(LpcRef::from(7)));

    // The read observed committed state, so it joined the conflict set;
    // the merge stays a merge.
    let (_, changeset) = transaction.into_parts();
    assert!(changeset.conflicts_with(&[var_id].into_iter().collect()));
    assert_eq!(changeset.pending_merges(var_id), &[MergeOp::IntAdd(2)]);
}

#[test]
fn two_merge_only_changesets_from_one_base_both_commit() {
    let mut committer = Committer::new();
    let counter = VarId::new();
    let mut seed = Changeset::new(committer.current_version());
    seed.write(counter, WorldValue::ref_of(LpcRef::from(10)));
    committer.commit(seed).expect("seed should commit");
    let base = committer.current_version();

    let mut a = Changeset::new(base);
    a.merge(counter, MergeOp::IntAdd(1));
    let mut b = Changeset::new(base);
    b.merge(counter, MergeOp::IntAdd(1));

    committer.commit(a).expect("first merge should commit");
    committer.commit(b).expect("second merge must not conflict");
    assert_eq!(
        committer.committed(counter),
        WorldValue::ref_of(LpcRef::from(12))
    );
}

#[test]
fn a_tracked_reader_conflicts_with_an_interleaved_merge_commit() {
    let mut committer = Committer::new();
    let counter = VarId::new();
    let mut seed = Changeset::new(committer.current_version());
    seed.write(counter, WorldValue::ref_of(LpcRef::from(0)));
    committer.commit(seed).expect("seed should commit");
    let base = committer.current_version();

    let mut merger = Changeset::new(base);
    merger.merge(counter, MergeOp::IntAdd(1));

    let other = VarId::new();
    let mut reader = Changeset::new(base);
    reader.track_read(counter);
    reader.write(other, WorldValue::ref_of(LpcRef::from(1)));

    committer.commit(merger).expect("the merge should commit");
    assert!(
        committer.commit(reader).is_err(),
        "a tracked read of a merged var must conflict"
    );
}

#[test]
fn a_merge_type_mismatch_rejects_as_conflict() {
    let mut committer = Committer::new();
    let cell = VarId::new();
    let contents = WorldValue::ref_of(LpcRef::from("not an int"));
    let mut seed = Changeset::new(committer.current_version());
    seed.write(cell, contents.clone());
    committer.commit(seed).expect("seed should commit");

    let mut bumper = Changeset::new(committer.current_version());
    bumper.merge(cell, MergeOp::IntAdd(1));

    assert!(committer.commit(bumper).is_err(), "mismatch must reject");
    assert_eq!(committer.committed(cell), contents, "value untouched");
}

#[test]
fn a_merge_on_an_absent_var_applies_onto_the_identity() {
    let mut committer = Committer::new();
    let cell = VarId::new();

    let mut bumper = Changeset::new(committer.current_version());
    bumper.merge(cell, MergeOp::IntAdd(4));
    committer.commit(bumper).expect("the merge should commit");

    assert_eq!(
        committer.committed(cell),
        WorldValue::ref_of(LpcRef::from(4))
    );
}

#[test]
fn a_merge_only_commit_creates_a_version() {
    let mut committer = Committer::new();
    let cell = VarId::new();
    let before = committer.current_version();

    let mut bumper = Changeset::new(before);
    bumper.merge(cell, MergeOp::IntAdd(1));
    committer.commit(bumper).expect("the merge should commit");

    let after = committer.current_version();
    assert!(before < after, "a merge is a write, not a read-only commit");
    assert!(
        committer.retains_version(after),
        "its write set joins history"
    );
}

#[test]
fn a_merge_wraps_like_the_eval_loop() {
    let mut committer = Committer::new();
    let cell = VarId::new();
    let mut seed = Changeset::new(committer.current_version());
    seed.write(cell, WorldValue::ref_of(LpcRef::from(i64::MAX)));
    committer.commit(seed).expect("seed should commit");

    let mut bumper = Changeset::new(committer.current_version());
    bumper.merge(cell, MergeOp::IntAdd(1));
    committer.commit(bumper).expect("the merge should commit");

    assert_eq!(
        committer.committed(cell),
        WorldValue::ref_of(LpcRef::from(i64::MIN))
    );
}

fn committed_array(committer: &Committer, var_id: VarId) -> Vec<LpcRef> {
    match committer.committed(var_id) {
        WorldValue::Array(a) => a.array.to_vec(),
        other => panic!("expected an array, got {other:?}"),
    }
}

#[test]
fn an_append_then_a_remove_of_the_same_value_leaves_it_absent() {
    let mut committer = Committer::new();
    let cell = VarId::new();

    let mut changeset = Changeset::new(committer.current_version());
    changeset.merge(cell, MergeOp::ArrayAppend(vec![LpcRef::from(1)]));
    changeset.merge(cell, MergeOp::ArrayRemoveValue(LpcRef::from(1)));
    committer
        .commit(changeset)
        .expect("the merges should commit");

    assert!(committed_array(&committer, cell).is_empty());
}

#[test]
fn a_remove_then_an_append_keeps_the_value() {
    let mut committer = Committer::new();
    let cell = VarId::new();

    let mut changeset = Changeset::new(committer.current_version());
    changeset.merge(cell, MergeOp::ArrayRemoveValue(LpcRef::from(1)));
    changeset.merge(cell, MergeOp::ArrayAppend(vec![LpcRef::from(1)]));
    committer
        .commit(changeset)
        .expect("the merges should commit");

    assert_eq!(committed_array(&committer, cell), vec![LpcRef::from(1)]);
}

#[test]
fn a_remove_value_removes_every_match() {
    let mut committer = Committer::new();
    let cell = VarId::new();
    let mut seed = Changeset::new(committer.current_version());
    seed.write(
        cell,
        WorldValue::Array(Arc::new(LpcArray::new([
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(1),
        ]))),
    );
    committer.commit(seed).expect("seed should commit");

    let mut remover = Changeset::new(committer.current_version());
    remover.merge(cell, MergeOp::ArrayRemoveValue(LpcRef::from(1)));
    committer.commit(remover).expect("the remove should commit");

    assert_eq!(committed_array(&committer, cell), vec![LpcRef::from(2)]);
}

#[test]
fn appends_land_in_commit_order() {
    let mut committer = Committer::new();
    let cell = VarId::new();
    let base = committer.current_version();

    let mut a = Changeset::new(base);
    a.merge(cell, MergeOp::ArrayAppend(vec![LpcRef::from(1)]));
    let mut b = Changeset::new(base);
    b.merge(cell, MergeOp::ArrayAppend(vec![LpcRef::from(2)]));

    committer.commit(a).expect("first append should commit");
    committer
        .commit(b)
        .expect("second append must not conflict");

    assert_eq!(
        committed_array(&committer, cell),
        vec![LpcRef::from(1), LpcRef::from(2)]
    );
}

#[test]
fn a_container_merge_type_mismatch_rejects() {
    let mut committer = Committer::new();
    let cell = VarId::new();
    let contents = WorldValue::ref_of(LpcRef::from(5));
    let mut seed = Changeset::new(committer.current_version());
    seed.write(cell, contents.clone());
    committer.commit(seed).expect("seed should commit");

    let mut appender = Changeset::new(committer.current_version());
    appender.merge(cell, MergeOp::ArrayAppend(vec![LpcRef::from(1)]));

    assert!(committer.commit(appender).is_err());
    assert_eq!(committer.committed(cell), contents, "value untouched");
}

#[test]
fn map_inserts_apply_in_order_and_overwrite() {
    let mut committer = Committer::new();
    let cell = VarId::new();

    let mut inserter = Changeset::new(committer.current_version());
    inserter.merge(cell, MergeOp::MapInsert(LpcRef::from("a"), LpcRef::from(1)));
    inserter.merge(cell, MergeOp::MapInsert(LpcRef::from("b"), LpcRef::from(2)));
    committer
        .commit(inserter)
        .expect("the inserts should commit");

    let mut overwriter = Changeset::new(committer.current_version());
    overwriter.merge(cell, MergeOp::MapInsert(LpcRef::from("a"), LpcRef::from(9)));
    committer
        .commit(overwriter)
        .expect("the overwrite should commit");

    match committer.committed(cell) {
        WorldValue::Mapping(m) => {
            assert_eq!(m.len(), 2);
            assert_eq!(
                m.iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<Vec<_>>(),
                vec![
                    (LpcRef::from("a"), LpcRef::from(9)),
                    (LpcRef::from("b"), LpcRef::from(2)),
                ],
                "insertion order survives an overwrite"
            );
        }
        other => panic!("expected a mapping, got {other:?}"),
    }
}

#[test]
fn a_cow_store_after_a_merge_materializes_into_a_write() {
    let cell = VarId::new();
    let mut map = HashMap::new();
    map.insert(
        cell,
        WorldValue::Array(Arc::new(LpcArray::new([LpcRef::from(1), LpcRef::from(2)]))),
    );
    let mut transaction = Transaction::new(Snapshot::new(Version::new(), map));

    transaction.merge(cell, MergeOp::ArrayAppend(vec![LpcRef::from(3)]));
    transaction
        .with_array_cow(cell, |array| {
            array.array[0] = LpcRef::from(9);
            Ok(())
        })
        .expect("the store closure is infallible");

    let contents = transaction.read_array(cell).expect("the cell has contents");
    assert_eq!(
        contents.array.to_vec(),
        vec![LpcRef::from(9), LpcRef::from(2), LpcRef::from(3)]
    );

    let (_, changeset) = transaction.into_parts();
    assert!(changeset.pending_merges(cell).is_empty());
    assert!(changeset.conflicts_with(&[cell].into_iter().collect()));
}

/// The move shape: both movers read and write their own token's environment
/// cell and merge the shared room's inventory.
#[test]
fn movers_of_distinct_tokens_commute_and_same_token_movers_conflict() {
    let mut committer = Committer::new();
    let room_inventory = VarId::new();
    let env_a = VarId::new();
    let env_b = VarId::new();
    let base = committer.current_version();

    let mover = |env: VarId, token: i64, base| {
        let mut changeset = Changeset::new(base);
        changeset.track_read(env);
        changeset.write(env, WorldValue::ref_of(LpcRef::from(1)));
        changeset.merge(
            room_inventory,
            MergeOp::ArrayAppend(vec![LpcRef::from(token)]),
        );
        changeset
    };

    let a = mover(env_a, 1, base);
    let b = mover(env_b, 2, base);
    committer.commit(a).expect("first mover should commit");
    committer
        .commit(b)
        .expect("a distinct token's move must not conflict");

    let base = committer.current_version();
    let c = mover(env_a, 1, base);
    let d = mover(env_a, 1, base);
    committer.commit(c).expect("the winner commits");
    assert!(
        committer.commit(d).is_err(),
        "two movers of one token conflict on its environment cell"
    );
}
