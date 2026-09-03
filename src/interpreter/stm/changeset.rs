//! Tracking for writes accumulated over a transaction

use std::sync::Arc;

use ahash::{AHashMap, AHashSet};

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::{MergeOp, VarId, Version, WorldValue, merge::MergeMismatch},
};

/// This attempt's own change to one var: at most one kind at a time, a
/// write superseding queued merges and a removal clearing both.
#[derive(Debug, Clone, Default)]
enum Change {
    #[default]
    None,
    Write(WorldValue),
    /// Merge writes in record order; the committer applies them to the
    /// committed value at commit time.
    Merge(Vec<MergeOp>),
    /// A transactional `destruct`: the var reads back as absent here, and
    /// the committer removes it from the world on commit.
    Remove,
}

/// What this attempt observed of the committed world for one var.
#[derive(Debug, Clone, Default)]
enum Observed {
    /// Nothing: every read so far was answered by the attempt's own change.
    #[default]
    Nothing,
    /// Tracked for conflict detection; no world answer cached yet.
    Tracked,
    /// Tracked, with the world's answer (`None` = absent) memoized.
    Cached(Option<WorldValue>),
}

/// One var this attempt touched.
#[derive(Debug, Clone, Default)]
struct Entry {
    change: Change,
    observed: Observed,
}

impl Entry {
    fn is_tracked(&self) -> bool {
        !matches!(self.observed, Observed::Nothing)
    }

    fn is_changed(&self) -> bool {
        !matches!(self.change, Change::None)
    }
}

/// One entry per touched var, so a read is one lookup.
#[derive(Debug, Clone)]
pub(crate) struct Changeset {
    version: Version,
    entries: AHashMap<VarId, Entry>,
}

impl Changeset {
    pub(crate) fn new(version: Version) -> Self {
        Self {
            version,
            entries: AHashMap::new(),
        }
    }

    fn entry(&mut self, var_id: VarId) -> &mut Entry {
        self.entries.entry(var_id).or_default()
    }

    #[cfg(test)]
    /// The attempt's own written value for `var_id`.
    pub(crate) fn read(&self, var_id: VarId) -> Option<WorldValue> {
        self.written(var_id).cloned()
    }

    /// The value of `var_id` as this attempt sees it: a removal reads as
    /// absent, an own write answers untracked, and otherwise the world's
    /// answer — `world` asked once per attempt and the answer memoized,
    /// tracked — with pending merges folded onto it and kept as the write.
    pub(crate) fn read_value(
        &mut self,
        var_id: VarId,
        world: impl FnOnce() -> Option<WorldValue>,
    ) -> Option<WorldValue> {
        let entry = self.entry(var_id);
        match &entry.change {
            Change::Remove => return None,
            Change::Write(value) => return Some(value.clone()),
            Change::None | Change::Merge(_) => {}
        }
        let committed = match &entry.observed {
            Observed::Cached(value) => value.clone(),
            Observed::Nothing | Observed::Tracked => {
                let value = world();
                entry.observed = Observed::Cached(value.clone());
                value
            }
        };
        let Change::Merge(ops) = &mut entry.change else {
            return committed;
        };
        let ops = std::mem::take(ops);
        let value = MergeOp::fold_onto(committed, &ops)
            .expect("the caller peeks the type before merging")
            .expect("at least one op ran");
        entry.change = Change::Write(value.clone());
        Some(value)
    }

    /// The attempt's own written array payload for `var_id`, mutably —
    /// `None` if this attempt has not written an array there (or removed it).
    pub(crate) fn written_array_mut(&mut self, var_id: VarId) -> Option<&mut Arc<LpcArray>> {
        match self.entries.get_mut(&var_id).map(|entry| &mut entry.change) {
            Some(Change::Write(WorldValue::Array(arc))) => Some(arc),
            _ => None,
        }
    }

    /// The attempt's own written mapping payload for `var_id`, mutably, as
    /// in [`written_array_mut`](Self::written_array_mut).
    pub(crate) fn written_mapping_mut(&mut self, var_id: VarId) -> Option<&mut Arc<LpcMapping>> {
        match self.entries.get_mut(&var_id).map(|entry| &mut entry.change) {
            Some(Change::Write(WorldValue::Mapping(arc))) => Some(arc),
            _ => None,
        }
    }

    /// Write a variable to the changeset. A write is the whole value, so it
    /// supersedes any merges queued before it.
    pub(crate) fn write(&mut self, var_id: VarId, value: WorldValue) {
        self.entry(var_id).change = Change::Write(value);
    }

    /// Record a merge write of `var_id`: a pending removal or write absorbs
    /// the op; otherwise it queues for the committer, folding into its
    /// predecessor when the kinds compose.
    pub(crate) fn merge(&mut self, var_id: VarId, op: MergeOp) {
        let entry = self.entry(var_id);
        match &mut entry.change {
            Change::Remove => {
                let value = op.apply_to(None).expect("an op applies onto its identity");
                entry.change = Change::Write(value);
            }
            Change::Write(written) => {
                op.apply_in_place(written)
                    .expect("the caller peeks the type before merging");
            }
            Change::Merge(ops) => match ops.last_mut() {
                Some(last) => {
                    if let Some(op) = last.fold(op) {
                        ops.push(op);
                    }
                }
                None => ops.push(op),
            },
            Change::None => entry.change = Change::Merge(vec![op]),
        }
    }

    /// Fold every pending merge onto the world value `world` supplies,
    /// making it a write; a type mismatch rejects the changeset.
    pub(crate) fn fold_merges(
        &mut self,
        world: impl Fn(VarId) -> Option<WorldValue>,
    ) -> Result<(), MergeMismatch> {
        for (var_id, entry) in &mut self.entries {
            let Change::Merge(ops) = &mut entry.change else {
                continue;
            };
            let ops = std::mem::take(ops);
            let value = MergeOp::fold_onto(world(*var_id), &ops)?.expect("at least one op ran");
            entry.change = Change::Write(value);
        }
        Ok(())
    }

    #[cfg(test)]
    /// The attempt's own written value for `var_id`, if any.
    pub(crate) fn written(&self, var_id: VarId) -> Option<&WorldValue> {
        match self.entries.get(&var_id).map(|entry| &entry.change) {
            Some(Change::Write(value)) => Some(value),
            _ => None,
        }
    }

    #[cfg(test)]
    /// The pending merge writes for `var_id`, in record order.
    pub(crate) fn pending_merges(&self, var_id: VarId) -> &[MergeOp] {
        match self.entries.get(&var_id).map(|entry| &entry.change) {
            Some(Change::Merge(ops)) => ops,
            _ => &[],
        }
    }

    /// Whether an int merge can apply, as far as this attempt's own change
    /// decides it: `Some(true)` for a removal (the op applies onto its
    /// identity) or queued merges (an earlier peek accepted the cell),
    /// `Some(is_int)` for a write, `None` when only the world can say.
    pub(crate) fn peek_int(&self, var_id: VarId) -> Option<bool> {
        match self.entries.get(&var_id).map(|entry| &entry.change) {
            Some(Change::Remove | Change::Merge(_)) => Some(true),
            Some(Change::Write(value)) => Some(matches!(value, WorldValue::Ref(LpcRef::Int(_)))),
            Some(Change::None) | None => None,
        }
    }

    /// Record that this attempt removes a var from the world. A `destruct` of
    /// an object cell. A subsequent `write` of the same var cancels the
    /// removal (the object is alive again); a `read` of a removed var returns
    /// `None` in the changeset.
    pub(crate) fn drop_var(&mut self, var_id: VarId) {
        self.entry(var_id).change = Change::Remove;
    }

    /// Whether this attempt removes the var (and did not re-write it). A
    /// removed var reads back as `None` here, but a read must also skip the
    /// committed world and the physical object map — the removal is
    /// authoritative for this attempt until a re-write cancels it.
    pub(crate) fn is_removed(&self, var_id: VarId) -> bool {
        matches!(
            self.entries.get(&var_id).map(|entry| &entry.change),
            Some(Change::Remove)
        )
    }

    /// Track the read of a variable. Needed for conflict detection. An
    /// entry that already caches a world answer keeps it.
    pub(crate) fn track_read(&mut self, var_id: VarId) {
        let entry = self.entry(var_id);
        if !entry.is_tracked() {
            entry.observed = Observed::Tracked;
        }
    }

    /// Whether any var this attempt read is in `written` — the read side of
    /// the conflict rule.
    pub(crate) fn conflicts_with(&self, written: &AHashSet<VarId>) -> bool {
        if written.len() <= self.entries.len() {
            written
                .iter()
                .any(|var| self.entries.get(var).is_some_and(Entry::is_tracked))
        } else {
            self.entries
                .iter()
                .any(|(var, entry)| entry.is_tracked() && written.contains(var))
        }
    }

    /// The version that was current when this changeset was created.
    pub(crate) fn base_version(&self) -> Version {
        self.version
    }

    /// Whether this attempt writes, merges, or removes anything.
    pub(crate) fn has_changes(&self) -> bool {
        self.entries.values().any(Entry::is_changed)
    }

    /// The vars this attempt changes in any way, for conflict bookkeeping:
    /// writes, merges, and removals. All count against the read-write
    /// conflict rule (a concurrent reader of a removed or merged var
    /// conflicts, so it re-runs).
    pub(crate) fn touched_vars(&self) -> AHashSet<VarId> {
        self.entries
            .iter()
            .filter(|(_, entry)| entry.is_changed())
            .map(|(var_id, _)| *var_id)
            .collect()
    }

    /// The writes (`Some`) and removals (`None`) this attempt made, for the
    /// snapshot to apply on commit; the committer folds the merges first.
    pub(crate) fn into_changes(self) -> impl Iterator<Item = (VarId, Option<WorldValue>)> {
        self.entries
            .into_iter()
            .filter_map(|(var_id, entry)| match entry.change {
                Change::Write(value) => Some((var_id, Some(value))),
                Change::Remove => Some((var_id, None)),
                Change::Merge(_) => unreachable!("merges are folded before apply"),
                Change::None => None,
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_merge_tracks_no_read() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::IntAdd(1));

        let written = [var_id].into_iter().collect();
        assert!(!changeset.conflicts_with(&written));
        assert!(changeset.touched_vars().contains(&var_id));
    }

    #[test]
    fn a_write_then_merge_folds_into_the_write() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.write(var_id, WorldValue::ref_of(5.into()));
        changeset.merge(var_id, MergeOp::IntAdd(2));

        assert_eq!(changeset.read(var_id), Some(WorldValue::ref_of(7.into())));
        assert!(changeset.pending_merges(var_id).is_empty());
    }

    #[test]
    fn a_merge_onto_an_own_written_mapping_keeps_its_allocation() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        let payload = std::sync::Arc::new(crate::interpreter::lpc_mapping::LpcMapping::default());
        let allocation = std::sync::Arc::as_ptr(&payload);
        changeset.write(var_id, WorldValue::Mapping(payload));

        changeset.merge(var_id, MergeOp::MapInsert("a".into(), 1.into()));

        let Some(WorldValue::Mapping(written)) = changeset.written(var_id) else {
            panic!("the mapping stays a mapping");
        };
        assert!(std::ptr::eq(std::sync::Arc::as_ptr(written), allocation));
        assert_eq!(written.get(&"a".into()), Some(&1.into()));
    }

    #[test]
    fn a_merge_then_write_drops_the_merges() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::IntAdd(2));
        changeset.write(var_id, WorldValue::ref_of(9.into()));

        assert_eq!(changeset.read(var_id), Some(WorldValue::ref_of(9.into())));
        assert!(changeset.pending_merges(var_id).is_empty());
    }

    #[test]
    fn a_merge_then_drop_drops_the_merges() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::IntAdd(2));
        changeset.drop_var(var_id);

        assert!(changeset.is_removed(var_id));
        assert!(changeset.pending_merges(var_id).is_empty());
    }

    #[test]
    fn a_drop_then_merge_becomes_a_write_on_the_identity() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.drop_var(var_id);
        changeset.merge(var_id, MergeOp::IntAdd(3));

        assert!(!changeset.is_removed(var_id));
        assert_eq!(changeset.read(var_id), Some(WorldValue::ref_of(3.into())));
        assert!(changeset.pending_merges(var_id).is_empty());
    }

    #[test]
    fn consecutive_appends_fold_into_one_op() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::ArrayAppend(vec![1.into()]));
        changeset.merge(var_id, MergeOp::ArrayAppend(vec![2.into()]));

        assert_eq!(
            changeset.pending_merges(var_id),
            &[MergeOp::ArrayAppend(vec![1.into(), 2.into()])]
        );
    }

    #[test]
    fn an_append_and_a_remove_keep_their_order() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::ArrayAppend(vec![1.into()]));
        changeset.merge(var_id, MergeOp::ArrayRemoveValue(1.into()));

        assert_eq!(
            changeset.pending_merges(var_id),
            &[
                MergeOp::ArrayAppend(vec![1.into()]),
                MergeOp::ArrayRemoveValue(1.into()),
            ]
        );
    }

    #[test]
    fn consecutive_int_adds_fold_into_one_op() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::IntAdd(1));
        changeset.merge(var_id, MergeOp::IntAdd(1));

        assert_eq!(changeset.pending_merges(var_id), &[MergeOp::IntAdd(2)]);
    }
    #[test]
    fn read_returns_none_for_unwritten_var() {
        let changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        assert_eq!(changeset.read(var_id), None);
    }
    #[test]
    fn read_returns_most_recent_written_value() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);

        let value1 = WorldValue::ref_of(666.into());
        changeset.write(var_id, value1.clone());
        assert_eq!(changeset.read(var_id), Some(value1));

        let value2 = WorldValue::ref_of(42.into());
        changeset.write(var_id, value2.clone());
        assert_eq!(changeset.read(var_id), Some(value2));
    }

    #[test]
    fn dropped_var_reads_absent_until_a_write_cancels_the_removal() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);

        let value = WorldValue::ref_of(1.into());
        changeset.write(var_id, value.clone());
        assert!(!changeset.is_removed(var_id));

        // A destruct: the var reads back as absent, and the changeset
        // records the removal (the committer removes it from the world on
        // commit, conflict bookkeeping counts it).
        changeset.drop_var(var_id);
        assert_eq!(changeset.read(var_id), None);
        assert!(changeset.is_removed(var_id));
        assert!(changeset.touched_vars().contains(&var_id));

        // A re-create in the same attempt cancels the removal: the value is
        // back and no removal is pending.
        changeset.write(var_id, value.clone());
        assert_eq!(changeset.read(var_id), Some(value));
        assert!(!changeset.is_removed(var_id));

        // And the cycle repeats: drop again, and the removal is recorded
        // again even though the var was written before.
        changeset.drop_var(var_id);
        assert_eq!(changeset.read(var_id), None);
        assert!(changeset.is_removed(var_id));
    }

    #[test]
    fn read_value_memoizes_the_world_answer_and_tracks_the_var() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        let mut misses = 0;
        let mut world = || {
            misses += 1;
            Some(WorldValue::ref_of(5.into()))
        };

        let first = changeset.read_value(var_id, &mut world);
        let second = changeset.read_value(var_id, &mut world);

        assert_eq!(first, Some(WorldValue::ref_of(5.into())));
        assert_eq!(second, first);
        assert_eq!(misses, 1);
        assert!(changeset.conflicts_with(&[var_id].into_iter().collect()));
    }

    #[test]
    fn read_value_folds_pending_merges_onto_the_world_and_keeps_the_write() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.merge(var_id, MergeOp::IntAdd(2));

        let value = changeset.read_value(var_id, || Some(WorldValue::ref_of(5.into())));

        assert_eq!(value, Some(WorldValue::ref_of(7.into())));
        assert_eq!(
            changeset.written(var_id),
            Some(&WorldValue::ref_of(7.into()))
        );
        assert!(changeset.pending_merges(var_id).is_empty());
        assert!(changeset.conflicts_with(&[var_id].into_iter().collect()));
    }

    #[test]
    fn read_value_of_an_own_write_never_asks_the_world() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.write(var_id, WorldValue::ref_of(9.into()));

        let value = changeset.read_value(var_id, || panic!("the world is not asked"));

        assert_eq!(value, Some(WorldValue::ref_of(9.into())));
        assert!(!changeset.conflicts_with(&[var_id].into_iter().collect()));
    }

    #[test]
    fn read_value_of_a_removed_var_is_absent() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        changeset.drop_var(var_id);

        let value = changeset.read_value(var_id, || panic!("the world is not asked"));

        assert_eq!(value, None);
        assert!(!changeset.conflicts_with(&[var_id].into_iter().collect()));
    }

    #[test]
    fn a_tracked_read_keeps_its_cached_answer() {
        let mut changeset = Changeset::new(Version(0));
        let var_id = VarId(0);
        let cached = changeset.read_value(var_id, || Some(WorldValue::ref_of(1.into())));
        changeset.track_read(var_id);

        let again = changeset.read_value(var_id, || panic!("the answer is cached"));

        assert_eq!(again, cached);
    }

    #[test]
    fn into_changes_yields_writes_as_some_and_removals_as_none() {
        let mut changeset = Changeset::new(Version(0));
        let (written, removed, read) = (VarId(0), VarId(1), VarId(2));
        changeset.write(written, WorldValue::ref_of(4.into()));
        changeset.drop_var(removed);
        changeset.track_read(read);

        let mut changes = changeset.into_changes().collect::<Vec<_>>();
        changes.sort_by_key(|(var_id, _)| *var_id);

        assert_eq!(
            changes,
            vec![
                (written, Some(WorldValue::ref_of(4.into()))),
                (removed, None)
            ]
        );
    }

    #[test]
    fn peek_int_answers_from_the_own_change_or_defers_to_the_world() {
        let mut changeset = Changeset::new(Version(0));
        let (removed, int, string, merged, untouched) =
            (VarId(0), VarId(1), VarId(2), VarId(3), VarId(4));
        changeset.drop_var(removed);
        changeset.write(int, WorldValue::ref_of(1.into()));
        changeset.write(string, WorldValue::ref_of("s".into()));
        changeset.merge(merged, MergeOp::IntAdd(1));

        assert_eq!(changeset.peek_int(removed), Some(true));
        assert_eq!(changeset.peek_int(int), Some(true));
        assert_eq!(changeset.peek_int(string), Some(false));
        assert_eq!(changeset.peek_int(merged), Some(true));
        assert_eq!(changeset.peek_int(untouched), None);
    }
}
