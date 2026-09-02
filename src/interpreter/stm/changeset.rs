//! Tracking for writes accumulated over a transaction

use std::collections::hash_map::Entry;

use ahash::{AHashMap, AHashSet};

use crate::interpreter::stm::{MergeOp, VarId, Version, WorldValue};

/// One observed var: tracked for conflict detection, and — once a world
/// lookup answered — carrying that answer so a re-read skips the world.
#[derive(Debug, Clone)]
enum ReadEntry {
    /// Observed; no world answer cached yet.
    Tracked,
    /// Observed, with the world's answer (`None` = absent) memoized.
    Cached(Option<WorldValue>),
}

/// A changeset dismantled for application: writes, merges, removals.
pub(crate) type ChangesetParts = (
    AHashMap<VarId, WorldValue>,
    AHashMap<VarId, Vec<MergeOp>>,
    AHashSet<VarId>,
);

#[derive(Debug, Clone)]
pub(crate) struct Changeset {
    version: Version,
    writes: AHashMap<VarId, WorldValue>,
    reads: AHashMap<VarId, ReadEntry>,
    /// Vars this attempt removes from the world (a transactional `destruct`).
    /// Kept out of `writes` so a `drop` isn't mistaken for a write of a new
    /// value; the committer removes them from the world on commit.
    removals: AHashSet<VarId>,
    /// Merge writes, in record order per var; applied by the committer to
    /// the committed value at commit time. A var never holds both a write
    /// and merges: recording folds one into the other.
    merges: AHashMap<VarId, Vec<MergeOp>>,
}

impl Changeset {
    pub(crate) fn new(version: Version) -> Self {
        Self {
            version,
            writes: AHashMap::new(),
            reads: AHashMap::new(),
            removals: AHashSet::new(),
            merges: AHashMap::new(),
        }
    }

    /// Read a variable from the changeset. A var this attempt removed reads
    /// back as absent, even if it was also written (the removal is the
    /// intent; the write is the pre-removal state).
    pub(crate) fn read(&self, var_id: VarId) -> Option<WorldValue> {
        if self.removals.contains(&var_id) {
            return None;
        }
        self.writes.get(&var_id).cloned()
    }

    /// The attempt's own written array payload for `var_id`, mutably —
    /// `None` if this attempt has not written an array there (or removed it).
    pub(crate) fn written_array_mut(
        &mut self,
        var_id: VarId,
    ) -> Option<&mut std::sync::Arc<crate::interpreter::lpc_array::LpcArray>> {
        match self.writes.get_mut(&var_id) {
            Some(WorldValue::Array(arc)) => Some(arc),
            _ => None,
        }
    }

    /// The attempt's own written mapping payload for `var_id`, mutably, as
    /// in [`written_array_mut`](Self::written_array_mut).
    pub(crate) fn written_mapping_mut(
        &mut self,
        var_id: VarId,
    ) -> Option<&mut std::sync::Arc<crate::interpreter::lpc_mapping::LpcMapping>> {
        match self.writes.get_mut(&var_id) {
            Some(WorldValue::Mapping(arc)) => Some(arc),
            _ => None,
        }
    }

    /// Write a variable to the changeset. A write is the whole value, so it
    /// supersedes any merges queued before it.
    pub(crate) fn write(&mut self, var_id: VarId, value: WorldValue) {
        self.removals.remove(&var_id);
        self.merges.remove(&var_id);
        self.writes.insert(var_id, value);
    }

    /// Record a merge write of `var_id`: a pending removal or write absorbs
    /// the op; otherwise it queues for the committer, folding into its
    /// predecessor when the kinds compose.
    pub(crate) fn merge(&mut self, var_id: VarId, op: MergeOp) {
        if self.removals.remove(&var_id) {
            let value = op.apply_to(None).expect("an op applies onto its identity");
            self.writes.insert(var_id, value);
            return;
        }
        if let Some(written) = self.writes.get_mut(&var_id) {
            *written = op
                .apply_to(Some(written))
                .expect("the caller peeks the type before merging");
            return;
        }
        let ops = self.merges.entry(var_id).or_default();
        match ops.last_mut() {
            Some(last) => {
                if let Some(op) = last.fold(op) {
                    ops.push(op);
                }
            }
            None => ops.push(op),
        }
    }

    /// The pending merge writes by var, for the committer's type precheck.
    pub(crate) fn merges(&self) -> impl Iterator<Item = (&VarId, &Vec<MergeOp>)> {
        self.merges.iter()
    }

    /// The attempt's own written value for `var_id`, if any.
    pub(crate) fn written(&self, var_id: VarId) -> Option<&WorldValue> {
        self.writes.get(&var_id)
    }

    /// The pending merge writes for `var_id`, in record order.
    pub(crate) fn pending_merges(&self, var_id: VarId) -> &[MergeOp] {
        self.merges.get(&var_id).map(Vec::as_slice).unwrap_or(&[])
    }

    /// Record that this attempt removes a var from the world. A `destruct` of
    /// an object cell. A subsequent `write` of the same var cancels the
    /// removal (the object is alive again); a `read` of a removed var returns
    /// `None` in the changeset.
    pub(crate) fn drop_var(&mut self, var_id: VarId) {
        self.writes.remove(&var_id);
        self.merges.remove(&var_id);
        self.removals.insert(var_id);
    }

    /// Whether this attempt removes the var (and did not re-write it). A
    /// removed var reads back as `None` here, but a read must also skip the
    /// committed world and the physical object map — the removal is
    /// authoritative for this attempt until a re-write cancels it.
    pub(crate) fn is_removed(&self, var_id: VarId) -> bool {
        self.removals.contains(&var_id)
    }

    /// Track the read of a variable. Needed for conflict detection. An
    /// entry that already caches a world answer keeps it.
    pub(crate) fn track_read(&mut self, var_id: VarId) {
        self.reads.entry(var_id).or_insert(ReadEntry::Tracked);
    }

    /// The tracked-and-memoized world read: a cached answer is returned as
    /// is; otherwise `miss` supplies it and the answer (absence included) is
    /// cached — sound because the snapshot never changes within an attempt.
    pub(crate) fn read_through(
        &mut self,
        var_id: VarId,
        miss: impl FnOnce() -> Option<WorldValue>,
    ) -> Option<WorldValue> {
        match self.reads.entry(var_id) {
            Entry::Occupied(mut occupied) => match occupied.get() {
                ReadEntry::Cached(value) => value.clone(),
                ReadEntry::Tracked => {
                    let value = miss();
                    occupied.insert(ReadEntry::Cached(value.clone()));
                    value
                }
            },
            Entry::Vacant(vacant) => {
                let value = miss();
                vacant.insert(ReadEntry::Cached(value.clone()));
                value
            }
        }
    }

    /// Whether any var this attempt read is in `written` — the read side of
    /// the conflict rule.
    pub(crate) fn conflicts_with(&self, written: &AHashSet<VarId>) -> bool {
        if written.len() <= self.reads.len() {
            written.iter().any(|var| self.reads.contains_key(var))
        } else {
            self.reads.keys().any(|var| written.contains(var))
        }
    }

    /// The version that was current when this changeset was created.
    pub(crate) fn base_version(&self) -> Version {
        self.version
    }

    /// Whether this attempt writes, merges, or removes anything.
    pub(crate) fn has_changes(&self) -> bool {
        !(self.writes.is_empty() && self.merges.is_empty() && self.removals.is_empty())
    }

    /// The vars this attempt changes in any way, for conflict bookkeeping:
    /// writes, merges, and removals. All count against the read-write
    /// conflict rule (a concurrent reader of a removed or merged var
    /// conflicts, so it re-runs).
    pub(crate) fn touched_vars(&self) -> AHashSet<VarId> {
        let mut vars = self.writes.keys().copied().collect::<AHashSet<_>>();
        vars.extend(self.merges.keys().copied());
        vars.extend(self.removals.iter().copied());
        vars
    }

    /// The writes, merges, and removals this attempt made, for the snapshot
    /// to apply on commit.
    pub(crate) fn into_parts(self) -> ChangesetParts {
        (self.writes, self.merges, self.removals)
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
}
