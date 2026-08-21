//! Tracking for writes accumulated over a transaction

use std::collections::{BTreeMap, BTreeSet};

use crate::interpreter::stm::{VarId, Version, WorldValue};

#[derive(Debug, Clone)]
pub(crate) struct Changeset {
    version: Version,
    writes: BTreeMap<VarId, WorldValue>,
    reads: BTreeSet<VarId>,
    /// Vars this attempt removes from the world (a transactional `destruct`).
    /// Kept out of `writes` so a `drop` isn't mistaken for a write of a new
    /// value; the committer removes them from the world on commit.
    removals: BTreeSet<VarId>,
}

impl Changeset {
    pub(crate) fn new(version: Version) -> Self {
        Self {
            version,
            writes: BTreeMap::new(),
            reads: BTreeSet::new(),
            removals: BTreeSet::new(),
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

    /// Write a variable to the changeset.
    pub(crate) fn write(&mut self, var_id: VarId, value: WorldValue) {
        self.removals.remove(&var_id);
        self.writes.insert(var_id, value);
    }

    /// Record that this attempt removes a var from the world. A `destruct` of
    /// an object cell. A subsequent `write` of the same var cancels the
    /// removal (the object is alive again); a `read` of a removed var returns
    /// `None` in the changeset.
    pub(crate) fn drop_var(&mut self, var_id: VarId) {
        self.writes.remove(&var_id);
        self.removals.insert(var_id);
    }

    /// Whether this attempt removes the var (and did not re-write it). A
    /// removed var reads back as `None` here, but a read must also skip the
    /// committed world and the physical object map — the removal is
    /// authoritative for this attempt until a re-write cancels it.
    pub(crate) fn is_removed(&self, var_id: VarId) -> bool {
        self.removals.contains(&var_id)
    }

    /// Track the read of a variable. Needed for conflict detection.
    pub(crate) fn track_read(&mut self, var_id: VarId) {
        self.reads.insert(var_id);
    }

    /// The version that was current when this changeset was created.
    pub(crate) fn base_version(&self) -> Version {
        self.version
    }

    pub(crate) fn read_set(&self) -> &BTreeSet<VarId> {
        &self.reads
    }

    /// The vars this attempt changes in any way, for conflict bookkeeping:
    /// writes plus removals. Both count against the read-write conflict rule
    /// (a concurrent reader of a removed var conflicts, so it re-runs).
    pub(crate) fn touched_vars(&self) -> BTreeSet<VarId> {
        let mut vars = self.writes.keys().copied().collect::<BTreeSet<_>>();
        vars.extend(self.removals.iter().copied());
        vars
    }

    /// The writes and removals this attempt made, for the snapshot to apply on
    /// commit.
    pub(crate) fn into_parts(self) -> (BTreeMap<VarId, WorldValue>, BTreeSet<VarId>) {
        (self.writes, self.removals)
    }

    /// The values this changeset has written, for GC rooting.
    pub(crate) fn written_values(&self) -> impl Iterator<Item = &WorldValue> {
        self.writes.values()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
