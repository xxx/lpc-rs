//! Immutable view of world state at a single [`Version`].

use imbl::HashMap;

use crate::interpreter::stm::{VarId, Version, WorldValue, changeset::Changeset};

#[derive(Debug, Clone)]
pub(crate) struct Snapshot {
    version: Version,
    // HAMT: O(1) structurally-shared clones, hash-probed lookups.
    state: HashMap<VarId, WorldValue>,
}

impl Snapshot {
    pub(crate) fn new(version: Version, state: HashMap<VarId, WorldValue>) -> Self {
        Self { version, state }
    }

    pub(crate) fn read(&self, var_id: VarId) -> Option<WorldValue> {
        self.state.get(&var_id).cloned()
    }

    /// Borrow the value of a var, for probes that must not clone.
    pub(crate) fn peek(&self, var_id: VarId) -> Option<&WorldValue> {
        self.state.get(&var_id)
    }

    /// The world's vars by id, for the quiescent sweep.
    pub(crate) fn state(&self) -> impl Iterator<Item = (&VarId, &WorldValue)> {
        self.state.iter()
    }

    /// Remove a var from the world (memory hygiene for swept upvalue
    /// cells; a re-transaction would re-commit the value).
    pub(crate) fn drop_var(&mut self, var_id: VarId) {
        self.state.remove(&var_id);
    }

    pub(crate) fn version(&self) -> Version {
        self.version
    }

    pub(crate) fn apply(&self, version: Version, changeset: Changeset) -> Self {
        let mut state = self.state.clone();

        for (var_id, change) in changeset.into_changes() {
            match change {
                Some(value) => state.insert(var_id, value),
                None => state.remove(&var_id),
            };
        }
        Self::new(version, state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_returns_none_for_missing_var_id() {
        let snapshot = Snapshot::new(Version::new(), HashMap::new());
        assert_eq!(snapshot.read(VarId::new()), None);
    }
}
