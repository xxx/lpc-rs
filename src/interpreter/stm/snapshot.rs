//! Immutable view of world state at a single [`Version`].

use imbl::OrdMap;

use crate::interpreter::stm::{VarId, Version, WorldValue, changeset::Changeset};

#[derive(Debug, Clone)]
pub(crate) struct Snapshot {
    version: Version,
    // OrdMap, so that transactions always replay in the same order, plus O(1) clones
    state: OrdMap<VarId, WorldValue>,
}

impl Snapshot {
    pub(crate) fn new(version: Version, state: OrdMap<VarId, WorldValue>) -> Self {
        Self { version, state }
    }

    pub(crate) fn read(&self, var_id: VarId) -> Option<WorldValue> {
        self.state.get(&var_id).cloned()
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

        let (writes, removals) = changeset.into_parts();
        for (var_id, value) in writes {
            state.insert(var_id, value);
        }
        for var_id in removals {
            state.remove(&var_id);
        }
        Self::new(version, state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_returns_none_for_missing_var_id() {
        let snapshot = Snapshot::new(Version::new(), OrdMap::new());
        assert_eq!(snapshot.read(VarId::new()), None);
    }
}
