//! Immutable view of world state at a single [`Version`].

use imbl::OrdMap;

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{VarId, Version},
};

#[derive(Debug, Clone)]
pub(crate) struct Snapshot {
    version: Version,
    // OrdMap, so that transactions always replay in the same order, plus O(1) clones
    state: OrdMap<VarId, LpcRef>,
}

impl Snapshot {
    pub(crate) fn new(version: Version, state: OrdMap<VarId, LpcRef>) -> Self {
        Self { version, state }
    }

    pub(crate) fn read(&self, var_id: VarId) -> Option<LpcRef> {
        self.state.get(&var_id).cloned()
    }

    pub(crate) fn version(&self) -> Version {
        self.version
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
