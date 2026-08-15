//! Immutable view of world state at a single [`Version`].

use std::{collections::BTreeMap, sync::Arc};

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{VarId, Version},
};

#[derive(Debug)]
pub(crate) struct Snapshot {
    pub(crate) version: Version,
    // BTreeMap, so that all transactions always replay in the same order
    state: Arc<BTreeMap<VarId, LpcRef>>,
}

impl Snapshot {
    pub(crate) fn new(version: Version, state: Arc<BTreeMap<VarId, LpcRef>>) -> Self {
        Self { version, state }
    }

    pub(crate) fn read(&self, var_id: VarId) -> Option<LpcRef> {
        self.state.get(&var_id).cloned()
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::sync::Arc;
    use crate::interpreter::stm::snapshot::Snapshot;
    use crate::interpreter::stm::{VarId, Version};

    #[test]
    fn read_returns_none_for_missing_var_id() {
        let snapshot = Snapshot::new(Version::new(), Arc::new(BTreeMap::new()));
        assert_eq!(snapshot.read(VarId::new()), None);
    }
}
