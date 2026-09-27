//! Immutable view of world state at a single [`Version`].

use std::sync::{Arc, Weak};

use imbl::HashMap;
use lpc_rs_core::null_mode::NullMode;

use crate::interpreter::{
    object_space::ObjectSpace,
    process::Process,
    stm::{VarId, Version, WorldValue, changeset::Changeset},
};

#[derive(Debug, Clone)]
pub(crate) struct ObjectChange {
    key: Arc<str>,
    // Cancelled placements must not keep destructed objects alive in a retained attempt.
    process: Weak<Process>,
    insert: bool,
}

impl ObjectChange {
    pub(crate) fn new(process: &Arc<Process>, insert: bool) -> Self {
        Self {
            key: ObjectSpace::with_leading_slash(process.filename().as_ref()).into(),
            process: Arc::downgrade(process),
            insert,
        }
    }

    pub(crate) fn apply(&self, objects: &mut HashMap<Arc<str>, Arc<Process>>) {
        if self.insert {
            if let Some(process) = self.process.upgrade() {
                objects.insert(self.key.clone(), process);
            }
        } else if objects
            .get(&self.key)
            .is_some_and(|current| std::ptr::eq(current.as_ref(), self.process.as_ptr()))
        {
            objects.remove(&self.key);
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Snapshot {
    pub(crate) null_mode: NullMode,
    version: Version,
    // HAMT: O(1) structurally-shared clones, hash-probed lookups.
    state: HashMap<VarId, WorldValue>,
    pub(crate) objects: HashMap<Arc<str>, Arc<Process>>,
    /// A read dependency for membership, including insertions at previously unseen paths.
    pub(crate) object_membership: VarId,
}

impl Snapshot {
    pub(crate) fn new(version: Version, state: HashMap<VarId, WorldValue>) -> Self {
        let objects = state
            .values()
            .filter_map(|value| match value {
                WorldValue::Process(process) => Some((
                    Arc::<str>::from(ObjectSpace::with_leading_slash(process.filename().as_ref())),
                    process.clone(),
                )),
                _ => None,
            })
            .collect();
        Self {
            version,
            state,
            null_mode: Default::default(),
            objects,
            object_membership: VarId::new(),
        }
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
        let mut objects = self.objects.clone();
        for change in &changeset.object_changes {
            change.apply(&mut objects);
        }

        for (var_id, change) in changeset.into_changes() {
            match change {
                Some(value) => state.insert(var_id, value),
                None => state.remove(&var_id),
            };
        }
        Self {
            version,
            state,
            null_mode: self.null_mode,
            objects,
            object_membership: self.object_membership,
        }
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
