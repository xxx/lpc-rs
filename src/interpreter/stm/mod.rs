//! Software transactional memory implementation

use std::sync::atomic::AtomicU64;

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{changeset::Changeset, snapshot::Snapshot},
};
mod changeset;
mod snapshot;

static VAR_ID_COUNT: AtomicU64 = AtomicU64::new(0);
// Stable ID for transactional cells
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) struct VarId(u64);

impl VarId {
    pub(crate) fn new() -> Self {
        Self(VAR_ID_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

// _Global_ version counter because this is single-committer.
static VERSION_COUNT: AtomicU64 = AtomicU64::new(0);
#[derive(Debug, Copy, Clone)]
#[expect(dead_code)]
pub(crate) struct Version(u64);

impl Version {
    pub(crate) fn new() -> Self {
        Self(VERSION_COUNT.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug)]
pub(crate) struct Transaction {
    snapshot: Snapshot,
    changeset: Changeset,
}

impl Transaction {
    pub(crate) fn new(snapshot: Snapshot) -> Self {
        let version = snapshot.version;
        Self {
            snapshot,
            changeset: Changeset::new(version),
        }
    }

    pub(crate) fn read(&self, var_id: VarId) -> Option<LpcRef> {
        self.changeset
            .read(var_id)
            .or_else(|| self.snapshot.read(var_id))
    }

    pub(crate) fn write(&mut self, var_id: VarId, value: LpcRef) {
        self.changeset.write(var_id, value);
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, sync::Arc};

    use crate::interpreter::{
        lpc_ref::LpcRef,
        stm::{Transaction, VarId, Version, snapshot::Snapshot},
    };

    #[test]
    fn read_sees_previous_writes_before_falling_back_to_state() {
        let var_id = VarId::new();
        let mut map = BTreeMap::new();
        map.insert(var_id, LpcRef::from(123));

        let snapshot = Snapshot::new(Version::new(), Arc::new(map));
        let mut transaction = Transaction::new(snapshot);
        assert_eq!(transaction.read(var_id), Some(LpcRef::from(123)));

        let value = LpcRef::from(42);
        transaction.write(var_id, value.clone());

        assert_eq!(transaction.read(var_id), Some(value));
    }
}
