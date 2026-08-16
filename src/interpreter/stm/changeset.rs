//! Tracking for writes accumulated over a transaction

use std::collections::BTreeMap;

use crate::interpreter::{
    lpc_ref::LpcRef,
    stm::{VarId, Version},
};

#[derive(Debug)]
pub(crate) struct Changeset {
    version: Version,
    writes: BTreeMap<VarId, LpcRef>,
}

impl Changeset {
    pub(crate) fn new(version: Version) -> Self {
        Self {
            version,
            writes: BTreeMap::new(),
        }
    }

    pub(crate) fn read(&self, var_id: VarId) -> Option<LpcRef> {
        self.writes.get(&var_id).cloned()
    }

    pub(crate) fn write(&mut self, var_id: VarId, value: LpcRef) {
        self.writes.insert(var_id, value);
    }

    pub(crate) fn base_version(&self) -> Version {
        self.version
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

        let value1 = LpcRef::from(666);
        changeset.write(var_id, value1.clone());
        assert_eq!(changeset.read(var_id), Some(value1));

        let value2 = LpcRef::from(42);
        changeset.write(var_id, value2.clone());
        assert_eq!(changeset.read(var_id), Some(value2));
    }
}
