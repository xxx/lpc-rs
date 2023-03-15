use std::collections::HashSet;
use bit_set::BitSet;
use delegate::delegate;
use indexmap::IndexMap;
use qcell::QCellOwner;
use crate::interpreter::gc::unique_id::{GcMark, UniqueId};
use crate::interpreter::lpc_ref::{HashedLpcRef, LpcRef};

/// A newtype wrapper for a map of [`HashedLpcRef`]s to [`LpcRef`]s,
/// with a [`UniqueId`] for GC purposes.
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct LpcMapping {
    pub unique_id: UniqueId,
    pub mapping: IndexMap<HashedLpcRef, LpcRef>,
}

impl LpcMapping {
    /// Create a new [`LpcMapping`].
    #[inline]
    pub fn new(mapping: IndexMap<HashedLpcRef, LpcRef>) -> Self {
        Self {
            unique_id: UniqueId::new(),
            mapping,
        }
    }

    delegate! {
        to self.mapping {
            pub fn get(&self, key: &HashedLpcRef) -> Option<&LpcRef>;
            pub fn get_index(&self, index: usize) -> Option<(&HashedLpcRef, &LpcRef)>;
            pub fn insert(&mut self, key: HashedLpcRef, value: LpcRef) -> Option<LpcRef>;
            pub fn extend<T>(&mut self, iter: T)
                where T: IntoIterator<Item = (HashedLpcRef, LpcRef)>;
            pub fn iter(&self) -> indexmap::map::Iter<HashedLpcRef, LpcRef>;
            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
            pub fn keys(&self) -> indexmap::map::Keys<HashedLpcRef, LpcRef>;
            pub fn values(&self) -> indexmap::map::Values<HashedLpcRef, LpcRef>;
        }
    }
}

impl GcMark for LpcMapping {
    fn mark(&self, marked: &mut BitSet, processed: &mut HashSet<UniqueId>, cell_key: &QCellOwner) -> lpc_rs_errors::Result<()> {
        if !processed.insert(self.unique_id) {
            return Ok(());
        }

        for (key, value) in &self.mapping {
            key.value.mark(marked, processed, cell_key)?;
            value.mark(marked, processed, cell_key)?;
        }

        Ok(())
    }
}

impl IntoIterator for LpcMapping {
    type Item = (HashedLpcRef, LpcRef);
    type IntoIter = indexmap::map::IntoIter<HashedLpcRef, LpcRef>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.mapping.into_iter()
    }
}

impl PartialEq<IndexMap<HashedLpcRef, LpcRef>> for LpcMapping {
    fn eq(&self, other: &IndexMap<HashedLpcRef, LpcRef>) -> bool {
        &self.mapping == other
    }
}