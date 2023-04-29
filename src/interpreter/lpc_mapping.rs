use std::fmt::{Debug, Display, Formatter};

use bit_set::BitSet;
use delegate::delegate;
use if_chain::if_chain;
use indexmap::IndexMap;
use tracing::{instrument, trace};
use thin_vec::thin_vec;

use crate::interpreter::{
    gc::{mark::Mark, unique_id::UniqueId},
    heap::Heap,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::LpcRef,
};

/// A newtype wrapper for a map of [`LpcRef`]s to [`LpcRef`]s,
/// with a [`UniqueId`] for GC purposes.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct LpcMapping {
    pub unique_id: UniqueId,
    pub mapping: IndexMap<LpcRef, LpcRef>,
}

impl LpcMapping {
    /// Create a new [`LpcMapping`].
    #[inline]
    pub fn new(mapping: IndexMap<LpcRef, LpcRef>) -> Self {
        Self {
            unique_id: UniqueId::new(),
            mapping,
        }
    }

    delegate! {
        to self.mapping {
            pub fn get(&self, key: &LpcRef) -> Option<&LpcRef>;
            pub fn get_index(&self, index: usize) -> Option<(&LpcRef, &LpcRef)>;
            pub fn insert(&mut self, key: LpcRef, value: LpcRef) -> Option<LpcRef>;
            pub fn extend<T>(&mut self, iter: T)
                where T: IntoIterator<Item = (LpcRef, LpcRef)>;
            pub fn iter(&self) -> indexmap::map::Iter<LpcRef, LpcRef>;
            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
            pub fn keys(&self) -> indexmap::map::Keys<LpcRef, LpcRef>;
            pub fn values(&self) -> indexmap::map::Values<LpcRef, LpcRef>;
        }
    }
}

fn format_mapping<F>(mapping: &LpcMapping, fun: F) -> String
where
    F: Fn(&LpcRef) -> String,
{
    let mut result = String::with_capacity(32);
    for (i, (key, value)) in mapping.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        if_chain! {
            if let LpcRef::Mapping(other) = &key;
            if &*other.read() == mapping;
            then {
                result.push_str("([ this ])");
                continue;
            } else {
                result.push_str(&fun(key));
            }
        }

        result.push_str(": ");

        if_chain! {
            if let LpcRef::Mapping(other) = &value;
            if &*other.read() == mapping;
            then {
                result.push_str("([ this ])");
                continue;
            } else {
                result.push_str(&fun(value));
            }
        }
    }

    result
}

impl Display for LpcMapping {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "([")?;
        f.write_str(&format_mapping(self, |value| format!("{}", value)))?;
        write!(f, " ])")
    }
}

impl Debug for LpcMapping {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LpcMapping {{")?;
        f.write_str(&format_mapping(self, |value| format!("{:?}", value)))?;
        write!(f, " }}")
    }
}

impl Mark for LpcMapping {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> lpc_rs_errors::Result<()> {
        trace!("marking mapping");

        if !processed.insert(*self.unique_id.as_ref()) {
            return Ok(());
        }

        for (key, value) in &self.mapping {
            key.mark(marked, processed)?;
            value.mark(marked, processed)?;
        }

        Ok(())
    }
}

impl IntoIterator for LpcMapping {
    type Item = (LpcRef, LpcRef);
    type IntoIter = indexmap::map::IntoIter<LpcRef, LpcRef>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.mapping.into_iter()
    }
}

impl PartialEq<IndexMap<LpcRef, LpcRef>> for LpcMapping {
    fn eq(&self, other: &IndexMap<LpcRef, LpcRef>) -> bool {
        &self.mapping == other
    }
}

impl IntoLpcRef for LpcMapping {
    fn into_lpc_ref(self, pool: &Heap) -> LpcRef {
        pool.alloc_mapping(self)
    }
}

#[cfg(test)]
mod tests {
    use factori::create;
    use lpc_rs_core::register::Register;

    use super::*;
    use crate::test_support::factories::*;

    #[test]
    fn test_mark() {
        let memory = Heap::new(5);

        let ptr = create!(FunctionPtr, upvalue_ptrs: thin_vec![Register(4), Register(33)]);
        let key_id = *ptr.unique_id.as_ref();
        let key_ref = ptr.into_lpc_ref(&memory);

        let ptr2 = create!(FunctionPtr, upvalue_ptrs: thin_vec![Register(4), Register(666)]);
        let value_id = *ptr2.unique_id.as_ref();
        let value_ref = ptr2.into_lpc_ref(&memory);

        let mut mapping = LpcMapping::new(IndexMap::new());

        mapping.insert(key_ref, value_ref);

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();

        mapping.mark(&mut marked, &mut processed).unwrap();

        let mut marked_expected = BitSet::new();
        marked_expected.extend([4_usize, 33_usize, 666_usize].into_iter());

        assert_eq!(marked, marked_expected);

        let mut processed_expected = BitSet::new();
        processed_expected.extend([key_id, value_id, *mapping.unique_id.as_ref()].into_iter());

        assert_eq!(processed, processed_expected);
    }
}
