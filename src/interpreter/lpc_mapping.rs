use std::{
    cmp::Ordering,
    collections::HashSet,
    fmt::Formatter,
    hash::{Hash, Hasher},
};
use std::fmt::{Debug, Display};

use bit_set::BitSet;
use delegate::delegate;
use if_chain::if_chain;
use indexmap::IndexMap;
use qcell::QCellOwner;
use tracing::{instrument, trace};

use crate::{
    interpreter::{
        gc::{mark::Mark, unique_id::UniqueId},
        lpc_ref::{HashedLpcRef, LpcRef},
        lpc_value::LpcValue,
    },
    util::keyable::Keyable,
};

/// A newtype wrapper for a map of [`HashedLpcRef`]s to [`LpcRef`]s,
/// with a [`UniqueId`] for GC purposes.
#[derive(Default, Clone, PartialEq, Eq)]
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
            if let LpcRef::Mapping(other) = &key.value;
            if let LpcValue::Mapping(other) = &*other.borrow();
            if &*other == mapping;
            then {
                result.push_str("([ this ])");
                continue;
            } else {
                result.push_str(&fun(&key.value));
            }
        }

        result.push_str(": ");

        if_chain! {
            if let LpcRef::Mapping(other) = &value;
            if let LpcValue::Mapping(other) = &*other.borrow();
            if &*other == mapping;
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
    #[instrument(skip(self, cell_key))]
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut HashSet<UniqueId>,
        cell_key: &QCellOwner,
    ) -> lpc_rs_errors::Result<()> {
        trace!("marking mapping");

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

impl<'a> Keyable<'a> for LpcMapping {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "LpcMapping {{")?;
        f.write_str(&format_mapping(self, |value| format!("{:?}", value.with_key(cell_key))))?;
        write!(f, " }}")
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "([")?;
        f.write_str(&format_mapping(self, |value| format!("{}", value.with_key(cell_key))))?;
        write!(f, " ])")
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner) {
        self.unique_id.hash(state);
        self.mapping.keyable_hash(state, cell_key);
    }

    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool {
        self.unique_id == other.unique_id && self.mapping.keyable_eq(&other.mapping, cell_key)
    }

    fn keyable_partial_cmp(&self, other: &Self, cell_key: &QCellOwner) -> Option<Ordering> {
        self.unique_id
            .partial_cmp(&other.unique_id)
            .and_then(|order| match order {
                Ordering::Equal => self.mapping.keyable_partial_cmp(&other.mapping, cell_key),
                _ => Some(order),
            })
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
