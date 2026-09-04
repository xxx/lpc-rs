use std::fmt::{Debug, Display, Formatter};

use delegate::delegate;
use indexmap::IndexMap;

use crate::interpreter::lpc_ref::LpcRef;

/// A newtype wrapper for a map of [`LpcRef`]s to [`LpcRef`]s.
#[derive(Default, Clone, PartialEq, Eq)]
pub struct LpcMapping {
    pub mapping: IndexMap<LpcRef, LpcRef>,
}

impl LpcMapping {
    /// Create a new [`LpcMapping`].
    #[inline]
    pub fn new(mapping: IndexMap<LpcRef, LpcRef>) -> Self {
        Self { mapping }
    }

    delegate! {
        to self.mapping {
            pub fn contains_key(&self, key: &LpcRef) -> bool;
            pub fn get(&self, key: &LpcRef) -> Option<&LpcRef>;
            pub fn get_index(&self, index: usize) -> Option<(&LpcRef, &LpcRef)>;
            pub fn insert(&mut self, key: LpcRef, value: LpcRef) -> Option<LpcRef>;
            pub fn extend<T>(&mut self, iter: T)
                where T: IntoIterator<Item = (LpcRef, LpcRef)>;
            pub fn iter(&self) -> indexmap::map::Iter<'_, LpcRef, LpcRef>;
            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
            pub fn keys(&self) -> indexmap::map::Keys<'_, LpcRef, LpcRef>;
            pub fn values(&self) -> indexmap::map::Values<'_, LpcRef, LpcRef>;
            pub fn retain<F>(&mut self, keep: F)
                where F: FnMut(&LpcRef, &mut LpcRef) -> bool;
            pub fn shift_remove(&mut self, key: &LpcRef) -> Option<LpcRef>;
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
        result.push_str(&fun(key));
        result.push_str(": ");
        result.push_str(&fun(value));
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
