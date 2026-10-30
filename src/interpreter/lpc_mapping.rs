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

#[cfg(test)]
mod tests {
    use lpc_rs_core::LpcIntInner;

    use super::{LpcMapping, LpcRef};

    fn mapping_from_pairs(entries: &[(LpcIntInner, LpcIntInner)]) -> LpcMapping {
        LpcMapping::new(
            entries
                .iter()
                .map(|&(key, value)| (LpcRef::from(key), LpcRef::from(value)))
                .collect(),
        )
    }

    #[test]
    fn display_preserves_delimiters_and_insertion_order() {
        for (entries, expected) in [
            ([].as_slice(), "([ ])"),
            ([(7, 11)].as_slice(), "([7: 11 ])"),
            ([(7, 11), (3, 5)].as_slice(), "([7: 11, 3: 5 ])"),
        ] {
            assert_eq!(mapping_from_pairs(entries).to_string(), expected);
        }
    }

    #[test]
    fn debug_includes_the_mapping_type_and_debug_entries() {
        let mapping = mapping_from_pairs(&[(7, 11), (3, 5)]);
        assert_eq!(
            format!("{mapping:?}"),
            "LpcMapping {LpcInt(7): LpcInt(11), LpcInt(3): LpcInt(5) }"
        );
    }

    #[test]
    fn raw_map_comparison_checks_entries_independently_of_order() {
        let mapping = mapping_from_pairs(&[(7, 11), (3, 5)]);
        for (entries, expected) in [
            ([(3, 5), (7, 11)].as_slice(), true),
            ([(7, 11), (3, 6)].as_slice(), false),
            ([(7, 11), (4, 5)].as_slice(), false),
            ([(7, 11)].as_slice(), false),
            ([].as_slice(), false),
        ] {
            let other = mapping_from_pairs(entries).mapping;
            assert_eq!(mapping == other, expected, "{entries:?}");
        }
    }

    #[test]
    fn owned_iteration_preserves_pairs_and_insertion_order() {
        let entries = [(7, 11), (3, 5)];
        let actual: Vec<_> = mapping_from_pairs(&entries).into_iter().collect();
        let expected: Vec<_> = entries
            .into_iter()
            .map(|(key, value)| (LpcRef::from(key), LpcRef::from(value)))
            .collect();
        assert_eq!(actual, expected);
    }
}
