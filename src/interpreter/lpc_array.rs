use std::{
    fmt::{Debug, Display, Formatter},
    hash::{Hash},
    ops::{Deref, Index, IndexMut, Range, RangeInclusive},
};

use bit_set::BitSet;
use delegate::delegate;
use if_chain::if_chain;
use tracing::{instrument, trace};

use crate::{
    interpreter::{
        gc::{mark::Mark, unique_id::UniqueId},
        lpc_ref::LpcRef,
        lpc_value::LpcValue,
    },
};

/// A newtype wrapper for an array of [`LpcRef`]s, with a [`UniqueId`] for GC
/// purposes.
#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct LpcArray {
    pub unique_id: UniqueId,
    pub array: Vec<LpcRef>,
}

impl LpcArray {
    /// Create a new [`LpcArray`].
    #[inline]
    pub fn new(array: Vec<LpcRef>) -> Self {
        Self {
            unique_id: UniqueId::new(),
            array,
        }
    }

    delegate! {
        to self.array {
            pub fn extend<T>(&mut self, iter: T)
                where T: IntoIterator<Item = LpcRef>;

            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
        }
    }
}

fn format_array<F>(array: &LpcArray, fun: F) -> String
where
    F: Fn(&LpcRef) -> String,
{
    let mut result = String::with_capacity(32);
    for (i, item) in array.iter().enumerate() {
        if i > 0 {
            result.push_str(", ");
        }
        if_chain! {
            if let LpcRef::Array(other) = item;
            if let LpcValue::Array(other) = &*other.read();
            if other == array;
            then {
                result.push_str("({ self })");
                continue;
            }
        }
        result.push_str(&fun(item));
    }

    result
}

impl Debug for LpcArray {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LpcArray {{ ")?;
        write!(f, "unique_id: {:?}, ", self.unique_id)?;
        write!(f, "array: [")?;
        f.write_str(&format_array(self, |item| format!("{:?}", item)))?;
        write!(f, "] }}")
    }
}

impl Display for LpcArray {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        f.write_str(&format_array(self, |item| format!("{}", item)))?;
        write!(f, "]")
    }
}

impl Mark for LpcArray {
    #[instrument(skip(self))]
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
    ) -> lpc_rs_errors::Result<()> {
        trace!("marking array");

        if !processed.insert(*self.unique_id.as_ref()) {
            return Ok(());
        }

        for item in &self.array {
            item.mark(marked, processed)?;
        }

        Ok(())
    }
}

impl Deref for LpcArray {
    type Target = [LpcRef];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.array
    }
}

impl<'a> FromIterator<&'a LpcRef> for LpcArray {
    #[inline]
    fn from_iter<T: IntoIterator<Item = &'a LpcRef>>(iter: T) -> Self {
        Self::new(iter.into_iter().cloned().collect())
    }
}

impl FromIterator<LpcRef> for LpcArray {
    #[inline]
    fn from_iter<T: IntoIterator<Item = LpcRef>>(iter: T) -> Self {
        Self::new(iter.into_iter().collect())
    }
}

impl IntoIterator for LpcArray {
    type Item = LpcRef;
    type IntoIter = std::vec::IntoIter<LpcRef>;

    #[inline]
    fn into_iter(self) -> std::vec::IntoIter<LpcRef> {
        self.array.into_iter()
    }
}

impl Index<usize> for LpcArray {
    type Output = LpcRef;

    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        &self.array[index]
    }
}

impl IndexMut<usize> for LpcArray {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.array[index]
    }
}

impl Index<Range<usize>> for LpcArray {
    type Output = [LpcRef];

    #[inline]
    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.array[index]
    }
}

impl Index<RangeInclusive<usize>> for LpcArray {
    type Output = [LpcRef];

    #[inline]
    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.array[index]
    }
}

impl PartialEq<Vec<LpcRef>> for LpcArray {
    #[inline]
    fn eq(&self, other: &Vec<LpcRef>) -> bool {
        self.array == *other
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use factori::create;
    use lpc_rs_core::register::Register;
    use shared_arena::SharedArena;

    use super::*;
    use crate::{test_support::factories::*, value_to_ref};

    #[test]
    fn test_mark() {

        let pool = SharedArena::with_capacity(5);

        let ptr = create!(FunctionPtr, upvalue_ptrs: vec![Register(4), Register(33)]);
        let ptr_id = *ptr.unique_id.as_ref();

        let function_ref = value_to_ref!(LpcValue::Function(ptr), pool);

        let array = LpcArray::new(vec![function_ref]);

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();

        array.mark(&mut marked, &mut processed).unwrap();

        let mut marked_expected = BitSet::new();
        marked_expected.extend([4_usize, 33_usize].into_iter());

        let mut processed_expected = BitSet::new();
        processed_expected.extend([ptr_id, *array.unique_id.as_ref()].into_iter());

        assert_eq!(marked, marked_expected);
        assert_eq!(processed, processed_expected);
    }
}
