use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    ops::{Add, Deref, Index, IndexMut, Range, RangeInclusive},
};

use bit_set::BitSet;
use delegate::delegate;
use if_chain::if_chain;
use thin_vec::ThinVec;
use tracing::{instrument, trace};

use crate::interpreter::{
    gc::{mark::Mark, unique_id::UniqueId},
    heap::Heap,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::LpcRef,
};

/// A newtype wrapper for an array of [`LpcRef`]s, with a [`UniqueId`] for GC
/// purposes.
#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct LpcArray {
    pub unique_id: UniqueId,
    pub array: ThinVec<LpcRef>,
}

impl LpcArray {
    /// Create a new [`LpcArray`].
    #[inline]
    pub fn new<A>(array: A) -> Self
    where
        A: Into<ThinVec<LpcRef>>,
    {
        Self {
            unique_id: UniqueId::new(),
            array: array.into(),
        }
    }

    /// Create a new [`LpcArray`] with the given capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            unique_id: UniqueId::new(),
            array: ThinVec::with_capacity(capacity),
        }
    }

    delegate! {
        to self.array {
            pub fn extend<T>(&mut self, iter: T)
                where T: IntoIterator<Item = LpcRef>;

            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
            pub fn push(&mut self, value: LpcRef);
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
            if &*other.read() == array;
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
        // Use display format to avoid infinite loops via inventory, etc.
        // TODO: this could be more elegant
        f.write_str(&format_array(self, |item| format!("{}", item)))?;
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
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> lpc_rs_errors::Result<()> {
        trace!("marking array");

        if !processed.insert(*self.unique_id.as_ref() as usize) {
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

impl From<&[LpcRef]> for LpcArray {
    #[inline]
    fn from(array: &[LpcRef]) -> Self {
        Self::new(ThinVec::from(array))
    }
}

impl<'a> FromIterator<&'a LpcRef> for LpcArray {
    #[inline]
    fn from_iter<T: IntoIterator<Item = &'a LpcRef>>(iter: T) -> Self {
        Self::new(iter.into_iter().cloned().collect::<ThinVec<_>>())
    }
}

impl FromIterator<LpcRef> for LpcArray {
    #[inline]
    fn from_iter<T: IntoIterator<Item = LpcRef>>(iter: T) -> Self {
        Self::new(iter.into_iter().collect::<ThinVec<_>>())
    }
}

impl IntoIterator for LpcArray {
    type Item = LpcRef;
    type IntoIter = thin_vec::IntoIter<LpcRef>;

    #[inline]
    fn into_iter(self) -> thin_vec::IntoIter<LpcRef> {
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

impl<T> PartialEq<[T]> for LpcArray
where
    T: Into<LpcRef> + Copy,
{
    fn eq(&self, other: &[T]) -> bool {
        self.array
            .iter()
            .zip(other.iter())
            .all(|(a, b)| a == &Into::<LpcRef>::into(*b))
    }
}

impl Add<LpcArray> for LpcArray {
    type Output = LpcArray;

    #[inline]
    fn add(self, rhs: LpcArray) -> Self::Output {
        let mut array = self.array;
        array.extend(rhs.array);
        Self::new(array)
    }
}

impl IntoLpcRef for LpcArray {
    #[inline]
    fn into_lpc_ref(self, memory: &Heap) -> LpcRef {
        memory.alloc_array(self)
    }
}

#[cfg(test)]
mod tests {
    use factori::create;
    use lpc_rs_core::register::Register;
    use thin_vec::thin_vec;

    use super::*;
    use crate::test_support::factories::*;

    #[test]
    fn test_mark() {
        let memory = Heap::new(5);

        let ptr = create!(
            FunctionPtr,
            upvalue_ptrs: thin_vec![Register(4), Register(33)]
        );
        let ptr_id = *ptr.unique_id.as_ref();

        let function_ref = ptr.into_lpc_ref(&memory);

        let array = LpcArray::new(thin_vec![function_ref]);

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();

        array.mark(&mut marked, &mut processed).unwrap();

        let mut marked_expected = BitSet::new();
        marked_expected.extend([4_usize, 33_usize].into_iter());

        let mut processed_expected = BitSet::new();
        processed_expected
            .extend([ptr_id as usize, *array.unique_id.as_ref() as usize].into_iter());

        assert_eq!(marked, marked_expected);
        assert_eq!(processed, processed_expected);
    }
}
