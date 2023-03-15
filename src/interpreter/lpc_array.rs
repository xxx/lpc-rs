use std::collections::HashSet;
use std::ops::{Deref, Index, IndexMut, Range, RangeInclusive};
use bit_set::BitSet;
use delegate::delegate;
use qcell::QCellOwner;
use crate::interpreter::gc::unique_id::{GcMark, UniqueId};
use crate::interpreter::lpc_ref::LpcRef;

/// A newtype wrapper for an array of [`LpcRef`]s, with a [`UniqueId`] for GC purposes.
#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
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

            pub fn len(&self) -> usize;
        }
    }
}

impl GcMark for LpcArray {
    fn mark(&self, marked: &mut BitSet, processed: &mut HashSet<UniqueId>, cell_key: &QCellOwner) -> lpc_rs_errors::Result<()> {
        if !processed.insert(self.unique_id) {
            return Ok(());
        }

        for item in &self.array {
            item.mark(marked, processed, cell_key)?;
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
    fn from_iter<T: IntoIterator<Item=&'a LpcRef>>(iter: T) -> Self {
        Self::new(iter.into_iter().cloned().collect())
    }
}

impl FromIterator<LpcRef> for LpcArray {
    #[inline]
    fn from_iter<T: IntoIterator<Item=LpcRef>>(iter: T) -> Self {
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