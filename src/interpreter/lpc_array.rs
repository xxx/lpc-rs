use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
    ops::{Deref, Index, IndexMut, Range, RangeInclusive},
};

use bit_set::BitSet;
use delegate::delegate;
use if_chain::if_chain;
use qcell::QCellOwner;
use tracing::{instrument, trace};

use crate::{
    interpreter::{
        gc::{mark::Mark, unique_id::UniqueId},
        lpc_ref::LpcRef,
        lpc_value::LpcValue,
    },
    util::keyable::Keyable,
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
            if let LpcValue::Array(other) = &*other.borrow();
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
    #[instrument(skip(self, cell_key))]
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
        cell_key: &QCellOwner,
    ) -> lpc_rs_errors::Result<()> {
        trace!("marking array");

        if !processed.insert(*self.unique_id.as_ref()) {
            return Ok(());
        }

        for item in &self.array {
            item.mark(marked, processed, cell_key)?;
        }

        Ok(())
    }
}

impl<'a> Keyable<'a> for LpcArray {
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "LpcArray {{ ")?;
        write!(f, "unique_id: {:?}, ", self.unique_id)?;
        write!(f, "array: [")?;
        f.write_str(&format_array(self, |item| {
            format!("{:?}", item.with_key(cell_key))
        }))?;
        write!(f, "] }}")
    }

    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "[")?;
        f.write_str(&format_array(self, |item| {
            format!("{}", item.with_key(cell_key))
        }))?;
        write!(f, "]")
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner) {
        self.unique_id.hash(state);
        for item in &self.array {
            item.with_key(cell_key).hash(state);
        }
    }

    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool {
        self.unique_id == other.unique_id
            && self
                .array
                .iter()
                .zip(other.array.iter())
                .all(|(a, b)| a.with_key(cell_key) == b.with_key(cell_key))
    }

    fn keyable_partial_cmp(&self, other: &Self, cell_key: &QCellOwner) -> Option<Ordering> {
        self.unique_id
            .partial_cmp(&other.unique_id)
            .and_then(|ord| {
                if ord == Ordering::Equal {
                    self.array
                        .iter()
                        .zip(other.array.iter())
                        .map(|(a, b)| a.with_key(cell_key).partial_cmp(&b.with_key(cell_key)))
                        .find(|ord| ord != &Some(Ordering::Equal))
                        .unwrap_or(Some(Ordering::Equal))
                } else {
                    Some(ord)
                }
            })
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
