use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    ops::{Add, Deref, DerefMut},
};

use delegate::delegate;
use thin_vec::ThinVec;

use crate::interpreter::lpc_ref::LpcRef;

/// A newtype wrapper for an array of [`LpcRef`]s.
#[derive(Default, Clone, PartialEq, Eq, Hash)]
pub struct LpcArray {
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
            array: array.into(),
        }
    }

    /// Create a new [`LpcArray`] with the given capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
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
        result.push_str(&fun(item));
    }

    result
}

impl Debug for LpcArray {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "LpcArray {{ ")?;
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

impl Deref for LpcArray {
    type Target = [LpcRef];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.array
    }
}

impl DerefMut for LpcArray {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.array
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
