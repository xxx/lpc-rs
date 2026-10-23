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
        self.array.len() == other.len()
            && self
                .array
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

#[cfg(test)]
mod tests {
    use lpc_rs_core::LpcIntInner;

    use super::{LpcArray, LpcRef};

    fn array_from_ints(values: &[LpcIntInner]) -> LpcArray {
        LpcArray::new(values.iter().copied().map(LpcRef::from).collect::<Vec<_>>())
    }

    #[test]
    fn slice_comparison_checks_lengths_and_contents() {
        let cases: &[(&[LpcIntInner], &[LpcIntInner], bool)] = &[
            (&[1, 2], &[1], false),
            (&[1], &[1, 2], false),
            (&[], &[1], false),
            (&[1], &[], false),
            (&[], &[], true),
            (&[1, 2], &[1, 2], true),
            (&[1, 2], &[1, 3], false),
        ];
        for &(left, right, expected) in cases {
            assert_eq!(
                array_from_ints(left).eq(right),
                expected,
                "{left:?} compared with {right:?}"
            );
        }
    }

    #[test]
    fn collection_constructors_preserve_values_and_order() {
        let expected = vec![LpcRef::from(7), LpcRef::from(3), LpcRef::from(-1)];
        let from_slice = LpcArray::from(expected.as_slice());
        let from_borrowed: LpcArray = expected.iter().collect();
        let from_owned: LpcArray = expected.clone().into_iter().collect();
        for array in [from_slice, from_borrowed, from_owned] {
            assert_eq!(&*array, expected.as_slice());
        }
    }

    #[test]
    fn reserved_capacity_does_not_add_elements() {
        for capacity in [0, 3, 128] {
            let array = LpcArray::with_capacity(capacity);
            assert!(array.is_empty());
            assert!(array.array.capacity() >= capacity);
        }
    }

    #[test]
    fn formatting_preserves_contents_and_separators() {
        for (values, display, debug) in [
            ([].as_slice(), "[]", "LpcArray { array: [] }"),
            ([7].as_slice(), "[7]", "LpcArray { array: [7] }"),
            ([7, 3].as_slice(), "[7, 3]", "LpcArray { array: [7, 3] }"),
        ] {
            let array = array_from_ints(values);
            assert_eq!(array.to_string(), display);
            assert_eq!(format!("{array:?}"), debug);
        }
    }

    #[test]
    fn mutable_dereference_updates_the_same_array() {
        let mut array = array_from_ints(&[7, 3]);
        array[1] = LpcRef::from(11);
        assert_eq!(&*array, [LpcRef::from(7), LpcRef::from(11)].as_slice());
    }

    #[test]
    fn owned_iteration_preserves_values_and_order() {
        let expected = vec![LpcRef::from(7), LpcRef::from(3)];
        let actual: Vec<_> = LpcArray::new(expected.clone()).into_iter().collect();
        assert_eq!(actual, expected);
    }

    #[test]
    fn vector_comparison_checks_lengths_and_contents() {
        let array = array_from_ints(&[7, 3]);
        for (values, expected) in [
            ([7, 3].as_slice(), true),
            ([7, 4].as_slice(), false),
            ([7].as_slice(), false),
            ([7, 3, 1].as_slice(), false),
            ([].as_slice(), false),
        ] {
            let other: Vec<_> = values.iter().copied().map(LpcRef::from).collect();
            assert_eq!(array == other, expected, "{values:?}");
        }
    }

    #[test]
    fn concatenation_preserves_left_then_right_values() {
        let array = array_from_ints(&[7, 3]) + array_from_ints(&[11, 5]);
        assert_eq!(
            &*array,
            [
                LpcRef::from(7),
                LpcRef::from(3),
                LpcRef::from(11),
                LpcRef::from(5),
            ]
            .as_slice()
        );
    }
}
