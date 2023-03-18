use std::{
    cmp::Ordering,
    fmt::{Debug, Display, Formatter},
    hash::{Hash, Hasher},
};

use indexmap::IndexMap;
use qcell::QCellOwner;

#[derive(Clone)]
pub struct WithKey<'a, T> {
    pub value: &'a T,
    pub cell_key: &'a QCellOwner,
}

/// A trait for types that can be paired with a [`QCellOwner`].
pub trait Keyable<'a> {
    /// Get a pairing of me and the cell key.
    fn with_key(&'a self, cell_key: &'a QCellOwner) -> WithKey<'a, Self>
    where
        Self: Sized,
    {
        WithKey {
            value: self,
            cell_key,
        }
    }

    /// Key-aware `Debug`
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result;

    /// Key-aware `Display`
    fn keyable_display(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        self.keyable_debug(f, cell_key)
    }

    /// Key-aware `Hash`
    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner);

    /// Key-aware `PartialEq`
    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool;

    /// Key-aware `PartialOrd`
    fn keyable_partial_cmp(
        &self,
        other: &Self,
        cell_key: &QCellOwner,
    ) -> Option<std::cmp::Ordering>;
}

impl<'a, T> Display for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.keyable_display(f, self.cell_key)
    }
}

impl<'a, T> Debug for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.value.keyable_debug(f, self.cell_key)
    }
}

impl<'a, T> Hash for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.value.keyable_hash(state, self.cell_key)
    }
}

impl<'a, T> PartialEq for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn eq(&self, other: &Self) -> bool {
        self.value.keyable_eq(other.value, self.cell_key)
    }
}

impl<'a, T> PartialEq<T> for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn eq(&self, other: &T) -> bool {
        self.value.keyable_eq(other, self.cell_key)
    }
}

impl<'a, T> PartialEq<&T> for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn eq(&self, other: &&T) -> bool {
        self.value.keyable_eq(*other, self.cell_key)
    }
}

impl<'a, T> Eq for WithKey<'a, T> where T: Keyable<'a> {}

impl<'a, T> PartialOrd for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.value.keyable_partial_cmp(other.value, self.cell_key)
    }
}

impl<'a, T> PartialOrd<T> for WithKey<'a, T>
where
    T: Keyable<'a>,
{
    fn partial_cmp(&self, other: &T) -> Option<std::cmp::Ordering> {
        self.value.keyable_partial_cmp(other, self.cell_key)
    }
}

impl<'a, T> Keyable<'a> for Vec<T>
where
    T: Keyable<'a>,
{
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, item) in self.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            item.keyable_debug(f, cell_key)?;
        }
        write!(f, "]")
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner) {
        for item in self.iter() {
            item.keyable_hash(state, cell_key);
        }
    }

    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (a, b) in self.iter().zip(other.iter()) {
            if !a.keyable_eq(b, cell_key) {
                return false;
            }
        }
        true
    }

    fn keyable_partial_cmp(&self, other: &Self, cell_key: &QCellOwner) -> Option<Ordering> {
        if self.len() != other.len() {
            return Some(self.len().cmp(&other.len()));
        }
        for (a, b) in self.iter().zip(other.iter()) {
            if let Some(ordering) = a.keyable_partial_cmp(b, cell_key) {
                return Some(ordering);
            }
        }
        Some(Ordering::Equal)
    }
}

impl<'a, K, V> Keyable<'a> for IndexMap<K, V>
where
    K: Keyable<'a> + Eq + Hash,
    V: Keyable<'a>,
{
    fn keyable_debug(&self, f: &mut Formatter<'_>, cell_key: &QCellOwner) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            key.keyable_debug(f, cell_key)?;
            write!(f, ": ")?;
            value.keyable_debug(f, cell_key)?;
        }
        write!(f, "}}")
    }

    fn keyable_hash<H: Hasher>(&self, state: &mut H, cell_key: &QCellOwner) {
        for (key, value) in self.iter() {
            key.keyable_hash(state, cell_key);
            value.keyable_hash(state, cell_key);
        }
    }

    fn keyable_eq(&self, other: &Self, cell_key: &QCellOwner) -> bool {
        if self.len() != other.len() {
            return false;
        }
        for (key, value) in self.iter() {
            let Some(other_value) = other.get(key) else {
                return false;
            };

            if !value.keyable_eq(other_value, cell_key) {
                return false;
            }
        }
        true
    }

    fn keyable_partial_cmp(&self, other: &Self, cell_key: &QCellOwner) -> Option<Ordering> {
        if self.len() != other.len() {
            return Some(self.len().cmp(&other.len()));
        }
        for (key, value) in self.iter() {
            let Some(other_value) = other.get(key) else {
                return Some(Ordering::Greater);
            };

            if let Some(ordering) = value.keyable_partial_cmp(other_value, cell_key) {
                return Some(ordering);
            }
        }
        Some(Ordering::Equal)
    }
}
