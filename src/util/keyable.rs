use std::fmt::{Debug, Display, Formatter};
use qcell::QCellOwner;

pub struct WithKey<'a, T> {
    pub value: &'a T,
    pub cell_key: &'a QCellOwner,
}

/// A trait for types that can be paired with a [`QCellOwner`].
pub trait Keyable<'a> {
    /// Get a pairing of me and the cell key.
    fn with_key(&'a self, cell_key: &'a QCellOwner) -> WithKey<'a, Self>
        where
            Self: Sized
    {
        WithKey {
            value: self,
            cell_key,
        }
    }

    /// Key-aware `Debug`
    fn keyable_debug(&self, cell_key: &QCellOwner) -> String;

    /// Key-aware `Display`
    fn keyable_display(&self, cell_key: &QCellOwner) -> String {
        self.keyable_debug(cell_key)
    }
}

impl<'a, T> Display for WithKey<'a, T>
where
    T: Keyable<'a>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value.keyable_display(self.cell_key))
    }
}

impl<'a, T> Debug for WithKey<'a, T>
where
    T: Keyable<'a>
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.value.keyable_debug(self.cell_key))
    }
}

impl<'a, T> PartialEq for WithKey<'a, T>
where
    T: Keyable<'a> + PartialEq
{
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}