use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::Add,
};

/// Really just a `pc` index in the vm.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Address(pub usize);

impl Display for Address {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:04x}", self.0)
    }
}

impl From<usize> for Address {
    #[inline]
    fn from(value: usize) -> Self {
        Self(value)
    }
}

impl From<Address> for usize {
    #[inline]
    fn from(value: Address) -> Self {
        value.0
    }
}

impl PartialEq<usize> for Address {
    #[inline]
    fn eq(&self, other: &usize) -> bool {
        self.0 == *other
    }
}

impl Add<usize> for Address {
    type Output = Self;

    #[inline]
    fn add(self, rhs: usize) -> Self::Output {
        Self(self.0 + rhs)
    }
}

pub type Label = String;
