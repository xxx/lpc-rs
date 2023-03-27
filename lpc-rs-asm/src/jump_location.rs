use std::{
    fmt,
    fmt::{Display, Formatter},
    ops::Add,
};

use serde::{Deserialize, Serialize};

/// Really just a `pc` index in the vm.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

/// A jump target, either a label or an address
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum JumpLocation {
    /// Jump to a label
    Label(Label),
    /// Jump to an address
    Address(Address),
}

impl Display for JumpLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            JumpLocation::Label(label) => write!(f, "{}", label),
            JumpLocation::Address(address) => write!(f, "{}", address),
        }
    }
}

impl From<Label> for JumpLocation {
    #[inline]
    fn from(label: Label) -> Self {
        JumpLocation::Label(label)
    }
}

impl From<&Label> for JumpLocation {
    #[inline]
    fn from(label: &Label) -> Self {
        JumpLocation::Label(label.clone())
    }
}

impl From<&str> for JumpLocation {
    #[inline]
    fn from(label: &str) -> Self {
        JumpLocation::Label(label.to_string())
    }
}

impl From<Address> for JumpLocation {
    #[inline]
    fn from(address: Address) -> Self {
        JumpLocation::Address(address)
    }
}
