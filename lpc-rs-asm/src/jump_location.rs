use std::fmt::{Display, Formatter};
use std::fmt;
use serde::{Deserialize, Serialize};

/// Really just a `pc` index in the vm.
pub type Address = usize;

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
    fn from(label: Label) -> Self {
        JumpLocation::Label(label)
    }
}

impl From<&Label> for JumpLocation {
    fn from(label: &Label) -> Self {
        JumpLocation::Label(label.clone())
    }
}

impl From<&str> for JumpLocation {
    fn from(label: &str) -> Self {
        JumpLocation::Label(label.to_string())
    }
}

impl From<Address> for JumpLocation {
    fn from(address: Address) -> Self {
        JumpLocation::Address(address)
    }
}
