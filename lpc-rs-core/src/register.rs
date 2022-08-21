use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

/// A struct to handle the split between normal, in-function registers,
/// and previously-closed-over local variables
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum RegisterVariant {
    Local(Register),
    Upvalue(Register),
}

impl RegisterVariant {
    pub fn index(&self) -> usize {
        match self {
            RegisterVariant::Local(reg) | RegisterVariant::Upvalue(reg) => reg.index(),
        }
    }
}

impl Display for RegisterVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            RegisterVariant::Local(r) => r.to_string(),
            RegisterVariant::Upvalue(r) => format!("u{}", r.index()),
        };

        write!(f, "{}", s)
    }
}

/// A newtype around a usize representing a Register numbered with its value,
/// `x.0`.
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct Register(pub usize);

impl Register {
    #[inline]
    /// An alias to get the number.
    pub fn index(self) -> usize {
        self.0
    }

    /// convenience method
    pub fn as_local(&self) -> RegisterVariant {
        RegisterVariant::Local(*self)
    }

    /// convenience method
    pub fn as_upvalue(&self) -> RegisterVariant {
        RegisterVariant::Upvalue(*self)
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl From<Register> for usize {
    fn from(f: Register) -> Self {
        f.0
    }
}

impl From<&Register> for usize {
    fn from(f: &Register) -> Self {
        f.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_index_is_correct() {
        let register = Register(666);
        assert_eq!(register.index(), register.0);
    }

    #[test]
    fn test_display_displays_with_the_prefix() {
        assert_eq!(format!("{}", Register(666)), "r666");
    }
}
