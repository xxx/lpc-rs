use std::fmt::{Display, Formatter};

use serde::{Deserialize, Serialize};

use crate::RegisterSize;

/// A struct to handle the split between normal, in-function registers,
/// and previously-closed-over local variables
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub enum RegisterVariant {
    Local(Register),
    Global(Register),
    Upvalue(Register),
}

impl RegisterVariant {
    #[inline]
    pub fn index(&self) -> RegisterSize {
        match self {
            RegisterVariant::Local(reg)
            | RegisterVariant::Global(reg)
            | RegisterVariant::Upvalue(reg) => reg.index(),
        }
    }

    #[inline]
    pub fn as_register(&self) -> Register {
        match self {
            RegisterVariant::Local(reg)
            | RegisterVariant::Global(reg)
            | RegisterVariant::Upvalue(reg) => *reg,
        }
    }
}

impl Display for RegisterVariant {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            RegisterVariant::Local(r) => r.to_string(),
            RegisterVariant::Global(r) => format!("g{}", r.index()),
            RegisterVariant::Upvalue(r) => format!("u{}", r.index()),
        };

        write!(f, "{s}")
    }
}

/// A newtype around a usize representing a Register numbered with its value,
/// `x.0`.
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct Register(pub RegisterSize);

impl Register {
    /// An alias to get the number.
    #[inline]
    pub fn index(self) -> RegisterSize {
        self.0
    }

    /// convenience method
    #[inline]
    pub fn as_local(&self) -> RegisterVariant {
        RegisterVariant::Local(*self)
    }

    /// convenience method
    #[inline]
    pub fn as_global(&self) -> RegisterVariant {
        RegisterVariant::Global(*self)
    }

    /// convenience method
    #[inline]
    pub fn as_upvalue(&self) -> RegisterVariant {
        RegisterVariant::Upvalue(*self)
    }
}

impl Display for Register {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl From<Register> for RegisterSize {
    #[inline]
    fn from(f: Register) -> Self {
        f.0
    }
}

impl From<&Register> for RegisterSize {
    #[inline]
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
