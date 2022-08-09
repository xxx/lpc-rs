use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};

pub enum RegisterVariant {
    Register(Register),
    Upvalue(Register)
}

/// A newtype around a usize representing a Register numbered with its value, `x.0`.
#[derive(Debug, Hash, Copy, Clone, Eq, PartialEq, Default, Serialize, Deserialize)]
pub struct Register(pub usize);

impl Register {
    #[inline]
    /// An alias to get the number.
    pub fn index(self) -> usize {
        self.0
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
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
