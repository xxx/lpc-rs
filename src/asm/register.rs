use std::fmt::{Display, Formatter};

/// A thin wrapper around a usize representing a Register numbered with its value, `x.0`.
/// This type exists to allow us to attach the Display trait.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct Register(pub usize);

impl Register {
    #[inline(always)]
    /// An alias to get the number.
    pub fn index(&self) -> usize {
        self.0
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "r{}", self.0)
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
