use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
pub struct Register(pub usize);

impl Register {
    pub fn value(&self) -> usize {
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
    fn test_value_is_correct() {
        let register = Register(666);
        assert_eq!(register.value(), register.0);
    }

    #[test]
    fn test_display_displays_with_the_prefix() {
        assert_eq!(format!("{}", Register(666)), "r666");
    }

    #[test]
    fn test_eq() {
        assert_eq!(Register(0), Register(0));
        assert_ne!(Register(0), Register(1));
    }
}
