use std::fmt::{Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub struct Register(pub usize);

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "r{}", self.0)
    }
}

impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(format!("{}", Register(666)), "r666");
    }

    #[test]
    fn test_eq() {
        assert_eq!(Register(0), Register(0));
        assert_ne!(Register(0), Register(1));
    }
}
