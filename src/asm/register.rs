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