use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct Register(pub String);

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.0)
    }
}