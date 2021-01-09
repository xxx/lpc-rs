use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt;
use std::fmt::{Formatter, Display};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ILoad(pub Register, pub Register, pub i64);

impl InstructionTrait for ILoad {}

impl Display for ILoad {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "iload {}, {}, {}", self.0, self.1, self.2)
    }
}