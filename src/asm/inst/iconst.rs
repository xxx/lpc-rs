use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt;
use std::fmt::{Formatter, Display};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IConst(pub Register, pub i64);

impl InstructionTrait for IConst {}

impl Display for IConst {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "iconst {}, {}", self.0, self.1)
    }
}