use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Print(pub Register);

impl InstructionTrait for Print {}

impl Display for Print {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "print {}", self.0)
    }
}