use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct IAdd(pub Register, pub Register, pub Register);

impl InstructionTrait for IAdd {}

impl Display for IAdd {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "iadd {}, {}, {}", self.0, self.1, self.2)
    }
}