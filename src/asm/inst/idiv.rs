use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct IDiv(pub Register, pub Register, pub Register);

impl InstructionTrait for IDiv {}

impl Display for IDiv {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "idiv {}, {}, {}", self.0, self.1, self.2)
    }
}