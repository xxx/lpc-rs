use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct IMul(pub Register, pub Register, pub Register);

impl InstructionTrait for IMul {}

impl Display for IMul {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "imul {}, {}, {}", self.0, self.1, self.2)
    }
}