use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct RegCopy(pub Register, pub Register);

impl InstructionTrait for RegCopy {}

impl Display for RegCopy {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "regcopy {}, {}", self.0, self.1)
    }
}