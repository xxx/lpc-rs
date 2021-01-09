use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ISub(pub Register, pub Register, pub Register);

impl InstructionTrait for ISub {}

impl Display for ISub {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "isub {}, {}, {}", &self.0, &self.1, &self.2)
    }
}