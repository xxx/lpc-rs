use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IConst0(pub Register);

impl InstructionTrait for IConst0 {}

impl Display for IConst0 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "iconst0 {}", &self.0)
    }
}