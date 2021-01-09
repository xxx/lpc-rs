use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct IConst1(pub Register);

impl InstructionTrait for IConst1 {}

impl Display for IConst1 {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "iconst1 {}", &self.0)
    }
}