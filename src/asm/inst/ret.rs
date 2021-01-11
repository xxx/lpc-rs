use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ret;

impl InstructionTrait for Ret {}

impl Display for Ret {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ret")
    }
}