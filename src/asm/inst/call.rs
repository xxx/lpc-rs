use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Call {
    pub name: String,
    pub num_args: usize,
    pub initial_arg: Register
}

impl InstructionTrait for Call {}

impl Display for Call {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "call {}, {}, {}", self.name, self.num_args, self.initial_arg)
    }
}