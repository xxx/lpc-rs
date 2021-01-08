use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone)]
pub struct IStore(pub Register, pub Register, pub i64);

impl InstructionTrait for IStore {}

impl Display for IStore {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "istore {}, {}, {}", self.0, self.1, self.2)
    }
}