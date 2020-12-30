use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IStore(pub Register, pub Register, pub i64);

impl InstructionTrait for IStore {
    fn to_str(&self) -> String {
        format!("istore {}, {}, {}", &self.0, &self.1, &self.2)
    }
}