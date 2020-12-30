use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct ILoad(pub Register, pub Register, pub i64);

impl InstructionTrait for ILoad {
    fn to_str(&self) -> String {
        format!("iload {}, {}, {}", &self.0, &self.1, &self.2)
    }
}