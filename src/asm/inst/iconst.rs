use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IConst(pub Register, pub i64);

impl InstructionTrait for IConst {
    fn to_str(&self) -> String {
        format!("iconst {}, {}", &self.0, &self.1)
    }
}