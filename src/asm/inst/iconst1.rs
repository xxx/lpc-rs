use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IConst1(pub Register);

impl InstructionTrait for IConst1 {
    fn to_str(&self) -> String {
        format!("iconst1 {}", &self.0)
    }
}