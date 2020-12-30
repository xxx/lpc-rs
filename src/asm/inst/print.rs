use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct Print(pub Register);

impl InstructionTrait for Print {
    fn to_str(&self) -> String {
        format!("print {}", &self.0)
    }
}