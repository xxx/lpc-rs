use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IDiv(pub Register, pub Register, pub Register);

impl InstructionTrait for IDiv {
    fn to_str(&self) -> String {
        format!("idiv {}, {}, {}", &self.0, &self.1, &self.2)
    }
}