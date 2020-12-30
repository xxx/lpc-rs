use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct ISub(pub Register, pub Register, pub Register);

impl InstructionTrait for ISub {
    fn to_str(&self) -> String {
        format!("isub {}, {}, {}", &self.0, &self.1, &self.2)
    }
}