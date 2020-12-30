use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IMul(pub Register, pub Register, pub Register);

impl InstructionTrait for IMul {
    fn to_str(&self) -> String {
        format!("imul {}, {}, {}", &self.0, &self.1, &self.2)
    }
}