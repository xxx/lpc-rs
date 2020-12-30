use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IAdd(pub Register, pub Register, pub Register);

impl InstructionTrait for IAdd {
    fn to_str(&self) -> String {
        format!("iadd {}, {}, {}", &self.0, &self.1, &self.2)
    }
}