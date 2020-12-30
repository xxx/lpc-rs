use crate::asm::register::Register;
use crate::asm::instruction::InstructionTrait;

#[derive(Debug, Clone)]
pub struct IConst0(pub Register);

impl InstructionTrait for IConst0 {
    fn to_str(&self) -> String {
        format!("iconst0 {}", &self.0)
    }
}