use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IConst1(Register);

impl Instruction for IConst1 {
    fn to_str(&self) -> String {
        format!("iconst1 {}", &self.0)
    }
}