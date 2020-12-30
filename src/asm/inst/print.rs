use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct Print(Register);

impl Instruction for Print {
    fn to_str(&self) -> String {
        format!("print {}", &self.0)
    }
}