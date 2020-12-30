use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct ISub(Register, Register, Register);

impl Instruction for ISub {
    fn to_str(&self) -> String {
        format!("isub {}, {}, {}", &self.0, &self.1, &self.2)
    }
}