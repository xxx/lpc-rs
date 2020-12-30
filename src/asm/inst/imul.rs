use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IMul(Register, Register, Register);

impl Instruction for IMul {
    fn to_str(&self) -> String {
        format!("imul {}, {}, {}", &self.0, &self.1, &self.2)
    }
}