use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IDiv(Register, Register, Register);

impl Instruction for IDiv {
    fn to_str(&self) -> String {
        format!("idiv {}, {}, {}", &self.0, &self.1, &self.2)
    }
}