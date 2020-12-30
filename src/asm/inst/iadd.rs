use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IAdd(Register, Register, Register);

impl Instruction for IAdd {
    fn to_str(&self) -> String {
        format!("iadd {}, {}, {}", &self.0, &self.1, &self.2)
    }
}