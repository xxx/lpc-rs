use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct ILoad(Register, Register, i64);

impl Instruction for ILoad {
    fn to_str(&self) -> String {
        format!("iload {}, {}, {}", &self.0, &self.1, &self.2)
    }
}