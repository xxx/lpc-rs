use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IStore(Register, Register, i64);

impl Instruction for IStore {
    fn to_str(&self) -> String {
        format!("istore {}, {}, {}", &self.0, &self.1, &self.2)
    }
}