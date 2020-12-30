use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IConst(Register, i64);

impl Instruction for IConst {
    fn to_str(&self) -> String {
        format!("iconst {}, {}", &self.0, &self.1)
    }
}