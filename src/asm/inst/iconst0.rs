use crate::asm::register::Register;
use crate::asm::instruction::Instruction;

struct IConst0(Register);

impl Instruction for IConst0 {
    fn to_str(&self) -> String {
        format!("iconst0 {}", &self.0)
    }
}