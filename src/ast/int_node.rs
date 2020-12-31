use crate::ast::ast_node::ASTNode;
use crate::asm::instruction::Instruction;
use crate::asm::register_counter::RegisterCounter;
use crate::asm::inst::iconst0::IConst0;
use crate::asm::inst::iconst::IConst;
use crate::asm::register::Register;
use crate::asm::inst::iconst1::IConst1;

#[derive(Debug, Copy, Clone)]
pub struct IntNode {
    pub value: i64,
}

impl ASTNode for IntNode {
    fn to_str(&self) -> String {
        format!("IntNode[{}]", self.value)
    }

    fn to_asm(&self) -> Vec<Box<Instruction>> {
        match self.value {
            0 => vec![Box::new(IConst0(RegisterCounter::next()).into())],
            1 => vec![Box::new(IConst1(RegisterCounter::next()).into())],
            n => vec![Box::new(IConst(RegisterCounter::next(), n).into())]
        }
    }
}