use crate::ast::ast_node::ASTNodeTrait;
use crate::asm::instruction::Instruction;
use crate::asm::register_counter::RegisterCounter;
use crate::asm::inst::iconst0::IConst0;
use crate::asm::inst::iconst::IConst;
use crate::asm::inst::iconst1::IConst1;
use crate::codegen::tree_walker::TreeWalkerTrait;

#[derive(Debug, Copy, Clone)]
pub struct IntNode {
    pub value: i64,
}

impl ASTNodeTrait for IntNode {
    fn to_str(&self) -> String {
        format!("IntNode[{}]", self.value)
    }

    fn visit(&self, tree_walker: &impl TreeWalkerTrait) { tree_walker.visit_int(self); }
}