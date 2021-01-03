use crate::ast::ast_node::{ASTNodeTrait, ASTNode};
use crate::asm::instruction::Instruction;
use crate::asm::inst::iadd::IAdd;
use crate::asm::register_counter::RegisterCounter;
use crate::asm::inst::isub::ISub;
use crate::asm::inst::imul::IMul;
use crate::asm::inst::idiv::IDiv;
use crate::codegen::tree_walker::TreeWalkerTrait;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
pub struct ExpressionNode {
    pub l: Box<ASTNode>,
    pub r: Box<ASTNode>,
    pub op: BinaryOperation
}

impl ASTNodeTrait for ExpressionNode {
    fn to_str(&self) -> String {
        format!("ExpressionNode[{:?}]", self)
    }

    fn visit(&self, tree_walker: &impl TreeWalkerTrait) { tree_walker.visit_expression(self); }
}