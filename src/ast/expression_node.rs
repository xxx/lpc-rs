use crate::ast::ast_node::{ASTNode, ASTNodeType};
use crate::asm::instruction::Instruction;
use crate::asm::inst::iadd::IAdd;
use crate::asm::register_counter::RegisterCounter;
use crate::asm::inst::isub::ISub;
use crate::asm::inst::imul::IMul;
use crate::asm::inst::idiv::IDiv;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
pub struct ExpressionNode {
    pub l: Box<ASTNodeType>,
    pub r: Box<ASTNodeType>,
    pub op: BinaryOperation
}

impl ASTNode for ExpressionNode {
    fn to_str(&self) -> String {
        format!("ExpressionNode[{:?}]", self)
    }

    fn to_asm(&self) -> Vec<Box<Instruction>> {
        let op_instruction: Instruction = match self.op {
            BinaryOperation::Add =>
                IAdd(RegisterCounter::next(), RegisterCounter::next(), RegisterCounter::next()).into(),
            BinaryOperation::Sub =>
                ISub(RegisterCounter::next(), RegisterCounter::next(), RegisterCounter::next()).into(),
            BinaryOperation::Mul =>
                IMul(RegisterCounter::next(), RegisterCounter::next(), RegisterCounter::next()).into(),
            BinaryOperation::Div =>
                IDiv(RegisterCounter::next(), RegisterCounter::next(), RegisterCounter::next()).into()
        };
        let mut left = (*(self.l).to_asm()).to_vec();
        let right = (*(self.r).to_asm()).to_vec();

        left.extend(right);
        left.push(Box::new(op_instruction));
        left
    }
}