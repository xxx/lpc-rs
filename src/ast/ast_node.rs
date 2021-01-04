use core::fmt::Debug;
use crate::ast::{expression_node, int_node, program_node};
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;
use crate::codegen::tree_walker::TreeWalkerTrait;
use auto_impl::auto_impl;
use crate::ast::binary_op_node::BinaryOpNode;

#[derive(Debug)]
pub enum ASTNode {
    Expression(Box<ExpressionNode>),
    Program(Box<ProgramNode>)
}

#[auto_impl(&, &mut)]
pub trait ASTNodeTrait {
    fn to_str(&self) -> String;
    fn visit(&self, tree_walker: &mut impl TreeWalkerTrait);
}

impl From<ExpressionNode> for ASTNode {
    fn from(node: ExpressionNode) -> Self {
        ASTNode::Expression(Box::new(node))
    }
}

impl From<IntNode> for ASTNode {
    fn from(node: IntNode) -> Self {
        ASTNode::Expression(Box::new(ExpressionNode::Int(Box::new(node))))
    }
}

impl From<BinaryOpNode> for ASTNode {
    fn from(node: BinaryOpNode) -> Self {
        ASTNode::Expression(Box::new(ExpressionNode::BinaryOp(Box::new(node))))
    }
}

impl From<ProgramNode> for ASTNode {
    fn from(node: ProgramNode) -> Self {
        ASTNode::Program(Box::new(node))
    }
}