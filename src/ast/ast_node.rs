use core::fmt::Debug;
use crate::ast::{expression_node, int_node, program_node};
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;
use crate::codegen::tree_walker::TreeWalker;
use auto_impl::auto_impl;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;

#[derive(Debug)]
pub enum ASTNode {
    Program(ProgramNode),
    Expression(ExpressionNode),
    Call(CallNode)
}

#[auto_impl(&, &mut)]
pub trait ASTNodeTrait {
    fn to_str(&self) -> String;
    fn visit(&self, tree_walker: &mut impl TreeWalker);
}

impl From<ExpressionNode> for ASTNode {
    fn from(node: ExpressionNode) -> Self {
        ASTNode::Expression(node)
    }
}

impl From<IntNode> for ASTNode {
    fn from(node: IntNode) -> Self {
        ASTNode::Expression(ExpressionNode::Int(node))
    }
}

impl From<BinaryOpNode> for ASTNode {
    fn from(node: BinaryOpNode) -> Self {
        ASTNode::Expression(ExpressionNode::BinaryOp(node))
    }
}

impl From<ProgramNode> for ASTNode {
    fn from(node: ProgramNode) -> Self {
        ASTNode::Program(node)
    }
}