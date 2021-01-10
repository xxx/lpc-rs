use core::fmt::Debug;
use crate::ast::{expression_node, int_node, program_node};
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;
use crate::codegen::tree_walker::TreeWalker;
use auto_impl::auto_impl;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use std::fmt::Display;

#[derive(Debug, Eq, PartialEq)]
pub enum ASTNode {
    Program(ProgramNode),
    Expression(ExpressionNode),
    Call(CallNode)
}

#[auto_impl(&, &mut)]
pub trait ASTNodeTrait: PartialEq + Display {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::binary_op_node::BinaryOperation;

    #[test]
    fn test_from_expression_node() {
        let node = ExpressionNode::Int(IntNode::new(123));
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(clone));
    }

    #[test]
    fn test_from_int_node() {
        let node = IntNode::new(123);
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(ExpressionNode::Int(clone)));
    }

    #[test]
    fn test_from_binary_op_node() {
        let node: BinaryOpNode = BinaryOpNode {
            l: Box::new(ExpressionNode::Int(IntNode::new(123))),
            r: Box::new(ExpressionNode::Int(IntNode::new(1233))),
            op: BinaryOperation::Add
        };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(ExpressionNode::BinaryOp(clone)));
    }

    #[test]
    fn test_from_program_node() {
        let node: ProgramNode = Default::default();
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Program(clone));
    }
}