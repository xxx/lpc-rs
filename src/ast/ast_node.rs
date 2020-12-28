use core::fmt::Debug;
use enum_dispatch::enum_dispatch;
use crate::ast::{expression_node, int_node, program_node};
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;

#[enum_dispatch]
#[derive(Debug)]
pub enum ASTNodeType {
    ExpressionNode,
    IntNode,
    ProgramNode
}

#[enum_dispatch(ASTNodeType)]
pub trait ASTNode {
    fn to_str(&self) -> String;
}

impl ASTNode for Box<dyn ASTNode> {
    fn to_str(&self) -> String {
        self.as_ref().to_str()
    }
}

impl Debug for dyn ASTNode {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}
