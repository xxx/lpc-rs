use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::expression_node::ExpressionNode;
use std::fmt::{Display, Formatter};
use std::fmt;

/// All possible binary operations
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperation::Add => "+",
            BinaryOperation::Sub => "-",
            BinaryOperation::Mul => "*",
            BinaryOperation::Div => "/"
        };

        write!(f, "{}", s)
    }
}

/// Representation of a binary operation
#[derive(Debug, Eq, PartialEq)]
pub struct BinaryOpNode {
    /// Left-hand side
    pub l: Box<ExpressionNode>,
    /// Right-hand side
    pub r: Box<ExpressionNode>,
    /// The operation to perform
    pub op: BinaryOperation
}

impl ASTNodeTrait for BinaryOpNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_binary_op(self).unwrap();
    }
}

impl Display for BinaryOpNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "BinaryOpNode[{:?}]", self)
    }
}

impl Clone for BinaryOpNode {
    fn clone(&self) -> Self {
        Self {
            l: Box::new((*self.l).clone()),
            r: Box::new((*self.r).clone()),
            op: self.op
        }
    }
}
