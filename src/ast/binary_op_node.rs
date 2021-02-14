use crate::{
    ast::{ast_node::ASTNodeTrait, expression_node::ExpressionNode},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// All possible binary operations
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div,

    /// Index into an array or mapping
    Index
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperation::Add => "+",
            BinaryOperation::Sub => "-",
            BinaryOperation::Mul => "*",
            BinaryOperation::Div => "/",
            BinaryOperation::Index => "[]",
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
    pub op: BinaryOperation,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,
}

impl ASTNodeTrait for BinaryOpNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_binary_op(self)
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
            op: self.op,
            span: self.span,
        }
    }
}
