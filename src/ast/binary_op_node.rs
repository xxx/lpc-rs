use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    Result,
};

/// All possible binary operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryOperation {
    Add,
    AndAnd,
    Div,
    EqEq,
    Gt,
    Gte,
    Index,
    Lt,
    Lte,
    Mul,
    OrOr,
    Sub,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperation::Add => "+",
            BinaryOperation::AndAnd => "&&",
            BinaryOperation::Div => "/",
            BinaryOperation::EqEq => "==",
            BinaryOperation::Gt => ">",
            BinaryOperation::Gte => ">=",
            BinaryOperation::Index => "[]",
            BinaryOperation::Lt => "<",
            BinaryOperation::Lte => "<=",
            BinaryOperation::Mul => "*",
            BinaryOperation::OrOr => "||",
            BinaryOperation::Sub => "-",
        };

        write!(f, "{}", s)
    }
}

/// Representation of a binary operation
#[derive(Hash, Debug, Eq, PartialEq)]
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

impl SpannedNode for BinaryOpNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for BinaryOpNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
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
