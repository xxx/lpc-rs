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

/// All possible unary operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum UnaryOperation {
    Negate,
    Inc,
    Dec,
    Bang,
    Tilde,
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOperation::Negate => "-",
            UnaryOperation::Inc => "++",
            UnaryOperation::Dec => "--",
            UnaryOperation::Bang => "!",
            UnaryOperation::Tilde => "~",
        };

        write!(f, "{}", s)
    }
}

/// Representation of a binary operation
#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub struct UnaryOpNode {
    pub expr: Box<ExpressionNode>,

    pub op: UnaryOperation,

    /// Is this a post increment/decrement?
    /// We're doing this instead of having separate node types for
    /// pre/post unary ops
    pub is_post: bool,

    pub span: Option<Span>,
}

impl SpannedNode for UnaryOpNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for UnaryOpNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_unary_op(self)
    }
}

impl Display for UnaryOpNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "UnaryOpNode[{:?}]", self)
    }
}