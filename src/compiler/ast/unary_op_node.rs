use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// All possible unary operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub enum UnaryOperation {
    Negate,
    Inc,
    Dec,
    Bang,
    BitwiseNot,
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            UnaryOperation::Negate => "-",
            UnaryOperation::Inc => "++",
            UnaryOperation::Dec => "--",
            UnaryOperation::Bang => "!",
            UnaryOperation::BitwiseNot => "~",
        };

        write!(f, "{s}")
    }
}

/// Representation of a binary operation
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
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

#[async_trait]
impl AstNodeTrait for UnaryOpNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_unary_op(self).await
    }
}

impl Display for UnaryOpNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.is_post {
            write!(f, "{}{}", self.expr, self.op)
        } else {
            write!(f, "{}{}", self.op, self.expr)
        }
    }
}
