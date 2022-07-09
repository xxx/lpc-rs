use std::{
    fmt,
    fmt::{Display, Formatter},
};
use lpc_rs_errors::Result;

use lpc_rs_errors::span::Span;
use crate::compiler::ast::{
    ast_node::{AstNodeTrait, SpannedNode},
    expression_node::ExpressionNode,
};
use crate::compiler::codegen::tree_walker::TreeWalker;

/// A node representing a ternary expression
#[derive(Debug, Clone, Hash, Eq, PartialOrd, PartialEq)]
pub struct TernaryNode {
    pub condition: Box<ExpressionNode>,
    pub body: Box<ExpressionNode>,
    pub else_clause: Box<ExpressionNode>,
    pub span: Option<Span>,
}

impl TernaryNode {
    pub fn new(
        condition: ExpressionNode,
        body: ExpressionNode,
        else_clause: ExpressionNode,
        span: Option<Span>,
    ) -> Self {
        Self {
            condition: Box::new(condition),
            body: Box::new(body),
            else_clause: Box::new(else_clause),
            span,
        }
    }
}

impl SpannedNode for TernaryNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for TernaryNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_ternary(self)
    }
}

impl Display for TernaryNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ? {} : {}",
            self.condition, self.body, self.else_clause
        )
    }
}
