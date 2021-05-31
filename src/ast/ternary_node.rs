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

/// A node representing a ternary expression
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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
            "TernaryNode[{} ? {} : {}]",
            self.condition, self.body, self.else_clause
        )
    }
}
