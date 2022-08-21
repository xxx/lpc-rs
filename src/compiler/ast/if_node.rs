use std::{
    fmt,
    fmt::{Display, Formatter},
};

use indextree::NodeId;
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing an `if` statement
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
pub struct IfNode {
    pub condition: ExpressionNode,
    pub body: Box<AstNode>,
    pub else_clause: Box<Option<AstNode>>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
}

impl IfNode {
    pub fn new(
        condition: ExpressionNode,
        body: AstNode,
        else_clause: Option<AstNode>,
        span: Option<Span>,
    ) -> Self {
        Self {
            condition,
            body: Box::new(body),
            else_clause: Box::new(else_clause),
            scope_id: None,
            span,
        }
    }
}

impl AstNodeTrait for IfNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_if(self)
    }
}

impl Display for IfNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let e = if let Some(clause) = &*self.else_clause {
            format!(" else {{ {} }}", clause)
        } else {
            String::from("")
        };

        write!(f, "if({}) {{ {} }}{}]", self.condition, self.body, e)
    }
}
