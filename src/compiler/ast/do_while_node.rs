use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::compiler::ast::expression_node::ExpressionNode;
use indextree::NodeId;
use lpc_rs_errors::Result;
use lpc_rs_errors::span::Span;
use crate::compiler::ast::ast_node::{AstNode, AstNodeTrait};
use crate::compiler::codegen::tree_walker::TreeWalker;

/// A node representing a `do {} while` loop
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct DoWhileNode {
    pub condition: ExpressionNode,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
}

impl DoWhileNode {
    pub fn new(body: AstNode, condition: ExpressionNode, span: Option<Span>) -> Self {
        Self {
            condition,
            body: Box::new(body),
            scope_id: None,
            span,
        }
    }
}

impl AstNodeTrait for DoWhileNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_do_while(self)
    }
}

impl Display for DoWhileNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "do {{ {} }} while ({})]", self.body, self.condition)
    }
}
