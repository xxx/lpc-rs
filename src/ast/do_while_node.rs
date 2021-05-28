use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNode, AstNodeTrait},
    codegen::tree_walker::TreeWalker,
    Result
};

use crate::{ast::expression_node::ExpressionNode, parser::span::Span};
use indextree::NodeId;

/// A node representing a `do {} while` loop
#[derive(Debug, Clone, PartialEq)]
pub struct DoWhileNode {
    pub condition: ExpressionNode,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
}

impl DoWhileNode {
    pub fn new( body: AstNode, condition: ExpressionNode, span: Option<Span>) -> Self {
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
        write!(
            f,
            "DoWhileNode[body: {}, cond: {}]",
            self.body, self.condition
        )
    }
}
