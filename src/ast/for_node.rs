use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNode, AstNodeTrait},
    codegen::tree_walker::TreeWalker,
    Result,
};

use crate::{ast::expression_node::ExpressionNode, parser::span::Span};
use indextree::NodeId;

/// A node representing a `while` loop
#[derive(Debug, Clone, PartialEq)]
pub struct ForNode {
    pub initializer: Box<Option<AstNode>>,
    pub condition: Option<ExpressionNode>,
    pub incrementer: Option<ExpressionNode>,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
}

impl ForNode {
    pub fn new(
        initializer: Option<AstNode>,
        condition: Option<ExpressionNode>,
        incrementer: Option<ExpressionNode>,
        body: AstNode,
        span: Option<Span>,
    ) -> Self {
        Self {
            initializer: Box::new(initializer),
            condition,
            incrementer,
            body: Box::new(body),
            scope_id: None,
            span,
        }
    }
}

impl AstNodeTrait for ForNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_for(self)
    }
}

impl Display for ForNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let opt_display = |opt: &Option<ExpressionNode>| {
            if let Some(c) = opt {
                c.to_string()
            } else {
                String::from("(None)")
            }
        };

        write!(
            f,
            "ForNode[init: {}, cond: {}, inc: {}, body: {}]",
            if let Some(c) = &*self.initializer {
                c.to_string()
            } else {
                String::from("(None)")
            },
            opt_display(&self.condition),
            opt_display(&self.incrementer),
            self.body
        )
    }
}