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

/// A node representing a `switch` statement
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct SwitchNode {
    pub expression: ExpressionNode,
    pub body: Box<AstNode>,
    pub span: Option<Span>,
}

impl SwitchNode {
    pub fn new(expression: ExpressionNode, body: AstNode, span: Option<Span>) -> Self {
        Self {
            expression,
            body: Box::new(body),
            span,
        }
    }
}

impl AstNodeTrait for SwitchNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_switch(self)
    }
}

impl Display for SwitchNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "switch({}) {{ {} }}]", self.expression, self.body)
    }
}