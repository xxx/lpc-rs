use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNode, AstNodeTrait},
    codegen::tree_walker::TreeWalker,
};

use crate::errors::LpcError;
use crate::ast::expression_node::ExpressionNode;
use indextree::NodeId;
use crate::parser::span::Span;

/// A node representing a `while` loop
#[derive(Debug, Clone, PartialEq)]
pub struct WhileNode {
    pub condition: ExpressionNode,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>
}

impl WhileNode {
    pub fn new(condition: ExpressionNode, body: AstNode, span: Option<Span>) -> Self {
        Self {
            condition,
            body: Box::new(body),
            scope_id: None,
            span,
        }
    }
}

impl AstNodeTrait for WhileNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
        tree_walker.visit_while(self)
    }
}

impl Display for WhileNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "WhileNode[cond: {}, body: {}]", self.condition, self.body)
    }
}
