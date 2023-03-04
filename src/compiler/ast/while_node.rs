use std::{
    fmt,
    fmt::{Display, Formatter},
};

use indextree::NodeId;
use qcell::QCellOwner;
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing a `while` loop
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
pub struct WhileNode {
    pub condition: ExpressionNode,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker, cell_key: &mut QCellOwner) -> Result<()> {
        tree_walker.visit_while(self, cell_key)
    }
}

impl Display for WhileNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "while({}) {{ {} }}]", self.condition, self.body)
    }
}
