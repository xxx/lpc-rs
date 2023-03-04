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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker, cell_key: &mut QCellOwner) -> Result<()> {
        tree_walker.visit_for(self, cell_key)
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
            "for ({}; {}; {}) {{ {} }}",
            if let Some(c) = &*self.initializer {
                c.to_string()
            } else {
                String::from("")
            },
            opt_display(&self.condition),
            opt_display(&self.incrementer),
            self.body
        )
    }
}
