use std::{
    fmt,
    fmt::{Display, Formatter},
};

use itertools::Itertools;
use lpc_rs_errors::{span::Span, Result};
use qcell::QCellOwner;

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing a comma-separated list of expressions
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct CommaExpressionNode {
    pub value: Vec<ExpressionNode>,

    /// The full span of all expressions in the list - from the left side of the
    /// first item to the right side of the last
    pub span: Option<Span>,
}

impl CommaExpressionNode {
    /// Construct a new `CommaExpressionNode`. The `span` will be calculated
    /// as the span from the start of the first item, to the end of
    /// the last.
    pub fn new(value: Vec<ExpressionNode>) -> Self {
        let span = if value.is_empty() {
            None
        } else if let (Some(node1), Some(node2)) = (value[0].span(), value.last().unwrap().span()) {
            Some(Span {
                file_id: node1.file_id,
                l: node1.l,
                r: node2.r,
            })
        } else {
            None
        };

        Self { value, span }
    }
}

impl SpannedNode for CommaExpressionNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for CommaExpressionNode {
    fn visit(
        &mut self,
        tree_walker: &mut impl TreeWalker,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        tree_walker.visit_comma_expression(self, cell_key)
    }
}

impl Display for CommaExpressionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self.value.iter().map(|item| format!("{item}")).join(", ");
        write!(f, "{s}")
    }
}
