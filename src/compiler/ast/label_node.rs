use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representing a `switch` label
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct LabelNode {
    /// The node for `case` labels. `None` indicates `default`.
    pub case: Option<ExpressionNode>,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl LabelNode {
    /// Create a new [`LabelNode`] with using the passed expression as the case
    /// value
    pub fn new(case: ExpressionNode, span: Option<Span>) -> Self {
        Self {
            case: Some(case),
            span,
        }
    }

    /// Create a new `default` [`LabelNode`]
    pub fn new_default(span: Option<Span>) -> Self {
        Self { case: None, span }
    }

    pub fn is_default(&self) -> bool {
        self.case.is_none()
    }
}

impl SpannedNode for LabelNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for LabelNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_label(self)
    }
}

impl Display for LabelNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if let Some(expr) = &self.case {
            write!(f, "{expr}")
        } else {
            write!(f, "default")
        }
    }
}
