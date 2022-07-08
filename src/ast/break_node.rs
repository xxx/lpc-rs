use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
    Result,
};
use lpc_rs_errors::span::Span;

/// A node representing a `break` statement.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct BreakNode {
    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl BreakNode {
    pub fn new(span: Option<Span>) -> Self {
        Self { span }
    }
}

impl SpannedNode for BreakNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for BreakNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_break(self)
    }
}

impl Display for BreakNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "break")
    }
}
