use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_core::LpcInt;
use lpc_rs_errors::{span::Span, Result};
use qcell::QCellOwner;

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};

/// A node representing an integer literal
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub struct IntNode {
    pub value: LpcInt,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl IntNode {
    pub fn new(value: LpcInt) -> Self {
        Self { value, span: None }
    }
}

impl SpannedNode for IntNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for IntNode {
    fn visit(
        &mut self,
        tree_walker: &mut impl TreeWalker,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        tree_walker.visit_int(self, cell_key)
    }
}

impl Display for IntNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
