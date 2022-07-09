use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_core::LpcInt;
use lpc_rs_errors::Result;
use lpc_rs_errors::span::Span;
use crate::compiler::ast::ast_node::{AstNodeTrait, SpannedNode};
use crate::compiler::codegen::tree_walker::TreeWalker;

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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_int(self)
    }
}

impl Display for IntNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
