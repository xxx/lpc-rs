use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    LpcInt, Result,
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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_int(self)
    }
}

impl Display for IntNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
