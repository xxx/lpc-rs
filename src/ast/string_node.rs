use crate::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing a string literal
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StringNode {
    pub value: String,

    /// The span of the string in the original file, including quotes
    pub span: Option<Span>,
}

impl StringNode {
    pub fn new(value: &str) -> Self {
        Self {
            value: String::from(value),
            span: None,
        }
    }
}

impl SpannedNode for StringNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for StringNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_string(self)
    }
}

impl Display for StringNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "StringNode[{}]", self.value)
    }
}
