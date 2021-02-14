use crate::{
    ast::ast_node::ASTNodeTrait, codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError, parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing an integer literal
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IntNode {
    pub value: i64,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl IntNode {
    pub fn new(value: i64) -> Self {
        Self { value, span: None }
    }
}

impl ASTNodeTrait for IntNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_int(self)
    }
}

impl Display for IntNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "IntNode[{}]", self.value)
    }
}
