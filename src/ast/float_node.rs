use crate::{
    ast::ast_node::ASTNodeTrait, codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError, parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing a float literal
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct FloatNode {
    pub value: f64,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl FloatNode {
    pub fn new(value: f64) -> Self {
        Self { value, span: None }
    }
}

impl ASTNodeTrait for FloatNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_float(self)
    }
}

impl Display for FloatNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "FloatNode[{}]", self.value)
    }
}
