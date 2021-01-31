use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::errors::CompilerError;

/// A node representing a string literal
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StringNode {
    pub value: String,
}

impl StringNode {
    pub fn new(value: &str) -> Self {
        Self { value: String::from(value) }
    }
}

impl ASTNodeTrait for StringNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_string(self)
    }
}

impl Display for StringNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "StringNode[{}]", self.value)
    }
}