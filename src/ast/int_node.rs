use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;

/// A node representing an integer literal
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct IntNode {
    pub value: i64,
}

impl IntNode {
    pub fn new(value: i64) -> Self {
        Self { value }
    }
}

impl ASTNodeTrait for IntNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_int(self).unwrap();
    }
}

impl Display for IntNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "IntNode[{}]", self.value)
    }
}