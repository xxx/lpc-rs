use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;

/// A node representing the use of a variable.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarNode {
    /// The name of the variable.
    pub name: String,
}

impl VarNode {
    pub fn new(name: &str) -> Self {
        Self { name: String::from(name) }
    }
}

impl ASTNodeTrait for VarNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_var(self).unwrap();
    }
}

impl Display for VarNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VarNode[{}]", self.name)
    }
}