use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarNode {
    pub value: String,
}

impl VarNode {
    pub fn new(value: &str) -> Self {
        Self { value: String::from(value) }
    }
}

impl ASTNodeTrait for VarNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_var(self); }
}

impl Display for VarNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VarNode[{}]", self.value)
    }
}