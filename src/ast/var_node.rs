use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::errors::CompilerError;
use crate::parser::span::Span;

/// A node representing the use of a variable.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarNode {
    /// The name of the variable.
    pub name: String,

    /// The span of the string in the original file
    pub span: Option<Span>
}

impl VarNode {
    pub fn new(name: &str) -> Self {
        Self { name: String::from(name), span: None }
    }
}

impl ASTNodeTrait for VarNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_var(self)
    }
}

impl Display for VarNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VarNode[{}]", self.name)
    }
}