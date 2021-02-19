use crate::{
    ast::ast_node::{ASTNode, ASTNodeTrait},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing a full object. This is the top-level translation unit.
#[derive(Debug, PartialEq, Clone, Default)]
pub struct ProgramNode {
    /// The list of function defs for this program
    pub body: Vec<ASTNode>,
}

impl ASTNodeTrait for ProgramNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_program(self)
    }
}

impl Display for ProgramNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ProgramNode[{:?}]", self)
    }
}
