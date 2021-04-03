use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{ast_node::AstNodeTrait, var_init_node::VarInitNode},
    codegen::tree_walker::TreeWalker,
    semantic::lpc_type::LpcType,
};
use crate::compiler::compiler_error::CompilerError;

/// A container for a set of variable declarations.
#[derive(Debug, Clone, PartialEq)]
pub struct DeclNode {
    /// The declared type
    pub type_: LpcType,
    /// The list of variables, with their optional initializations
    pub initializations: Vec<VarInitNode>,
}

impl DeclNode {
    pub fn new(type_: LpcType, initializations: Vec<VarInitNode>) -> Self {
        Self {
            type_,
            initializations,
        }
    }
}

impl AstNodeTrait for DeclNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_decl(self)
    }
}

impl Display for DeclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "DeclNode[{} {:?}]", self.type_, self.initializations)
    }
}
