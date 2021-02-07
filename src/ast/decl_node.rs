use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::var_init_node::VarInitNode;
use crate::semantic::lpc_type::LPCType;
use crate::errors::compiler_error::CompilerError;

/// A container for a set of variable declarations.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DeclNode {
    /// The declared type
    pub type_: LPCType,
    /// The list of variables, with their optional initializations
    pub initializations: Vec<VarInitNode>,
}

impl DeclNode {
    pub fn new(type_: LPCType, initializations: Vec<VarInitNode>) -> Self {
        Self { type_, initializations }
    }
}

impl ASTNodeTrait for DeclNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_decl(self)
    }
}

impl Display for DeclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "DeclNode[{} {:?}]", self.type_, self.initializations)
    }
}