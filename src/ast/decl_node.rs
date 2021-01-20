use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::var_init_node::VarInitNode;
use crate::semantic::lpc_type::LPCVarType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct DeclNode {
    pub var_type: LPCVarType,
    pub initializations: Vec<VarInitNode>,
}

impl DeclNode {
    pub fn new(var_type: LPCVarType, initializations: Vec<VarInitNode>) -> Self {
        Self { var_type, initializations }
    }
}

impl ASTNodeTrait for DeclNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_decl(self); }
}

impl Display for DeclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "DeclNode[{} {:?}]", self.var_type, self.initializations)
    }
}