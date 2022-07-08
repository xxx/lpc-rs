use crate::{
    ast::{ast_node::AstNodeTrait, var_init_node::VarInitNode},
    codegen::tree_walker::TreeWalker,
    Result,
};
use itertools::Itertools;
use lpc_rs_core::lpc_type::LpcType;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A container for a set of variable declarations.
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct DeclNode {
    /// The declared type
    pub type_: LpcType,
    /// The list of variables, with their optional initializations
    pub initializations: Vec<VarInitNode>,
}

impl AstNodeTrait for DeclNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_decl(self)
    }
}

impl Display for DeclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .initializations
            .iter()
            .map(|item| format!("{}", item))
            .join(", ");

        write!(f, "{} {}", self.type_, s)
    }
}
