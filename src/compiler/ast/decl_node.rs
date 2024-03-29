use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use itertools::Itertools;
use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::Result;

use crate::compiler::{
    ast::{ast_node::AstNodeTrait, var_init_node::VarInitNode},
    codegen::tree_walker::TreeWalker,
};

/// A container for a set of variable declarations.
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
pub struct DeclNode {
    /// The declared type
    pub type_: LpcType,
    /// The list of variables, with their optional initializations
    pub initializations: Vec<VarInitNode>,
}

#[async_trait]
impl AstNodeTrait for DeclNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_decl(self).await
    }
}

impl Display for DeclNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .initializations
            .iter()
            .map(|item| format!("{item}"))
            .join(", ");

        write!(f, "{} {}", self.type_, s)
    }
}
