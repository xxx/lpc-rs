//! A `ref name` argument: the variable is passed by reference.

use std::fmt::{self, Display, Formatter};

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};

/// A variable passed by reference in a call's argument list.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct RefNode {
    /// The variable's name.
    pub name: Ustr,
    /// The span of `ref name` in the source.
    pub span: Option<Span>,
    /// Whether the variable is a global, set by the scope walker.
    pub global: bool,
}

impl RefNode {
    /// Record whether the variable resolved to a global.
    pub fn set_global(&mut self, val: bool) {
        self.global = val;
    }
}

impl SpannedNode for RefNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for RefNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_ref(self).await
    }
}

impl Display for RefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ref {}", self.name)
    }
}
