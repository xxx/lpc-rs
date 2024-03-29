use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::{span::Span, Result};
use ustr::Ustr;

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};

/// A node representing an `inherit` statement.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct InheritNode {
    pub path: Ustr,

    pub namespace: Option<Ustr>,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl SpannedNode for InheritNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for InheritNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_inherit(self).await
    }
}

impl Display for InheritNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ns = match &self.namespace {
            Some(ns) => format!(" {ns}"),
            None => String::new(),
        };

        write!(f, "inherit {}{}", self.path, ns)
    }
}
