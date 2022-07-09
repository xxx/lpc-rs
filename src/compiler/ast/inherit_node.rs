use lpc_rs_errors::Result;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};
use lpc_rs_errors::span::Span;

/// A node representing an `inherit` statement.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct InheritNode {
    pub path: String,

    pub namespace: Option<String>,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl SpannedNode for InheritNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for InheritNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_inherit(self)
    }
}

impl Display for InheritNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let ns = match &self.namespace {
            Some(ns) => format!(" {}", ns),
            None => String::new(),
        };

        write!(f, "inherit {}{}", self.path, ns)
    }
}
