use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::{span::Span, Result};

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};

/// A node representing a `continue` statement.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct ContinueNode {
    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl ContinueNode {
    pub fn new(span: Option<Span>) -> Self {
        Self { span }
    }
}

impl SpannedNode for ContinueNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for ContinueNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_continue(self).await
    }
}

impl Display for ContinueNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "continue")
    }
}
