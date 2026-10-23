use std::fmt::{Display, Formatter};

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A creation-time expression, replaced with a hidden variable read during lowering.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct CaptureNode {
    /// Unique even when parser desugaring duplicates this node.
    pub name: Ustr,
    pub expr: Box<ExpressionNode>,
    pub span: Option<Span>,
}

impl SpannedNode for CaptureNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for CaptureNode {
    async fn visit(&mut self, walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        walker.visit_capture(self).await
    }
}

impl Display for CaptureNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "$({})", self.expr)
    }
}
