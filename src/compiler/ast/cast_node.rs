use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_errors::{Result, span::Span};

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// `(type) expr`: an assertion that the value is 0 or of `type_`, never a
/// conversion.
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct CastNode {
    pub expr: Box<ExpressionNode>,
    pub type_: LpcType,
    pub span: Option<Span>,
}

impl SpannedNode for CastNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for CastNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_cast(self).await
    }
}

impl Display for CastNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}) {}", self.type_, self.expr)
    }
}
