use std::fmt::{Display, Formatter};

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};

use crate::compiler::{
    ast::{ast_node::AstNodeTrait, block_node::BlockNode},
    codegen::tree_walker::TreeWalker,
};

/// A statement block evaluated for its catch result.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub struct CatchNode {
    pub body: BlockNode,
    pub span: Option<Span>,
}

#[async_trait]
impl AstNodeTrait for CatchNode {
    async fn visit(&mut self, walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        walker.visit_catch(self).await
    }
}

impl Display for CatchNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "catch {}", self.body)
    }
}
