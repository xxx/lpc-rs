use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::Result;

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        label_node::LabelNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A wrapper for nodes to allow labels to be applied.
/// They are only allowed in `switch` statements.
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
pub struct LabeledStatementNode {
    node: Box<AstNode>,
    label: LabelNode,
}

impl LabeledStatementNode {
    pub fn new(node: AstNode, label: LabelNode) -> Self {
        Self {
            node: node.into(),
            label,
        }
    }
}

#[async_trait]
impl AstNodeTrait for LabeledStatementNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        self.label.visit(tree_walker).await?;
        self.node.visit(tree_walker).await
    }
}

impl Display for LabeledStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.label, self.node)
    }
}
