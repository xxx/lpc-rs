use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
    },
    codegen::tree_walker::TreeWalker,
};

use crate::errors::LpcError;
use indextree::NodeId;

/// A node representing a code block
#[derive(Debug, Clone, PartialEq)]
pub struct BlockNode {
    pub body: Vec<AstNode>,
    pub scope_id: Option<NodeId>
}

impl BlockNode {
    pub fn new(body: Vec<AstNode>) -> Self {
        Self { body, scope_id: None }
    }
}

impl AstNodeTrait for BlockNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
        tree_walker.visit_block(self)
    }
}

impl Display for BlockNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .body
            .iter()
            .map(|item| format!("{}", item))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "BlockNode[{}]", s)
    }
}
