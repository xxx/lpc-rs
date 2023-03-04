use std::{
    fmt,
    fmt::{Display, Formatter},
};

use indextree::NodeId;
use qcell::QCellOwner;
use lpc_rs_errors::Result;

use crate::compiler::{
    ast::ast_node::{AstNode, AstNodeTrait},
    codegen::tree_walker::TreeWalker,
};

/// A node representing a code block
#[derive(Debug, Clone, PartialOrd, PartialEq, Hash, Eq)]
pub struct BlockNode {
    pub body: Vec<AstNode>,
    pub scope_id: Option<NodeId>,
}

impl BlockNode {
    pub fn new(body: Vec<AstNode>) -> Self {
        Self {
            body,
            scope_id: None,
        }
    }
}

impl AstNodeTrait for BlockNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker, cell_key: &mut QCellOwner) -> Result<()> {
        tree_walker.visit_block(self, cell_key)
    }
}

impl Display for BlockNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .body
            .iter()
            .map(|item| format!("{item}"))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "{{ {s} }}")
    }
}
