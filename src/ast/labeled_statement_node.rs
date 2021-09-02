use crate::ast::ast_node::{AstNode, AstNodeTrait};
use crate::ast::label_node::LabelNode;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use crate::Result;
use std::fmt;

/// A wrapper for nodes to allow labels to be applied.
/// They are only allowed in `switch` statements.
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct LabeledStatementNode {
    node: Box<AstNode>,
    label: LabelNode
}

impl LabeledStatementNode {
    pub fn new(node: AstNode, label: LabelNode) -> Self {
        Self {
            node: node.into(),
            label
        }
    }
}

impl AstNodeTrait for LabeledStatementNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        self.label.visit(tree_walker)?;
        self.node.visit(tree_walker)
    }
}

impl Display for LabeledStatementNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.label, self.node)
    }
}
