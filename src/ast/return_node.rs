use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::expression_node::ExpressionNode;

/// A node representing a function return call.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ReturnNode {
    /// The value to return from the function.
    pub value: Option<ExpressionNode>,
}

impl ReturnNode {
    pub fn new(value: Option<ExpressionNode>) -> Self {
        Self { value }
    }
}

impl ASTNodeTrait for ReturnNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_return(self).unwrap();
    }
}

impl Display for ReturnNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ReturnNode[{:?}]", self.value)
    }
}