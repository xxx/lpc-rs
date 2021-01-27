use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::expression_node::ExpressionNode;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
/// All possible assignment operations
pub enum AssignmentOperation {
    /// Simple assignment - `var = 2;`
    Simple,
}

#[derive(Debug, Eq, PartialEq)]
/// A node representing an assignment.
pub struct AssignmentNode {
    /// left-hand side
    pub lhs: Box<ExpressionNode>,
    /// right-hand side
    pub rhs: Box<ExpressionNode>,
    /// the operation
    pub op: AssignmentOperation
}

impl ASTNodeTrait for AssignmentNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_assignment(self); }
}

impl Display for AssignmentNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "AssignmentNode[{:?}]", self)
    }
}

impl Clone for AssignmentNode {
    fn clone(&self) -> Self {
        Self {
            lhs: self.lhs.clone(),
            rhs: self.rhs.clone(),
            op: self.op
        }
    }
}
