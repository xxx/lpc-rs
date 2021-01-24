use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::expression_node::ExpressionNode;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignmentOperation {
    Simple,
}

#[derive(Debug, Eq, PartialEq)]
pub struct AssignmentNode {
    pub lhs: Box<ExpressionNode>,
    pub rhs: Box<ExpressionNode>,
    pub op: AssignmentOperation
}

impl ASTNodeTrait for AssignmentNode {
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
