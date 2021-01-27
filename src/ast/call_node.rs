use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::expression_node::ExpressionNode;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone)]
/// Representation of a function call.
pub struct CallNode {
    /// The list of function arguments being passed.
    pub arguments: Vec<ExpressionNode>,
    /// The name of the function being called
    pub name: String
}

impl ASTNodeTrait for CallNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_call(self);
    }
}

impl Display for CallNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "CallNode[{:?}]", self)
    }
}