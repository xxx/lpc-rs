use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::expression_node::ExpressionNode;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarInitNode {
    pub name: String,
    pub value: Option<ExpressionNode>,
    pub mixed: bool,
    pub array: bool,
}

impl VarInitNode {
    pub fn new(name: String, value: Option<ExpressionNode>, mixed: bool, array: bool) -> Self {
        Self { name, value, mixed, array }
    }
}

impl ASTNodeTrait for VarInitNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_var_init(self); }
}

impl Display for VarInitNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VarInitNode[{} {:?}]", self.name, self.value)
    }
}