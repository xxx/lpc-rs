use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::expression_node::ExpressionNode;
use crate::semantic::lpc_type::LPCVarType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarInitNode {
    pub type_: LPCVarType,
    pub name: String,
    pub value: Option<ExpressionNode>,
    pub array: bool,
}

impl ASTNodeTrait for VarInitNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_var_init(self); }
}

impl Display for VarInitNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VarInitNode[{} {} {:?}]", self.type_, self.name, self.value)
    }
}