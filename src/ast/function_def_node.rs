use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::expression_node::ExpressionNode;
use crate::semantic::lpc_type::LPCReturnType;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionDefNode {
    pub return_type: LPCReturnType,
    pub name: String,
    pub body: Vec<ExpressionNode>
}

impl ASTNodeTrait for FunctionDefNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_function_def(self); }
}

impl Display for FunctionDefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionDefNode[{} {:?}]", self.name, self.body)
    }
}