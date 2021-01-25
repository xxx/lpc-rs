use crate::ast::ast_node::{ASTNodeTrait, ASTNode};
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::semantic::lpc_type::LPCReturnType;
use crate::ast::var_init_node::VarInitNode;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDefNode {
    pub return_type: LPCReturnType,
    pub name: String,
    pub parameters: Vec<VarInitNode>,
    pub body: Vec<ASTNode>
}

impl ASTNodeTrait for FunctionDefNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_function_def(self); }
}

impl Display for FunctionDefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionDefNode[{}, {} {:?} {:?}]", self.return_type, self.name, self.parameters, self.body)
    }
}

impl Clone for FunctionDefNode {
    fn clone(&self) -> Self {
        Self {
            return_type: self.return_type,
            name: self.name.clone(),
            parameters: self.parameters.to_vec(),
            body: self.body.to_vec()
        }
    }
}