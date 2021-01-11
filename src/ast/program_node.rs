use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::function_def_node::FunctionDefNode;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;

#[derive(Debug, Eq, PartialEq, Clone, Default)]
pub struct ProgramNode {
    pub functions: Vec<FunctionDefNode>
}

impl ASTNodeTrait for ProgramNode {
    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_program(self);
    }
}

impl Display for ProgramNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "ProgramNode[{:?}]", self)
    }
}