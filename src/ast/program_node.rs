use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::expression_node::ExpressionNode;
use crate::codegen::tree_walker::TreeWalkerTrait;

#[derive(Debug)]
pub struct ProgramNode {
    pub expressions: Vec<ExpressionNode>
}

impl ASTNodeTrait for ProgramNode {
    fn to_str(&self) -> String {
        format!("ProgramNode[{:?}]", self)
    }

    fn visit(&self, tree_walker: &mut impl TreeWalkerTrait) {
        tree_walker.visit_program(self);
    }
}