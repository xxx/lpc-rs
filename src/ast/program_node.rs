use crate::ast::ast_node::ASTNodeTrait;
use crate::ast::expression_node::ExpressionNode;
use crate::codegen::tree_walker::TreeWalker;

#[derive(Debug, Eq, PartialEq, Default)]
pub struct ProgramNode {
    pub expressions: Vec<ExpressionNode>
}

impl ASTNodeTrait for ProgramNode {
    fn to_str(&self) -> String {
        format!("ProgramNode[{:?}]", self)
    }

    fn visit(&self, tree_walker: &mut impl TreeWalker) {
        tree_walker.visit_program(self);
    }
}