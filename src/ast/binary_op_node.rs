use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::expression_node::ExpressionNode;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
pub struct BinaryOpNode {
    pub l: Box<ExpressionNode>,
    pub r: Box<ExpressionNode>,
    pub op: BinaryOperation
}

impl ASTNodeTrait for BinaryOpNode {
    fn to_str(&self) -> String {
        format!("BinaryOpNode[{:?}]", self)
    }

    fn visit(&self, tree_walker: &mut impl TreeWalker) { tree_walker.visit_binary_op(self); }
}
