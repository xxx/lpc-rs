use crate::ast::ast_node::ASTNode;

#[derive(Debug, Copy, Clone)]
pub enum BinaryOperation {
    Add,
    Sub,
    Mul,
    Div
}

#[derive(Debug)]
pub struct ExpressionNode {
    pub l: Box<dyn ASTNode>,
    pub r: Box<dyn ASTNode>,
    pub op: BinaryOperation
}

impl ASTNode for ExpressionNode {
    fn to_str(&self) -> String {
        format!("ExpressionNode[{:?}]", self)
    }
}