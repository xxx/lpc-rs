use crate::ast::ast_node::ASTNode;
use crate::ast::expression_node::ExpressionNode;

#[derive(Debug)]
pub struct ProgramNode {
    pub expressions: Vec<ExpressionNode>
}

impl ASTNode for ProgramNode {
    fn to_str(&self) -> String {
        format!("ProgramNode[{:?}]", self)
    }
}