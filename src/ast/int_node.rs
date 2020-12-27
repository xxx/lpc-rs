use crate::ast::ast_node::ASTNode;

#[derive(Debug, Copy, Clone)]
pub struct IntNode {
    pub value: i64,
}

impl ASTNode for IntNode {
    fn to_str(&self) -> String {
        format!("IntNode[{}]", self.value)
    }
}