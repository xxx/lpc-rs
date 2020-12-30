use crate::ast::ast_node::ASTNode;
use crate::ast::expression_node::ExpressionNode;
use crate::asm::instruction::Instruction;

#[derive(Debug)]
pub struct ProgramNode {
    pub expressions: Vec<ExpressionNode>
}

impl ASTNode for ProgramNode {
    fn to_str(&self) -> String {
        format!("ProgramNode[{:?}]", self)
    }

    fn to_asm(&self) -> Vec<Box<Instruction>> {
        self.expressions.iter().map(|expr| expr.to_asm()).flatten().collect()
    }
}