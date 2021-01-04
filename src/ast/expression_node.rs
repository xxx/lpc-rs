use crate::ast::ast_node::{ASTNodeTrait, ASTNode};
use crate::codegen::tree_walker::TreeWalkerTrait;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::int_node::IntNode;

#[derive(Debug)]
pub enum ExpressionNode {
    BinaryOp(Box<BinaryOpNode>),
    Int(Box<IntNode>)
}

impl ASTNodeTrait for ExpressionNode {
    fn to_str(&self) -> String {
        match self {
            ExpressionNode::BinaryOp(x) => x.to_str(),
            ExpressionNode::Int(x) => x.to_str()
        }
    }

    fn visit(&self, tree_walker: &mut impl TreeWalkerTrait) {
        match self {
            ExpressionNode::BinaryOp(x) => x.visit(tree_walker),
            ExpressionNode::Int(x) => x.visit(tree_walker)
        }
    }
}

impl From<BinaryOpNode> for ExpressionNode {
    fn from(node: BinaryOpNode) -> Self {
        Self::BinaryOp(Box::new(node))
    }
}

impl From<IntNode> for ExpressionNode {
    fn from(node: IntNode) -> Self {
        Self::Int(Box::new(node))
    }
}

impl From<ASTNode> for ExpressionNode {
    fn from(node: ASTNode) -> Self {
        match node {
            ASTNode::Expression(x) => *x,
            x => panic!("unimplemented From<ASTNode> for ExpressionNode arm: {:?}", x)
        }
    }
}