use crate::ast::ast_node::{ASTNodeTrait, ASTNode};
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::int_node::IntNode;
use crate::ast::call_node::CallNode;


#[derive(Debug)]
pub enum ExpressionNode {
    BinaryOp(BinaryOpNode),
    Int(IntNode),
    Call(CallNode)
}

macro_rules! ast_node_trait {
    ( $( $x:path ),+ ) => {
        impl ASTNodeTrait for ExpressionNode {
            fn to_str(&self) -> String {
                match self {
                $(
                    $x(y) => y.to_str(),
                )*
                }
            }

            fn visit(&self, tree_walker: &mut impl TreeWalker) {
                match self {
                $(
                    $x(y) => y.visit(tree_walker),
                )*
                }
            }
        }
    };
}

ast_node_trait!(ExpressionNode::BinaryOp, ExpressionNode::Int, ExpressionNode::Call);

impl From<BinaryOpNode> for ExpressionNode {
    fn from(node: BinaryOpNode) -> Self {
        Self::BinaryOp(node)
    }
}

impl From<IntNode> for ExpressionNode {
    fn from(node: IntNode) -> Self {
        Self::Int(node)
    }
}

impl From<ASTNode> for ExpressionNode {
    fn from(node: ASTNode) -> Self {
        match node {
            ASTNode::Expression(x) => x,
            x => panic!("unimplemented From<ASTNode> for ExpressionNode arm: {:?}", x)
        }
    }
}