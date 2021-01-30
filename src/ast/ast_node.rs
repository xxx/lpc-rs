use core::fmt::Debug;
use crate::ast::{expression_node, int_node, program_node};
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;
use crate::codegen::tree_walker::TreeWalker;
use auto_impl::auto_impl;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::call_node::CallNode;
use std::fmt::{Display, Formatter};
use crate::ast::function_def_node::FunctionDefNode;
use crate::ast::return_node::ReturnNode;
use std::fmt;
use crate::ast::decl_node::DeclNode;
use crate::ast::var_init_node::VarInitNode;

/// Representation of a top-level node in the AST.
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ASTNode {
    Call(CallNode),
    Decl(DeclNode),
    Expression(ExpressionNode),
    FunctionDef(FunctionDefNode),
    Program(ProgramNode),
    Return(ReturnNode),
    VarInit(VarInitNode)
}

#[auto_impl(&, &mut)]
pub trait ASTNodeTrait: PartialEq + Display {
    fn visit(&self, tree_walker: &mut impl TreeWalker);
}

macro_rules! node_defs {
    ( $( $x:ident ),+ ) => {
        impl ASTNodeTrait for ASTNode {
            fn visit(&self, tree_walker: &mut impl TreeWalker) {
                match self {
                 $(
                    ASTNode::$x(y) => y.visit(tree_walker),
                 )*
                }
            }
        }
    };
}

node_defs!(Call, Decl, Expression, FunctionDef, Program, Return, VarInit);

impl Display for ASTNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<ExpressionNode> for ASTNode {
    fn from(node: ExpressionNode) -> Self {
        ASTNode::Expression(node)
    }
}

impl From<IntNode> for ASTNode {
    fn from(node: IntNode) -> Self {
        ASTNode::Expression(ExpressionNode::Int(node))
    }
}

impl From<BinaryOpNode> for ASTNode {
    fn from(node: BinaryOpNode) -> Self {
        ASTNode::Expression(ExpressionNode::BinaryOp(node))
    }
}

impl From<ProgramNode> for ASTNode {
    fn from(node: ProgramNode) -> Self {
        ASTNode::Program(node)
    }
}

impl From<ReturnNode> for ASTNode {
    fn from(node: ReturnNode) -> Self {
        ASTNode::Return(node)
    }
}

impl From<DeclNode> for ASTNode {
    fn from(node: DeclNode) -> Self {
        ASTNode::Decl(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::binary_op_node::BinaryOperation;
    use crate::semantic::lpc_type::LPCVarType;

    #[test]
    fn test_from_expression_node() {
        let node = ExpressionNode::Int(IntNode::new(123));
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(clone));
    }

    #[test]
    fn test_from_int_node() {
        let node = IntNode::new(123);
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(ExpressionNode::Int(clone)));
    }

    #[test]
    fn test_from_binary_op_node() {
        let node: BinaryOpNode = BinaryOpNode {
            l: Box::new(ExpressionNode::Int(IntNode::new(123))),
            r: Box::new(ExpressionNode::Int(IntNode::new(1233))),
            op: BinaryOperation::Add,
            span: None
        };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(ExpressionNode::BinaryOp(clone)));
    }

    #[test]
    fn test_from_program_node() {
        let node = ProgramNode::default();
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Program(clone));
    }

    #[test]
    fn test_from_return_node() {
        let node = ReturnNode { value: None };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Return(clone));
    }

    #[test]
    fn test_from_decl_node() {
        let node = DeclNode { type_: LPCVarType::Int, initializations: vec![] };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Decl(clone));
    }
}