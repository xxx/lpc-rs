use core::fmt::Debug;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

use auto_impl::auto_impl;

use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;

use crate::{
    ast::{
        binary_op_node::BinaryOpNode, block_node::BlockNode, call_node::CallNode,
        decl_node::DeclNode, expression_node, function_def_node::FunctionDefNode, int_node,
        program_node, return_node::ReturnNode, var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::LpcError,
    parser::span::Span,
};

/// Representation of a top-level node in the AST.
#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Block(BlockNode),
    Call(CallNode),
    Decl(DeclNode),
    Expression(ExpressionNode),
    FunctionDef(FunctionDefNode),
    Program(ProgramNode),
    Return(ReturnNode),
    VarInit(VarInitNode),
}

#[auto_impl(&mut)]
pub trait AstNodeTrait: PartialEq + Display {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError>;
}

pub trait SpannedNode {
    /// Return the implementing node's span.
    fn span(&self) -> Option<Span>;
}

macro_rules! node_defs {
    ( $( $x:ident ),+ ) => {
        impl AstNodeTrait for AstNode {
            fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), LpcError> {
                match self {
                 $(
                    AstNode::$x(y) => y.visit(tree_walker),
                 )*
                }
            }
        }
    };
}

node_defs!(
    Block,
    Call,
    Decl,
    Expression,
    FunctionDef,
    Program,
    Return,
    VarInit
);

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<ExpressionNode> for AstNode {
    fn from(node: ExpressionNode) -> Self {
        AstNode::Expression(node)
    }
}

impl From<FunctionDefNode> for AstNode {
    fn from(node: FunctionDefNode) -> Self {
        AstNode::FunctionDef(node)
    }
}

impl From<IntNode> for AstNode {
    fn from(node: IntNode) -> Self {
        AstNode::Expression(ExpressionNode::Int(node))
    }
}

impl From<BinaryOpNode> for AstNode {
    fn from(node: BinaryOpNode) -> Self {
        AstNode::Expression(ExpressionNode::BinaryOp(node))
    }
}

impl From<ProgramNode> for AstNode {
    fn from(node: ProgramNode) -> Self {
        AstNode::Program(node)
    }
}

impl From<ReturnNode> for AstNode {
    fn from(node: ReturnNode) -> Self {
        AstNode::Return(node)
    }
}

impl From<DeclNode> for AstNode {
    fn from(node: DeclNode) -> Self {
        AstNode::Decl(node)
    }
}

impl From<VarInitNode> for AstNode {
    fn from(node: VarInitNode) -> Self {
        AstNode::VarInit(node)
    }
}

impl From<BlockNode> for AstNode {
    fn from(node: BlockNode) -> Self {
        AstNode::Block(node)
    }
}

#[cfg(test)]
mod tests {
    use crate::{ast::binary_op_node::BinaryOperation, semantic::lpc_type::LpcType};

    use super::*;

    #[test]
    fn test_from_expression_node() {
        let node = ExpressionNode::Int(IntNode::new(123));
        let clone = node.clone();

        assert_eq!(AstNode::from(node), AstNode::Expression(clone));
    }

    #[test]
    fn test_from_function_def_node() {
        let node = FunctionDefNode {
            return_type: LpcType::Void,
            name: "foo".to_string(),
            parameters: vec![],
            body: vec![],
            span: None,
        };
        let clone = node.clone();

        assert_eq!(AstNode::from(node), AstNode::FunctionDef(clone));
    }

    #[test]
    fn test_from_int_node() {
        let node = IntNode::new(123);
        let clone = node.clone();

        assert_eq!(
            AstNode::from(node),
            AstNode::Expression(ExpressionNode::Int(clone))
        );
    }

    #[test]
    fn test_from_binary_op_node() {
        let node: BinaryOpNode = BinaryOpNode {
            l: Box::new(ExpressionNode::Int(IntNode::new(123))),
            r: Box::new(ExpressionNode::Int(IntNode::new(1233))),
            op: BinaryOperation::Add,
            span: None,
        };
        let clone = node.clone();

        assert_eq!(
            AstNode::from(node),
            AstNode::Expression(ExpressionNode::BinaryOp(clone))
        );
    }

    #[test]
    fn test_from_program_node() {
        let node = ProgramNode::default();
        let clone = node.clone();

        assert_eq!(AstNode::from(node), AstNode::Program(clone));
    }

    #[test]
    fn test_from_return_node() {
        let node = ReturnNode {
            value: None,
            span: None,
        };
        let clone = node.clone();

        assert_eq!(AstNode::from(node), AstNode::Return(clone));
    }

    #[test]
    fn test_from_decl_node() {
        let node = DeclNode {
            type_: LpcType::Int(false),
            initializations: vec![],
        };
        let clone = node.clone();

        assert_eq!(AstNode::from(node), AstNode::Decl(clone));
    }

    #[test]
    fn test_from_var_init_node() {
        let node = VarInitNode {
            type_: LpcType::String(true),
            name: "tacos".to_string(),
            value: None,
            array: false,
            global: false,
            span: None,
        };
        let clone = node.clone();

        assert_eq!(AstNode::from(node), AstNode::VarInit(clone));
    }
}
