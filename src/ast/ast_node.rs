use crate::{
    ast::{
        binary_op_node::BinaryOpNode, call_node::CallNode, decl_node::DeclNode, expression_node,
        function_def_node::FunctionDefNode, int_node, program_node, return_node::ReturnNode,
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
};
use auto_impl::auto_impl;
use core::fmt::Debug;
use expression_node::ExpressionNode;
use int_node::IntNode;
use program_node::ProgramNode;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Representation of a top-level node in the AST.
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Call(CallNode),
    Decl(DeclNode),
    Expression(ExpressionNode),
    FunctionDef(FunctionDefNode),
    Program(ProgramNode),
    Return(ReturnNode),
    VarInit(VarInitNode),
}

#[auto_impl(&mut)]
pub trait ASTNodeTrait: PartialEq + Display {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError>;
}

macro_rules! node_defs {
    ( $( $x:ident ),+ ) => {
        impl ASTNodeTrait for ASTNode {
            fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
                match self {
                 $(
                    ASTNode::$x(y) => y.visit(tree_walker),
                 )*
                }
            }
        }
    };
}

node_defs!(
    Call,
    Decl,
    Expression,
    FunctionDef,
    Program,
    Return,
    VarInit
);

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

impl From<FunctionDefNode> for ASTNode {
    fn from(node: FunctionDefNode) -> Self {
        ASTNode::FunctionDef(node)
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

impl From<VarInitNode> for ASTNode {
    fn from(node: VarInitNode) -> Self {
        ASTNode::VarInit(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::binary_op_node::BinaryOperation, semantic::lpc_type::LPCType};

    #[test]
    fn test_from_expression_node() {
        let node = ExpressionNode::Int(IntNode::new(123));
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Expression(clone));
    }

    #[test]
    fn test_from_function_def_node() {
        let node = FunctionDefNode {
            return_type: LPCType::Void,
            name: "foo".to_string(),
            parameters: vec![],
            body: vec![],
            span: None,
        };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::FunctionDef(clone));
    }

    #[test]
    fn test_from_int_node() {
        let node = IntNode::new(123);
        let clone = node.clone();

        assert_eq!(
            ASTNode::from(node),
            ASTNode::Expression(ExpressionNode::Int(clone))
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
            ASTNode::from(node),
            ASTNode::Expression(ExpressionNode::BinaryOp(clone))
        );
    }

    #[test]
    fn test_from_program_node() {
        let node = ProgramNode::default();
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Program(clone));
    }

    #[test]
    fn test_from_return_node() {
        let node = ReturnNode {
            value: None,
            span: None,
        };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Return(clone));
    }

    #[test]
    fn test_from_decl_node() {
        let node = DeclNode {
            type_: LPCType::Int(false),
            initializations: vec![],
        };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::Decl(clone));
    }

    #[test]
    fn test_from_var_init_node() {
        let node = VarInitNode {
            type_: LPCType::String(true),
            name: "tacos".to_string(),
            value: None,
            array: false,
            global: false,
            span: None,
        };
        let clone = node.clone();

        assert_eq!(ASTNode::from(node), ASTNode::VarInit(clone));
    }
}
