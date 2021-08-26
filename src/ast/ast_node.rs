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
        binary_op_node::BinaryOpNode, block_node::BlockNode, break_node::BreakNode,
        call_node::CallNode, continue_node::ContinueNode, decl_node::DeclNode,
        do_while_node::DoWhileNode, expression_node, for_node::ForNode,
        function_def_node::FunctionDefNode, if_node::IfNode, int_node, program_node,
        return_node::ReturnNode, var_init_node::VarInitNode, while_node::WhileNode,
    },
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    Result,
};

/// Representation of a top-level node in the AST.
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub enum AstNode {
    Block(BlockNode),
    Break(BreakNode),
    Call(CallNode),
    Continue(ContinueNode),
    Decl(DeclNode),
    DoWhile(DoWhileNode),
    Expression(ExpressionNode),
    For(ForNode),
    FunctionDef(FunctionDefNode),
    If(IfNode),
    Program(ProgramNode),
    Return(ReturnNode),
    VarInit(VarInitNode),
    While(WhileNode),
}

#[auto_impl(&mut)]
pub trait AstNodeTrait: PartialEq + Display {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()>;
}

pub trait SpannedNode {
    /// Return the implementing node's span.
    fn span(&self) -> Option<Span>;
}

macro_rules! node_defs {
    ( $( $x:ident ),+ ) => {
        impl AstNodeTrait for AstNode {
            fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
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
    Break,
    Call,
    Continue,
    Decl,
    DoWhile,
    Expression,
    For,
    FunctionDef,
    If,
    Program,
    Return,
    VarInit,
    While
);

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<BreakNode> for AstNode {
    fn from(node: BreakNode) -> Self {
        AstNode::Break(node)
    }
}

impl From<ContinueNode> for AstNode {
    fn from(node: ContinueNode) -> Self {
        AstNode::Continue(node)
    }
}

impl From<ExpressionNode> for AstNode {
    fn from(node: ExpressionNode) -> Self {
        AstNode::Expression(node)
    }
}

impl From<ForNode> for AstNode {
    fn from(node: ForNode) -> Self {
        AstNode::For(node)
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

impl From<IfNode> for AstNode {
    fn from(node: IfNode) -> Self {
        AstNode::If(node)
    }
}

impl From<WhileNode> for AstNode {
    fn from(node: WhileNode) -> Self {
        AstNode::While(node)
    }
}

impl From<DoWhileNode> for AstNode {
    fn from(node: DoWhileNode) -> Self {
        AstNode::DoWhile(node)
    }
}
