use std::fmt::Debug;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

use auto_impl::auto_impl;
use expression_node::ExpressionNode;
use int_node::IntNode;
use lpc_rs_errors::{span::Span, Result};
use program_node::ProgramNode;

use crate::compiler::{
    ast::{
        binary_op_node::BinaryOpNode, block_node::BlockNode, break_node::BreakNode,
        call_node::CallNode, continue_node::ContinueNode, decl_node::DeclNode,
        do_while_node::DoWhileNode, expression_node, for_each_node::ForEachNode, for_node::ForNode,
        function_def_node::FunctionDefNode, if_node::IfNode, int_node,
        labeled_statement_node::LabeledStatementNode, program_node, return_node::ReturnNode,
        switch_node::SwitchNode, var_init_node::VarInitNode, while_node::WhileNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// Representation of a top-level node in the AST.
#[derive(Debug, PartialEq, PartialOrd, Clone, Hash, Eq)]
pub enum AstNode {
    Block(BlockNode),
    Break(BreakNode),
    Call(CallNode),
    Continue(ContinueNode),
    Decl(DeclNode),
    DoWhile(DoWhileNode),
    Expression(ExpressionNode),
    For(ForNode),
    ForEach(Box<ForEachNode>),
    FunctionDef(FunctionDefNode),
    If(IfNode),
    LabeledStatement(LabeledStatementNode),
    Program(ProgramNode),
    Return(ReturnNode),
    Switch(SwitchNode),
    VarInit(VarInitNode),
    While(WhileNode),

    /// Used for anything that is parsed, but ignored (e.g. function prototypes)
    NoOp,
}

#[auto_impl(&mut)]
pub trait AstNodeTrait {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()>;
}

pub trait SpannedNode {
    /// Return the implementing node's span.
    fn span(&self) -> Option<Span>;
}

impl AstNodeTrait for AstNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        match self {
            AstNode::Block(y) => y.visit(tree_walker),
            AstNode::Break(y) => y.visit(tree_walker),
            AstNode::Call(y) => y.visit(tree_walker),
            AstNode::Continue(y) => y.visit(tree_walker),
            AstNode::Decl(y) => y.visit(tree_walker),
            AstNode::DoWhile(y) => y.visit(tree_walker),
            AstNode::Expression(y) => y.visit(tree_walker),
            AstNode::For(y) => y.visit(tree_walker),
            AstNode::ForEach(y) => y.visit(tree_walker),
            AstNode::FunctionDef(y) => y.visit(tree_walker),
            AstNode::If(y) => y.visit(tree_walker),
            AstNode::LabeledStatement(y) => y.visit(tree_walker),
            AstNode::Program(y) => y.visit(tree_walker),
            AstNode::Return(y) => y.visit(tree_walker),
            AstNode::Switch(y) => y.visit(tree_walker),
            AstNode::VarInit(y) => y.visit(tree_walker),
            AstNode::While(y) => y.visit(tree_walker),
            AstNode::NoOp => Ok(()),
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl From<BinaryOpNode> for AstNode {
    fn from(node: BinaryOpNode) -> Self {
        AstNode::Expression(ExpressionNode::BinaryOp(node))
    }
}

impl From<BlockNode> for AstNode {
    fn from(node: BlockNode) -> Self {
        AstNode::Block(node)
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

impl From<DeclNode> for AstNode {
    fn from(node: DeclNode) -> Self {
        AstNode::Decl(node)
    }
}

impl From<DoWhileNode> for AstNode {
    fn from(node: DoWhileNode) -> Self {
        AstNode::DoWhile(node)
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

impl From<ForEachNode> for AstNode {
    fn from(node: ForEachNode) -> Self {
        AstNode::ForEach(node.into())
    }
}

impl From<FunctionDefNode> for AstNode {
    fn from(node: FunctionDefNode) -> Self {
        AstNode::FunctionDef(node)
    }
}

impl From<IfNode> for AstNode {
    fn from(node: IfNode) -> Self {
        AstNode::If(node)
    }
}

impl From<IntNode> for AstNode {
    fn from(node: IntNode) -> Self {
        AstNode::Expression(ExpressionNode::Int(node))
    }
}

impl From<LabeledStatementNode> for AstNode {
    fn from(node: LabeledStatementNode) -> Self {
        AstNode::LabeledStatement(node)
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

impl From<SwitchNode> for AstNode {
    fn from(node: SwitchNode) -> Self {
        AstNode::Switch(node)
    }
}

impl From<VarInitNode> for AstNode {
    fn from(node: VarInitNode) -> Self {
        AstNode::VarInit(node)
    }
}

impl From<WhileNode> for AstNode {
    fn from(node: WhileNode) -> Self {
        AstNode::While(node)
    }
}
