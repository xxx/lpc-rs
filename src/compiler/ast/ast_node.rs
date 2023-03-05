use std::{
    fmt,
    fmt::{Debug, Display, Formatter},
};

use auto_impl::auto_impl;
use expression_node::ExpressionNode;
use int_node::IntNode;
use lpc_rs_errors::{span::Span, Result};
use program_node::ProgramNode;
use qcell::QCellOwner;

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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker, cell_key: &mut QCellOwner)
        -> Result<()>;
}

pub trait SpannedNode {
    /// Return the implementing node's span.
    fn span(&self) -> Option<Span>;

    /// Return the source code represented by the Span
    fn to_code(&self) -> String {
        self.span()
            .and_then(|s| s.code())
            .unwrap_or_else(|| String::from("<No data>"))
    }
}

impl AstNodeTrait for AstNode {
    fn visit(
        &mut self,
        tree_walker: &mut impl TreeWalker,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        match self {
            AstNode::Block(y) => y.visit(tree_walker, cell_key),
            AstNode::Break(y) => y.visit(tree_walker, cell_key),
            AstNode::Call(y) => y.visit(tree_walker, cell_key),
            AstNode::Continue(y) => y.visit(tree_walker, cell_key),
            AstNode::Decl(y) => y.visit(tree_walker, cell_key),
            AstNode::DoWhile(y) => y.visit(tree_walker, cell_key),
            AstNode::Expression(y) => y.visit(tree_walker, cell_key),
            AstNode::For(y) => y.visit(tree_walker, cell_key),
            AstNode::ForEach(y) => y.visit(tree_walker, cell_key),
            AstNode::FunctionDef(y) => y.visit(tree_walker, cell_key),
            AstNode::If(y) => y.visit(tree_walker, cell_key),
            AstNode::LabeledStatement(y) => y.visit(tree_walker, cell_key),
            AstNode::Program(y) => y.visit(tree_walker, cell_key),
            AstNode::Return(y) => y.visit(tree_walker, cell_key),
            AstNode::Switch(y) => y.visit(tree_walker, cell_key),
            AstNode::VarInit(y) => y.visit(tree_walker, cell_key),
            AstNode::While(y) => y.visit(tree_walker, cell_key),
            AstNode::NoOp => Ok(()),
        }
    }
}

impl Display for AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
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
