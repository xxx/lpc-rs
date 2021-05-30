use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    Result,
};

/// All possible assignment operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignmentOperation {
    /// Simple assignment - `var = 2;`
    Simple,

    /// Index assigment - `a[2] = "hello";`
    Index,

    /// Plus-equals assignment - `a += 1`;
    PlusEq,

    /// Plus-equals assignment - `a -= 1`;
    MinusEq,

    /// Plus-equals assignment - `a *= 2`;
    MulEq,

    /// Plus-equals assignment - `a /= `0`;
    DivEq,
}

impl Display for AssignmentOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            AssignmentOperation::Simple => "=",
            AssignmentOperation::Index => "[]",
            AssignmentOperation::PlusEq => "+=",
            AssignmentOperation::MinusEq => "-=",
            AssignmentOperation::MulEq => "*=",
            AssignmentOperation::DivEq => "/=",
        };

        write!(f, "{}", s)
    }
}

/// A node representing an assignment.
#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub struct AssignmentNode {
    /// left-hand side
    pub lhs: Box<ExpressionNode>,

    /// right-hand side
    pub rhs: Box<ExpressionNode>,

    #[deprecated(
        note = "All assignments are `Simple` assignments, after being decomposed in the parser"
    )]
    /// the operation
    pub op: AssignmentOperation,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,
}

impl SpannedNode for AssignmentNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for AssignmentNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_assignment(self)
    }
}

impl Display for AssignmentNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "AssignmentNode[{:?}]", self)
    }
}
