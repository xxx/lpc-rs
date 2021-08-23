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
    AddEq,

    /// Minus-equals assignment - `a -= 1`;
    SubEq,

    /// Times-equals assignment - `a *= 2`;
    MulEq,

    /// Divide-equals assignment - `a /= 0`;
    DivEq,

    /// Modulo-equals assignment - `a %= 0`;
    ModEq,

    /// AndAnd-equals assignment - `a &&= 12`;
    AndAndEq,

    /// OrOr-equals assignment - `a ||= 12`;
    OrOrEq,

    /// bitwise And-equals assignment - `a &= 12`;
    AndEq,

    /// bitwise Or-equals assignment - `a |= 12`;
    OrEq,
}

impl Display for AssignmentOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            AssignmentOperation::Simple => "=",
            AssignmentOperation::Index => "[]",
            AssignmentOperation::AddEq => "+=",
            AssignmentOperation::SubEq => "-=",
            AssignmentOperation::MulEq => "*=",
            AssignmentOperation::DivEq => "/=",
            AssignmentOperation::ModEq => "%=",
            AssignmentOperation::AndAndEq => "&&=",
            AssignmentOperation::OrOrEq => "||=",
            AssignmentOperation::AndEq => "&=",
            AssignmentOperation::OrEq => "|=",
        };

        write!(f, "{}", s)
    }
}

/// A node representing an assignment.
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct AssignmentNode {
    /// left-hand side
    pub lhs: Box<ExpressionNode>,

    /// right-hand side
    pub rhs: Box<ExpressionNode>,

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
        write!(f, "{} = {}", self.lhs, self.rhs)
    }
}
