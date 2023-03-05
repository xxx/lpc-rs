use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_errors::{span::Span, Result};
use qcell::QCellOwner;

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// All possible assignment operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialEq)]
pub enum AssignmentOperation {
    /// Simple assignment - `var = 2;`
    Simple,

    /// Plus-equals assignment - `a += 1`;
    AddEq,

    /// AndAnd-equals assignment - `a &&= 12`;
    AndAndEq,

    /// bitwise And-equals assignment - `a &= 12`;
    AndEq,

    /// Divide-equals assignment - `a /= 0`;
    DivEq,

    /// Index assigment - `a[2] = "hello";`
    Index,

    /// Modulo-equals assignment - `a %= 0`;
    ModEq,

    /// Times-equals assignment - `a *= 2`;
    MulEq,

    /// bitwise Or-equals assignment - `a |= 12`;
    OrEq,

    /// OrOr-equals assignment - `a ||= 12`;
    OrOrEq,

    /// shift-left-equals assignment - `a <<= 1`;
    ShlEq,

    /// shift-right-equals assignment - `a >>= 1`;
    ShrEq,

    /// Minus-equals assignment - `a -= 1`;
    SubEq,

    /// bitwise Xor-equals assignment - `a ^= 12`;
    XorEq,
}

impl Display for AssignmentOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            AssignmentOperation::Simple => "=",
            AssignmentOperation::AddEq => "+=",
            AssignmentOperation::AndAndEq => "&&=",
            AssignmentOperation::AndEq => "&=",
            AssignmentOperation::DivEq => "/=",
            AssignmentOperation::Index => "[]",
            AssignmentOperation::ModEq => "%=",
            AssignmentOperation::MulEq => "*=",
            AssignmentOperation::OrEq => "|=",
            AssignmentOperation::OrOrEq => "||=",
            AssignmentOperation::ShlEq => "<<=",
            AssignmentOperation::ShrEq => ">>=",
            AssignmentOperation::SubEq => "-=",
            AssignmentOperation::XorEq => "^=",
        };

        write!(f, "{s}")
    }
}

/// A node representing an assignment.
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct AssignmentNode {
    /// left-hand side
    pub lhs: Box<ExpressionNode>,

    /// right-hand side
    pub rhs: Box<ExpressionNode>,

    /// The text span in the original file that this node represents. Used for
    /// error messages.
    pub span: Option<Span>,
}

impl SpannedNode for AssignmentNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for AssignmentNode {
    fn visit(
        &mut self,
        tree_walker: &mut impl TreeWalker,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        tree_walker.visit_assignment(self, cell_key)
    }
}

impl Display for AssignmentNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = {}", self.lhs, self.rhs)
    }
}
