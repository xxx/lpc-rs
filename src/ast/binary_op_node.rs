use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        assignment_node::AssignmentOperation,
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::LpcError,
    parser::span::Span,
    Result,
};
use std::convert::TryFrom;

/// All possible binary operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub enum BinaryOperation {
    Add,
    AndAnd,
    Div,
    EqEq,
    Gt,
    Gte,
    Index,
    Lt,
    Lte,
    Mul,
    OrOr,
    Sub,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperation::Add => "+",
            BinaryOperation::AndAnd => "&&",
            BinaryOperation::Div => "/",
            BinaryOperation::EqEq => "==",
            BinaryOperation::Gt => ">",
            BinaryOperation::Gte => ">=",
            BinaryOperation::Index => "[]",
            BinaryOperation::Lt => "<",
            BinaryOperation::Lte => "<=",
            BinaryOperation::Mul => "*",
            BinaryOperation::OrOr => "||",
            BinaryOperation::Sub => "-",
        };

        write!(f, "{}", s)
    }
}

impl TryFrom<AssignmentOperation> for BinaryOperation {
    type Error = LpcError;

    fn try_from(value: AssignmentOperation) -> Result<Self> {
        match value {
            AssignmentOperation::Simple => Err(LpcError::new(format!(
                "Failure to convert `{}` into a BinaryOperation",
                value
            ))),
            AssignmentOperation::Index => Ok(Self::Index),
            AssignmentOperation::AddEq => Ok(Self::Add),
            AssignmentOperation::SubEq => Ok(Self::Sub),
            AssignmentOperation::MulEq => Ok(Self::Mul),
            AssignmentOperation::DivEq => Ok(Self::Div),
            AssignmentOperation::AndAndEq => Ok(Self::AndAnd),
            AssignmentOperation::OrOrEq => Ok(Self::OrOr),
        }
    }
}

/// Representation of a binary operation
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct BinaryOpNode {
    /// Left-hand side
    pub l: Box<ExpressionNode>,

    /// Right-hand side
    pub r: Box<ExpressionNode>,

    /// The operation to perform
    pub op: BinaryOperation,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,
}

impl SpannedNode for BinaryOpNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for BinaryOpNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_binary_op(self)
    }
}

impl Display for BinaryOpNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.l, self.op, self.r)
    }
}
