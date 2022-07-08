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
    Result,
};
use std::convert::TryFrom;
use crate::errors::span::Span;

/// All possible binary operations
#[derive(Hash, Debug, Copy, Clone, Eq, PartialOrd, PartialEq)]
pub enum BinaryOperation {
    Add,
    And,
    AndAnd,
    Div,
    EqEq,
    Gt,
    Gte,
    Index,
    Lt,
    Lte,
    Mod,
    Mul,
    Or,
    OrOr,
    Shl,
    Shr,
    Sub,
    Xor,
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            BinaryOperation::Add => "+",
            BinaryOperation::And => "&",
            BinaryOperation::AndAnd => "&&",
            BinaryOperation::Div => "/",
            BinaryOperation::EqEq => "==",
            BinaryOperation::Gt => ">",
            BinaryOperation::Gte => ">=",
            BinaryOperation::Index => "[]",
            BinaryOperation::Lt => "<",
            BinaryOperation::Lte => "<=",
            BinaryOperation::Mod => "%",
            BinaryOperation::Mul => "*",
            BinaryOperation::Or => "|",
            BinaryOperation::OrOr => "||",
            BinaryOperation::Sub => "-",
            BinaryOperation::Shl => "<<",
            BinaryOperation::Shr => ">>",
            BinaryOperation::Xor => "^",
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
            AssignmentOperation::AddEq => Ok(Self::Add),
            AssignmentOperation::AndAndEq => Ok(Self::AndAnd),
            AssignmentOperation::AndEq => Ok(Self::And),
            AssignmentOperation::DivEq => Ok(Self::Div),
            AssignmentOperation::Index => Ok(Self::Index),
            AssignmentOperation::ModEq => Ok(Self::Mod),
            AssignmentOperation::MulEq => Ok(Self::Mul),
            AssignmentOperation::OrEq => Ok(Self::Or),
            AssignmentOperation::OrOrEq => Ok(Self::OrOr),
            AssignmentOperation::ShlEq => Ok(Self::Shl),
            AssignmentOperation::ShrEq => Ok(Self::Shr),
            AssignmentOperation::SubEq => Ok(Self::Sub),
            AssignmentOperation::XorEq => Ok(Self::Xor),
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
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_binary_op(self)
    }
}

impl Display for BinaryOpNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.l, self.op, self.r)
    }
}
