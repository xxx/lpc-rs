//! Construction and member expressions for nominal classes.

use std::fmt;

use async_trait::async_trait;
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use super::{
    ast_node::{AstNodeTrait, SpannedNode},
    binary_op_node::BinaryOperation,
    expression_node::ExpressionNode,
    parsed_type::ParsedType,
};
use crate::compiler::codegen::tree_walker::TreeWalker;

#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct ClassInitNode {
    pub type_: ParsedType,
    pub initializers: Vec<(Ustr, ExpressionNode)>,
    pub span: Option<Span>,
}

#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct MemberNode {
    pub receiver: Box<ExpressionNode>,
    pub name: Ustr,
    pub span: Option<Span>,
}

/// Retains the target separately so compound member writes evaluate it once.
#[derive(Hash, Debug, Eq, PartialOrd, PartialEq, Clone)]
pub struct CompoundAssignmentNode {
    pub lhs: Box<ExpressionNode>,
    pub rhs: Box<ExpressionNode>,
    pub op: BinaryOperation,
    pub span: Option<Span>,
}

macro_rules! node {
    ($name:ident, $visit:ident) => {
        impl SpannedNode for $name {
            fn span(&self) -> Option<Span> {
                self.span
            }
        }
        #[async_trait]
        impl AstNodeTrait for $name {
            async fn visit(&mut self, walker: &mut (impl TreeWalker + Send)) -> Result<()> {
                walker.$visit(self).await
            }
        }
    };
}
node!(ClassInitNode, visit_class_init);
node!(MemberNode, visit_member);
node!(CompoundAssignmentNode, visit_compound_assignment);

impl fmt::Display for ClassInitNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "new({}", self.type_)?;
        for (name, value) in &self.initializers {
            write!(f, ", {name}: {value}")?;
        }
        f.write_str(")")
    }
}
impl fmt::Display for MemberNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.receiver, self.name)
    }
}
impl fmt::Display for CompoundAssignmentNode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}= {}", self.lhs, self.op, self.rhs)
    }
}
