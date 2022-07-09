use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::compiler::ast::{expression_node::ExpressionNode, var_init_node::VarInitNode};
use indextree::NodeId;
use lpc_rs_errors::Result;
use lpc_rs_errors::span::Span;
use crate::compiler::ast::ast_node::{AstNode, AstNodeTrait};
use crate::compiler::codegen::tree_walker::TreeWalker;

/// A constant to track the implicit variable we reserve space for,
/// for `foreach` loops.
/// Note the name of this is not parsable, so the user cannot refer to it or manipulate it.
pub const FOREACH_INDEX: &str = "foreach-index";

/// A constant to cache the length of the collection we're `foreach`ing over
pub const FOREACH_LENGTH: &str = "foreach-length";

#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub enum ForEachInit {
    Array(VarInitNode),
    Mapping {
        key: VarInitNode,
        value: VarInitNode,
    },
}

impl Display for ForEachInit {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ForEachInit::Array(node) => write!(f, "{}", node),
            ForEachInit::Mapping { key, value } => write!(f, "{} {}", key, value),
        }
    }
}

/// A node representing a `while` loop
#[derive(Debug, Clone, PartialOrd, PartialEq)]
pub struct ForEachNode {
    pub initializer: ForEachInit,
    pub collection: ExpressionNode,
    pub body: Box<AstNode>,
    pub scope_id: Option<NodeId>,
    pub span: Option<Span>,
}

impl ForEachNode {
    pub fn new(
        initializer: ForEachInit,
        collection: ExpressionNode,
        body: AstNode,
        span: Option<Span>,
    ) -> Self {
        Self {
            initializer,
            collection,
            body: Box::new(body),
            scope_id: None,
            span,
        }
    }
}

impl AstNodeTrait for ForEachNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_foreach(self)
    }
}

impl Display for ForEachNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "foreach ({}: {}) {{ {} }}",
            self.initializer, &self.collection, self.body
        )
    }
}
