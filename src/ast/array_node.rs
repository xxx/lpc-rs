use crate::{
    ast::{ast_node::ASTNodeTrait, expression_node::ExpressionNode},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing an array literal
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ArrayNode {
    pub value: Vec<ExpressionNode>,

    /// The full span of all expressions in the array - from the left side of the first item
    /// to the right side of the last
    pub span: Option<Span>,
}

impl ArrayNode {
    /// Construct a new `ArrayNode`. The `span` will be calculated
    /// as the span from the start of the first item, to the end of
    /// the last.
    pub fn new(value: Vec<ExpressionNode>) -> Self {
        let span = if value.is_empty() {
            None
        } else if let (Some(node1), Some(node2)) = (value[0].span(), value.last().unwrap().span()) {
            Some(Span {
                l: node1.l,
                r: node2.r,
            })
        } else {
            None
        };

        Self { value, span }
    }
}

impl ASTNodeTrait for ArrayNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_array(self)
    }
}

impl Display for ArrayNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self
            .value
            .iter()
            .map(|item| format!("{}", item))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "ArrayNode[{}]", s)
    }
}
