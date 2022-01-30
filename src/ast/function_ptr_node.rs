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
use itertools::Itertools;
use lazy_format::lazy_format;

/// A node representing a pointer to a function
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct FunctionPtrNode {
    pub receiver: Option<Box<ExpressionNode>>,
    pub arguments: Option<Vec<Option<ExpressionNode>>>,
    pub name: String,

    /// The span of the string in the original file
    pub span: Option<Span>,
}

impl SpannedNode for FunctionPtrNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for FunctionPtrNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_function_ptr(self)
    }
}

impl Display for FunctionPtrNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args = match &self.arguments {
            None => "".into(),
            Some(args) => args
                .iter()
                .map(|a| match a {
                    Some(x) => x.to_string(),
                    None => String::new(),
                })
                .join(" ,"),
        };
        let arg_fmt = lazy_format!(
            if args.is_empty() => ("")
            else ("({})", args)
        );
        let fmt = lazy_format!(
            if let Some(e) = &self.receiver => ("({})->{}{}", e, self.name, arg_fmt)
            else ("{}{}", self.name, arg_fmt)
        );
        write!(f, "&{}", fmt)
    }
}
