use std::{
    fmt,
    fmt::{Display, Formatter},
};

use itertools::Itertools;
use lazy_format::lazy_format;
use lpc_rs_errors::{span::Span, Result};
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// Receiver types. Function pointers can be declared with a dynamic receiver
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum FunctionPtrReceiver {
    Static(Box<ExpressionNode>),
    Dynamic,
}

impl Display for FunctionPtrReceiver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionPtrReceiver::Static(x) => write!(f, "({x})"),
            FunctionPtrReceiver::Dynamic => write!(f, "&"),
        }
    }
}

/// A node representing a pointer to a function
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct FunctionPtrNode {
    pub receiver: Option<FunctionPtrReceiver>,
    pub arguments: Option<Vec<Option<ExpressionNode>>>,
    pub name: Ustr,

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
            else => ("({})", args)
        );
        let fmt = lazy_format!(
            if let Some(e) = &self.receiver => ("{}->{}{}", e, self.name, arg_fmt)
            else => ("{}{}", self.name, arg_fmt)
        );
        write!(f, "&{fmt}")
    }
}
