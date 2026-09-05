use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use itertools::Itertools;
use lpc_rs_errors::{Result, span::Span};
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

#[async_trait]
impl AstNodeTrait for FunctionPtrNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_function_ptr(self).await
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
        write!(f, "&")?;
        if let Some(e) = &self.receiver {
            write!(f, "{e}->")?;
        }
        write!(f, "{}", self.name)?;
        if !args.is_empty() {
            write!(f, "({args})")?;
        }
        Ok(())
    }
}
