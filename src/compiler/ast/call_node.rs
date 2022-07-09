use lazy_format::lazy_format;
use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    codegen::tree_walker::TreeWalker,
};
use itertools::Itertools;
use lpc_rs_core::call_namespace::CallNamespace;
use lpc_rs_errors::Result;
use lpc_rs_errors::span::Span;
use crate::compiler::ast::{
    ast_node::{AstNodeTrait, SpannedNode},
    expression_node::ExpressionNode,
};

/// Representation of a function call.
#[derive(Hash, Debug, Eq, PartialEq, PartialOrd, Clone)]
pub struct CallNode {
    /// The receiver, for the case of `call_other`
    pub receiver: Option<Box<ExpressionNode>>,

    /// The list of function arguments being passed.
    pub arguments: Vec<ExpressionNode>,

    /// The name of the function being called
    pub name: String,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>,

    /// When searching for this function, where do we start?
    pub namespace: CallNamespace,
}

impl SpannedNode for CallNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for CallNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_call(self)
    }
}

impl Display for CallNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args = self.arguments.iter().map(|a| a.to_string()).join(" ,");
        let fmt = lazy_format!(
            if let Some(e) = &self.receiver => ("{}->{}{}({})", e, self.namespace, self.name, args)
            else ("{}({})", self.name, args)
        );
        write!(f, "{}", fmt)
    }
}
