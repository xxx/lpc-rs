use std::{
    fmt,
    fmt::{Display, Formatter},
};

use itertools::Itertools;
use lazy_format::lazy_format;
use lpc_rs_core::call_namespace::CallNamespace;
use lpc_rs_errors::{span::Span, Result};
use qcell::QCellOwner;
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::{AstNodeTrait, SpannedNode},
        expression_node::ExpressionNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// Help handle chained calls, like foo("bar")("baz");
#[derive(Hash, Debug, Eq, PartialEq, PartialOrd, Clone)]
pub enum CallChain {
    Root {
        /// The receiver, for the case of `call_other`
        receiver: Option<Box<ExpressionNode>>,

        /// When searching for this function, where do we start?
        namespace: CallNamespace,

        /// The name of the function being called
        name: Ustr,
    },
    Node(Box<CallNode>),
}

/// Representation of a function call.
#[derive(Hash, Debug, Eq, PartialEq, PartialOrd, Clone)]
pub struct CallNode {
    /// Is this call chained off of another call?
    pub chain: CallChain,

    /// The list of function arguments being passed.
    pub arguments: Vec<ExpressionNode>,

    /// The text span in the original file that this node represents. Used for
    /// error messages.
    pub span: Option<Span>,
}

impl CallNode {
    pub fn set_receiver(&mut self, new_receiver: ExpressionNode) {
        match &mut self.chain {
            CallChain::Root { ref mut receiver, .. } => {
                *receiver = Some(Box::new(new_receiver));
            }
            CallChain::Node(node) => {
                node.set_receiver(new_receiver);
            }
        }
    }
}

impl SpannedNode for CallNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for CallNode {
    fn visit(
        &mut self,
        tree_walker: &mut impl TreeWalker,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        tree_walker.visit_call(self, cell_key)
    }
}

impl Display for CallNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let args = self.arguments.iter().map(|a| a.to_string()).join(" ,");
        match &self.chain {
            CallChain::Root { receiver, name, namespace } => {
                let fmt = lazy_format!(
                    if let Some(rcvr) = receiver => ("{}->{}{}({})", rcvr, namespace, name, args)
                    else => ("{}({})", name, args)
                );

                write!(f, "{fmt}")
            }
            CallChain::Node(node) => {
                let fmt = lazy_format!("{}({})", node, args);

                write!(f, "{fmt}")
            }
        }
    }
}
