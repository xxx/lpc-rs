use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use itertools::Itertools;
use lpc_rs_core::function_flags::FunctionFlags;
use lpc_rs_errors::{Result, span::Span};
use ustr::Ustr;

use super::parsed_type::ParsedType;

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A constant for the `argv` variable that's automatically created in functions
/// with ellipsis args
pub const ARGV: &str = "argv";

/// A node representation a function definition
#[derive(Debug, PartialEq, PartialOrd, Clone, Hash, Eq)]
pub struct FunctionDefNode {
    pub return_type: ParsedType,
    pub name: Ustr,
    pub parameters: Vec<VarInitNode>,
    /// The named collector, excluded from the fixed `parameters` and call arity.
    pub variadic: Option<Box<VarInitNode>>,
    pub flags: FunctionFlags,
    pub body: Vec<AstNode>,
    pub span: Option<Span>,
}

impl SpannedNode for FunctionDefNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for FunctionDefNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_function_def(self).await
    }
}

impl Display for FunctionDefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut parameters = self
            .parameters
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<_>>();
        if let Some(collector) = &self.variadic {
            parameters.push(format!("{} {}...", collector.type_, collector.name));
        } else if self.flags.ellipsis() {
            parameters.push("...".into());
        }
        let p = parameters.join(", ");
        let b = self.body.iter().map(|a| a.to_string()).join(", ");
        write!(f, "{} {}({}) {{ {} }}", self.return_type, self.name, p, b)
    }
}
