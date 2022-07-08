use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
    parser::span::Span,
    semantic::function_flags::FunctionFlags,
    Result,
};
use itertools::Itertools;
use lpc_rs_core::lpc_type::LpcType;

/// A constant for the `argv` variable that's automatically created in functions with ellipsis args
pub const ARGV: &str = "argv";

/// A node representation a function definition
#[derive(Debug, PartialEq, PartialOrd, Clone)]
pub struct FunctionDefNode {
    pub return_type: LpcType,
    pub name: String,
    pub parameters: Vec<VarInitNode>,
    pub flags: FunctionFlags,
    pub body: Vec<AstNode>,
    pub span: Option<Span>,
}

impl SpannedNode for FunctionDefNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for FunctionDefNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_function_def(self)
    }
}

impl Display for FunctionDefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let p = self.parameters.iter().map(|a| a.to_string()).join(", ");
        let b = self.body.iter().map(|a| a.to_string()).join(", ");
        write!(f, "{} {}({}) {{ {} }}", self.return_type, self.name, p, b)
    }
}
