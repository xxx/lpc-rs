use std::{
    fmt,
    fmt::{Display, Formatter},
};

use itertools::Itertools;
use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType, ScopeId};
use lpc_rs_errors::{span::Span, Result};
use qcell::QCellOwner;
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::{AstNode, AstNodeTrait, SpannedNode},
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
};

/// A node representation a function definition
#[derive(Debug, PartialEq, PartialOrd, Clone, Hash, Eq)]
pub struct ClosureNode {
    pub name: Ustr,
    pub return_type: LpcType,
    pub parameters: Option<Vec<VarInitNode>>,
    pub flags: FunctionFlags,
    pub body: Vec<AstNode>,
    pub span: Option<Span>,
    pub scope_id: Option<ScopeId>,
}

impl SpannedNode for ClosureNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for ClosureNode {
    fn visit(
        &mut self,
        tree_walker: &mut impl TreeWalker,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        tree_walker.visit_closure(self, cell_key)
    }
}

impl Display for ClosureNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let b = self.body.iter().map(|a| a.to_string()).join(", ");
        write!(f, "{{: {b} :}}")
    }
}
