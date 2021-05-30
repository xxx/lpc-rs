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
    semantic::lpc_type::LpcType,
    Result,
};

/// A node representation a function definition
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDefNode {
    pub return_type: LpcType,
    pub name: String,
    pub parameters: Vec<VarInitNode>,
    pub body: Vec<AstNode>,
    pub span: Option<Span>,
}

impl SpannedNode for FunctionDefNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for FunctionDefNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_function_def(self)
    }
}

impl Display for FunctionDefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "FunctionDefNode[{}, {} {:?} {:?}]",
            self.return_type, self.name, self.parameters, self.body
        )
    }
}
