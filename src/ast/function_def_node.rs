use crate::{
    ast::{
        ast_node::{ASTNode, ASTNodeTrait},
        var_init_node::VarInitNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
    semantic::lpc_type::LPCType,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representation a function definition
#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDefNode {
    pub return_type: LPCType,
    pub name: String,
    pub parameters: Vec<VarInitNode>,
    pub body: Vec<ASTNode>,
    pub span: Option<Span>,
}

impl ASTNodeTrait for FunctionDefNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
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

impl Clone for FunctionDefNode {
    fn clone(&self) -> Self {
        Self {
            return_type: self.return_type,
            name: self.name.clone(),
            parameters: self.parameters.to_vec(),
            body: self.body.to_vec(),
            span: self.span,
        }
    }
}
