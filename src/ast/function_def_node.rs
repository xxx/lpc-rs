use crate::ast::ast_node::{ASTNodeTrait, ASTNode};
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::var_init_node::VarInitNode;
use crate::errors::CompilerError;
use crate::parser::span::Span;
use crate::semantic::lpc_type::LPCType;

/// A node representation a function definition
#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDefNode {
    pub return_type: LPCType,
    pub name: String,
    pub parameters: Vec<VarInitNode>,
    pub body: Vec<ASTNode>,
    pub span: Option<Span>
}

impl ASTNodeTrait for FunctionDefNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_function_def(self)
    }
}

impl Display for FunctionDefNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "FunctionDefNode[{}, {} {:?} {:?}]", self.return_type, self.name, self.parameters, self.body)
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