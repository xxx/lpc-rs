use crate::ast::ast_node::ASTNodeTrait;
use crate::codegen::tree_walker::TreeWalker;
use std::fmt::{Display, Formatter};
use std::fmt;
use crate::ast::expression_node::ExpressionNode;
use crate::parser::span::Span;
use crate::errors::CompilerError;
use crate::semantic::lpc_type::LPCType;

/// A node representing a variable definition, with optional initialization
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarInitNode {
    /// The LPC type. This is the "true" type, even for mixed vars.
    pub type_: LPCType,

    /// The var name
    pub name: String,

    /// The initialization value. When missing, defaults to 0.
    pub value: Option<ExpressionNode>,

    /// Is this var actually an array?
    pub array: bool,

    /// The text span in the original file that this node represents. Used for error messages.
    pub span: Option<Span>
}

impl ASTNodeTrait for VarInitNode {
    /// This is the double-dispatch endpoint for tree-walking
    fn visit(&self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_var_init(self)
    }
}

impl Display for VarInitNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VarInitNode[{} {} {:?}]", self.type_, self.name, self.value)
    }
}