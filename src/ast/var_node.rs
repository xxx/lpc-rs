use crate::{
    ast::ast_node::{ASTNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::CompilerError,
    parser::span::Span,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// A node representing the use of a variable.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VarNode {
    /// The name of the variable.
    pub name: String,

    /// The span of the string in the original file
    pub span: Option<Span>,

    /// Is this node referring to a global? Tracked in case a var is used,
    /// then subsequently defined.
    pub global: bool,
}

impl VarNode {
    pub fn new(name: &str) -> Self {
        Self {
            name: String::from(name),
            span: None,
            global: false,
        }
    }

    pub fn set_global(&mut self, val: bool) {
        self.global = val;
    }
}

impl SpannedNode for VarNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl ASTNodeTrait for VarNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<(), CompilerError> {
        tree_walker.visit_var(self)
    }
}

impl Display for VarNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "VarNode[{}] {}",
            self.name,
            if self.global { "(global)" } else { "" }
        )
    }
}
