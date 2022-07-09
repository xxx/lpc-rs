use std::{
    fmt,
    fmt::{Display, Formatter},
};
use lpc_rs_errors::Result;

use lpc_rs_errors::span::Span;
use crate::compiler::ast::ast_node::{AstNodeTrait, SpannedNode};
use crate::compiler::codegen::tree_walker::TreeWalker;

/// A node representing the use of a variable.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct VarNode {
    /// The name of the variable.
    pub name: String,

    /// The span of the string in the original file
    pub span: Option<Span>,

    /// Is this node referring to a global? Tracked in case a var is used,
    /// then subsequently defined.
    pub global: bool,

    /// Is this variable actually the name of a function? Function pointers create this ambiguity.
    /// The only time this ends up true is in the case of a bareword function var init, e.g.:
    /// `function f = dump;` as `dump` in this case would otherwise be seen as a variable.
    /// The alternate `&name()` syntax is parsed as a function pointer from the start.
    pub function_name: bool,
}

impl VarNode {
    pub fn new(name: &str) -> Self {
        Self {
            name: String::from(name),
            span: None,
            global: false,
            function_name: false,
        }
    }

    /// Set whether this node is global
    pub fn set_global(&mut self, val: bool) {
        self.global = val;
    }

    /// Set whether this node is actually a known function name, rather than a variable.
    pub fn set_function_name(&mut self, val: bool) {
        self.function_name = val;
    }
}

impl SpannedNode for VarNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

impl AstNodeTrait for VarNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_var(self)
    }
}

impl Display for VarNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
