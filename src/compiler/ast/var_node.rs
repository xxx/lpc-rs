use std::{
    fmt,
    fmt::{Display, Formatter},
};

use async_trait::async_trait;
use lpc_rs_errors::{span::Span, Result};
use ustr::Ustr;

use crate::compiler::{
    ast::ast_node::{AstNodeTrait, SpannedNode},
    codegen::tree_walker::TreeWalker,
};

/// A node representing the use of a variable.
#[derive(Hash, Debug, Clone, Eq, PartialOrd, PartialEq)]
pub struct VarNode {
    /// The name of the variable.
    pub name: Ustr,

    /// The span of the string in the original file
    pub span: Option<Span>,

    /// Is this node referring to a global? Tracked in case a var is used,
    /// then subsequently defined.
    pub global: bool,

    /// Is this variable actually the name of a function? Function pointers
    /// create this ambiguity. The only time this ends up true is in the
    /// case of a bareword function var init, e.g.: `function f = dump;` as
    /// `dump` in this case would otherwise be seen as a variable.
    /// The alternate `&name()` syntax is parsed as a function pointer from the
    /// start.
    pub function_name: bool,

    /// Is this variable captured from another function scope?
    pub external_capture: bool,
}

impl VarNode {
    pub fn new(name: &str) -> Self {
        Self {
            name: Ustr::from(name),
            span: None,
            global: false,
            function_name: false,
            external_capture: false,
        }
    }

    /// Set whether this node is global
    pub fn set_global(&mut self, val: bool) {
        self.global = val;
    }

    /// Set whether this node is actually a known function name, rather than a
    /// variable.
    pub fn set_function_name(&mut self, val: bool) {
        self.function_name = val;
    }

    /// Is this var a reference to a positional closure parameter?
    pub fn is_closure_arg_var(&self) -> bool {
        self.name.starts_with('$')
    }
}

impl SpannedNode for VarNode {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

#[async_trait]
impl AstNodeTrait for VarNode {
    async fn visit(&mut self, tree_walker: &mut (impl TreeWalker + Send)) -> Result<()> {
        tree_walker.visit_var(self).await
    }
}

impl Display for VarNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_closure_arg_var() {
        let mut node = VarNode::new("$0");
        assert!(node.is_closure_arg_var());
        node.name = "asdf".into();
        assert!(!node.is_closure_arg_var());
    }
}
