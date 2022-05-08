use std::{
    fmt,
    fmt::{Display, Formatter},
};

use crate::{
    ast::{
        ast_node::{AstNode, AstNodeTrait},
        inherit_node::InheritNode,
    },
    codegen::tree_walker::TreeWalker,
    Result,
};
use itertools::Itertools;

/// A node representing a full object. This is the top-level translation unit.
#[derive(Debug, PartialEq, PartialOrd, Clone, Default)]
pub struct ProgramNode {
    /// The list of inherits of this Program
    pub inherits: Vec<InheritNode>,

    /// The list of global variables and function defs for this program
    pub body: Vec<AstNode>,
}

impl AstNodeTrait for ProgramNode {
    fn visit(&mut self, tree_walker: &mut impl TreeWalker) -> Result<()> {
        tree_walker.visit_program(self)
    }
}

impl Display for ProgramNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = self.body.iter().map(|a| a.to_string()).join(", ");

        write!(f, "{}", s)
    }
}
