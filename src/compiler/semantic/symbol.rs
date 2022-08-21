use indextree::NodeId;
use lpc_rs_core::{
    global_var_flags::GlobalVarFlags, lpc_type::LpcType, register::RegisterVariant,
    visibility::Visibility,
};
use lpc_rs_errors::span::Span;
use lpc_rs_function_support::function_prototype::FunctionPrototype;
use serde::{Deserialize, Serialize};

use crate::compiler::ast::var_init_node::VarInitNode;

/// Representation of a Symbol, to be stored in the Scopes
#[derive(Debug, Clone, Eq, PartialEq, Serialize, Deserialize)]
pub struct Symbol {
    /// The name of this symbol
    pub name: String,
    /// The type of this var
    pub type_: LpcType,
    /// Which register is tracking this variable?
    pub location: Option<RegisterVariant>,
    /// to which scope do i belong?
    pub scope_id: Option<NodeId>,
    /// The text span that first defined this symbol.
    pub span: Option<Span>,
    /// The flags, used for global variables
    pub flags: GlobalVarFlags,
    /// Whether this variable is referred to by a closure, meaning we
    /// need to store it beyond the invocation of the function it's defined
    /// within.
    pub upvalue: bool,
}

impl Symbol {
    /// Create a new, location-less Symbol
    pub fn new(name: &str, type_: LpcType) -> Self {
        Self {
            name: String::from(name),
            type_,
            location: None,
            scope_id: None,
            span: None,
            flags: GlobalVarFlags::default(),
            upvalue: false,
        }
    }

    /// We're global if we're in the top-level scope.
    #[inline]
    pub fn is_global(&self) -> bool {
        // NodeId uses a 1-based usize for its index
        self.scope_id.is_none() || Into::<usize>::into(self.scope_id.unwrap()) == 1_usize
    }

    /// Visibility only matters for globals.
    #[inline]
    pub fn public(&self) -> bool {
        self.flags.visibility() == Visibility::Public
    }
}

impl From<&mut VarInitNode> for Symbol {
    fn from(node: &mut VarInitNode) -> Self {
        let s = Self::new(&node.name, node.type_);

        Self {
            span: node.span,
            flags: node.flags.unwrap_or_default(),
            ..s
        }
    }
}

impl From<&FunctionPrototype> for Symbol {
    fn from(proto: &FunctionPrototype) -> Self {
        Self {
            name: proto.name.clone().into_owned(),
            type_: LpcType::Function(false),
            location: None,
            scope_id: None,
            span: proto.span,
            flags: GlobalVarFlags::default(),
            upvalue: false,
        }
    }
}

impl PartialEq<Symbol> for &Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        *self == other
    }
}

impl Default for Symbol {
    fn default() -> Self {
        Self {
            name: "".to_string(),
            type_: LpcType::Int(false),
            location: None,
            scope_id: None,
            span: None,
            flags: GlobalVarFlags::default(),
            upvalue: false,
        }
    }
}
