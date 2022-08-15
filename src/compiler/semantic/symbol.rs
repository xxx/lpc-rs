use crate::compiler::ast::var_init_node::VarInitNode;
use lpc_rs_core::{
    global_var_flags::GlobalVarFlags, lpc_type::LpcType, register::Register, visibility::Visibility,
};
use lpc_rs_errors::span::Span;
use lpc_rs_function_support::function_prototype::FunctionPrototype;
use serde::{Deserialize, Serialize};
use lpc_rs_core::register::RegisterVariant;

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
    pub scope_id: usize,
    /// The text span that first defined this symbol.
    pub span: Option<Span>,
    /// The flags, used for global variables
    pub flags: GlobalVarFlags,
}

impl Symbol {
    /// Create a new, location-less Symbol
    pub fn new(name: &str, type_: LpcType) -> Self {
        Self {
            name: String::from(name),
            type_,
            location: None,
            scope_id: 0,
            span: None,
            flags: GlobalVarFlags::default(),
        }
    }

    /// We're global if we're in the top-level scope.
    #[inline]
    pub fn is_global(&self) -> bool {
        self.scope_id == 0
    }

    /// This variable public Only matters for globals.
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
            scope_id: 0,
            span: proto.span,
            flags: GlobalVarFlags::default(),
        }
    }
}

impl PartialEq<Symbol> for &Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        *self == other
    }
}
