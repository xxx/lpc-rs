use crate::semantic::lpc_type::LPCVarType;
use crate::ast::var_init_node::VarInitNode;
use crate::asm::register::Register;
use crate::parser::span::Span;

/// Representation of a Symbol, to be stored in the Scopes
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    /// The name of this symbol
    pub name: String,
    /// The type of this var
    pub type_: LPCVarType,
    /// Is this var an array?
    pub array: bool,
    // pub privacy: LPCPrivacy,
    /// Is this var static?
    pub static_: bool,
    /// Which register is tracking this variable?
    pub location: Option<Register>,
    /// to which scope do i belong?
    pub scope_id: usize,
    /// The text span that first defined this symbol.
    pub span: Option<Span>
}

impl Symbol {
    /// Create a new, location-less Symbol
    pub fn new(name: &str, type_: LPCVarType, array: bool) -> Self {
        Self {
            name: String::from(name),
            type_,
            array,
            static_: false,
            location: None,
            scope_id: 0,
            span: None
        }
    }
}

impl From<&VarInitNode> for Symbol {
    fn from(node: &VarInitNode) -> Self {
        let s = Self::new(&node.name, node.type_, node.array);

        Self {
            span: node.span,
            ..s
        }
    }
}

impl PartialEq<Symbol> for &Symbol {
    fn eq(&self, other: &Symbol) -> bool {
        *self == other
    }
}