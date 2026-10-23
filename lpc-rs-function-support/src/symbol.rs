use std::fmt::{Display, Formatter};

use lpc_rs_core::{
    ScopeId, global_var_flags::GlobalVarFlags, lpc_type::LpcType, register::RegisterVariant,
    visibility::Visibility,
};
use lpc_rs_errors::span::Span;

use crate::function_prototype::FunctionPrototype;

/// Representation of a Symbol, to be stored in the Scopes
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Symbol {
    /// The name of this symbol
    pub name: String,
    /// The type of this var
    pub type_: LpcType,
    /// Which register is tracking this variable?
    pub location: Option<RegisterVariant>,
    /// to which scope do i belong?
    pub scope_id: Option<ScopeId>,
    /// The text span that first defined this symbol.
    pub span: Option<Span>,
    /// The flags, used for global variables
    pub flags: GlobalVarFlags,
    /// Whether this variable is referred to by a closure, meaning we
    /// need to store it beyond the invocation of the function it's defined
    /// within.
    pub upvalue: bool,
    /// Whether this is a `ref` parameter: its cell is the caller's variable.
    pub by_ref: bool,
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
            by_ref: false,
        }
    }

    /// We're global if we're in the top-level scope.
    #[inline]
    pub fn is_global(&self) -> bool {
        // NodeId uses a 1-based usize for its index
        self.scope_id.is_none() || Into::<usize>::into(self.scope_id.unwrap()) == 1_usize
    }

    /// Whether inheriting programs can see this global.
    #[inline]
    pub fn visible_to_children(&self) -> bool {
        self.flags.visibility() != Visibility::Private
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_)
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
            by_ref: false,
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
            by_ref: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indextree::Arena;
    use lpc_rs_core::lpc_path::LpcPath;

    use super::*;
    use crate::function_prototype::FunctionPrototypeBuilder;

    #[test]
    fn globals_have_no_scope_or_the_root_scope() {
        let mut scopes = Arena::new();
        let root = scopes.new_node(());
        let local = scopes.new_node(());
        root.append(local, &mut scopes);

        for (scope_id, expected) in [(None, true), (Some(root), true), (Some(local), false)] {
            let mut symbol = Symbol::new("value", LpcType::Int(false));
            symbol.scope_id = scope_id;

            assert_eq!(symbol.is_global(), expected);
        }
    }

    #[test]
    fn inherited_globals_exclude_only_private_visibility() {
        for (visibility, expected) in [
            (Visibility::Public, true),
            (Visibility::Protected, true),
            (Visibility::Private, false),
        ] {
            let mut symbol = Symbol::new("value", LpcType::Int(false));
            symbol.flags.set_visibility(visibility);

            assert_eq!(symbol.visible_to_children(), expected);
        }
    }

    #[test]
    fn function_symbols_keep_the_name_span_and_callable_type() {
        let span = Some(Span::new(0, 4..9));
        let prototype = FunctionPrototypeBuilder::default()
            .name("work")
            .filename(Arc::new(LpcPath::from("/work.c")))
            .return_type(LpcType::Int(false))
            .span(span)
            .build()
            .unwrap();

        let symbol = Symbol::from(&prototype);

        assert_eq!(symbol.name, "work");
        assert_eq!(symbol.type_, LpcType::Function(false));
        assert_eq!(symbol.span, span);
        assert_eq!(symbol.location, None);
    }

    #[test]
    fn borrowed_symbol_equality_uses_the_symbol_metadata() {
        let symbol = Symbol::new("value", LpcType::Int(false));
        let same = symbol.clone();
        let other = Symbol::new("other", LpcType::Int(false));
        let borrowed = &symbol;

        assert!(<&Symbol as PartialEq<Symbol>>::eq(&borrowed, &same));
        assert!(!<&Symbol as PartialEq<Symbol>>::eq(&borrowed, &other));
    }

    #[test]
    fn display_includes_the_symbol_name_and_type() {
        assert_eq!(
            Symbol::new("value", LpcType::Int(false)).to_string(),
            "value: int"
        );
    }
}
