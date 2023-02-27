use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
};

use delegate::delegate;
use lpc_rs_core::ScopeId;
use lpc_rs_function_support::symbol::Symbol;

/// A representation of a local scope / symbol table
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalScope {
    /// My ID
    pub id: Option<ScopeId>,

    /// Map of symbol names to the symbol itself
    pub symbols: HashMap<String, Symbol>,
}

impl LocalScope {
    /// Create a new [`LocalScope`] with the passed `id`
    pub fn new(id: Option<ScopeId>) -> Self {
        Self {
            id,
            symbols: HashMap::new(),
        }
    }

    /// Insert a new symbol into this scope.
    ///
    /// # Arguments
    ///
    /// `symbol` - The [`Symbol`] to insert
    pub fn insert(&mut self, mut symbol: Symbol) {
        symbol.scope_id = self.id;
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    delegate! {
        to self.symbols {
            /// Get a reference to a symbol in this specific scope.
            #[call(get)]
            pub fn lookup(&self, name: &str) -> Option<&Symbol>;

            /// Get a mutable reference to a symbol in this specific scope.
            #[call(get_mut)]
            pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol>;

            /// Get whether or not a symbol is defined in this scope.
            /// This returns results for this LocalScope only, not any parent scopes.
            #[call(contains_key)]
            pub fn contains(&self, name: &str) -> bool;
        }
    }
}

impl Default for LocalScope {
    fn default() -> Self {
        Self::new(None)
    }
}

impl Display for LocalScope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let id = self.id.map(|id| id.into()).unwrap_or(0);
        write!(f, "LocalScope {{ id: {id}, symbols: [")?;

        for (name, symbol) in self.symbols.iter() {
            let loc = symbol
                .location
                .map(|loc| loc.to_string())
                .unwrap_or_else(|| "<none>".to_string());
            write!(
                f,
                "{{ name: {}, type: {}, location: {} }}",
                name, symbol.type_, loc
            )?;
        }

        write!(f, "] }}")
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_type::LpcType;

    use super::*;

    #[test]
    fn test_lookup_looks_up_the_symbols() {
        let mut scope = LocalScope {
            id: None,
            symbols: HashMap::new(),
        };

        let sym = Symbol {
            name: "foo".to_string(),
            type_: LpcType::Int(false),
            ..Default::default()
        };

        scope.insert(sym);

        if let Some(symbol) = scope.lookup("foo") {
            assert_eq!(symbol.name, "foo");
            assert_eq!(symbol.type_, LpcType::Int(false));
        } else {
            panic!("symbol not found");
        }

        assert_eq!(scope.lookup("unknown"), None);
    }
}
