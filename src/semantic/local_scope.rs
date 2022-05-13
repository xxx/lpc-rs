use crate::semantic::symbol::Symbol;
use delegate::delegate;
use std::collections::HashMap;

/// A representation of a local scope / symbol table
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LocalScope {
    /// My ID
    pub id: usize,

    /// Map of symbol names to the symbol itself
    pub symbols: HashMap<String, Symbol>,
}

impl LocalScope {
    /// Create a new [`LocalScope`] with the passed `id`
    pub fn new(id: usize) -> Self {
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
            #[call(contains_key)]
            pub fn contains(&self, name: &str) -> bool;
        }
    }
}

impl Default for LocalScope {
    fn default() -> Self {
        Self::new(0)
    }
}

#[cfg(test)]
mod tests {
    use crate::core::lpc_type::LpcType;
    use super::*;

    #[test]
    fn test_lookup_looks_up_the_symbols() {
        let mut scope = LocalScope {
            id: 0,
            symbols: HashMap::new(),
        };

        let sym = Symbol {
            name: "foo".to_string(),
            type_: LpcType::Int(false),
            scope_id: 0,
            static_: false,
            location: None,
            span: None,
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
