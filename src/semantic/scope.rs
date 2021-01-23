use std::collections::HashMap;
use delegate::delegate;
use crate::semantic::symbol::Symbol;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Scope {
    pub id: usize,
    pub parent_id: Option<usize>,
    pub symbols: HashMap<String, Symbol>
}

impl Scope {
    pub fn insert(&mut self, mut symbol: Symbol) {
        symbol.scope_id = self.id;
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    delegate! {
        to self.symbols {
            #[call(get)]
            pub fn lookup(&self, name: &str) -> Option<&Symbol>;

            #[call(get_mut)]
            pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol>;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::lpc_type::LPCVarType;

    #[test]
    fn test_lookup_looks_up_the_symbols() {
        let mut scope = Scope {
            id: 0,
            parent_id: None,
            symbols: HashMap::new()
        };

        let sym = Symbol {
            name: "foo".to_string(),
            type_: LPCVarType::Int,
            array: false,
            scope_id: 0,
            static_: false,
            location: None
        };

        scope.insert(sym);

        if let Some(symbol) = scope.lookup("foo") {
            assert_eq!(symbol.name, "foo");
            assert_eq!(symbol.type_, LPCVarType::Int);
        } else {
            panic!("symbol not found");
        }

        assert_eq!(scope.lookup("unknown"), None);
    }
}