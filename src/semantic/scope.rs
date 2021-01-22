use std::collections::HashMap;
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

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
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
            var_type: LPCVarType::Int,
            scope_id: 0
        };

        scope.insert(sym);

        if let Some(symbol) = scope.lookup("foo") {
            assert_eq!(symbol.name, "foo");
            assert_eq!(symbol.var_type, LPCVarType::Int);
        } else {
            panic!("symbol not found");
        }

        assert_eq!(scope.lookup("unknown"), None);
    }
}