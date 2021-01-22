use std::collections::HashMap;
use std::borrow::BorrowMut;
use crate::semantic::scope::Scope;
use crate::semantic::symbol::Symbol;

#[derive(Debug, Clone)]
pub struct ScopeCollection {
    pub scopes: Vec<Scope>,
}

impl ScopeCollection {
    pub fn insert(&mut self, scope: Scope) -> usize {
        let id = self.scopes.len();

        self.scopes.push(scope);
        id
    }

    pub fn insert_with_parent(&mut self, parent_id: Option<usize>) -> &mut Scope {
        let id = self.scopes.len();

        let object = Scope {
            id,
            parent_id,
            symbols: HashMap::new()
        };
        self.scopes.push(object);

        self.scopes[id].borrow_mut()
    }

    pub fn lookup(&self, name: &str, start_id: usize) -> Option<&Symbol> {
        if let Some(scope) = self.scopes.get(start_id) {
            let sym = scope.lookup(name);
            if sym.is_some() {
                return sym;
            }

            if let Some(parent_id) = scope.parent_id {
                return self.lookup(name, parent_id);
            }
        }

        None
    }
}

impl Default for ScopeCollection {
    fn default() -> Self {
        Self {
            scopes: vec![]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::lpc_type::LPCVarType;

    #[test]
    fn test_insert_with_parent() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.insert_with_parent(None);
        let scope1_id = scope1.id;
        let scope2 = collection.insert_with_parent(Some(scope1_id));
        let scope2_id = scope2.id;

        assert_eq!(collection.scopes.first().unwrap().id, scope1_id);

        let last = collection.scopes.last().unwrap();
        assert_eq!(last.id, scope2_id);
        assert_eq!(last.parent_id.unwrap(), scope1_id);
    }

    #[test]
    fn test_lookup_finds_the_symbol() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.insert_with_parent(None);
        let scope1_id = scope1.id;
        let sym = Symbol::new("foo", LPCVarType::String);
        scope1.insert(sym);

        if let Some(scope_ref) = collection.lookup("foo", scope1_id) {
            assert_eq!(scope_ref.var_type, LPCVarType::String);
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_checks_parent_recursively() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.insert_with_parent(None);
        let scope1_id = scope1.id;

        let sym = Symbol::new("foo", LPCVarType::String);
        scope1.insert(sym);

        let scope2 = collection.insert_with_parent(Some(scope1_id));
        let scope2_id = scope2.id;

        if let Some(scope_ref) = collection.lookup("foo", scope2_id) {
            assert_eq!(scope_ref.var_type, LPCVarType::String);
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_returns_none_when_not_found() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.insert_with_parent(None);
        let scope1_id = scope1.id;

        let result = collection.lookup("asdf", scope1_id);
        assert_eq!(result, None);
    }
}