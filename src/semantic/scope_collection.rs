use std::collections::HashMap;
use std::borrow::BorrowMut;
use crate::semantic::scope::Scope;
use crate::semantic::symbol::Symbol;

#[derive(Debug, Clone)]
/// Represent a tree of scopes
pub struct ScopeCollection {
    pub scopes: Vec<Scope>,
}

impl ScopeCollection {
    /// Push a new scope onto the stack.
    pub fn push_new(&mut self) -> &mut Scope {
        let parent_id = if let Some(scope) = self.scopes.last() {
            Some(scope.id)
        } else {
            None
        };

        let id = self.scopes.len();

        let scope = Scope {
            id,
            parent_id,
            symbols: HashMap::new()
        };
        self.scopes.push(scope);

        self.scopes[id].borrow_mut()
    }

    /// Pop the top scope off of the stack.
    pub fn pop(&mut self) {
        self.scopes.pop();
    }

    /// Return a reference to the top scope of the stack.
    pub fn last(&self) -> Option<&Scope> {
        self.scopes.last()
    }

    /// Return a mutable reference to the scope on top of the stack.
    pub fn last_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }

    /// Lookup a symbol, recursing up to parent scopes as necessary.
    ///
    /// # Arguments
    ///
    /// * `name`: The name of the symbol to look up.
    /// * `start_id`: The ID of the scope in which to start the search.
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
    fn test_push_new() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.push_new();
        let scope1_id = scope1.id;
        let scope2 = collection.push_new();
        let scope2_id = scope2.id;

        assert_eq!(collection.scopes.first().unwrap().id, scope1_id);

        let last = collection.scopes.last().unwrap();
        assert_eq!(last.id, scope2_id);
        assert_eq!(last.parent_id.unwrap(), scope1_id);
    }

    #[test]
    fn test_lookup_finds_the_symbol() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.push_new();
        let scope1_id = scope1.id;
        let sym = Symbol::new("foo", LPCVarType::String, false);
        scope1.insert(sym);

        if let Some(scope_ref) = collection.lookup("foo", scope1_id) {
            assert_eq!(scope_ref.type_, LPCVarType::String);
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_checks_parent_recursively() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.push_new();

        let sym = Symbol::new("foo", LPCVarType::String, false);
        scope1.insert(sym);

        let scope2 = collection.push_new();
        let scope2_id = scope2.id;

        if let Some(scope_ref) = collection.lookup("foo", scope2_id) {
            assert_eq!(scope_ref.type_, LPCVarType::String);
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_returns_none_when_not_found() {
        let mut collection = ScopeCollection::default();
        let scope1 = collection.push_new();
        let scope1_id = scope1.id;

        let result = collection.lookup("asdf", scope1_id);
        assert_eq!(result, None);
    }
}