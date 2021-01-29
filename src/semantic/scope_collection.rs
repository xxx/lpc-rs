use std::collections::HashMap;
use std::borrow::BorrowMut;
use indextree::{Arena, NodeId, Node};
use crate::semantic::local_scope::LocalScope;
use crate::semantic::symbol::Symbol;
use crate::codegen::scope_walker::ScopeWalker;

#[derive(Debug, Clone)]
/// Represent a tree of scopes
pub struct ScopeCollection {
    pub scopes: Arena<LocalScope>,
    pub current: Option<NodeId>,
    pub root: Option<NodeId>
}

impl ScopeCollection {
    /// Push a new scope onto the stack.
    pub fn push_new(&mut self) -> NodeId {
        let id = self.scopes.count();

        let scope = LocalScope {
            id,
            symbols: HashMap::new()
        };

        self.current = match self.current {
            Some(parent) => {
                let kid = self.scopes.new_node(scope);
                parent.append(kid, self.scopes.borrow_mut());
                Some(kid)
            },
            None => {
                Some(self.scopes.new_node(scope))
            }
        };

        if id == 0 {
            self.root = self.current;
        }

        self.current.unwrap()
    }

    // Get a scope based on its ID
    pub fn get(&self, index: NodeId) -> Option<&LocalScope> {
        let opt = self.scopes.get(index);

        if let Some(node) = opt {
            Some(node.get())
        } else {
            None
        }
    }

    // Get a mutable reference to a scope based on its ID
    pub fn get_mut(&mut self, index: NodeId) -> Option<&mut LocalScope> {
        let opt = self.scopes.get_mut(index);

        if let Some(node) = opt {
            Some(node.get_mut())
        } else {
            None
        }
    }

    // Get the current scope
    pub fn get_current(&self) -> Option<&LocalScope> {
         match self.current {
             Some(x) => self.get(x),
             None => None
         }
    }

    // Get a mutable reference to the current scope
    pub fn get_current_mut(&mut self) -> Option<&mut LocalScope> {
        match self.current {
            Some(x) => self.get_mut(x),
            None => None
        }
    }

    // Get the node for the current scope, used for traversal.
    pub fn get_current_node(&self) -> Option<&Node<LocalScope>> {
        match self.current {
            Some(x) => self.scopes.get(x),
            None => None
        }
    }

    /// Pop the top scope off of the stack.
    pub fn pop(&mut self) {
        println!("scaskdjadfskjasd {:?} :: {:?}", self.current, self);
        self.current = self.get_current_node().unwrap().parent();
    }

    /// Advance to the next node that would come during a depth-first traversal
    pub fn next(&mut self) -> Option<NodeId> {
        println!("next (current): {:?}", self.current);
        match self.current {
            Some(node_id) => {
                let kid = self.scopes.get(node_id).unwrap().first_child();

                println!("next (kid): {:?}", kid);

                if kid.is_some() {
                    self.current = kid;
                    return kid;
                }

                let sibling = self.scopes.get(node_id).unwrap().next_sibling();

                println!("next (sibling): {:?}", sibling);

                if sibling.is_some() {
                    self.current = sibling;
                    sibling
                } else {
                    self.current = None;
                    None
                }
            },
            None => {
                self.current = self.root;
                self.root
            }
        }
    }

    pub fn new_program(&mut self) {
        self.current = self.root;
    }

    /// Lookup a symbol, recursing up to parent scopes as necessary.
    ///
    /// # Arguments
    ///
    /// * `name`: The name of the symbol to look up.
    /// * `start_id`: The ID of the scope in which to start the search.
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut node_id = self.current?;
        loop {
            let node = self.scopes.get(node_id)?;
            let sym = node.get().lookup(name);
            if sym.is_some() {
                return sym;
            }

            node_id = node.parent()?;
        }
    }
}

impl Default for ScopeCollection {
    fn default() -> Self {
        Self {
            scopes: Arena::new(),
            current: None,
            root: None
        }
    }
}

impl From<ScopeWalker> for ScopeCollection {
    fn from(walker: ScopeWalker) -> Self {
        walker.scopes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::lpc_type::LPCVarType;

    #[test]
    fn test_push_new() {
        let mut collection = ScopeCollection::default();
        let scope1_id = collection.push_new();
        let scope2_id = collection.push_new();

        scope1_id.append(scope2_id, collection.scopes.borrow_mut());

        assert_eq!(collection.scopes.get(scope2_id).unwrap().parent(), Some(scope1_id));
    }

    #[test]
    fn test_lookup_finds_the_symbol() {
        let mut collection = ScopeCollection::default();
        collection.push_new();
        let sym = Symbol::new("foo", LPCVarType::String, false);
        collection.get_current_mut().unwrap().insert(sym);

        if let Some(scope_ref) = collection.lookup("foo") {
            assert_eq!(scope_ref.type_, LPCVarType::String);
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_checks_parent_recursively() {
        let mut collection = ScopeCollection::default();
        collection.push_new();
        let scope1 = collection.get_current_mut();

        let sym = Symbol::new("foo", LPCVarType::String, false);
        scope1.unwrap().insert(sym);

        collection.push_new();

        if let Some(scope_ref) = collection.lookup("foo") {
            assert_eq!(scope_ref.type_, LPCVarType::String);
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_returns_none_when_not_found() {
        let mut collection = ScopeCollection::default();
        collection.push_new();

        let result = collection.lookup("asdf");
        assert_eq!(result, None);
    }
}