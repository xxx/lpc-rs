use std::collections::HashMap;
use std::borrow::BorrowMut;
use indextree::{Arena, NodeId, Node};
use crate::semantic::local_scope::LocalScope;
use crate::semantic::symbol::Symbol;
use crate::codegen::scope_walker::ScopeWalker;

#[derive(Debug, Clone)]
/// Represent a tree of scopes
pub struct ScopeTree {
    /// The actual scopes
    pub scopes: Arena<LocalScope>,

    /// ID of the current scope
    pub current_id: Option<NodeId>,

    /// ID of the root of the tree
    pub root_id: Option<NodeId>
}

impl ScopeTree {
    /// Push a new scope onto the tree, and make it the current scope.
    pub fn push_new(&mut self) -> NodeId {
        let id = self.scopes.count();

        let scope = LocalScope {
            id,
            symbols: HashMap::new()
        };

        self.current_id = match self.current_id {
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
            self.root_id = self.current_id;
        }

        self.current_id.unwrap()
    }

    /// Get a scope based on its ID
    pub fn get(&self, index: NodeId) -> Option<&LocalScope> {
        Some(self.scopes.get(index)?.get())
    }

    /// Get a mutable reference to a scope based on its ID
    pub fn get_mut(&mut self, index: NodeId) -> Option<&mut LocalScope> {
        Some(self.scopes.get_mut(index)?.get_mut())
    }

    /// Get the current scope
    pub fn get_current(&self) -> Option<&LocalScope> {
         match self.current_id {
             Some(x) => self.get(x),
             None => None
         }
    }

    /// Get a mutable reference to the current scope
    pub fn get_current_mut(&mut self) -> Option<&mut LocalScope> {
        match self.current_id {
            Some(x) => self.get_mut(x),
            None => None
        }
    }

    /// Get the node for the current scope, used for traversal.
    pub fn get_current_node(&self) -> Option<&Node<LocalScope>> {
        match self.current_id {
            Some(x) => self.scopes.get(x),
            None => None
        }
    }

    /// Set the current scope to the current's parent.
    pub fn pop(&mut self) {
        self.current_id = self.get_current_node().unwrap().parent();
    }

    pub fn goto_root(&mut self) {
        self.current_id = self.root_id;
    }

    /// Lookup a symbol, recursing up to parent scopes as necessary.
    ///
    /// # Arguments
    ///
    /// * `name`: The name of the symbol to look up.
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let mut node_id = self.current_id?;
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

impl Default for ScopeTree {
    fn default() -> Self {
        Self {
            scopes: Arena::new(),
            current_id: None,
            root_id: None
        }
    }
}

impl Iterator for ScopeTree {
    type Item = NodeId;

    /// Advance to the next node that would come during a depth-first traversal
    fn next(&mut self) -> Option<Self::Item> {
        match self.current_id {
            Some(node_id) => {
                let kid = self.scopes.get(node_id).unwrap().first_child();

                if kid.is_some() {
                    self.current_id = kid;
                    return kid;
                }

                let sibling = self.scopes.get(node_id).unwrap().next_sibling();

                if sibling.is_some() {
                    self.current_id = sibling;
                    sibling
                } else {
                    self.current_id = None;
                    None
                }
            },
            None => {
                self.current_id = self.root_id;
                self.root_id
            }
        }
    }
}

impl From<ScopeWalker> for ScopeTree {
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
        let mut collection = ScopeTree::default();
        let scope1_id = collection.push_new();
        let scope2_id = collection.push_new();

        scope1_id.append(scope2_id, collection.scopes.borrow_mut());

        assert_eq!(collection.scopes.get(scope2_id).unwrap().parent(), Some(scope1_id));
    }

    #[test]
    fn test_lookup_finds_the_symbol() {
        let mut collection = ScopeTree::default();
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
        let mut collection = ScopeTree::default();
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
        let mut tree = ScopeTree::default();
        tree.push_new();

        let result = tree.lookup("asdf");
        assert_eq!(result, None);
    }

    mod test_next {
        use super::*;

        #[test]
        fn test_next() {
            let mut tree = ScopeTree::default();
            let _root = tree.push_new();
            let child = tree.push_new();
            tree.goto_root();
            let child2 = tree.push_new();
            tree.goto_root();

            assert_eq!(tree.next().unwrap(), child);
            assert_eq!(tree.next().unwrap(), child2);
            assert_eq!(tree.next(), None);
        }
    }
}