use crate::{
    context::Context,
    semantic::{local_scope::LocalScope, symbol::Symbol},
};
use indextree::{Arena, Node, NodeId};
use std::collections::HashMap;

#[derive(Debug, Clone)]
/// Represent a tree of scopes
pub struct ScopeTree {
    /// The actual scopes
    pub scopes: Arena<LocalScope>,

    /// ID of the current scope
    pub current_id: Option<NodeId>,

    /// ID of the root of the tree (i.e. the global scope)
    pub root_id: Option<NodeId>,

    /// ID of the scopes of this program's functions
    function_scopes: HashMap<String, NodeId>,
}

impl ScopeTree {
    /// Push a new scope onto the tree, as a child of the current scope,
    /// and make it the current scope.
    ///
    /// # Example
    /// ```
    /// use lpc_rs::semantic::scope_tree::ScopeTree;
    ///
    /// let mut tree = ScopeTree::default();
    ///
    /// let root_id = tree.push_new();
    /// let root = tree.get(root_id);
    ///
    /// let child_id = tree.push_new();
    /// let grandchild_id = tree.push_new();
    /// tree.pop();
    /// let grandchild_id_2 = tree.push_new();
    ///
    /// // etc.
    ///
    pub fn push_new(&mut self) -> NodeId {
        let id = self.scopes.count();

        let scope = LocalScope {
            id,
            symbols: HashMap::new(),
        };

        self.current_id = match self.current_id {
            Some(parent) => {
                let kid = self.scopes.new_node(scope);
                parent.append(kid, &mut self.scopes);
                Some(kid)
            }
            None => Some(self.scopes.new_node(scope)),
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
            None => None,
        }
    }

    /// Get a mutable reference to the current scope
    pub fn get_current_mut(&mut self) -> Option<&mut LocalScope> {
        match self.current_id {
            Some(x) => self.get_mut(x),
            None => None,
        }
    }

    /// Get the node for the current scope, used for traversal.
    pub fn get_current_node(&self) -> Option<&Node<LocalScope>> {
        match self.current_id {
            Some(x) => self.scopes.get(x),
            None => None,
        }
    }

    /// Insert a new function scope
    pub fn insert_function(&mut self, name: &str, id: &NodeId) {
        self.function_scopes.insert(name.to_string(), *id);
    }

    /// Set the current scope to the current's parent.
    /// Note that this method does *not* actually remove the scope from the tree.
    /// It remains accessible.
    pub fn pop(&mut self) {
        self.current_id = self.get_current_node().unwrap().parent();
    }

    /// Set the current node to the root of the tree.
    pub fn goto_root(&mut self) {
        self.current_id = self.root_id;
    }

    /// Set the current node to the scope for function named `name`.
    ///
    /// # Panics
    /// Will panic if an unknown function name is passed.
    pub fn goto_function(&mut self, name: &str) {
        if let Some(id) = self.function_scopes.get(name) {
            self.current_id = Some(*id);
        } else {
            panic!("Unknown function passed to goto_function: {}", name);
        }
    }

    /// Look up a symbol, recursing up to parent scopes as necessary.
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

    /// Look up a symbol and return a mutable reference,
    /// recursing up to parent scopes as necessary.
    ///
    /// # Arguments
    ///
    /// * `name`: The name of the symbol to look up.
    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        let mut node_id = self.current_id?;
        loop {
            let node = self.scopes.get(node_id)?;
            let sym = node.get().lookup(name);

            if sym.is_some() {
                let node = self.scopes.get_mut(node_id)?;
                let sym = node.get_mut().lookup_mut(name);
                return sym;
            }

            node_id = node.parent()?;
        }
    }

    /// Look up a symbol in the global scope and return a reference if found.
    ///
    /// # Arguments
    ///
    /// * `name`: The name of the symbol to look up.
    pub fn lookup_global(&self, name: &str) -> Option<&Symbol> {
        if let Some(root_id) = self.root_id {
            self.scopes.get(root_id)?.get().lookup(name)
        } else {
            None
        }
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self {
            scopes: Arena::new(),
            current_id: None,
            root_id: None,
            function_scopes: HashMap::new(),
        }
    }
}

impl From<Context> for ScopeTree {
    fn from(context: Context) -> Self {
        context.scopes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic::lpc_type::LpcType;

    #[test]
    fn test_push_new() {
        let mut collection = ScopeTree::default();
        let scope1_id = collection.push_new();
        let scope2_id = collection.push_new();

        scope1_id.append(scope2_id, &mut collection.scopes);

        assert_eq!(
            collection.scopes.get(scope2_id).unwrap().parent(),
            Some(scope1_id)
        );
    }

    #[test]
    fn test_lookup_finds_the_symbol() {
        let mut collection = ScopeTree::default();
        collection.push_new();
        let sym = Symbol::new("foo", LpcType::String(false));
        collection.get_current_mut().unwrap().insert(sym);

        if let Some(scope_ref) = collection.lookup("foo") {
            assert_eq!(scope_ref.type_, LpcType::String(false));
        } else {
            panic!("symbol not found.");
        }
    }

    #[test]
    fn test_lookup_checks_parent_recursively() {
        let mut collection = ScopeTree::default();
        collection.push_new();
        let scope1 = collection.get_current_mut();

        let sym = Symbol::new("foo", LpcType::String(false));
        scope1.unwrap().insert(sym);

        collection.push_new();

        if let Some(scope_ref) = collection.lookup("foo") {
            assert_eq!(scope_ref.type_, LpcType::String(false));
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
}
