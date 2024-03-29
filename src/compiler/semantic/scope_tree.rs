use std::collections::HashMap;

use indextree::{Arena, Node, NodeId};
use lpc_rs_errors::{lpc_bug, Result};
use lpc_rs_function_support::symbol::Symbol;

use crate::compiler::{compilation_context::CompilationContext, semantic::local_scope::LocalScope};

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
    /// use lpc_rs::compiler::semantic::scope_tree::ScopeTree;
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
    /// // etc.    ///
    pub fn push_new(&mut self) -> NodeId {
        let id = self.scopes.count();

        let scope = LocalScope {
            id: None,
            symbols: HashMap::new(),
        };

        let new_id = self.scopes.new_node(scope);

        // now that we have the scope's final node ID,
        // we need to set it on the scope itself
        {
            let mut_scope = self.scopes.get_mut(new_id).unwrap();
            mut_scope.get_mut().id = Some(new_id);
        }

        if let Some(parent) = self.current_id {
            parent.append(new_id, &mut self.scopes);
        }

        self.current_id = Some(new_id);

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
    pub fn current(&self) -> Option<&LocalScope> {
        match self.current_id {
            Some(x) => self.get(x),
            None => None,
        }
    }

    /// Get a mutable reference to the current scope
    pub fn current_mut(&mut self) -> Option<&mut LocalScope> {
        match self.current_id {
            Some(x) => self.get_mut(x),
            None => None,
        }
    }

    /// Get the node for the current scope, used for traversal.
    pub fn current_node(&self) -> Option<&Node<LocalScope>> {
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
    /// Note that this method does *not* actually remove the scope from the
    /// tree. It remains accessible.
    pub fn pop(&mut self) {
        self.current_id = match self.current_node() {
            Some(n) => n.parent(),
            None => None,
        };
    }

    /// Set the current node to the passed `Option<NodeId>`
    pub fn goto(&mut self, id: Option<NodeId>) {
        self.current_id = id;
    }

    /// Set the current node to the root of the tree.
    pub fn goto_root(&mut self) {
        self.current_id = self.root_id;
    }

    /// Set the current node to the scope for function named `name`.
    pub fn goto_function(&mut self, name: &str) -> Result<()> {
        if let Some(id) = self.function_scopes.get(name) {
            self.current_id = Some(*id);
            Ok(())
        } else {
            Err(lpc_bug!(
                    "Unknown function passed to goto_function `{}`. This likely indicates a driver bug.",
                    name
                )
            )
        }
    }

    /// Get a mutable [`LocalScope`] reference, by function name
    pub fn function_scope_mut(&mut self, name: &'_ str) -> Option<&mut LocalScope> {
        if let Some(id) = self.function_scopes.get(name) {
            let id = *id;
            self.get_mut(id)
        } else {
            None
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

impl From<CompilationContext> for ScopeTree {
    fn from(context: CompilationContext) -> Self {
        context.scopes
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_type::LpcType;

    use super::*;

    #[test]
    fn push_new() {
        let mut collection = ScopeTree::default();
        let scope1_id = collection.push_new();
        let scope2_id = collection.push_new();

        scope1_id.append(scope2_id, &mut collection.scopes);

        assert_eq!(
            collection.scopes.get(scope2_id).unwrap().parent(),
            Some(scope1_id)
        );
    }

    mod test_lookup {
        use super::*;

        #[test]
        fn lookup_finds_the_symbol() {
            let mut collection = ScopeTree::default();
            collection.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            collection.current_mut().unwrap().insert(sym);

            if let Some(scope_ref) = collection.lookup("foo") {
                assert_eq!(scope_ref.type_, LpcType::String(false));
            } else {
                panic!("symbol not found.");
            }
        }

        #[test]
        fn lookup_checks_parent_recursively() {
            let mut collection = ScopeTree::default();
            collection.push_new();
            let scope1 = collection.current_mut();

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
        fn lookup_returns_none_when_not_found() {
            let mut tree = ScopeTree::default();
            tree.push_new();

            let result = tree.lookup("asdf");
            assert_eq!(result, None);
        }
    }

    mod test_function_scope_mut {
        use super::*;

        #[test]
        fn returns_the_scope() {
            let mut tree = ScopeTree::default();
            tree.push_new();
            let id = tree.push_new();
            tree.push_new();

            tree.insert_function("foobar", &id);

            tree.goto(Some(id));

            let mut scope = tree.current_mut().unwrap().clone();

            tree.goto_root();

            let looked_up_scope = tree.function_scope_mut("foobar").unwrap();

            assert_eq!(&mut scope, looked_up_scope);
        }

        #[test]
        fn returns_none_when_not_found() {
            let mut tree = ScopeTree::default();
            assert!(tree.function_scope_mut("foobar").is_none());
        }
    }
}
