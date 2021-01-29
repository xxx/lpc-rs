use crate::semantic::scope_tree::ScopeTree;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::program_node::ProgramNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::semantic::symbol::Symbol;
use crate::ast::function_def_node::FunctionDefNode;
use crate::ast::var_init_node::VarInitNode;
use crate::semantic::semantic_checks::check_var_redefinition;
use crate::semantic::semantic_error::var_redefinition_error;

/// A tree walker to handle populating all the scopes in the program
#[derive(Debug)]
pub struct ScopeWalker {
    /// Store the path to the original file, used for error messaging.
    filepath: String,

    /// Our collection of scopes
    pub scopes: ScopeTree
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, using the content at `filepath`
    /// as the file to check for error messaging.
    pub fn new(filepath: &str) -> Self {
        Self {
            filepath: String::from(filepath),
            scopes: ScopeTree::default()
        }
    }

    /// Insert a new symbol into the current scope
    fn insert_symbol(&mut self, symbol: Symbol) {
        if let Some(scope) = self.scopes.get_current_mut() {
            scope.insert(symbol)
        }
    }
}

impl TreeWalker for ScopeWalker {
    fn visit_program(&mut self, node: &ProgramNode) where Self: Sized {
        // Push the global scope
        self.scopes.push_new();

        for expr in &node.functions {
            expr.visit(self);
        }
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) where Self: Sized {
        node.l.visit(self);
        node.r.visit(self);
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) where Self: Sized {
        self.scopes.push_new();

        for parameter in &node.parameters {
            parameter.visit(self);
        }

        for expression in &node.body {
            expression.visit(self);
        }
    }

    fn visit_var_init(&mut self, node: &VarInitNode) where Self: Sized {
        if let Err(e) =
            check_var_redefinition(&node, &self.scopes.get_current().unwrap()) {
            var_redefinition_error(&self.filepath, &e);
            panic!();
        }

        self.insert_symbol(Symbol::from(node));
    }
}

impl Default for ScopeWalker {
    fn default() -> Self {
        // Push a default global scope.
        let mut scopes = ScopeTree::default();
        scopes.push_new();

        Self {
            filepath: String::new(),
            scopes
        }
    }
}

