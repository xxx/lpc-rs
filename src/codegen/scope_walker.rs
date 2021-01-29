use crate::semantic::scope_collection::ScopeCollection;
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
    pub scopes: ScopeCollection
}

impl ScopeWalker {
    pub fn new() -> Self {
        Self {
            scopes: ScopeCollection::default()
        }
    }

    fn insert_symbol(&mut self, symbol: Symbol) {
        if let Some(scope) = self.scopes.get_current_mut() {
            scope.insert(symbol)
        }
    }

    fn lookup_symbol(&self, name: &str) -> Option<&Symbol> {
        if let Some(scope) = self.scopes.get_current() {
            scope.lookup(name)
        } else {
            None
        }
    }

    fn lookup_symbol_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        if let Some(scope) = self.scopes.get_current_mut() {
            scope.lookup_mut(name)
        } else {
            None
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
        // if let Err(e) = check_var_redefinition(&node, &self.scopes.last().unwrap()) {
        //     var_redefinition_error(&self.filepath, &e);
        //     panic!();
        // }

        self.insert_symbol(Symbol::from(node));
    }
}

impl Default for ScopeWalker {
    fn default() -> Self {
        // Push a default global scope.
        let mut scopes = ScopeCollection::default();
        scopes.push_new();

        Self {
            scopes
        }
    }
}

