use std::collections::HashMap;
use crate::semantic::scope_tree::ScopeTree;
use crate::codegen::tree_walker::TreeWalker;
use crate::ast::program_node::ProgramNode;
use crate::ast::binary_op_node::BinaryOpNode;
use crate::ast::ast_node::ASTNodeTrait;
use crate::semantic::symbol::Symbol;
use crate::ast::function_def_node::FunctionDefNode;
use crate::ast::var_init_node::VarInitNode;
use crate::semantic::semantic_checks::check_var_redefinition;
use crate::errors::LPCError;
use crate::semantic::function_prototype::FunctionPrototype;
use crate::ast::var_node::VarNode;
use crate::errors::undefined_var_error::UndefinedVarError;

/// A tree walker to handle populating all the scopes in the program
#[derive(Debug)]
pub struct ScopeWalker {
    /// Store the path to the original file, used for error messaging.
    filepath: String,

    /// Our collection of scopes
    pub scopes: ScopeTree,

    /// The map of function names, to their respective prototypes.
    /// Used for checking forward references.
    pub function_prototypes: HashMap<String, FunctionPrototype>,

    /// Collected errors
    errors: Vec<LPCError>
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, using the content at `filepath`
    /// as the file to check for error messaging.
    pub fn new(filepath: &str) -> Self {
        Self {
            filepath: String::from(filepath),
            scopes: ScopeTree::default(),
            function_prototypes: HashMap::new(),
            errors: vec![]
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
    fn get_errors(&self) -> Vec<LPCError> {
        self.errors.to_vec()
    }

    fn visit_program(&mut self, node: &ProgramNode) -> Result<(), LPCError> {
        // Push the global scope
        self.scopes.push_new();

        for expr in &node.functions {
            expr.visit(self)?;
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) -> Result<(), LPCError> {
        node.l.visit(self)?;
        node.r.visit(self)?;

        Ok(())
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) -> Result<(), LPCError> {
        self.scopes.push_new();

        for parameter in &node.parameters {
            parameter.visit(self)?;
        }

        for expression in &node.body {
            expression.visit(self)?;
        }

        // Store the prototype now, to allow for forward references.
        let num_args = node.parameters.len();
        let arg_types = node.parameters.iter().map(|parm| parm.type_ ).collect::<Vec<_>>();
        self.function_prototypes.insert(node.name.clone(), FunctionPrototype {
            name: node.name.clone(),
            return_type: node.return_type,
            num_args,
            arg_types,
            span: node.span,
            arg_spans: {
                node
                    .parameters
                    .iter()
                    .map(|n| n.span)
                    .collect::<Vec<_>>()
                    .into_iter()
                    .flatten()
                    .collect::<Vec<_>>()
            }
        });

        Ok(())
    }

    fn visit_var_init(&mut self, node: &VarInitNode) -> Result<(), LPCError> {
        if let Err(e) = check_var_redefinition(&node, &self.scopes.get_current().unwrap()) {
            self.errors.push(LPCError::VarRedefinitionError(e));
        }

        self.insert_symbol(Symbol::from(node));

        Ok(())
    }

    fn visit_var(&mut self, node: &VarNode) -> Result<(), LPCError> {
        // We check for undefined vars here in case a symbol is subsequently defined.
        if let None = self.scopes.lookup(&node.name) {
            self.errors.push(LPCError::UndefinedVarError(
                UndefinedVarError {
                    name: node.name.clone(),
                    span: node.span
                }
            ));
        }

        Ok(())
    }
}

impl Default for ScopeWalker {
    fn default() -> Self {
        // Push a default global scope.
        let mut scopes = ScopeTree::default();
        scopes.push_new();

        Self {
            filepath: String::new(),
            scopes,
            function_prototypes: HashMap::new(),
            errors: vec![]
        }
    }
}

