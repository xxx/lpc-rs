use crate::{
    ast::{
        ast_node::ASTNodeTrait, binary_op_node::BinaryOpNode, function_def_node::FunctionDefNode,
        program_node::ProgramNode, var_init_node::VarInitNode, var_node::VarNode,
    },
    codegen::tree_walker::TreeWalker,
    errors::compiler_error::{undefined_var_error::UndefinedVarError, CompilerError},
    semantic::{
        function_prototype::FunctionPrototype, scope_tree::ScopeTree,
        semantic_checks::check_var_redefinition, symbol::Symbol,
    },
};
use std::collections::HashMap;

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
    errors: Vec<CompilerError>,
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, using the content at `filepath`
    /// as the file to check for error messaging.
    pub fn new(filepath: &str) -> Self {
        Self {
            filepath: String::from(filepath),
            scopes: ScopeTree::default(),
            function_prototypes: HashMap::new(),
            errors: vec![],
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
    fn get_errors(&self) -> Vec<CompilerError> {
        self.errors.to_vec()
    }

    fn visit_program(&mut self, node: &ProgramNode) -> Result<(), CompilerError> {
        // Push the global scope
        self.scopes.push_new();

        for expr in &node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    fn visit_binary_op(&mut self, node: &BinaryOpNode) -> Result<(), CompilerError> {
        node.l.visit(self)?;
        node.r.visit(self)?;

        Ok(())
    }

    fn visit_function_def(&mut self, node: &FunctionDefNode) -> Result<(), CompilerError> {
        let scope_id = self.scopes.push_new();
        self.scopes.insert_function(&node.name, &scope_id);

        let num_default_args = node.parameters.iter().filter(|p| p.value.is_some()).count();

        for parameter in &node.parameters {
            parameter.visit(self)?;
        }

        for expression in &node.body {
            expression.visit(self)?;
        }

        // Store the prototype now, to allow for forward references.
        let num_args = node.parameters.len();
        let arg_types = node
            .parameters
            .iter()
            .map(|parm| parm.type_)
            .collect::<Vec<_>>();
        self.function_prototypes.insert(
            node.name.clone(),
            FunctionPrototype {
                name: node.name.clone(),
                return_type: node.return_type,
                num_args,
                num_default_args,
                arg_types,
                span: node.span,
                arg_spans: {
                    node.parameters
                        .iter()
                        .flat_map(|n| n.span)
                        .collect::<Vec<_>>()
                },
            },
        );

        Ok(())
    }

    fn visit_var_init(&mut self, node: &VarInitNode) -> Result<(), CompilerError> {
        if let Err(e) = check_var_redefinition(&node, &self.scopes.get_current().unwrap()) {
            self.errors.push(CompilerError::VarRedefinitionError(e));
        }

        self.insert_symbol(Symbol::from(node));

        Ok(())
    }

    fn visit_var(&mut self, node: &VarNode) -> Result<(), CompilerError> {
        // We check for undefined vars here in case a symbol is subsequently defined.
        if self.scopes.lookup(&node.name).is_none() {
            self.errors
                .push(CompilerError::UndefinedVarError(UndefinedVarError {
                    name: node.name.clone(),
                    span: node.span,
                }));
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
            errors: vec![],
        }
    }
}
