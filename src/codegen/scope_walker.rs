use crate::{
    ast::{
        ast_node::ASTNodeTrait, function_def_node::FunctionDefNode, program_node::ProgramNode,
        var_init_node::VarInitNode, var_node::VarNode,
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

    fn visit_program(&mut self, node: &mut ProgramNode) -> Result<(), CompilerError> {
        // Push the global scope
        self.scopes.push_new();

        for expr in &mut node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<(), CompilerError> {
        let scope_id = self.scopes.push_new();
        self.scopes.insert_function(&node.name, &scope_id);

        let num_default_args = node.parameters.iter().filter(|p| p.value.is_some()).count();

        for parameter in &mut node.parameters {
            parameter.visit(self)?;
        }

        for expression in &mut node.body {
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

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<(), CompilerError> {
        if let Err(e) = check_var_redefinition(&node, &self.scopes.get_current().unwrap()) {
            self.errors.push(CompilerError::VarRedefinitionError(e));
        }

        if let Some(expr_node) = &mut node.value {
            expr_node.visit(self)?;
        }

        self.insert_symbol(Symbol::from(node));

        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode) -> Result<(), CompilerError> {
        let sym = self.scopes.lookup(&node.name);

        if let Some(symbol) = sym {
            if symbol.is_global() {
                node.set_global(true);
            }
        } else {
            // We check for undefined vars here in case a symbol is subsequently defined.
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

#[cfg(test)]
mod tests {
    use super::*;

    mod test_visit_function_def {
        use super::*;
        use crate::semantic::lpc_type::LPCType;

        #[test]
        fn test_stores_the_prototype() {
            let mut walker = ScopeWalker::default();
            let mut node = FunctionDefNode {
                return_type: LPCType::Mixed(false),
                name: "marf".to_string(),
                parameters: vec![
                    VarInitNode::new("foo", LPCType::Int(false)),
                    VarInitNode::new("bar", LPCType::Mapping(true)),
                ],
                body: vec![],
                span: None,
            };

            let _ = walker.visit_function_def(&mut node);

            if let Some(proto) = walker.function_prototypes.get("marf") {
                assert_eq!(
                    *proto,
                    FunctionPrototype {
                        name: "marf".to_string(),
                        return_type: LPCType::Mixed(false),
                        num_args: 2,
                        num_default_args: 0,
                        arg_types: vec![LPCType::Int(false), LPCType::Mapping(true)],
                        span: None,
                        arg_spans: vec![]
                    }
                )
            } else {
                panic!("prototype not found!")
            }
        }
    }

    mod test_visit_var_init {
        use super::*;
        use crate::semantic::lpc_type::LPCType;

        fn setup() -> (ScopeWalker, VarInitNode) {
            let mut walker = ScopeWalker::default();
            let node = VarInitNode {
                type_: LPCType::Int(false),
                name: "foo".to_string(),
                value: None,
                array: false,
                global: false,
                span: None,
            };

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LPCType::String(false),
                static_: false,
                location: None,
                scope_id: 0,
                span: None,
            });

            (walker, node)
        }

        #[test]
        fn test_sets_error_for_var_redefinition_in_same_scope() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var_init(&mut node);

            assert!(!walker.errors.is_empty());
        }

        #[test]
        fn test_does_not_error_for_var_shadow_in_different_scope() {
            let (mut walker, mut node) = setup();

            walker.scopes.push_new();

            let _ = walker.visit_var_init(&mut node);

            assert!(walker.errors.is_empty());
        }

        #[test]
        fn test_inserts_the_symbol() {
            let (mut walker, mut node) = setup();

            walker.scopes.push_new();

            let _ = walker.visit_var_init(&mut node);

            assert!(walker.scopes.get_current().unwrap().lookup("foo").is_some());
        }
    }

    mod test_visit_var {
        use super::*;
        use crate::semantic::lpc_type::LPCType;

        fn setup() -> (ScopeWalker, VarNode) {
            let walker = ScopeWalker::default();
            let node = VarNode {
                name: "foo".to_string(),
                global: false,
                span: None,
            };

            (walker, node)
        }

        #[test]
        fn test_sets_global_flag() {
            let (mut walker, mut node) = setup();

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LPCType::Int(false),
                static_: false,
                location: None,
                scope_id: 0, // denotes a global symbol
                span: None,
            });

            let _ = walker.visit_var(&mut node);

            assert!(node.global);
        }

        #[test]
        fn test_pushes_error_for_undefined_vars() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var(&mut node);

            assert!(!walker.errors.is_empty());
        }
    }
}
