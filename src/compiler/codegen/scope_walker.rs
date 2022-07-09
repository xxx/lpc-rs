use crate::compiler::{
    ast::{
        ast_node::AstNodeTrait,
        block_node::BlockNode,
        do_while_node::DoWhileNode,
        for_each_node::{ForEachInit, ForEachNode, FOREACH_INDEX, FOREACH_LENGTH},
        for_node::ForNode,
        function_def_node::{FunctionDefNode, ARGV},
        if_node::IfNode,
        program_node::ProgramNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
        while_node::WhileNode,
    },
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
    semantic::{semantic_checks::check_var_redefinition, symbol::Symbol},
};
use lpc_rs_core::{
    call_namespace::CallNamespace, global_var_flags::GlobalVarFlags, lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, Result};

/// A tree walker to handle populating all the scopes in the program, as well as generating
/// errors for undefined and redefined variables.
#[derive(Debug)]
pub struct ScopeWalker {
    /// The compilation context
    context: CompilationContext,
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, with `context` as the context.
    pub fn new(context: CompilationContext) -> Self {
        Self { context }
    }

    /// Insert a new symbol into the current scope
    fn insert_symbol(&mut self, symbol: Symbol) {
        if let Some(scope) = self.context.scopes.current_mut() {
            scope.insert(symbol)
        }
    }
}

impl ContextHolder for ScopeWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for ScopeWalker {
    fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();

        node.scope_id = Some(scope_id);

        for stmt in &mut node.body {
            stmt.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let _ = node.body.visit(self);
        let _ = node.condition.visit(self);

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_for(&mut self, node: &mut ForNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        if let Some(n) = &mut *node.initializer {
            let _ = n.visit(self);
        }
        if let Some(n) = &mut node.condition {
            let _ = n.visit(self);
        }

        let _ = node.body.visit(self);

        if let Some(n) = &mut node.incrementer {
            let _ = n.visit(self);
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let sym = Symbol {
            name: FOREACH_INDEX.to_string(),
            type_: LpcType::Int(false),
            location: None,
            scope_id: scope_id.into(),
            span: node.span,
            flags: GlobalVarFlags::default(),
        };
        self.insert_symbol(sym);

        let sym = Symbol {
            name: FOREACH_LENGTH.to_string(),
            type_: LpcType::Int(false),
            location: None,
            scope_id: scope_id.into(),
            span: node.span,
            flags: GlobalVarFlags::default(),
        };
        self.insert_symbol(sym);

        match &mut node.initializer {
            ForEachInit::Array(ref mut init) => {
                let _ = init.visit(self);
            }
            ForEachInit::Mapping {
                ref mut key,
                ref mut value,
            } => {
                let _ = key.visit(self);
                let _ = value.visit(self);
            }
        }
        let _ = node.collection.visit(self);
        let _ = node.body.visit(self);

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        self.context.scopes.insert_function(&node.name, &scope_id);

        for parameter in &mut node.parameters {
            parameter.visit(self)?;
        }

        if node.flags.ellipsis() {
            let sym = Symbol {
                name: ARGV.to_string(),
                type_: LpcType::Mixed(true),
                location: None,
                scope_id: scope_id.into(),
                span: node.span,
                flags: GlobalVarFlags::default(),
            };

            self.insert_symbol(sym);
        }

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let _ = node.condition.visit(self);
        let _ = node.body.visit(self);
        if let Some(n) = &mut *node.else_clause {
            let _ = n.visit(self);
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        // Push the global scope
        self.context.scopes.push_new();

        for expr in &mut node.body {
            expr.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        let sym = self.context.lookup_var(&node.name);

        if sym.is_none() {
            // check for functions e.g. declaring function pointers with no arguments
            if self
                .context
                .contains_function_complete(&node.name, &CallNamespace::default())
            {
                node.set_function_name(true);
                return Ok(());
            }
        };

        let mut errors = vec![];

        if let Some(symbol) = sym {
            if !symbol.public() && self.context.scopes.lookup(&node.name).is_none() {
                let e = LpcError::new(format!(
                    "private variable `{}` accessed outside of its file",
                    node.name
                ))
                .with_span(node.span)
                .with_label("defined here", symbol.span);

                errors.push(e);
            }

            if symbol.is_global() {
                // Set the node to global, so we know whether to look at the program registers,
                // or the global registers, during codegen.
                node.set_global(true);
            }
        } else {
            let e =
                LpcError::new(format!("undefined variable `{}`", node.name)).with_span(node.span);

            // We check for undefined vars here in case a symbol is subsequently defined.
            errors.push(e);
        }

        self.context.errors.extend(errors.into_iter());

        Ok(())
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        let scope = self.context.scopes.current();

        if scope.is_none() {
            return Err(LpcError::new(
                "There's no current scope for some reason? This is a pretty bad compiler bug.",
            ));
        }

        if let Err(e) = check_var_redefinition(node, scope.unwrap()) {
            self.context.errors.push(e);
        }

        if let Some(expr_node) = &mut node.value {
            expr_node.visit(self)?;
        }

        self.insert_symbol(Symbol::from(node));

        Ok(())
    }

    fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let _ = node.condition.visit(self);
        let _ = node.body.visit(self);

        self.context.scopes.pop();
        Ok(())
    }
}

impl Default for ScopeWalker {
    fn default() -> Self {
        let mut context = CompilationContext::default();
        // Push a default global scope.
        context.scopes.push_new();

        Self { context }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert_regex;

    mod test_visit_function_def {
        use lpc_rs_core::lpc_type::LpcType;

        use super::*;
        use lpc_rs_core::function_flags::FunctionFlags;

        #[test]
        fn sets_up_argv_for_ellipsis() {
            let mut walker = ScopeWalker::default();
            let mut node = FunctionDefNode {
                return_type: LpcType::Void,
                name: "marf".to_string(),
                flags: FunctionFlags::default().with_ellipsis(true),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let _ = walker.visit_function_def(&mut node);

            walker.context.scopes.goto_function("marf").unwrap();

            let argv = walker
                .context
                .scopes
                .current()
                .unwrap()
                .lookup(ARGV)
                .unwrap();

            assert_eq!(argv.name, ARGV);
            assert_eq!(argv.type_, LpcType::Mixed(true));
        }
    }

    mod test_visit_var_init {
        use lpc_rs_core::lpc_type::LpcType;

        use super::*;

        fn setup() -> (ScopeWalker, VarInitNode) {
            let mut walker = ScopeWalker::default();
            let node = VarInitNode {
                type_: LpcType::Int(false),
                name: "foo".to_string(),
                value: None,
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LpcType::String(false),
                location: None,
                scope_id: 0,
                span: None,
                flags: GlobalVarFlags::default(),
            });

            (walker, node)
        }

        #[test]
        fn sets_error_for_var_redefinition_in_same_scope() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var_init(&mut node);

            assert!(!walker.context.errors.is_empty());
        }

        #[test]
        fn does_not_error_for_var_shadow_in_different_scope() {
            let (mut walker, mut node) = setup();

            walker.context.scopes.push_new();

            let _ = walker.visit_var_init(&mut node);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn inserts_the_symbol() {
            let (mut walker, mut node) = setup();

            walker.context.scopes.push_new();

            let _ = walker.visit_var_init(&mut node);

            assert!(walker
                .context
                .scopes
                .current()
                .unwrap()
                .lookup("foo")
                .is_some());
        }
    }

    mod test_visit_var {
        use crate::interpreter::program::Program;
        use lpc_rs_core::lpc_type::LpcType;

        use super::*;

        fn setup() -> (ScopeWalker, VarNode) {
            let walker = ScopeWalker::default();
            let node = VarNode {
                name: "foo".to_string(),
                span: None,
                global: false,
                function_name: false,
            };

            (walker, node)
        }

        #[test]
        fn sets_global_flag() {
            let (mut walker, mut node) = setup();

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                location: None,
                scope_id: 0, // denotes a global symbol
                span: None,
                flags: GlobalVarFlags::default(),
            });

            let _ = walker.visit_var(&mut node);

            assert!(node.global);
        }

        #[test]
        fn pushes_error_for_undefined_vars() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var(&mut node);

            assert_regex!(
                walker.context.errors[0].as_ref(),
                "undefined variable `foo`"
            );
        }

        #[test]
        fn errors_if_accessing_private_variable_defined_elsewhere() {
            let (mut walker, mut node) = setup();

            let mut inherited = Program::default();

            let sym = Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                location: None,
                scope_id: 0,
                span: None,
                flags: GlobalVarFlags::from(vec!["private"]),
            };

            inherited.global_variables.insert("foo".to_string(), sym);

            walker.context.inherits.push(inherited);

            let _ = walker.visit_var(&mut node);

            assert_regex!(
                walker.context.errors[0].as_ref(),
                "private variable `foo` accessed outside of its file"
            );
        }

        #[test]
        fn allows_accessing_in_file_private_variable() {
            let (mut walker, mut node) = setup();

            let sym = Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                location: None,
                scope_id: 0,
                span: None,
                flags: GlobalVarFlags::from(vec!["private"]),
            };

            walker.insert_symbol(sym);

            let _ = walker.visit_var(&mut node);

            assert!(walker.context.errors.is_empty());
        }
    }
}