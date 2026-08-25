use std::collections::HashSet;

use async_trait::async_trait;
use if_chain::if_chain;
use itertools::Itertools;
use lpc_rs_core::{
    ScopeId, call_namespace::CallNamespace, global_var_flags::GlobalVarFlags, lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error, span::Span};
use lpc_rs_function_support::symbol::Symbol;
use tracing::trace;

use crate::compiler::{
    ast::{
        ast_node::AstNodeTrait,
        call_node::{CallChain, CallNode},
        closure_node::ClosureNode,
        for_each_node::{FOREACH_INDEX, FOREACH_LENGTH, ForEachNode},
        function_def_node::{ARGV, FunctionDefNode},
        program_node::ProgramNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
    },
    codegen::tree_walker::{ContextHolder, Pass, TreeWalker, walk_foreach},
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
    semantic::semantic_checks::check_var_redefinition,
};

/// A tree walker to handle populating all the scopes in the program, as well as
/// generating errors for undefined and redefined variables.
#[derive(Debug)]
pub struct ScopeWalker {
    /// The compilation context
    pub context: CompilationContext,

    /// track the scope IDs of each closure, to help determine if
    /// a variable needs to be upvalued or not.
    closure_scope_stack: Vec<ScopeId>,

    /// Every closure scope, for the capture layout at the end of the walk.
    closure_scopes: HashSet<ScopeId>,
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, with `context` as the context.
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            closure_scope_stack: vec![],
            closure_scopes: HashSet::new(),
        }
    }

    /// Insert a new symbol into the current scope
    fn insert_symbol(&mut self, symbol: Symbol) {
        if let Some(scope) = self.context.scopes.current_mut() {
            trace!("Inserting symbol {} into scope {}", symbol, scope);
            scope.insert(symbol)
        }
    }

    fn define_argv(&mut self, scope_id: ScopeId, span: Option<Span>) {
        let sym = Symbol {
            name: ARGV.to_string(),
            type_: LpcType::Mixed(true),
            location: None,
            scope_id: scope_id.into(),
            span,
            flags: GlobalVarFlags::default(),
            upvalue: false,
            by_ref: false,
        };

        self.insert_symbol(sym);
    }

    fn should_upvalue_symbol(&self, symbol: &Symbol) -> bool {
        if_chain! {
            if !symbol.is_global();
            if let Some(closure_scope_id) = self.closure_scope_stack.last().copied();
            if let Some(symbol_scope_id) = symbol.scope_id;
            if symbol_scope_id != closure_scope_id;
            let mut ancestors = symbol_scope_id.ancestors(&self.context.scopes.scopes);
            if !ancestors.contains(&closure_scope_id);
            then {
                true
            } else {
                false
            }
        }
    }
}

impl ContextHolder for ScopeWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for ScopeWalker {
    fn new(context: CompilationContext) -> Self {
        ScopeWalker::new(context)
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for ScopeWalker {
    fn enter_scope(&mut self, scope_id: &mut Option<ScopeId>) {
        *scope_id = Some(self.context.scopes.push_new());
    }

    fn exit_scope(&mut self) {
        self.context.scopes.pop();
    }

    async fn visit_call_root(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Root {
            receiver,
            name,
            namespace: _,
        } = &mut node.chain
        else {
            return Err(lpc_error!(
                node.span,
                "CallNode::chain was not a CallChain::Root"
            ));
        };

        if let Some(rcvr) = receiver {
            rcvr.visit(self).await?;
        }

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        if_chain! {
            if let Some(symbol) = self.context.lookup_var(&name);
            if symbol.type_.matches_type(LpcType::Function(false));
            if self.should_upvalue_symbol(symbol);
            then {
                trace!("upvaluing called function var {}", name);
                let symbol = self.context.lookup_var_mut(name).unwrap();
                symbol.upvalue = true;
            }
        }

        Ok(())
    }

    async fn visit_call_chain(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Node(chain_node) = &mut node.chain else {
            return Err(lpc_error!(
                node.span,
                "CallNode::chain was not a CallChain::Root"
            ));
        };

        chain_node.visit(self).await?;

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        Ok(())
    }

    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        let Some(scope_id) = node.scope_id else {
            return Err(lpc_bug!("closure scope was not entered before the visit"));
        };

        trace!("Defining closure {}", &node.name);

        self.closure_scope_stack.push(scope_id);
        self.closure_scopes.insert(scope_id);

        if let Some(parameters) = &mut node.parameters {
            for param in parameters {
                param.visit(self).await?;
            }
        }

        if node.flags.ellipsis() {
            self.define_argv(scope_id, node.span);
        }

        for statement in &mut node.body {
            statement.visit(self).await?;
        }

        self.closure_scope_stack.pop();

        Ok(())
    }

    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        let Some(scope_id) = node.scope_id else {
            return Err(lpc_bug!("foreach scope was not entered before the visit"));
        };

        let make_sym = |name: &str| Symbol {
            name: name.to_string(),
            type_: LpcType::Int(false),
            location: None,
            scope_id: scope_id.into(),
            span: node.span,
            flags: GlobalVarFlags::default(),
            upvalue: false,
            by_ref: false,
        };

        self.insert_symbol(make_sym(FOREACH_INDEX));
        self.insert_symbol(make_sym(FOREACH_LENGTH));

        walk_foreach(self, node).await
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        self.context.scopes.insert_function(&node.name, &scope_id);

        trace!("Defining function {}", &node.name);

        for parameter in &mut node.parameters {
            parameter.visit(self).await?;
        }

        if node.flags.ellipsis() {
            self.define_argv(scope_id, node.span);
        }

        for expression in &mut node.body {
            expression.visit(self).await?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        // Push the global scope
        self.context.scopes.push_new();

        for expr in &mut node.body {
            expr.visit(self).await?;
        }

        self.context.scopes.layout_upvalues(&self.closure_scopes)?;
        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        // positional closure arg references are
        // 1) always allowed (if we've made it this far)
        // 2) never global
        // 3) never upvalued
        // 4) will point to the same location regardless of what's in it.
        if node.is_closure_arg_var() {
            return Ok(());
        }

        let is_local = self.context.scopes.lookup(&node.name).is_some();

        let Some(symbol) = self.context.lookup_var(node.name) else {
            // check for functions (i.e. declaring function pointers with no arguments)
            if self
                .context
                .contains_function_complete(&node.name, &CallNamespace::default())
            {
                node.set_function_name(true);
                return Ok(());
            };

            // We check for undefined vars here, in case a symbol is subsequently defined.
            let e = lpc_error!(node.span, "undefined variable `{}`", node.name);

            self.context.diagnostics.record(e);

            return Ok(());
        };

        trace!("found symbol: {}", symbol);

        if !symbol.public() && !is_local {
            let e = LpcError::new(format!(
                "{} variable `{}` accessed outside of its file",
                symbol.flags.visibility(),
                node.name
            ))
            .with_span(node.span)
            .with_label("defined here", symbol.span);

            self.context.diagnostics.record(e);

            return Ok(());
        }

        if symbol.is_global() {
            // Set the node to global, so we know whether to look at the program registers,
            // or the global registers, during codegen.
            node.set_global(true);
        }

        // check for, and handle upvalues
        if self.should_upvalue_symbol(symbol) {
            trace!("upvaluing {}", &node.name);
            let symbol = self.context.lookup_var_mut(node.name).unwrap();
            symbol.upvalue = true;
        }

        Ok(())
    }

    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        let scope = self.context.scopes.current();

        if scope.is_none() {
            return Err(lpc_bug!(
                "There's no current scope for some reason? This is a pretty bad compiler bug.",
            ));
        }

        trace!("Defining variable {}", &node.name);

        if let Err(e) = check_var_redefinition(node, scope.unwrap()) {
            self.context.diagnostics.record(e);
        }

        // Inserted first, so a closure in the initializer can capture the variable.
        self.insert_symbol(Symbol::from(&mut *node));

        if let Some(expr_node) = &mut node.value {
            expr_node.visit(self).await?;
        }

        Ok(())
    }
}

impl Default for ScopeWalker {
    fn default() -> Self {
        let mut context = CompilationContext::default();
        // Push a default global scope.
        context.scopes.push_new();

        Self {
            context,
            closure_scope_stack: vec![],
            closure_scopes: HashSet::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use claims::assert_ok;
    use factori::create;
    use ustr::ustr;

    use super::*;
    use crate::{assert_regex, test_support::factories::*};

    mod test_visit_closure {
        use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};

        use super::*;

        #[tokio::test]
        async fn sets_up_argv_for_ellipsis() {
            let mut walker = ScopeWalker::default();
            let mut node = create!(
                ClosureNode,
                name: "marf".into(),
                flags: FunctionFlags::default().with_ellipsis(true),
            );

            walker.enter_scope(&mut node.scope_id);
            let _ = walker.visit_closure(&mut node).await;
            walker.exit_scope();

            walker.context.scopes.goto(node.scope_id);

            let argv = walker
                .context
                .scopes
                .current()
                .expect("where the scope?")
                .lookup(ARGV)
                .expect("where's argv?");

            assert_eq!(argv.name, ARGV);
            assert_eq!(argv.type_, LpcType::Mixed(true));
        }
    }

    mod test_visit_function_def {
        use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};
        use ustr::ustr;

        use super::*;

        #[tokio::test]
        async fn sets_up_argv_for_ellipsis() {
            let mut walker = ScopeWalker::default();
            let mut node = FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("marf"),
                flags: FunctionFlags::default().with_ellipsis(true),
                parameters: vec![],
                body: vec![],
                span: None,
            };

            let _ = walker.visit_function_def(&mut node).await;

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
        use ustr::ustr;

        use super::*;

        fn setup() -> (ScopeWalker, VarInitNode) {
            let mut walker = ScopeWalker::default();
            let node = VarInitNode {
                type_: LpcType::Int(false),
                name: ustr("foo"),
                value: None,
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LpcType::String(false),
                location: None,
                scope_id: None,
                span: None,
                flags: GlobalVarFlags::default(),
                upvalue: false,
                by_ref: false,
            });

            (walker, node)
        }

        #[tokio::test]
        async fn sets_error_for_var_redefinition_in_same_scope() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var_init(&mut node).await;

            assert!(!walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn does_not_error_for_var_shadow_in_different_scope() {
            let (mut walker, mut node) = setup();

            walker.context.scopes.push_new();

            let _ = walker.visit_var_init(&mut node).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn inserts_the_symbol() {
            let (mut walker, mut node) = setup();

            walker.context.scopes.push_new();

            let _ = walker.visit_var_init(&mut node).await;

            assert!(
                walker
                    .context
                    .scopes
                    .current()
                    .unwrap()
                    .lookup("foo")
                    .is_some()
            );
        }
    }

    mod test_visit_var {
        use lpc_rs_core::lpc_type::LpcType;

        use super::*;
        use crate::interpreter::program::Program;

        fn setup() -> (ScopeWalker, VarNode) {
            let walker = ScopeWalker::default();
            let node = VarNode {
                name: ustr("foo"),
                span: None,
                global: false,
                function_name: false,
            };

            (walker, node)
        }

        #[tokio::test]
        async fn sets_global_flag() {
            let (mut walker, mut node) = setup();

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                location: None,
                scope_id: None, // denotes a global symbol
                span: None,
                flags: GlobalVarFlags::default(),
                upvalue: false,
                by_ref: false,
            });

            let _ = walker.visit_var(&mut node).await;

            assert!(node.global);
        }

        #[tokio::test]
        async fn pushes_error_for_undefined_vars() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var(&mut node).await;

            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "undefined variable `foo`"
            );
        }

        #[tokio::test]
        async fn allows_closure_positional_arg_vars() {
            let mut walker = ScopeWalker::default();
            let mut node = create!(VarNode, name: ustr("$7"));

            let result = walker.visit_var(&mut node).await;

            assert_ok!(result);
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn errors_if_accessing_private_variable_defined_elsewhere() {
            let (mut walker, mut node) = setup();

            let mut inherited = Program::default();

            let sym = Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                location: None,
                scope_id: None,
                span: None,
                flags: GlobalVarFlags::from(vec!["private"]),
                upvalue: false,
                by_ref: false,
            };

            inherited.global_variables.insert("foo".to_string(), sym);

            walker.context.inherits.push(inherited);

            let _ = walker.visit_var(&mut node).await;

            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "private variable `foo` accessed outside of its file"
            );
        }

        #[tokio::test]
        async fn allows_accessing_in_file_private_variable() {
            let (mut walker, mut node) = setup();

            let sym = Symbol {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                location: None,
                scope_id: None,
                span: None,
                flags: GlobalVarFlags::from(vec!["private"]),
                upvalue: false,
                by_ref: false,
            };

            walker.insert_symbol(sym);

            let _ = walker.visit_var(&mut node).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn upvalues_variables() {
            let mut walker = ScopeWalker::default();
            let _local_scope = walker.context.scopes.push_new(); // push a non-global scope

            let mut node = create!(VarNode, name: ustr("foo"));

            let symbol_factory = SymbolFactory::new();
            let sym = symbol_factory.build(|sym| {
                sym.name = "foo".to_string();
                sym.type_ = LpcType::Int(false);
                sym.upvalue = false;
            });

            walker.insert_symbol(sym);

            let new_scope_id = walker.context.scopes.push_new();
            walker.closure_scope_stack.push(new_scope_id);

            let _ = walker.visit_var(&mut node).await;

            assert!(walker.context.diagnostics.errors().is_empty());

            let v = walker.context.lookup_var("foo").unwrap();
            assert!(v.upvalue);
        }
    }
}
