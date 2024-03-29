use async_trait::async_trait;
use if_chain::if_chain;
use itertools::Itertools;
use lpc_rs_core::{
    call_namespace::CallNamespace, global_var_flags::GlobalVarFlags, lpc_type::LpcType, ScopeId,
};
use lpc_rs_errors::{lpc_bug, lpc_error, span::Span, LpcError, Result};
use lpc_rs_function_support::symbol::Symbol;
use tracing::trace;

use crate::compiler::{
    ast::{
        ast_node::AstNodeTrait,
        block_node::BlockNode,
        call_node::{CallChain, CallNode},
        closure_node::ClosureNode,
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
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, with `context` as the context.
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            closure_scope_stack: vec![],
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

    async fn visit_call_root(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Root { receiver, name, namespace: _ } = &mut node.chain else {
            return Err(lpc_error!(node.span, "CallNode::chain was not a CallChain::Root"));
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
                let mut symbol = self.context.lookup_var_mut(name).unwrap();
                symbol.upvalue = true;
            }
        }

        Ok(())
    }

    async fn visit_call_chain(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Node(chain_node) = &mut node.chain else {
            return Err(lpc_error!(node.span, "CallNode::chain was not a CallChain::Root"));
        };

        chain_node.visit(self).await?;

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        Ok(())
    }
}

impl ContextHolder for ScopeWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

#[async_trait]
impl TreeWalker for ScopeWalker {
    async fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();

        node.scope_id = Some(scope_id);

        for stmt in &mut node.body {
            stmt.visit(self).await?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        match &node.chain {
            CallChain::Root { .. } => self.visit_call_root(node).await,
            CallChain::Node(_) => self.visit_call_chain(node).await,
        }
    }

    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        trace!("Defining closure {}", &node.name);

        self.closure_scope_stack.push(scope_id);

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

        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let _ = node.body.visit(self).await;
        let _ = node.condition.visit(self).await;

        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        if let Some(n) = &mut *node.initializer {
            let _ = n.visit(self).await;
        }
        if let Some(n) = &mut node.condition {
            let _ = n.visit(self).await;
        }

        let _ = node.body.visit(self).await;

        if let Some(n) = &mut node.incrementer {
            let _ = n.visit(self).await;
        }

        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let make_sym = |name: &str| Symbol {
            name: name.to_string(),
            type_: LpcType::Int(false),
            location: None,
            scope_id: scope_id.into(),
            span: node.span,
            flags: GlobalVarFlags::default(),
            upvalue: false,
        };

        self.insert_symbol(make_sym(FOREACH_INDEX));
        self.insert_symbol(make_sym(FOREACH_LENGTH));

        match &mut node.initializer {
            ForEachInit::Array(ref mut init) | ForEachInit::String(ref mut init) => {
                let _ = init.visit(self).await;
            }
            ForEachInit::Mapping {
                ref mut key,
                ref mut value,
            } => {
                let _ = key.visit(self).await;
                let _ = value.visit(self).await;
            }
        }
        let _ = node.collection.visit(self).await;
        let _ = node.body.visit(self).await;

        self.context.scopes.pop();
        Ok(())
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

    async fn visit_if(&mut self, node: &mut IfNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let _ = node.condition.visit(self).await;
        let _ = node.body.visit(self).await;
        if let Some(n) = &mut *node.else_clause {
            let _ = n.visit(self).await;
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

            self.context.errors.push(e);

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
            .with_label("defined here", symbol.span)
            .into();

            self.context.errors.push(e);

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
            let mut symbol = self.context.lookup_var_mut(node.name).unwrap();
            // *any* capture requires the symbol to be upvalued
            symbol.upvalue = true;
            // we also mark this specific reference as a non-local capture
            node.external_capture = true;
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
            self.context.errors.push(e);
        }

        if let Some(expr_node) = &mut node.value {
            expr_node.visit(self).await?;
        }

        self.insert_symbol(Symbol::from(node));

        Ok(())
    }

    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        node.scope_id = Some(scope_id);

        let _ = node.condition.visit(self).await;
        let _ = node.body.visit(self).await;

        self.context.scopes.pop();
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

            let _ = walker.visit_closure(&mut node).await;

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
            };

            walker.insert_symbol(Symbol {
                name: "foo".to_string(),
                type_: LpcType::String(false),
                location: None,
                scope_id: None,
                span: None,
                flags: GlobalVarFlags::default(),
                upvalue: false,
            });

            (walker, node)
        }

        #[tokio::test]
        async fn sets_error_for_var_redefinition_in_same_scope() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var_init(&mut node).await;

            assert!(!walker.context.errors.is_empty());
        }

        #[tokio::test]
        async fn does_not_error_for_var_shadow_in_different_scope() {
            let (mut walker, mut node) = setup();

            walker.context.scopes.push_new();

            let _ = walker.visit_var_init(&mut node).await;

            assert!(walker.context.errors.is_empty());
        }

        #[tokio::test]
        async fn inserts_the_symbol() {
            let (mut walker, mut node) = setup();

            walker.context.scopes.push_new();

            let _ = walker.visit_var_init(&mut node).await;

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
                external_capture: false,
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
            });

            let _ = walker.visit_var(&mut node).await;

            assert!(node.global);
        }

        #[tokio::test]
        async fn pushes_error_for_undefined_vars() {
            let (mut walker, mut node) = setup();

            let _ = walker.visit_var(&mut node).await;

            assert_regex!(
                (*walker.context.errors[0]).as_ref(),
                "undefined variable `foo`"
            );
        }

        #[tokio::test]
        async fn allows_closure_positional_arg_vars() {
            let mut walker = ScopeWalker::default();
            let mut node = create!(VarNode, name: ustr("$7"));

            let result = walker.visit_var(&mut node).await;

            assert_ok!(result);
            assert!(walker.context.errors.is_empty());
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
            };

            inherited.global_variables.insert("foo".to_string(), sym);

            walker.context.inherits.push(inherited);

            let _ = walker.visit_var(&mut node).await;

            assert_regex!(
                (*walker.context.errors[0]).as_ref(),
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
            };

            walker.insert_symbol(sym);

            let _ = walker.visit_var(&mut node).await;

            assert!(walker.context.errors.is_empty());
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

            assert!(walker.context.errors.is_empty());

            let v = walker.context.lookup_var("foo").unwrap();
            assert!(v.upvalue);
            assert!(node.external_capture);
        }
    }
}
