use std::collections::HashSet;

use async_trait::async_trait;
use if_chain::if_chain;
use itertools::Itertools;
use lpc_rs_core::{
    ScopeId, call_namespace::CallNamespace, global_var_flags::GlobalVarFlags, lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error, lpc_warning, span::Span};
use lpc_rs_function_support::symbol::Symbol;
use tracing::trace;
use ustr::Ustr;

use crate::compiler::{
    ast::{
        ast_node::AstNodeTrait,
        call_node::{CallChain, CallNode},
        closure_node::ClosureNode,
        expression_node::ExpressionNode,
        for_each_node::{FOREACH_INDEX, FOREACH_LENGTH, ForEachInit, ForEachNode},
        function_def_node::{ARGV, FunctionDefNode},
        program_node::ProgramNode,
        ref_node::RefNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
    },
    codegen::tree_walker::{ContextHolder, Pass, TreeWalker, walk_foreach},
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
    semantic::semantic_checks::check_var_redefinition,
};
use crate::interpreter::program::Program;

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

    /// Every local declared in the program, in declaration order.
    locals: Vec<DeclaredLocal>,

    /// The `(scope, name)` of every variable a reference resolved to.
    referenced: HashSet<(ScopeId, Ustr)>,

    /// Names already reported as declared by more than one parent.
    ambiguous: HashSet<Ustr>,
}

/// A local variable's identity and declaration site.
#[derive(Debug)]
struct DeclaredLocal {
    scope_id: ScopeId,
    name: Ustr,
    span: Option<Span>,
}

/// The programs' names for a message: "`/a.c`, `/b.c` and `/c.c`".
fn file_list(declarations: &[(&Program, &Symbol)]) -> String {
    let names: Vec<String> = declarations
        .iter()
        .map(|(program, _)| format!("`{}`", program.filename))
        .collect();
    match names.split_last() {
        None => String::new(),
        Some((last, [])) => last.clone(),
        Some((last, rest)) => format!("{} and {}", rest.join(", "), last),
    }
}

/// Each distinct declaration's site, keyed by slot — a diamond's two parents
/// carry the one declaration under two source-map ids.
fn declaration_sites(declarations: &[(&Program, &Symbol)]) -> Vec<Option<Span>> {
    declarations
        .iter()
        .unique_by(|(_, symbol)| symbol.location)
        .map(|(_, symbol)| symbol.span)
        .collect()
}

impl ScopeWalker {
    /// Create a new `ScopeWalker`, with `context` as the context.
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            closure_scope_stack: vec![],
            closure_scopes: HashSet::new(),
            locals: vec![],
            referenced: HashSet::new(),
            ambiguous: HashSet::new(),
        }
    }

    /// The visible declarations of `name` among the inherited programs, in
    /// inherit order.
    fn inherited_declarations(&self, name: &str) -> Vec<(&Program, &Symbol)> {
        self.context
            .inherits
            .iter()
            .filter_map(|program| {
                program
                    .global_variables
                    .get(name)
                    .filter(|symbol| symbol.public())
                    .map(|symbol| (program, symbol))
            })
            .collect()
    }

    /// The warning for a declaration of `node.name` when an enclosing scope,
    /// this file's globals, or a parent already declares that name.
    fn shadow_warning(&self, node: &VarInitNode) -> Option<LpcError> {
        if node.name.starts_with('$') {
            return None;
        }
        if let Some(shadowed) = self.context.scopes.lookup(&node.name) {
            let kind = if shadowed.is_global() {
                "a global"
            } else {
                "an outer"
            };
            return Some(
                lpc_warning!(node.span, "`{}` shadows {} variable", node.name, kind)
                    .with_label("shadowed declaration here", shadowed.span),
            );
        }
        let declarations = self.inherited_declarations(&node.name);
        if declarations.is_empty() {
            return None;
        }
        let mut warning = lpc_warning!(
            node.span,
            "`{}` shadows a global inherited from {}",
            node.name,
            file_list(&declarations)
        );
        for site in declaration_sites(&declarations) {
            warning = warning.with_label("shadowed declaration here", site);
        }
        if node.global {
            warning = warning.with_note(format!(
                "inherited functions keep their own `{}`",
                node.name
            ));
        }
        Some(warning)
    }

    /// Warn, once per name, when a reference to `name` outside this file's
    /// own declarations could mean more than one parent's global.
    fn check_ambiguity(&mut self, name: Ustr, span: Option<Span>) {
        if self.ambiguous.contains(&name) || self.context.scopes.lookup(&name).is_some() {
            return;
        }
        let declarations = self.inherited_declarations(&name);
        let sites = declaration_sites(&declarations);
        if sites.len() < 2 {
            return;
        }
        let Some((used, _)) = declarations.last() else {
            return;
        };
        let mut warning = lpc_warning!(
            span,
            "`{}` is declared by {}; `{}`'s is used",
            name,
            file_list(&declarations),
            used.filename
        );
        for site in sites {
            warning = warning.with_label("declared here", site);
        }
        self.ambiguous.insert(name);
        self.context.diagnostics.record(warning);
    }

    /// Record that a reference to `name` resolved to `symbol`.
    fn note_reference(referenced: &mut HashSet<(ScopeId, Ustr)>, symbol: &Symbol, name: Ustr) {
        if let Some(scope_id) = symbol.scope_id {
            referenced.insert((scope_id, name));
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

    /// Promote a local's symbol into a cell for its whole life, the way a
    /// captured local is upvalued; globals are left alone.
    fn mark_cell(&mut self, name: Ustr) {
        if let Some(symbol) = self.context.lookup_var_mut(name)
            && !symbol.is_global()
        {
            symbol.upvalue = true;
        }
    }

    /// The "accessed outside of its file" diagnostic for `name`'s `symbol`,
    /// or `None` when it's public or defined in the current file.
    fn visibility_error(
        name: Ustr,
        symbol: &Symbol,
        is_local: bool,
        span: Option<Span>,
    ) -> Option<LpcError> {
        if symbol.public() || is_local {
            return None;
        }

        Some(
            LpcError::new(format!(
                "{} variable `{}` accessed outside of its file",
                symbol.flags.visibility(),
                name
            ))
            .with_span(span)
            .with_label("defined here", symbol.span),
        )
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
            namespace,
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

        // An implicit efun lvalue (e.g. `sscanf`'s trailing variables) promotes
        // its bare-variable argument the same way an explicit `ref` does.
        if receiver.is_none() {
            let mut to_mark: Vec<Ustr> = Vec::new();
            if let Some(function_like) = self.context.lookup_function_complete(*name, namespace) {
                let prototype = function_like.as_ref();
                for (index, argument) in node.arguments.iter().enumerate() {
                    if prototype.is_ref_param(index)
                        && let ExpressionNode::Var(var) = argument
                    {
                        to_mark.push(var.name);
                    }
                }
            }
            for var_name in to_mark {
                self.mark_cell(var_name);
            }
        }

        if let Some(symbol) = self.context.lookup_var(&name)
            && symbol.type_.matches_type(LpcType::Function(false))
        {
            Self::note_reference(&mut self.referenced, symbol, *name);
            let upvalue = self.should_upvalue_symbol(symbol);
            self.check_ambiguity(*name, node.span);
            if upvalue {
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
                self.referenced.insert((scope_id, param.name));
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

        match &node.initializer {
            ForEachInit::Array(init) | ForEachInit::String(init) => {
                self.referenced.insert((scope_id, init.name));
            }
            ForEachInit::Mapping { key, value } => {
                self.referenced.insert((scope_id, key.name));
                self.referenced.insert((scope_id, value.name));
            }
        }

        walk_foreach(self, node).await
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        let scope_id = self.context.scopes.push_new();
        self.context.scopes.insert_function(&node.name, &scope_id);

        trace!("Defining function {}", &node.name);

        for parameter in &mut node.parameters {
            parameter.visit(self).await?;
            self.referenced.insert((scope_id, parameter.name));
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

        for local in &self.locals {
            if !self.referenced.contains(&(local.scope_id, local.name)) {
                let w = lpc_warning!(local.span, "unused variable `{}`", local.name);
                self.context.diagnostics.record(w);
            }
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
        self.check_ambiguity(node.name, node.span);

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
        Self::note_reference(&mut self.referenced, symbol, node.name);

        if let Some(e) = Self::visibility_error(node.name, symbol, is_local, node.span) {
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

    async fn visit_ref(&mut self, node: &mut RefNode) -> Result<()> {
        let is_local = self.context.scopes.lookup(&node.name).is_some();
        self.check_ambiguity(node.name, node.span);

        let Some(symbol) = self.context.lookup_var(node.name) else {
            let e = lpc_error!(node.span, "undefined variable `{}`", node.name);
            self.context.diagnostics.record(e);
            return Ok(());
        };
        Self::note_reference(&mut self.referenced, symbol, node.name);

        if let Some(e) = Self::visibility_error(node.name, symbol, is_local, node.span) {
            self.context.diagnostics.record(e);
            return Ok(());
        }

        node.set_global(symbol.is_global());

        // The callee aliases the variable, so it lives in a cell for its
        // whole life, as a captured local does.
        self.mark_cell(node.name);

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

        if !node.global
            && let Some(scope_id) = scope.and_then(|s| s.id)
        {
            self.locals.push(DeclaredLocal {
                scope_id,
                name: node.name,
                span: node.span,
            });
        }

        match check_var_redefinition(node, scope.unwrap()) {
            Err(e) => self.context.diagnostics.record(e),
            Ok(()) => {
                if let Some(w) = self.shadow_warning(node) {
                    self.context.diagnostics.record(w);
                }
            }
        }

        // Inserted first, so a closure in the initializer can capture the variable.
        let mut symbol = Symbol::from(&mut *node);
        symbol.by_ref = node.by_ref;
        if node.by_ref {
            // A `ref` parameter's cell is the caller's variable, for its
            // whole life, like a captured local.
            symbol.upvalue = true;
        }
        self.insert_symbol(symbol);

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

        Self::new(context)
    }
}

#[cfg(test)]
mod tests {
    use claims::assert_ok;
    use factori::create;
    use ustr::ustr;

    use super::*;
    use crate::{assert_regex, test_support::factories::*};

    mod references {
        use lpc_rs_core::register::{Register, RegisterVariant};

        use super::*;
        use crate::test_support::CompileThrough;

        async fn walk(code: &str) -> ScopeWalker {
            ScopeWalker::compile_through(code).await.unwrap()
        }

        #[tokio::test]
        async fn a_ref_argument_promotes_the_local_to_a_cell() {
            let walker =
                walk("void inc(int ref x) { x++; } void f() { int y = 1; inc(ref y); }").await;
            let scope = walker.context.scopes.function_scope("f").unwrap();
            let y = scope.lookup("y").unwrap();
            assert!(y.upvalue);
            assert!(!y.by_ref);
            assert!(matches!(y.location, Some(RegisterVariant::Upvalue(_))));
        }

        #[tokio::test]
        async fn a_ref_parameter_is_a_by_ref_cell() {
            let walker = walk("void inc(int ref x) { x++; }").await;
            let scope = walker.context.scopes.function_scope("inc").unwrap();
            let x = scope.lookup("x").unwrap();
            assert!(x.by_ref);
            assert!(x.upvalue);
            assert_eq!(x.location, Some(Register(0).as_upvalue()));
        }

        #[tokio::test]
        async fn a_ref_of_a_global_stays_global() {
            let walker = walk("int g; void inc(int ref x) { x++; } void f() { inc(ref g); }").await;
            let g = walker.context.scopes.lookup_global("g").unwrap();
            assert!(!g.upvalue);
        }

        #[tokio::test]
        async fn a_ref_of_a_private_inherited_global_is_rejected() {
            // `parent.c` declares `private int priv`, so `f` here is a
            // different file from where `priv` is defined.
            let walker = walk(
                "inherit \"./parent\"; void inc(int ref x) { x++; } void f() { inc(ref priv); }",
            )
            .await;
            let errors: Vec<_> = walker
                .context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| !e.is_warning())
                .collect();
            assert_regex!(
                errors[0].message(),
                "private variable `priv` accessed outside of its file"
            );
        }

        #[tokio::test]
        async fn an_implicit_efun_lvalue_promotes_the_local_to_a_cell() {
            let walker = walk(r#"void f() { int n; sscanf("1", "%d", n); }"#).await;
            let scope = walker.context.scopes.function_scope("f").unwrap();
            let n = scope.lookup("n").unwrap();
            assert!(n.upvalue);
        }
    }

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

            assert!(walker.context.diagnostics.is_clean());
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

    mod warnings {
        use indoc::indoc;

        use super::*;
        use crate::test_support::CompileThrough;

        async fn warnings(code: &str) -> Vec<String> {
            let walker = ScopeWalker::compile_through(code).await.unwrap();
            walker
                .context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| e.is_warning())
                .map(|e| e.to_string())
                .collect()
        }

        #[tokio::test]
        async fn an_unused_local_warns_in_declaration_order() {
            let code = indoc! { r#"
                void f() {
                    int a;
                    int b = 1;
                    { string c; }
                    function g = (: int d; 1 :);
                }
            "# };
            assert_eq!(
                warnings(code).await,
                [
                    "unused variable `a`",
                    "unused variable `b`",
                    "unused variable `c`",
                    "unused variable `g`",
                    "unused variable `d`",
                ]
            );
        }

        #[tokio::test]
        async fn any_reference_counts() {
            let code = indoc! { r#"
                void inc(int ref n) { n++; }
                mixed f() {
                    int read;
                    int assigned;
                    int reffed;
                    function called;
                    int captured;
                    if (read) assigned = 1;
                    inc(ref reffed);
                    called();
                    return (: captured :);
                }
            "# };
            let w = warnings(code).await;
            assert!(w.is_empty(), "{w:?}");
        }

        #[tokio::test]
        async fn parameters_globals_and_loop_variables_are_exempt() {
            let code = indoc! { r#"
                int g;
                void f(int p) {
                    foreach (i: ({ 1 })) {}
                    foreach (k, v: ([ ])) {}
                }
                void h() { function c = (: $1 :); c(); }
            "# };
            let w = warnings(code).await;
            assert!(w.is_empty(), "{w:?}");
        }

        #[tokio::test]
        async fn a_local_that_shadows_warns_with_the_shadowed_site() {
            let code = indoc! { r#"
                int name;
                void set_name(string name) { name = name; }
                void f() { int x = 1; x++; { int x = 2; x++; } }
            "# };
            let walker = ScopeWalker::compile_through(code).await.unwrap();
            let rendered: Vec<_> = walker
                .context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| e.is_warning())
                .map(|e| e.diagnostic_string())
                .collect();
            assert_eq!(rendered.len(), 2, "{rendered:?}");
            assert!(
                rendered[0].contains("`name` shadows a global variable"),
                "{}",
                rendered[0]
            );
            assert!(
                rendered[0].contains("shadowed declaration here"),
                "{}",
                rendered[0]
            );
            assert!(
                rendered[1].contains("`x` shadows an outer variable"),
                "{}",
                rendered[1]
            );
        }

        #[tokio::test]
        async fn an_inherited_global_is_shadowed_only_when_visible() {
            let code = r#"inherit "/parent"; void f(int b, int priv) { b++; priv++; }"#;
            assert_eq!(
                warnings(code).await,
                ["`b` shadows a global inherited from `/parent.c`"]
            );
        }

        async fn rendered(code: &str) -> Vec<String> {
            let walker = ScopeWalker::compile_through(code).await.unwrap();
            walker
                .context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| e.is_warning())
                .map(|e| e.diagnostic_string())
                .collect()
        }

        #[tokio::test]
        async fn a_global_that_shadows_an_inherited_global_names_the_parent() {
            let code = r#"inherit "/parent"; int b;"#;
            assert_eq!(
                warnings(code).await,
                ["`b` shadows a global inherited from `/parent.c`"]
            );
            let rendered = rendered(code).await;
            assert!(
                rendered[0].contains("shadowed declaration here"),
                "{}",
                rendered[0]
            );
            assert!(rendered[0].contains("/parent.c:3:"), "{}", rendered[0]);
            assert!(
                rendered[0].contains("inherited functions keep their own `b`"),
                "{}",
                rendered[0]
            );
        }

        #[tokio::test]
        async fn a_private_inherited_global_is_not_shadowed() {
            let code = r#"inherit "/parent"; int priv;"#;
            assert!(warnings(code).await.is_empty());
        }

        #[tokio::test]
        async fn a_later_private_does_not_hide_an_earlier_visible_global() {
            for code in [
                r#"inherit "/visible"; inherit "/hidden"; int f() { return shared; }"#,
                r#"inherit "/hidden"; inherit "/visible"; int f() { return shared; }"#,
            ] {
                let walker = ScopeWalker::compile_through(code).await.unwrap();
                let messages: Vec<_> = walker
                    .context
                    .diagnostics
                    .errors()
                    .iter()
                    .map(|e| e.to_string())
                    .collect();
                assert!(messages.is_empty(), "{code}: {messages:?}");
            }
        }

        #[tokio::test]
        async fn a_grandparents_global_is_reached_through_the_parent() {
            let code = r#"inherit "/parent"; int a;"#;
            let rendered = rendered(code).await;
            assert_eq!(rendered.len(), 1, "{rendered:?}");
            assert!(
                rendered[0].contains("`a` shadows a global inherited from `/parent.c`"),
                "{}",
                rendered[0]
            );
            assert!(rendered[0].contains("/grandparent.c:1:"), "{}", rendered[0]);
        }

        #[tokio::test]
        async fn every_parent_declaring_the_name_is_labeled() {
            let code = r#"inherit "/twin_a"; inherit "/twin_b"; int twin;"#;
            let rendered = rendered(code).await;
            assert_eq!(rendered.len(), 1, "{rendered:?}");
            assert!(
                rendered[0]
                    .contains("`twin` shadows a global inherited from `/twin_a.c` and `/twin_b.c`"),
                "{}",
                rendered[0]
            );
            assert_eq!(
                rendered[0].matches("shadowed declaration here").count(),
                2,
                "{}",
                rendered[0]
            );
        }

        #[tokio::test]
        async fn a_diamond_labels_the_shared_declaration_once() {
            let code = r#"inherit "/diamond_left"; inherit "/diamond_right"; int a;"#;
            let rendered = rendered(code).await;
            assert_eq!(rendered.len(), 1, "{rendered:?}");
            assert!(
                rendered[0].contains("inherited from `/diamond_left.c` and `/diamond_right.c`"),
                "{}",
                rendered[0]
            );
            assert_eq!(
                rendered[0].matches("shadowed declaration here").count(),
                1,
                "{}",
                rendered[0]
            );
        }

        #[tokio::test]
        async fn a_name_two_parents_declare_warns_once_at_its_first_reference() {
            let code = r#"inherit "/twin_a"; inherit "/twin_b"; int f() { twin++; return twin; }"#;
            let rendered = rendered(code).await;
            assert_eq!(rendered.len(), 1, "{rendered:?}");
            assert!(
                rendered[0].contains(
                    "`twin` is declared by `/twin_a.c` and `/twin_b.c`; `/twin_b.c`'s is used"
                ),
                "{}",
                rendered[0]
            );
            assert_eq!(
                rendered[0].matches("declared here").count(),
                2,
                "{}",
                rendered[0]
            );
        }

        #[tokio::test]
        async fn a_ref_to_an_ambiguous_name_warns_too() {
            let code = r#"inherit "/twin_a"; inherit "/twin_b"; void inc(int ref x) { x++; } void f() { inc(ref twin); }"#;
            assert_eq!(
                warnings(code).await,
                ["`twin` is declared by `/twin_a.c` and `/twin_b.c`; `/twin_b.c`'s is used"]
            );
        }

        #[tokio::test]
        async fn a_declaration_in_this_file_settles_the_name() {
            let code =
                r#"inherit "/twin_a"; inherit "/twin_b"; int twin; int f() { return twin; }"#;
            assert_eq!(
                warnings(code).await,
                ["`twin` shadows a global inherited from `/twin_a.c` and `/twin_b.c`"]
            );
        }

        #[tokio::test]
        async fn a_diamond_is_not_ambiguous() {
            let code =
                r#"inherit "/diamond_left"; inherit "/diamond_right"; int f() { return a; }"#;
            assert!(warnings(code).await.is_empty());
        }

        #[tokio::test]
        async fn a_same_scope_duplicate_is_the_redefinition_error_alone() {
            let code = "void f() { int x; int x; x++; }";
            let walker = ScopeWalker::compile_through(code).await.unwrap();
            let messages: Vec<_> = walker
                .context
                .diagnostics
                .errors()
                .iter()
                .map(|e| e.to_string())
                .collect();
            assert_eq!(messages, ["Redefinition of `x`"]);
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
