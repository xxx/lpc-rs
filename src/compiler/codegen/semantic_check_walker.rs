use async_trait::async_trait;
use lpc_rs_core::{EFUN, ScopeId, call_namespace::CallNamespace, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, Result, lpc_error, lpc_warning, span::Span};
use lpc_rs_utils::string::closure_arg_number;

use crate::{
    compile_time_config::MAX_CLOSURE_ARG_REFERENCE,
    compiler::{
        ast::{
            assignment_node::AssignmentNode,
            ast_node::{AstNode, AstNodeTrait, SpannedNode},
            binary_op_node::{BinaryOpNode, BinaryOperation},
            block_node::BlockNode,
            break_node::BreakNode,
            call_node::{CallChain, CallNode},
            cast_node::CastNode,
            closure_node::ClosureNode,
            continue_node::ContinueNode,
            do_while_node::DoWhileNode,
            expression_node::ExpressionNode,
            for_each_node::{ForEachInit, ForEachNode},
            for_node::ForNode,
            function_def_node::{ARGV, FunctionDefNode},
            function_ptr_node::FunctionPtrNode,
            label_node::LabelNode,
            operator_node::OperatorNode,
            program_node::ProgramNode,
            range_node::RangeNode,
            return_node::ReturnNode,
            switch_node::SwitchNode,
            unary_op_node::{UnaryOpNode, UnaryOperation},
            var_init_node::VarInitNode,
            var_node::VarNode,
            while_node::WhileNode,
        },
        callee::Callee,
        codegen::tree_walker::{
            ContextHolder, Pass, TreeWalker, walk_assignment, walk_binary_op, walk_block,
            walk_cast, walk_closure, walk_do_while, walk_for, walk_foreach, walk_function_def,
            walk_function_ptr, walk_label, walk_operator, walk_range, walk_return, walk_switch,
            walk_unary_op, walk_var_init,
        },
        compilation_context::CompilationContext,
        diagnostics::Diagnostics,
        semantic::semantic_checks::{
            check_binary_operation_types, check_unary_operation_types, conversion_efun,
            element_type, impossible_cast, is_keyword, mismatch, node_type,
        },
    },
    interpreter::efun::{CALL_OTHER, CATCH, SIZEOF},
};

struct BreakAllowed(bool);
struct ContinueAllowed(bool);
struct LabelAllowed(bool);

/// A tree walker to handle various semantic & type checks
pub struct SemanticCheckWalker {
    /// Track the current function, so we can type check returns.
    current_function: Option<FunctionDefNode>,

    /// Are break / continue allowed at this time?
    valid_jumps: Vec<(BreakAllowed, ContinueAllowed)>,

    /// Are `case` and `default` statements currently allowed?
    valid_labels: Vec<LabelAllowed>,

    context: CompilationContext,

    closure_depth: usize,
}

impl SemanticCheckWalker {
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            current_function: None,
            valid_jumps: vec![],
            valid_labels: vec![],
            closure_depth: 0,
        }
    }

    fn allow_jumps(&mut self) {
        self.valid_jumps
            .push((BreakAllowed(true), ContinueAllowed(true)));
    }

    /// `continue` inside a `switch` stays as the enclosing loop allows.
    fn allow_breaks(&mut self) {
        let can_continue = self.can_continue();
        self.valid_jumps
            .push((BreakAllowed(true), ContinueAllowed(can_continue)));
    }

    fn prevent_jumps(&mut self) {
        self.valid_jumps.pop();
    }

    fn allow_labels(&mut self) {
        self.valid_labels.push(LabelAllowed(true));
    }

    fn prevent_labels(&mut self) {
        self.valid_labels.pop();
    }

    fn can_break(&self) -> bool {
        !self.valid_jumps.is_empty() && self.valid_jumps.last().unwrap().0.0
    }

    fn can_continue(&self) -> bool {
        !self.valid_jumps.is_empty() && self.valid_jumps.last().unwrap().1.0
    }

    fn can_use_labels(&self) -> bool {
        !self.valid_labels.is_empty() && self.valid_labels.last().unwrap().0
    }

    /// Warn at the first statement of `body` that follows a `return`,
    /// `break`, or `continue`; a `case` or `default` label makes what follows
    /// reachable again.
    fn check_unreachable(&mut self, body: &[AstNode]) {
        let mut jump: Option<&AstNode> = None;
        for statement in body {
            match statement {
                AstNode::NoOp => {}
                AstNode::LabeledStatement(labeled) => {
                    jump = is_jump(&labeled.node).then_some(&*labeled.node);
                }
                _ if jump.is_some() => {
                    let w = lpc_warning!(statement_span(statement), "unreachable statement")
                        .with_label("control leaves here", jump.and_then(statement_span));
                    self.context.diagnostics.record(w);
                    return;
                }
                _ if is_jump(statement) => jump = Some(statement),
                _ => {}
            }
        }
    }

    /// Record the diagnostics for the spread arguments of a call to
    /// `callee` (`None` for a receiver, pointer or chained call).
    fn check_spreads(&mut self, callee: Option<&str>, arguments: &[ExpressionNode]) -> Result<()> {
        let Some(first) = arguments
            .iter()
            .position(|a| matches!(a, ExpressionNode::Spread(_)))
        else {
            return Ok(());
        };

        for argument in arguments {
            let ExpressionNode::Spread(spread) = argument else {
                continue;
            };
            let ty = node_type(&spread.expr, &self.context)?;
            if !ty.matches_type(LpcType::Mixed(true)) {
                let e = lpc_error!(spread.span, "`...` spreads an array, not {}", ty);
                self.context.diagnostics.record(e);
            }
        }

        let spread_span = arguments[first].span();
        match callee {
            Some(name @ (CATCH | SIZEOF)) => {
                let e = lpc_error!(spread_span, "`{}` takes one expression, not a spread", name);
                self.context.diagnostics.record(e);
            }
            Some(CALL_OTHER) if first < 2 => {
                let e = lpc_error!(
                    spread_span,
                    "`call_other`'s receiver and function name cannot be spread"
                );
                self.context.diagnostics.record(e);
            }
            _ => {}
        }

        if let Some(arg) = arguments[first..]
            .iter()
            .find(|a| matches!(a, ExpressionNode::Ref(_)))
        {
            let e = lpc_error!(arg.span(), "a `ref` argument cannot follow a spread");
            self.context.diagnostics.record(e);
        }

        Ok(())
    }
}

/// Whether `node` is a `return`, `break`, or `continue`.
fn is_jump(node: &AstNode) -> bool {
    matches!(
        node,
        AstNode::Return(_) | AstNode::Break(_) | AstNode::Continue(_)
    )
}

/// Where `node` starts: a block or declaration borrows its first member's span.
fn statement_span(node: &AstNode) -> Option<Span> {
    match node {
        AstNode::Block(block) => block.body.first().and_then(statement_span),
        AstNode::Decl(decl) => decl.initializations.first().and_then(|init| init.span),
        _ => node.span(),
    }
}

impl ContextHolder for SemanticCheckWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl Pass for SemanticCheckWalker {
    fn new(context: CompilationContext) -> Self {
        SemanticCheckWalker::new(context)
    }

    fn diagnostics_mut(&mut self) -> &mut Diagnostics {
        &mut self.context.diagnostics
    }
}

#[async_trait]
impl TreeWalker for SemanticCheckWalker {
    fn enter_scope(&mut self, scope_id: &mut Option<ScopeId>) {
        self.context.scopes.goto(*scope_id);
    }

    fn exit_scope(&mut self) {
        self.context.scopes.pop();
    }

    async fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        walk_assignment(self, node).await?;

        let left_type = node_type(&node.lhs, &self.context)?;

        if let Some(right_type) = mismatch(left_type, &node.rhs, &self.context)? {
            let e: LpcError = lpc_error!(
                node.span,
                "Mismatched types: `{}` ({}) = `{}` ({})",
                node.lhs,
                left_type,
                node.rhs,
                right_type
            );

            return Err(self.context.diagnostics.fail(e));
        }

        Ok(())
    }

    async fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
        walk_binary_op(self, node).await?;

        match check_binary_operation_types(node, &self.context) {
            Ok(_) => Ok(()),
            Err(err) => Err(self.context.diagnostics.fail(err)),
        }
    }

    async fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.check_unreachable(&node.body);
        walk_block(self, node).await
    }

    async fn visit_break(&mut self, node: &mut BreakNode) -> Result<()> {
        if !self.can_break() {
            let e = lpc_error!(node.span, "Invalid `break`.");
            self.context.diagnostics.record(e);

            // non-fatal
        }

        Ok(())
    }

    async fn visit_call_root(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Root {
            receiver,
            namespace,
            name,
        } = &mut node.chain
        else {
            return Err(lpc_error!(node.span, "invalid call chain"));
        };

        // A `ref` would alias a cell in another object's address space.
        if (receiver.is_some() || name.as_str() == CALL_OTHER)
            && let Some(arg) = node
                .arguments
                .iter()
                .find(|a| matches!(a, ExpressionNode::Ref(_)))
        {
            let e = lpc_error!(arg.span(), "`ref` cannot cross objects");
            self.context.diagnostics.record(e);
        }

        let callee = receiver.is_none().then(|| name.as_str());
        self.check_spreads(callee, &node.arguments)?;

        if receiver.is_some() {
            if namespace != &CallNamespace::Local {
                let e = lpc_error!(node.span, "namespaced `call_other` is not allowed");
                self.context.diagnostics.record(e);
            }

            // call_other is not type checked
            return Ok(());
        }

        let unknown_namespace = matches!(namespace, CallNamespace::Named(ns)
            if !self.context.inherit_names.contains_key(ns.as_str()) && ns.as_str() != EFUN);
        if unknown_namespace {
            let e = lpc_error!(node.span, "unknown namespace `{}`", namespace.as_str());
            self.context.diagnostics.record(e);
        }

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        // A function-typed variable answers a bare name only.
        let is_function_pointer = namespace == &CallNamespace::Local
            && self
                .context
                .lookup_var(*name)
                .is_some_and(|sym| sym.type_.matches_type(LpcType::Function(false)));

        // An unknown namespace is reported above, not searched.
        if !unknown_namespace
            && !is_function_pointer
            && !self
                .context
                .contains_function_complete(name.as_str(), namespace)
        {
            let e = lpc_error!(
                node.span,
                "call to unknown function `{}{}`",
                namespace,
                name
            );
            self.context.diagnostics.record(e);
            // Non-fatal. Continue.
        }

        // A call through a function-typed variable has no declared parameter
        // list to check `ref` against, so it never accepts one.
        if is_function_pointer
            && let Some(arg) = node
                .arguments
                .iter()
                .find(|a| matches!(a, ExpressionNode::Ref(_)))
        {
            let e = lpc_error!(
                arg.span(),
                "a function pointer cannot take an argument by reference"
            );
            self.context.diagnostics.record(e);
        }

        // Further checks require access to the function prototype for error messaging
        let proto_opt = self.context.lookup_function_complete(&name, namespace);

        let mut errors: Vec<LpcError> = vec![];

        if let Some(callee) = proto_opt {
            let prototype = callee.as_ref();
            if prototype.flags.private()
                && !self
                    .context
                    .function_prototypes
                    .values()
                    .any(|val| val == prototype)
            {
                let e = LpcError::new(format!("call to private function `{}`", name))
                    .with_span(node.span)
                    .with_label("defined here", prototype.span);
                errors.push(e);
            }

            let arg_len = node.arguments.len();
            let fixed = node
                .arguments
                .iter()
                .position(|a| matches!(a, ExpressionNode::Spread(_)))
                .unwrap_or(arg_len);
            let has_spread = fixed < arg_len;

            if !has_spread && !prototype.accepts_arg_count(arg_len) {
                let e = LpcError::new(format!(
                    "incorrect argument count in call to `{}`: expected: {}, received: {}",
                    name, prototype.arity.num_args, arg_len
                ))
                .with_span(node.span)
                .with_label("defined here", prototype.span);
                errors.push(e);
            }

            // `call_other`'s `ref` arguments were reported as the cross-object error above.
            if name.as_str() != CALL_OTHER {
                let is_efun = matches!(callee, Callee::Efun(_));
                for (index, arg) in node.arguments.iter().take(fixed).enumerate() {
                    let is_ref_arg = matches!(arg, ExpressionNode::Ref(_));
                    let wants_ref = prototype.is_ref_param(index);
                    if wants_ref && !is_ref_arg {
                        // An implicit efun lvalue accepts a bare variable.
                        if is_efun && matches!(arg, ExpressionNode::Var(_)) {
                            continue;
                        }
                        let e = if is_efun {
                            LpcError::new(format!(
                                "argument {} of `{}` must be a variable",
                                index + 1,
                                name
                            ))
                        } else {
                            let hint = match arg {
                                ExpressionNode::Var(v) => format!(": `ref {}`", v.name),
                                _ => String::new(),
                            };
                            LpcError::new(format!(
                                "argument {} of `{}` must be passed by reference{hint}",
                                index + 1,
                                name
                            ))
                        };
                        errors.push(
                            e.with_span(arg.span()).with_label(
                                "declared here",
                                prototype.arg_spans.get(index).cloned(),
                            ),
                        );
                    } else if is_ref_arg && !wants_ref {
                        errors.push(
                            LpcError::new(format!(
                                "`{}` does not take argument {} by reference",
                                name,
                                index + 1
                            ))
                            .with_span(arg.span())
                            .with_label("declared here", prototype.arg_spans.get(index).cloned()),
                        );
                    }
                }
            }

            // Check argument types.
            for (index, ty) in prototype.arg_types.iter().enumerate() {
                let Some(arg) = node.arguments[..fixed].get(index) else {
                    continue;
                };

                if let Some(arg_type) = mismatch(*ty, arg, &self.context)? {
                    let e = LpcError::new(format!(
                        "unexpected argument type to `{}`: {}. Expected {}.",
                        name, arg_type, ty
                    ))
                    .with_span(arg.span())
                    .with_label("declared here", prototype.arg_spans.get(index).cloned());

                    errors.push(e);
                }
            }
        }

        for e in errors {
            self.context.diagnostics.record(e);
        }

        Ok(())
    }

    async fn visit_call_chain(&mut self, node: &mut CallNode) -> Result<()> {
        let CallChain::Node(chain_node) = &mut node.chain else {
            return Err(lpc_error!(node.span, "invalid call chain"));
        };

        // A chained call's callee is a computed function value with no
        // declared parameter list to check `ref` against, so it never
        // accepts one either.
        if let Some(arg) = node
            .arguments
            .iter()
            .find(|a| matches!(a, ExpressionNode::Ref(_)))
        {
            let e = lpc_error!(
                arg.span(),
                "a function pointer cannot take an argument by reference"
            );
            self.context.diagnostics.record(e);
        }

        self.check_spreads(None, &node.arguments)?;

        chain_node.visit(self).await?;

        for argument in &mut node.arguments {
            argument.visit(self).await?;
        }

        Ok(())
    }

    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()> {
        self.closure_depth += 1;

        // A closure has no caller frame to alias into, so it cannot take `ref`.
        if let Some(parameters) = &node.parameters {
            for param in parameters {
                if param.by_ref {
                    let e = lpc_error!(param.span, "a closure cannot take a `ref` parameter");
                    self.context.diagnostics.record(e);
                }
            }
        }

        self.check_unreachable(&node.body);
        // A closure compiles to its own function, so its body cannot jump
        // to an enclosing loop or switch.
        self.valid_jumps
            .push((BreakAllowed(false), ContinueAllowed(false)));
        self.valid_labels.push(LabelAllowed(false));
        walk_closure(self, node).await?;
        self.prevent_labels();
        self.prevent_jumps();

        self.closure_depth -= 1;

        Ok(())
    }

    async fn visit_continue(&mut self, node: &mut ContinueNode) -> Result<()> {
        if !self.can_continue() {
            let e = lpc_error!(node.span, "invalid `continue`.");
            self.context.diagnostics.record(e);

            // non-fatal
        }

        Ok(())
    }

    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        self.allow_jumps();
        walk_do_while(self, node).await?;

        self.prevent_jumps();
        Ok(())
    }

    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()> {
        self.allow_jumps();

        walk_for(self, node).await?;

        self.prevent_jumps();
        Ok(())
    }

    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()> {
        self.allow_jumps();

        let collection_type = node_type(&node.collection, &self.context)?;
        if !collection_type.is_array()
            && !collection_type.matches_type(LpcType::Mapping(false))
            && !collection_type.matches_type(LpcType::String(false))
        {
            let e = lpc_error!(
                node.collection.span(),
                "`foreach` must iterate over an array or mapping, found {}",
                collection_type
            );
            self.context.diagnostics.record(e);
        }

        if let ForEachInit::Array(init) = &node.initializer
            && let Some(element) = element_type(collection_type)
            && !init.type_.matches_type(element)
        {
            let e = lpc_error!(
                init.span,
                "mismatched types: `foreach` variable `{}` ({}) over `{}` ({} elements)",
                init.name,
                init.type_,
                node.collection,
                element
            );
            self.context.diagnostics.record(e);
        }

        // `LoadMappingKey` only runs on a mapping at codegen, so the two-variable form
        // needs one; a plain string or any array can never provide it.
        if matches!(node.initializer, ForEachInit::Mapping { .. })
            && (collection_type.is_array() || collection_type == LpcType::String(false))
        {
            let e = lpc_error!(
                node.collection.span(),
                "a two-variable `foreach` needs a mapping, found {}",
                collection_type
            );
            self.context.diagnostics.record(e);
        }

        walk_foreach(self, node).await?;

        self.prevent_jumps();
        Ok(())
    }

    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        is_keyword(node.name)?;

        self.context.scopes.goto_function(&node.name)?;
        self.current_function = Some(node.clone());

        if let Some(prototype) = self.context.function_prototypes.get(node.name.as_str()) {
            let arity = prototype.arity;
            let required = (arity.num_args - arity.num_default_args) as usize;
            for (index, param) in node.parameters.iter().enumerate() {
                // A default value is already the other `ref` error; don't double-report.
                if param.by_ref
                    && param.value.is_none()
                    && (node.flags.varargs() || index >= required)
                {
                    let e = lpc_error!(param.span, "a `ref` parameter cannot be optional");
                    self.context.diagnostics.record(e);
                }
            }
        }

        self.check_unreachable(&node.body);
        walk_function_def(self, node).await?;

        self.context.scopes.pop();
        Ok(())
    }

    async fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()> {
        let proto_opt = self
            .context
            .lookup_function_complete(node.name, &CallNamespace::default());

        if let Some(callee) = proto_opt {
            let prototype = callee.as_ref();
            if prototype.flags.private()
                && !self
                    .context
                    .function_prototypes
                    .values()
                    .any(|val| val == prototype)
            {
                let e = LpcError::new(format!(
                    "attempt to point to private function `{}`",
                    node.name
                ))
                .with_span(node.span)
                .with_label("defined here", prototype.span)
                .with_note(concat!(
                    "A function pointer can only point to a private function if ",
                    "it is declared in the same file."
                ));
                self.context.diagnostics.record(e);
            }
        }

        walk_function_ptr(self, node).await
    }

    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
        if !self.can_use_labels() {
            let msg = if node.is_default() {
                "invalid `default`."
            } else {
                "invalid `case` statement."
            };

            let err = LpcError::new(msg).with_span(node.span);
            self.context.diagnostics.record(err);
        }

        walk_label(self, node).await
    }

    async fn visit_operator(&mut self, node: &mut OperatorNode) -> Result<()> {
        let arity = node.op.arity();
        if let Some(args) = &node.arguments
            && args.len() > arity
        {
            let e = lpc_error!(
                node.span,
                "`operator({})` takes {} argument{}, found {}",
                node.op,
                arity,
                if arity == 1 { "" } else { "s" },
                args.len()
            );
            self.context.diagnostics.record(e);
        }

        walk_operator(self, node).await
    }

    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        self.context.scopes.goto_root();

        for expr in &mut node.body {
            expr.visit(self).await?;
        }

        Ok(())
    }

    async fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        walk_range(self, node).await?;

        let left_type = if let Some(left) = &*node.l {
            node_type(left, &self.context)?
        } else {
            LpcType::Int(false)
        };

        let right_type = if let Some(right) = &*node.r {
            node_type(right, &self.context)?
        } else {
            LpcType::Int(false)
        };

        // These must resolve to ints at some point.
        let required_type = LpcType::Int(false);

        if left_type.matches_type(required_type) && right_type.matches_type(required_type) {
            Ok(())
        } else {
            let left_val = if let Some(node) = &*node.l {
                format!("{node}")
            } else {
                String::from("0")
            };

            let right_val = if let Some(node) = &*node.r {
                format!("{node}")
            } else {
                String::from("-1")
            };

            let e: LpcError = lpc_error!(
                node.span,
                "invalid range types: `{}` ({}) .. `{}` ({})",
                left_val,
                left_type,
                right_val,
                right_type
            );

            Err(self.context.diagnostics.fail(e))
        }
    }

    async fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        walk_return(self, node).await?;

        // closure return types are not type-checked
        if self.closure_depth > 0 {
            return Ok(());
        }

        if let Some(function_def) = &self.current_function {
            if let Some(expression) = &node.value {
                if let Some(return_type) =
                    mismatch(function_def.return_type, expression, &self.context)?
                {
                    let error = LpcError::new(format!(
                        "invalid return type {}. Expected {}.",
                        return_type, function_def.return_type
                    ))
                    .with_span(node.span)
                    .with_label("defined here", function_def.span);

                    self.context.diagnostics.record(error);
                }
            } else if function_def.return_type != LpcType::Void {
                let error = LpcError::new(format!(
                    "invalid return type {} - expected {}.",
                    LpcType::Void,
                    function_def.return_type
                ))
                .with_span(node.span)
                .with_label("defined here", function_def.span);

                self.context.diagnostics.record(error);
            }
        } // else warn?

        Ok(())
    }

    async fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        self.allow_labels();
        self.allow_breaks();

        walk_switch(self, node).await?;

        self.prevent_jumps();
        self.prevent_labels();

        Ok(())
    }

    async fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        walk_unary_op(self, node).await?;

        match check_unary_operation_types(node, &self.context) {
            Ok(_) => match node.op {
                UnaryOperation::Inc | UnaryOperation::Dec => {
                    if matches!(*node.expr, ExpressionNode::Int(_)) {
                        let err: LpcError = lpc_error!("Invalid operation on `int` literal");
                        return Err(self.context.diagnostics.fail(err));
                    }

                    // A slice is a value, not a place.
                    if let ExpressionNode::BinaryOp(BinaryOpNode {
                        op: BinaryOperation::Index,
                        r,
                        ..
                    }) = &*node.expr
                        && matches!(**r, ExpressionNode::Range(_))
                    {
                        let err: LpcError = lpc_error!(
                            node.span,
                            "`{}` cannot step a slice: `{}`",
                            node.op,
                            node.expr
                        );
                        return Err(self.context.diagnostics.fail(err));
                    }

                    Ok(())
                }
                _ => Ok(()),
            },
            Err(err) => Err(self.context.diagnostics.fail(err)),
        }
    }

    async fn visit_cast(&mut self, node: &mut CastNode) -> Result<()> {
        walk_cast(self, node).await?;

        if let Some(source) = impossible_cast(node, &self.context)? {
            let mut e = lpc_error!(
                node.span,
                "cast from `{}` to `{}` can never succeed",
                source,
                node.type_
            );
            if let Some(efun) = conversion_efun(node.type_) {
                e = e.with_note(format!("a cast converts nothing; `{efun}` does"));
            }
            self.context.diagnostics.record(e);
        }

        Ok(())
    }

    async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
        if node.is_closure_arg_var() {
            if self.closure_depth == 0 {
                let e = lpc_error!(
                    node.span,
                    "positional argument variables can only be used within a closure",
                );
                self.context.diagnostics.record(e);
            }

            if closure_arg_number(node.name)? > MAX_CLOSURE_ARG_REFERENCE {
                let e = lpc_error!(
                    node.span,
                    "positional argument variables can only be used up to `${}`",
                    MAX_CLOSURE_ARG_REFERENCE
                );
                self.context.diagnostics.record(e);
            }
        }

        Ok(())
    }

    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        is_keyword(node.name)?;

        if node.by_ref && node.value.is_some() {
            let e = lpc_error!(node.span, "a `ref` parameter cannot have a default value");
            self.context.diagnostics.record(e);
        }

        if node.name == ARGV
            && let Some(FunctionDefNode { flags, span, .. }) = self.current_function
            && flags.ellipsis()
        {
            let e: LpcError =
                LpcError::new("redeclaration of `argv` in a function with ellipsis arguments")
                    .with_span(node.span)
                    .with_label("Declared here", span);
            return Err(self.context.diagnostics.fail(e));
        }

        walk_var_init(self, node).await?;

        if let Some(expression) = &node.value
            && let Some(expr_type) = mismatch(node.type_, expression, &self.context)?
        {
            let e = lpc_error!(
                node.span,
                "mismatched types: `{}` ({}) = `{}` ({})",
                node.name,
                node.type_,
                expression,
                expr_type
            );

            self.context.diagnostics.record(e);
        }

        Ok(())
    }

    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        self.allow_jumps();
        node.condition.visit(self).await?;
        node.body.visit(self).await?;

        self.prevent_jumps();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, default::Default, sync::Arc};

    use claims::*;
    use factori::create;
    use indoc::indoc;
    use lpc_rs_core::{
        call_namespace::CallNamespace, function_arity::FunctionArity, lpc_type::LpcType,
    };
    use lpc_rs_function_support::symbol::Symbol;
    use ustr::ustr;

    use super::*;
    use crate::compiler::ast::{int_node::IntNode, ternary_node::TernaryNode};
    use crate::test_support::CompileThrough;
    use crate::{
        compiler::{
            ast::{ast_node::AstNode, expression_node::ExpressionNode, var_node::VarNode},
            codegen::semantic_check_walker::SemanticCheckWalker,
            semantic::scope_tree::ScopeTree,
        },
        test_support::factories::*,
    };

    fn context_with_var(name: &str, var_type: LpcType) -> CompilationContext {
        let mut scopes = ScopeTree::default();
        scopes.push_new();
        let sym = Symbol::new(name, var_type);
        scopes.current_mut().unwrap().insert(sym);
        CompilationContext {
            scopes,
            ..CompilationContext::default()
        }
    }

    async fn walk_code(code: &str) -> Result<CompilationContext> {
        Ok(SemanticCheckWalker::compile_through(code)
            .await?
            .into_context())
    }

    /// The messages a snippet earns, warnings aside. A checker `fail` stops
    /// the walk outright, so its diagnostics arrive via `Err` rather than the
    /// context.
    async fn messages(code: &str) -> Vec<String> {
        match walk_code(code).await {
            Ok(context) => context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| !e.is_warning())
                .map(ToString::to_string)
                .collect(),
            Err(e) => std::iter::once(e.clone())
                .chain(e.additional_errors().iter().cloned())
                .filter(|e| !e.is_warning())
                .map(|e| e.to_string())
                .collect(),
        }
    }

    mod test_visit_assignment {
        use super::*;
        use crate::compiler::ast::binary_op_node::BinaryOperation;

        #[tokio::test]
        async fn validates_both_sides() -> Result<()> {
            let mut node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(456)),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Int(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            node.visit(&mut walker).await
        }

        #[tokio::test]
        async fn always_allows_0() -> Result<()> {
            let mut node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(0)),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            node.visit(&mut walker).await
        }

        #[tokio::test]
        async fn disallows_differing_types() {
            let mut node = ExpressionNode::from(AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                rhs: Box::new(ExpressionNode::from(123)),
                span: None,
            });

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            assert!(node.visit(&mut walker).await.is_err());
        }

        #[tokio::test]
        async fn allows_mixed() {
            let mut init_node = VarInitNode {
                type_: LpcType::Mixed(false),
                name: ustr("foo"),
                value: Some(ExpressionNode::from(324)),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let var_node = VarNode::new("foo");

            let mut assignment_node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(var_node)),
                rhs: Box::new(ExpressionNode::from("foobar")),
                span: None,
            };

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Mixed(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = init_node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());

            let _ = assignment_node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_array_items() {
            let mut init_node = VarInitNode {
                type_: LpcType::Int(false),
                name: ustr("foo"),
                value: Some(ExpressionNode::from(324)),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let var_node = VarNode::new("foo");

            let mut assignment_node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(var_node)),
                rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(vec![1, 2, 3])),
                    r: Box::new(ExpressionNode::from(1)),
                    op: BinaryOperation::Index,
                    span: None,
                })),
                span: None,
            };

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Mixed(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = init_node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());

            let _ = assignment_node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_array_ranges() {
            let mut init_node = VarInitNode {
                type_: LpcType::Int(true),
                name: ustr("foo"),
                value: Some(ExpressionNode::from(vec![324])),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let var_node = VarNode::new("foo");

            let mut assignment_node = AssignmentNode {
                lhs: Box::new(ExpressionNode::Var(var_node)),
                rhs: Box::new(ExpressionNode::BinaryOp(BinaryOpNode {
                    l: Box::new(ExpressionNode::from(vec![1, 2, 3])),
                    r: Box::new(ExpressionNode::Range(RangeNode {
                        l: Box::new(Some(ExpressionNode::from(1))),
                        r: Box::new(Some(ExpressionNode::from(4))),
                        span: None,
                    })),
                    op: BinaryOperation::Index,
                    span: None,
                })),
                span: None,
            };

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Mixed(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = init_node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());

            let _ = assignment_node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_binary_op {
        use super::*;
        use crate::compiler::ast::binary_op_node::BinaryOperation;

        #[tokio::test]
        async fn validates_both_sides() -> Result<()> {
            let mut node = ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                r: Box::new(ExpressionNode::from(456)),
                op: BinaryOperation::Add,
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Int(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            node.visit(&mut walker).await
        }

        #[tokio::test]
        async fn disallows_differing_types() {
            let mut node = ExpressionNode::from(BinaryOpNode {
                l: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                r: Box::new(ExpressionNode::from(123)),
                op: BinaryOperation::Sub,
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            assert!(node.visit(&mut walker).await.is_err());
        }
    }

    mod test_visit_break {
        use super::*;

        #[tokio::test]
        async fn disallows_inside_a_closure_within_a_loop() {
            let code = r#"
                void create() {
                    while (1) {
                        dump((: break; :));
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "Invalid `break`."
            );
        }

        #[tokio::test]
        async fn allows_in_a_closures_own_loop_within_a_loop() {
            let code = r#"
                void create() {
                    while (1) {
                        dump((: while (1) { break; } :));
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(
                context.diagnostics.errors().is_empty(),
                "{:?}",
                context.diagnostics.errors()
            );
        }

        #[tokio::test]
        async fn disallows_outside_of_loop_or_switch() {
            let code = "void create() { break; }";
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(!context.diagnostics.errors().is_empty());
            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "Invalid `break`."
            );
        }

        #[tokio::test]
        async fn allows_in_while_loop() {
            let code = r#"
                void create() {
                    int i;

                    while(i < 10) {
                        i += 1;
                        if (i > 5) {
                            break;
                        }
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_in_for_loop() {
            let code = r#"
                void create() {
                    for(int i = 0; i < 10; i += 1) {
                        if (i > 5) {
                            break;
                        }
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_in_do_while_loop() {
            let code = r#"
                void create() {
                    int i;

                     do {
                        i += 1;
                        if (i > 5) {
                            break;
                        }
                    } while(i < 10);
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_in_switch() {
            let code = r#"
                void create() {
                    int i = 5;
                    switch (i) {
                        case 5:
                            dump("nice!");
                            break;
                        default:
                            dump("weeeeak");
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_continue {
        use super::*;

        #[tokio::test]
        async fn disallows_inside_a_closure_within_a_loop() {
            let code = r#"
                void create() {
                    while (1) {
                        dump((: continue; :));
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "invalid `continue`."
            );
        }

        #[tokio::test]
        async fn disallows_outside_of_a_loop() {
            let code = "void create() { continue; }";
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "invalid `continue`."
            );
        }

        #[tokio::test]
        async fn disallows_in_a_switch_outside_of_a_loop() {
            let code = r#"
                void create() {
                    int i;
                    switch (i) {
                        case 1: continue;
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "invalid `continue`."
            );
        }

        #[tokio::test]
        async fn allows_in_a_switch_inside_a_loop() {
            let code = r#"
                void create() {
                    int i;
                    while (i < 10) {
                        i += 1;
                        switch (i) {
                            case 1: continue;
                        }
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_call {
        use std::sync::Arc;

        use lpc_rs_core::{function_flags::FunctionFlags, visibility::Visibility};
        use lpc_rs_function_support::{
            function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
        };

        use super::*;
        use crate::{
            assert_regex, interpreter::program::Program, test_support::empty_compilation_context,
        };

        #[tokio::test]
        async fn allows_known_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("known"))
            ));

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("known"),
                FunctionPrototypeBuilder::default()
                    .name("known")
                    .filename(Arc::new("test.c".into()))
                    .return_type(LpcType::Int(false))
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_local_private_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("known"))
            ));

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("known"),
                FunctionPrototypeBuilder::default()
                    .name("known")
                    .filename(Arc::new("test.c".into()))
                    .return_type(LpcType::Int(false))
                    .flags(FunctionFlags::default().with_visibility(Visibility::Private))
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_known_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("known"))
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_parent_namespaced_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("known"), namespace: CallNamespace::Parent),
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_private_parent_namespaced_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("known"), namespace: CallNamespace::Parent),
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .flags(FunctionFlags::from(&["private"][..]))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "call to private function `known`"
            );
        }

        #[tokio::test]
        async fn allows_named_namespaced_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain:
                    create!(CallChain, name: ustr("known"), namespace: CallNamespace::Named(ustr("parent"))),
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            context
                .inherit_names
                .insert("parent".into(), context.inherits.len() - 1);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_efun_namespaced_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain:
                    create!(CallChain, name: ustr("this_object"), namespace: CallNamespace::Named(ustr("efun"))),
            ));

            let context = CompilationContext::default();
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_private_named_namespaced_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain:
                    create!(CallChain, name: ustr("known"), namespace: CallNamespace::Named(ustr("parent"))),
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .flags(FunctionFlags::from(&["private"][..]))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            context
                .inherit_names
                .insert("parent".into(), context.inherits.len() - 1);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "call to private function `known`"
            );
        }

        #[tokio::test]
        async fn disallows_unknown_named_namespaced_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain:
                    create!(CallChain, name: ustr("known"), namespace: CallNamespace::Named(ustr("unknown_namespace"))),
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "unknown namespace `unknown_namespace`"
            );
        }

        #[tokio::test]
        async fn disallows_private_inherited_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("known")),
            ));

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("test.c".into()))
                .return_type(LpcType::Int(false))
                .flags(FunctionFlags::default().with_visibility(Visibility::Private))
                .build()
                .unwrap();

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "call to private function `known`"
            );
        }

        #[tokio::test]
        async fn allows_known_efuns() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("dump")),
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
            ));

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_function_pointers() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_function_pointer")),
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
            ));

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("my_function_pointer", LpcType::Function(false));
            scopes.current_mut().unwrap().insert(sym);

            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_mixed_function_pointers() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_mixed_function_pointer")),
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
            ));

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("my_mixed_function_pointer", LpcType::Mixed(false));
            scopes.current_mut().unwrap().insert(sym);

            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_inherited_function_pointers() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("inherited_pointer")),
                arguments: vec![],
            ));

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let mut parent = Program::default();
            parent.global_variables.insert(
                "inherited_pointer".to_string(),
                Symbol::new("inherited_pointer", LpcType::Function(false)),
            );

            let context = CompilationContext {
                scopes,
                inherits: vec![parent],
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_pointers_to_non_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_non_function_pointer")),
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
            ));

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("my_non_function_pointer", LpcType::Int(false));
            scopes.current_mut().unwrap().insert(sym);

            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_eq!(
                walker
                    .context
                    .diagnostics
                    .errors()
                    .first()
                    .unwrap()
                    .to_string(),
                "call to unknown function `my_non_function_pointer`"
            );
        }

        #[tokio::test]
        async fn disallows_unknown_functions() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("unknown")),
            ));

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert_eq!(walker.context.diagnostics.errors().len(), 1);
        }

        #[tokio::test]
        async fn disallows_incorrect_function_arity() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("dump")),
            ));

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert_eq!(walker.context.diagnostics.errors().len(), 1);
        }

        #[tokio::test]
        async fn handles_ellipsis_argument_arity() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("call_other")),
                arguments:
                    vec![
                        ExpressionNode::from("bar.c"),
                        ExpressionNode::from("my_function"),
                        ExpressionNode::from(123),
                        ExpressionNode::from(111),
                        ExpressionNode::from("sha256"),
                    ], // `call_other` is specified as having 2 arguments, but we're passing more
            ));

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn handles_varargs_argument_arity() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_function")),
            ));

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_function"),
                FunctionPrototypeBuilder::default()
                    .name("my_function")
                    .filename(Arc::new("my_function.c".into()))
                    .return_type(LpcType::Int(false))
                    .arity(FunctionArity::new(5))
                    .arg_types(vec![
                        LpcType::Int(false),
                        LpcType::Float(false),
                        LpcType::Int(false),
                        LpcType::String(false),
                        LpcType::Int(false),
                    ])
                    .flags(FunctionFlags::default().with_varargs(true))
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn understands_argument_defaults() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_func")),
            ));

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototypeBuilder::default()
                    .name("my_func")
                    .filename(Arc::new("my_func.c".into()))
                    .return_type(LpcType::Int(false))
                    .arity(FunctionArity {
                        num_args: 1,
                        num_default_args: 1,
                    })
                    .arg_types(vec![LpcType::String(false)])
                    .flags(FunctionFlags::default())
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_invalid_arg_types() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_func")),
                arguments: vec![ExpressionNode::from(123)],
            ));

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototypeBuilder::default()
                    .name("my_func")
                    .filename(Arc::new("my_func.c".into()))
                    .return_type(LpcType::Int(false))
                    .arity(FunctionArity::new(1))
                    .arg_types(vec![LpcType::String(false)])
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert_eq!(walker.context.diagnostics.errors().len(), 1);
        }

        #[tokio::test]
        async fn allows_0() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain: create!(CallChain, name: ustr("my_func")),
                arguments: vec![ExpressionNode::from(0)],
            ));

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototypeBuilder::default()
                    .name("my_func")
                    .filename(Arc::new("my_func".into()))
                    .return_type(LpcType::String(false))
                    .arity(FunctionArity::new(1))
                    .arg_types(vec![LpcType::String(false)])
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_bad_data_with_call_other() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain:
                    create!(CallChain, receiver: Some(Box::new(ExpressionNode::from(23))), name: ustr("dump")),
                arguments: vec![],
            ));

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_non_local_namespace_with_call_other() {
            let mut node = ExpressionNode::from(create!(
                CallNode,
                chain:
                    create!(CallChain,
                        receiver: Some(Box::new(ExpressionNode::from(23))),
                        name: ustr("dump"),
                        namespace: CallNamespace::Parent
                    ),
                arguments: vec![],
            ));

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;
            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "namespaced `call_other` is not allowed"
            );
        }
    }

    mod test_visit_foreach {
        use super::*;

        #[tokio::test]
        async fn allows_array_collections() {
            let code = indoc! { r#"
                void create() {
                    int *a = ({ 1, 2, 3 });
                    foreach(i: a) {
                        dump(i);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_mapping_collections() {
            let code = indoc! { r#"
                void create() {
                    mapping a = ([ "a": 1, "b": 2, "c": 3 ]);
                    foreach(key, value: a) {
                        dump(key);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn rejects_two_variables_over_a_string() {
            let code = indoc! { r#"
                void create() {
                    string s = "hello, world!";
                    foreach(key, value: s) {
                        dump(key);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "a two-variable `foreach` needs a mapping, found string"
            );
        }

        #[tokio::test]
        async fn rejects_two_variables_over_an_array() {
            let code = indoc! { r#"
                void create() {
                    int *a = ({ 1, 2, 3 });
                    foreach(key, value: a) {
                        dump(key);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "a two-variable `foreach` needs a mapping, found int *"
            );
        }

        #[tokio::test]
        async fn allows_typed_variables_the_collection_can_fill() {
            let code = indoc! { r#"
                void create() {
                    int *a = ({ 1, 2, 3 });
                    string s = "abc";
                    mapping m = ([ "a": 1 ]);
                    foreach (int i : a) {
                        dump(i);
                    }
                    foreach (int c : s) {
                        dump(c);
                    }
                    foreach (string k, int v : m) {
                        dump(k);
                    }
                    foreach (string *row : ({ ({ "x" }) })) {
                        dump(row);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(
                context.diagnostics.errors().is_empty(),
                "{:?}",
                context.diagnostics.errors()
            );
        }

        #[tokio::test]
        async fn rejects_a_variable_type_the_elements_cannot_be() {
            let code = indoc! { r#"
                void create() {
                    int *a = ({ 1, 2, 3 });
                    foreach (string s : a) {
                        dump(s);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "mismatched types: `foreach` variable `s` (string) over `a` (int elements)"
            );
        }

        #[tokio::test]
        async fn a_string_yields_ints() {
            let code = indoc! { r#"
                void create() {
                    string s = "abc";
                    foreach (string c : s) {
                        dump(c);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "mismatched types: `foreach` variable `c` (string) over `s` (int elements)"
            );
        }

        #[tokio::test]
        async fn an_array_variable_over_a_flat_array_is_rejected() {
            let code = indoc! { r#"
                void create() {
                    int *a = ({ 1, 2, 3 });
                    foreach (int *row : a) {
                        dump(row);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "mismatched types: `foreach` variable `row` (int *) over `a` (int elements)"
            );
        }

        #[tokio::test]
        async fn disallows_invalid_collections() {
            let code = indoc! { r#"
                void create() {
                    int a = 0;
                    foreach(key: a) {
                        dump(key);
                    }
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "`foreach` must iterate over an array or mapping, found int"
            );
        }
    }

    mod test_visit_cast {
        use super::*;

        #[tokio::test]
        async fn allows_a_cast_from_mixed_and_from_a_call_other() {
            let code = indoc! { r#"
                void create() {
                    mixed m = 1;
                    object o;
                    int i = (int) m;
                    string *s = (string *) m;
                    string name = (string) o->query_name();
                    int *a = (int *) ({ });
                    string z = (string) 0;
                    dump(({ i, s, name, a, z }));
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(
                context.diagnostics.errors().is_empty(),
                "{:?}",
                context.diagnostics.errors()
            );
        }

        #[tokio::test]
        async fn rejects_a_cast_between_concrete_types() {
            let code = indoc! { r#"
                void create() {
                    string s = "1";
                    int i = (int) s;
                    dump(i);
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "cast from `string` to `int` can never succeed"
            );
            assert!(
                context.diagnostics.errors()[0]
                    .diagnostic_string()
                    .contains("to_int")
            );
        }

        #[tokio::test]
        async fn rejects_a_scalar_cast_of_an_array() {
            let code = indoc! { r#"
                void create() {
                    mixed *a = ({ 1 });
                    string s = (string) a;
                    dump(s);
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "cast from `mixed *` to `string` can never succeed"
            );
        }

        #[tokio::test]
        async fn a_cast_retypes_the_expression() {
            let code = indoc! { r#"
                void create() {
                    mixed m = 1;
                    string s = (int) m;
                    dump(s);
                }
            "# };
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "mismatched types: `s` (string) = `(int) m` (int)"
            );
        }
    }

    mod test_visit_function_def {
        use lpc_rs_core::function_flags::FunctionFlags;

        use super::*;
        use crate::{
            compiler::{
                ast::{ast_node::AstNode, binary_op_node::BinaryOperation},
                codegen::scope_walker::ScopeWalker,
            },
            test_support::empty_compilation_context,
        };

        #[tokio::test]
        async fn handles_scopes() {
            let _global = VarInitNode {
                type_: LpcType::Int(false),
                name: ustr("a"),
                value: Some(ExpressionNode::from(1)),
                array: false,
                global: true,
                span: None,
                flags: None,
                by_ref: false,
            };

            let param1 = VarInitNode {
                type_: LpcType::String(false),
                name: ustr("a"),
                value: Some(ExpressionNode::from("foo")),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let param2 = VarInitNode {
                type_: LpcType::Int(true),
                name: ustr("a"),
                value: Some(ExpressionNode::from(vec![1, 2, 3, 4])),
                array: true,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let mut function_def1 = FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("foo"),
                parameters: vec![param1],
                flags: FunctionFlags::default(),
                body: vec![AstNode::from(ExpressionNode::BinaryOp(BinaryOpNode {
                    op: BinaryOperation::Add,
                    l: Box::new(ExpressionNode::from("foo")),
                    r: Box::new(ExpressionNode::Var(VarNode {
                        name: ustr("a"),
                        span: None,
                        global: false,
                        function_name: false,
                    })),
                    span: None,
                }))],
                span: None,
            };

            let mut function_def2 = FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("snuh"),
                parameters: vec![param2],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };

            let context = empty_compilation_context();
            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_function_def(&mut function_def1).await;
            let _ = scope_walker.visit_function_def(&mut function_def2).await;

            let context = scope_walker.into_context();
            let mut walker = SemanticCheckWalker::new(context);

            let _ = walker.visit_function_def(&mut function_def1).await;
            let _ = walker.visit_function_def(&mut function_def2).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_keyword_name() {
            let mut node = FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("while"),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };
            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let result = walker.visit_function_def(&mut node).await;

            if let Err(e) = result {
                assert!(e.to_string().contains("is a keyword of the language"));
            } else {
                panic!("didn't error?")
            }
        }
    }

    mod test_visit_function_ptr {
        use lpc_rs_core::{function_flags::FunctionFlags, visibility::Visibility};
        use lpc_rs_function_support::{
            function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
        };

        use super::*;
        use crate::{
            assert_regex, interpreter::program::Program, test_support::empty_compilation_context,
        };

        #[tokio::test]
        async fn allows_local_private_functions() {
            let mut node = ExpressionNode::from(FunctionPtrNode {
                receiver: None,
                arguments: None,
                name: ustr("known"),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("known"),
                FunctionPrototypeBuilder::default()
                    .name("known")
                    .filename(Arc::new("known".into()))
                    .return_type(LpcType::Int(false))
                    .flags(FunctionFlags::default().with_visibility(Visibility::Private))
                    .build()
                    .unwrap(),
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_private_inherited_functions() {
            let mut node = ExpressionNode::from(FunctionPtrNode {
                receiver: None,
                arguments: None,
                name: ustr("known"),
                span: None,
            });

            let prototype = FunctionPrototypeBuilder::default()
                .name("known")
                .filename(Arc::new("known".into()))
                .return_type(LpcType::Int(false))
                .flags(FunctionFlags::default().with_visibility(Visibility::Private))
                .build()
                .expect("failed to build prototype");

            let program_function = ProgramFunction::new(prototype, 0);

            let mut program = Program::default();
            program
                .functions
                .insert(ustr("known"), program_function.into());

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let mut context = CompilationContext::default();
            context.inherits.push(program);
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            assert_ok!(result);
            assert!(!walker.context.diagnostics.errors().is_empty());
            assert_regex!(
                walker.context.diagnostics.errors()[0].message(),
                "attempt to point to private function `known`"
            );
        }

        #[tokio::test]
        async fn allows_known_efuns() {
            let mut node = ExpressionNode::from(FunctionPtrNode {
                receiver: None,
                arguments: Some(vec![Some(ExpressionNode::from(IntNode::new(12)))]),
                name: ustr("dump"),
                span: None,
            });

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_operator {
        use super::*;

        #[tokio::test]
        async fn refuses_more_bound_arguments_than_the_operator_takes() {
            let code = "function f; void create() { f = &operator(+)(1, 2, 3); }";

            assert_eq!(
                messages(code).await,
                ["`operator(+)` takes 2 arguments, found 3"]
            );
        }

        #[tokio::test]
        async fn a_hole_counts_as_a_bound_argument() {
            let code = "function f; void create() { f = &operator(!)(, 1); }";

            assert_eq!(
                messages(code).await,
                ["`operator(!)` takes 1 argument, found 2"]
            );
        }

        #[tokio::test]
        async fn accepts_every_slot_bound_or_none() {
            let code = "function f, g, h, i; void create() { f = &operator(+)(1, 2); g = &operator(!)(0); h = operator([]); i = &operator(-)(); }";
            let msgs = messages(code).await;

            assert!(msgs.is_empty(), "{:?}", msgs);
        }

        #[tokio::test]
        async fn bitwise_not_takes_a_mixed_operand() {
            let code = "mixed m; int r; void create() { r = ~m; }";
            let msgs = messages(code).await;

            assert!(msgs.is_empty(), "{:?}", msgs);
        }

        #[tokio::test]
        async fn bitwise_not_still_refuses_a_string() {
            let code = "string s; int r; void create() { r = ~s; }";

            assert_eq!(
                messages(code).await,
                ["Invalid Type: `~` `s` (string). Expected `int`"]
            );
        }

        #[tokio::test]
        async fn bitwise_not_takes_a_closure_positional() {
            let code = "function f; void create() { f = (: ~$1 :); }";
            let msgs = messages(code).await;

            assert!(msgs.is_empty(), "{:?}", msgs);
        }
    }

    mod test_visit_label {
        use super::*;

        #[tokio::test]
        async fn disallows_inside_a_closure_within_a_switch() {
            let code = r#"
                void create() {
                    switch (1) {
                        case 1:
                            dump((: case 2: 0; :));
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "invalid `case` statement."
            );
        }

        #[tokio::test]
        async fn disallows_case_outside_of_switch() {
            let code = "void create() { case 12: 1; }";
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(!context.diagnostics.errors().is_empty());
            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "invalid `case` statement."
            );
        }

        #[tokio::test]
        async fn disallows_default_outside_of_switch() {
            let code = "void create() { default: 1; }";
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(!context.diagnostics.errors().is_empty());
            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "invalid `default`."
            );
        }

        #[tokio::test]
        async fn allows_in_switch() {
            let code = r#"
                void create() {
                    int i = 5;
                    switch (i) {
                    case 5:
                        dump("nice!");
                        break;
                    default:
                        dump("weeeeak");
                    }
                }"#;
            let context = walk_code(code).await.expect("failed to parse?");
            assert!(context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_program {
        use super::*;
        use crate::test_support::empty_compilation_context;

        #[tokio::test]
        async fn checks_its_body() {
            let mut node = ProgramNode {
                inherits: vec![],
                body: vec![AstNode::from(VarInitNode {
                    type_: LpcType::String(false),
                    by_ref: false,
                    name: ustr("mapping"),
                    value: None,
                    array: false,
                    global: false,
                    span: None,
                    flags: None,
                })],
            };

            let mut walker = SemanticCheckWalker::new(empty_compilation_context());
            if let Err(e) = walker.visit_program(&mut node).await {
                assert!(e.to_string().contains("is a keyword of the language"));
            } else {
                panic!("did not error?");
            }
        }
    }

    mod test_visit_range {
        use super::*;
        use crate::test_support::empty_compilation_context;

        #[tokio::test]
        async fn allows_ints() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::Var(VarNode::new("foo")))),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::Int(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_non_ints() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::Var(VarNode::new("foo")))),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("foo", LpcType::String(false));
            scopes.current_mut().unwrap().insert(sym);
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(!walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_start_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_end_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::from(456))),
                r: Box::new(None),
                span: None,
            });

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_both_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(None),
                span: None,
            });

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_return {
        use super::*;

        #[tokio::test]
        async fn test_visit_return() {
            let mut void_node = ReturnNode {
                value: None, // indicates a Void return value.
                span: None,
            };

            let mut int_node = ReturnNode {
                value: Some(ExpressionNode::from(100)),
                span: None,
            };

            let void_function_def = create!(
                FunctionDefNode,
                return_type: LpcType::Void,
                name: ustr("foo")
            );

            let int_function_def = create!(
                FunctionDefNode,
                return_type: LpcType::Int(false),
                name: ustr("snuh")
            );

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);

            // return void from void function
            walker.current_function = Some(void_function_def.clone());
            let _ = void_node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());

            // return void from non-void function
            walker.current_function = Some(int_function_def);
            let _ = void_node.visit(&mut walker).await;
            assert!(!walker.context.diagnostics.errors().is_empty());

            walker.context.diagnostics.clear();

            // return int from int function
            let _ = int_node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());

            // return int from void function
            walker.current_function = Some(void_function_def);
            let _ = int_node.visit(&mut walker).await;
            assert!(!walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_0() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(0)),
                span: None,
            };

            let void_function_def = create!(
                FunctionDefNode,
                return_type: LpcType::Void,
                name: ustr("foo")
            );

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            walker.current_function = Some(void_function_def);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_mixed() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(123)),
                span: None,
            };

            let function_def = create!(
                FunctionDefNode,
                return_type: LpcType::Mixed(false),
                name: ustr("foo"),
            );

            let context = CompilationContext::default();

            let mut walker = SemanticCheckWalker::new(context);
            walker.current_function = Some(function_def);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn allows_return_of_differing_type_within_closure() {
            let function_def = create!(
                FunctionDefNode,
                return_type: LpcType::Float(false),
                name: ustr("foo"),
            );

            let mut node = ReturnNode {
                value: Some(ExpressionNode::from("blargh")),
                span: None,
            };

            let mut walker = SemanticCheckWalker::new(CompilationContext::default());

            walker.current_function = Some(function_def);

            let _ = node.visit(&mut walker).await;
            assert!(!walker.context.diagnostics.errors().is_empty());

            walker.context.diagnostics.clear();

            walker.closure_depth += 1;

            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_unary_op {
        use super::*;
        use crate::compiler::ast::unary_op_node::UnaryOperation;

        mod test_negate {
            use super::*;

            #[tokio::test]
            async fn works_allows_valid() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Negate,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::Int(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert!(node.visit(&mut walker).await.is_ok())
            }

            #[tokio::test]
            async fn disallows_invalid() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Negate,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::String(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert!(node.visit(&mut walker).await.is_err());
            }
        }

        mod test_inc {
            use super::*;
            use crate::test_support::empty_compilation_context;

            #[tokio::test]
            async fn allows_vars() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Inc,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::Int(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert_ok!(node.visit(&mut walker).await);
            }

            #[tokio::test]
            async fn disallows_literals() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::from(1)),
                    op: UnaryOperation::Inc,
                    is_post: false,
                    span: None,
                });

                let context = empty_compilation_context();
                let mut walker = SemanticCheckWalker::new(context);
                let result = node.visit(&mut walker).await;
                assert_err!(result.clone());
                assert_eq!(
                    result.unwrap_err().to_string().as_str(),
                    "Invalid operation on `int` literal"
                );
            }
        }

        mod test_dec {
            use super::*;
            use crate::test_support::empty_compilation_context;

            #[tokio::test]
            async fn allows_vars() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Dec,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::Int(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert_ok!(node.visit(&mut walker).await);
            }

            #[tokio::test]
            async fn disallows_literals() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::from(1)),
                    op: UnaryOperation::Dec,
                    is_post: false,
                    span: None,
                });

                let context = empty_compilation_context();
                let mut walker = SemanticCheckWalker::new(context);
                let result = node.visit(&mut walker).await;
                assert_err!(result.clone());
                assert_eq!(
                    result.unwrap_err().to_string().as_str(),
                    "Invalid operation on `int` literal"
                );
            }
        }
    }

    mod test_visit_var {
        use super::*;

        #[tokio::test]
        async fn disallows_closure_arg_vars_outside_of_closures() {
            let mut node = create!(VarNode,name: ustr("$2"));

            let mut walker = SemanticCheckWalker::new(CompilationContext::default());
            let _ = node.visit(&mut walker).await;

            assert_eq!(
                walker
                    .context
                    .diagnostics
                    .errors()
                    .first()
                    .unwrap()
                    .to_string()
                    .as_str(),
                "positional argument variables can only be used within a closure"
            );

            let mut walker = SemanticCheckWalker::new(CompilationContext::default());
            walker.closure_depth = 1;
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_closure_arg_vars_beyond_limit() {
            let mut node = create!(VarNode,name: ustr("$65"));

            let mut walker = SemanticCheckWalker::new(CompilationContext::default());
            walker.closure_depth = 1;

            let _ = node.visit(&mut walker).await;

            assert_eq!(
                walker
                    .context
                    .diagnostics
                    .errors()
                    .first()
                    .unwrap()
                    .to_string()
                    .as_str(),
                "positional argument variables can only be used up to `$64`"
            );

            walker.context.diagnostics.clear();

            let mut node = create!(VarNode,name: ustr("$64"));

            let _ = node.visit(&mut walker).await;
            assert!(walker.context.diagnostics.errors().is_empty());
        }
    }

    mod test_visit_var_init {
        use lpc_rs_core::function_flags::FunctionFlags;

        use super::*;
        use crate::test_support::empty_compilation_context;

        #[tokio::test]
        async fn comparisons_are_int() {
            let code = r#"
                int a = "a" == "b";
                int b = 1.0 < 2.0;
                int c = this_object() != this_object();
                int d = ({ 1 }) == ({ 1 });
                int e = 1 >= 2;
            "#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(
                context.diagnostics.errors().is_empty(),
                "{:?}",
                context.diagnostics.errors()
            );
        }

        #[tokio::test]
        async fn call_other_ignores_a_same_named_variable() {
            let code = r#"
                object copy = "/x"->copy();
                int n = copy->copy();
            "#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(
                context.diagnostics.errors().is_empty(),
                "{:?}",
                context.diagnostics.errors()
            );
        }

        #[tokio::test]
        async fn logical_not_is_int() {
            let code = r#"
                int a = !this_object();
                int b = !"a";
                int c = !({ 1 });
                float f = -1.5;
            "#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert!(
                context.diagnostics.errors().is_empty(),
                "{:?}",
                context.diagnostics.errors()
            );
        }

        #[tokio::test]
        async fn comparison_result_is_not_the_operand_type() {
            let code = r#"
                string s = "a" == "b";
            "#;
            let context = walk_code(code).await.expect("failed to parse?");

            assert_eq!(context.diagnostics.errors().len(), 1);
            assert_eq!(
                context.diagnostics.errors()[0].to_string(),
                "mismatched types: `s` (string) = `\"a\" == \"b\"` (int)"
            );
        }

        #[tokio::test]
        async fn validates_both_sides() {
            let mut node = VarInitNode {
                name: ustr("foo"),
                type_: LpcType::Int(false),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn always_allows_0() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: ustr("foo"),
                value: Some(ExpressionNode::from(0)),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_differing_types() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: ustr("foo"),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker).await;

            assert!(!walker.context.diagnostics.errors().is_empty());
        }

        #[tokio::test]
        async fn disallows_keyword_name() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: ustr("switch"),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker).await;

            if let Err(e) = result {
                assert!(e.to_string().contains("is a keyword of the language"));
            } else {
                panic!("didn't error?")
            }
        }

        #[tokio::test]
        async fn disallows_argv_in_ellipsis_function() {
            let mut node = VarInitNode {
                type_: LpcType::Mixed(true),
                name: ustr("argv"),
                value: Some(ExpressionNode::from(vec![ExpressionNode::from(11)])),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);

            // Fake it, as if we're currently walking a function def
            walker.current_function = Some(FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("moop"),
                parameters: vec![],
                flags: FunctionFlags::default().with_ellipsis(true),
                body: vec![],
                span: None,
            });

            let result = node.visit(&mut walker).await;

            if let Err(e) = result {
                assert!(
                    e.to_string()
                        .contains("redeclaration of `argv` in a function with ellipsis arguments")
                );
            } else {
                panic!("didn't error?")
            }
        }

        #[tokio::test]
        async fn allows_argv_in_non_ellipsis_function() {
            let mut node = VarInitNode {
                type_: LpcType::Mixed(true),
                name: ustr("argv"),
                value: Some(ExpressionNode::from(vec![ExpressionNode::from(11)])),
                array: false,
                global: false,
                span: None,
                flags: None,
                by_ref: false,
            };

            let context = empty_compilation_context();
            let mut walker = SemanticCheckWalker::new(context);

            // Fake it, as if we're currently walking a function def
            walker.current_function = Some(FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("moop"),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            });

            let result = node.visit(&mut walker).await;

            assert!(result.is_ok());
        }
    }

    mod test_expression_type {
        use super::*;

        #[tokio::test]
        async fn a_mixed_right_operand_absorbs() {
            let code = r#"
                void create() {
                    mixed m = 1;
                    string s = 1 + m;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_float_right_operand_promotes() {
            let code = r#"
                void create() {
                    float f = 1 + 1.5;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn an_array_with_a_mixed_operand_takes_any_array() {
            let code = r#"
                void create() {
                    mixed m = 1;
                    int *a = ({ 1 });
                    string *s = a + m;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_ternary_takes_a_literal_zero_on_either_branch() {
            let code = r#"
                void create() {
                    int c = 1;
                    string s = c ? "a" : 0;
                    string t = c ? 0 : "a";
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_ternary_with_a_mixed_branch_is_mixed() {
            let code = r#"
                void create() {
                    int c = 1;
                    mixed m = 1;
                    int i = c ? m : 1;
                    string s = c ? "a" : m;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_call_others_answer_or_zero_is_a_string() {
            let code = r#"
                void create() {
                    object ob;
                    string s = ob ? ob->query_name() : 0;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_union_typed_ternary_is_rejected_by_a_type_missing_a_branch() {
            let code = r#"
                void create() {
                    int c = 1;
                    string s = c ? 1 : "a";
                }"#;
            assert_eq!(
                messages(code).await,
                vec![
                    r#"mismatched types: `s` (string) = `c ? 1 : "a"` (int | string)"#.to_string()
                ]
            );
        }

        #[tokio::test]
        async fn a_union_typed_ternary_is_taken_by_mixed() {
            let code = r#"
                void create() {
                    int c = 1;
                    mixed m = c ? 1 : "a";
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_float_or_int_ternary_is_rejected_by_a_float() {
            let code = r#"
                void create() {
                    int c = 1;
                    float f = c ? 1.5 : 1;
                }"#;
            assert_eq!(
                messages(code).await,
                vec!["mismatched types: `f` (float) = `c ? 1.5 : 1` (int | float)".to_string()]
            );
        }

        #[tokio::test]
        async fn previous_object_answers_either_form() {
            let code = r#"
                void create() {
                    object ob = previous_object();
                    object *all = previous_object(-1);
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_mixed_branch_absorbs_the_other() {
            let code = r#"
                void create() {
                    int c = 1;
                    mixed m = 1;
                    int i = (c ? m : 1) + 1;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_closure_literal_is_a_function() {
            let code = r#"
                void create() {
                    function f = (: 1 :);
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_closure_literal_is_not_an_int() {
            let code = r#"
                void create() {
                    int i = (: 1 :);
                }"#;
            assert_eq!(
                messages(code).await,
                vec!["mismatched types: `i` (int) = `(: 1 :)` (function)".to_string()]
            );
        }

        #[tokio::test]
        async fn a_closure_argument_to_an_int_parameter_is_rejected() {
            let code = r#"
                void g(int x) {}
                void create() {
                    g((: 1 :));
                }"#;
            assert_eq!(
                messages(code).await,
                vec!["unexpected argument type to `g`: function. Expected int.".to_string()]
            );
        }

        #[tokio::test]
        async fn a_closure_returned_from_an_int_function_is_rejected() {
            let code = r#"
                int f() {
                    return (: 1 :);
                }"#;
            assert_eq!(
                messages(code).await,
                vec!["invalid return type function. Expected int.".to_string()]
            );
        }

        #[tokio::test]
        async fn an_int_from_a_string_operation_is_still_rejected() {
            let code = r#"
                void create() {
                    string x = "a";
                    int i = 1 + x;
                }"#;
            assert_eq!(
                messages(code).await,
                vec!["mismatched types: `i` (int) = `1 + x` (string)".to_string()]
            );
        }

        #[tokio::test]
        async fn a_union_typed_index_is_refused() {
            let code = r#"
                int *a = ({ 1, 2 });
                int c = 1;
                int i;
                void create() { i = a[c ? 1 : "x"]; }"#;
            assert_eq!(
                messages(code).await,
                vec![
                    r#"Mismatched types: `a` (int *) [] `c ? 1 : "x"` (int | string)"#.to_string()
                ]
            );
        }
    }

    /// `mixed` is taken in every operand and index position; the runtime
    /// checks the value.
    mod mixed_operands {
        use super::*;

        #[tokio::test]
        async fn a_unary_operator_takes_mixed() {
            let code = r#"
                mixed m = 1;
                mixed a; mixed b; mixed c; mixed d; mixed e;
                void create() {
                    a = -m;
                    b = m++;
                    c = ++m;
                    d = m--;
                    e = --m;
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn an_element_and_a_positional_argument_take_a_step() {
            let code = r#"
                mapping counts = ([ "a": 1 ]);
                mixed *list = ({ 1 });
                function f;
                void create() {
                    counts["a"]++;
                    list[0]--;
                    f = (: $1++ :);
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_mixed_array_is_not_negated() {
            let code = r#"
                mixed *ma = ({ 1 });
                mixed a;
                void create() { a = -ma; }"#;
            assert_eq!(
                messages(code).await,
                vec!["Invalid Type: `-` `ma` (mixed *). Expected `int`, or `float`".to_string()]
            );
        }

        #[tokio::test]
        async fn a_mixed_array_is_not_stepped() {
            let code = r#"
                mixed *ma = ({ 1 });
                void create() { ma++; }"#;
            assert_eq!(
                messages(code).await,
                vec!["Invalid Type: `++` `ma` (mixed *). Expected `int`".to_string()]
            );
        }

        #[tokio::test]
        async fn an_array_and_a_string_take_a_mixed_index() {
            let code = r#"
                mixed m = 1;
                int *a = ({ 1, 2 });
                string s = "ab";
                int i; int c; mixed x;
                void create() {
                    i = a[m];
                    c = s[m];
                    a[m] = 3;
                    a[m] += 1;
                    a[m]++;
                    x = m[i];
                    x = m[s];
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_slice_of_mixed_is_mixed() {
            let code = r#"
                mixed m = "hello";
                mixed *ma = ({ 1, 2 });
                string s; int *a; int *b; mixed x;
                void create() {
                    s = m[1..2];
                    a = m[1..];
                    b = ma[0..1];
                    x = m[..m];
                }"#;
            assert_eq!(messages(code).await, Vec::<String>::new());
        }

        #[tokio::test]
        async fn a_mixed_array_is_not_an_index() {
            let code = r#"
                mixed *ma = ({ 1 });
                int *a = ({ 1, 2 });
                int i;
                void create() { i = a[ma]; }"#;
            assert_eq!(
                messages(code).await,
                vec!["Mismatched types: `a` (int *) [] `ma` (mixed *)".to_string()]
            );
        }

        #[tokio::test]
        async fn a_string_is_still_not_an_array_index() {
            let code = r#"
                int *a = ({ 1, 2 });
                int i;
                void create() { i = a["x"]; }"#;
            assert_eq!(
                messages(code).await,
                vec![r#"Mismatched types: `a` (int *) [] `"x"` (string)"#.to_string()]
            );
        }

        #[tokio::test]
        async fn a_slice_of_an_array_is_not_stepped() {
            let code = r#"
                mixed m = ({ 10, 20, 30 });
                void create() { m[1..2]++; }"#;
            assert_eq!(
                messages(code).await,
                vec!["`++` cannot step a slice: `m [] 1..2`".to_string()]
            );
        }

        #[tokio::test]
        async fn a_slice_of_a_mapping_is_not_stepped() {
            let code = r#"
                mapping mm = ([ 2: 7 ]);
                void create() { mm[1..2]--; }"#;
            assert_eq!(
                messages(code).await,
                vec!["`--` cannot step a slice: `mm [] 1..2`".to_string()]
            );
        }
    }

    mod test_visit_ternary {
        use super::*;

        #[tokio::test]
        async fn differing_branch_types_are_accepted() {
            let mut node = ExpressionNode::from(TernaryNode {
                condition: Box::new(ExpressionNode::from(1)),
                body: Box::new(ExpressionNode::from(1)),
                else_clause: Box::new(ExpressionNode::from("foo")),
                span: None,
            });

            let mut walker = SemanticCheckWalker::new(CompilationContext::default());
            let _ = node.visit(&mut walker).await;

            assert!(walker.context.diagnostics.errors().is_empty());
        }
    }

    mod unreachable {
        use indoc::indoc;

        use super::*;

        async fn warnings(code: &str) -> Vec<String> {
            let context = walk_code(code).await.unwrap();
            context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| e.is_warning())
                .map(|e| e.to_string())
                .collect()
        }

        #[tokio::test]
        async fn the_first_statement_after_a_jump_warns_once_per_list() {
            let code = indoc! { r#"
                int f(int x) {
                    while (x) { break; x++; x--; }
                    do { continue; x++; } while (x);
                    return x;
                    x++;
                    x--;
                }
            "# };
            assert_eq!(warnings(code).await, ["unreachable statement"; 3]);
        }

        #[tokio::test]
        async fn the_warning_points_at_the_statement_and_labels_the_jump() {
            let context = walk_code("void f() {\n    return;\n    1;\n}")
                .await
                .unwrap();
            let warning = context.diagnostics.errors().iter().find(|e| e.is_warning());
            let rendered = warning.unwrap().diagnostic_string();
            assert!(rendered.contains(":3:5"), "{rendered}");
            assert!(rendered.contains("control leaves here"), "{rendered}");
        }

        #[tokio::test]
        async fn a_label_makes_the_rest_reachable() {
            let code = indoc! { r#"
                int f(int x) {
                    switch (x) {
                        case 1: return 1;
                        case 2: x++; break;
                        default: return 3; x++;
                    }
                    return 0;
                }
            "# };
            assert_eq!(warnings(code).await, ["unreachable statement"]);
        }

        #[tokio::test]
        async fn a_closure_body_is_a_list_too() {
            let code = "void f() { function g = (: return 1; 2; :); g(); }";
            assert_eq!(warnings(code).await, ["unreachable statement"]);
        }

        #[tokio::test]
        async fn an_empty_statement_after_a_jump_is_not_a_statement() {
            let code = indoc! { r#"
                int f(int x) {
                    return x;
                    ;
                }
            "# };
            assert!(warnings(code).await.is_empty());
        }
    }

    mod references {
        use super::*;

        async fn errors_of(code: &str) -> Vec<String> {
            let context = walk_code(code).await.unwrap();
            context
                .diagnostics
                .errors()
                .iter()
                .filter(|e| !e.is_warning())
                .map(|e| e.to_string())
                .collect()
        }

        fn assert_one_error(errors: &[String], expected: &str) {
            assert_eq!(errors.len(), 1, "{errors:?}");
            assert!(errors[0].contains(expected), "{}", errors[0]);
        }

        #[tokio::test]
        async fn a_matching_ref_call_is_clean() {
            let errors =
                errors_of("void inc(int ref x) { x++; } void f() { int y; inc(ref y); }").await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn a_ref_parameter_needs_a_ref_argument() {
            let errors = errors_of("void inc(int ref x) { } void f() { int y; inc(y); }").await;
            assert_one_error(
                &errors,
                "argument 1 of `inc` must be passed by reference: `ref y`",
            );
        }

        #[tokio::test]
        async fn a_plain_parameter_refuses_a_ref_argument() {
            let errors = errors_of("void inc(int x) { } void f() { int y; inc(ref y); }").await;
            assert_one_error(&errors, "`inc` does not take argument 1 by reference");
        }

        #[tokio::test]
        async fn a_ref_argument_is_type_checked() {
            let errors =
                errors_of("void inc(int ref x) { } void f() { string s; inc(ref s); }").await;
            assert_one_error(&errors, "unexpected argument type to `inc`");
        }

        #[tokio::test]
        async fn a_ref_parameter_cannot_have_a_default() {
            let errors = errors_of("void inc(int ref x = 3) { }").await;
            assert_one_error(&errors, "a `ref` parameter cannot have a default value");
        }

        #[tokio::test]
        async fn a_ref_parameter_cannot_be_optional() {
            let errors = errors_of("varargs void inc(int ref x) { }").await;
            assert_one_error(&errors, "a `ref` parameter cannot be optional");
        }

        #[tokio::test]
        async fn a_closure_cannot_take_a_ref_parameter() {
            let errors = errors_of("void f() { function g = (: [int ref x] x++ :); }").await;
            assert_one_error(&errors, "a closure cannot take a `ref` parameter");
        }

        #[tokio::test]
        async fn a_pointer_call_cannot_pass_a_ref() {
            let errors = errors_of("void f(function fp) { int y; fp(ref y); }").await;
            assert_one_error(
                &errors,
                "a function pointer cannot take an argument by reference",
            );
        }

        #[tokio::test]
        async fn a_ref_cannot_cross_objects() {
            let errors = errors_of("void f(object o) { int y; o->g(ref y); }").await;
            assert_one_error(&errors, "`ref` cannot cross objects");
            let errors =
                errors_of("void f(object o) { int y; call_other(o, \"g\", ref y); }").await;
            assert_one_error(&errors, "`ref` cannot cross objects");
        }

        #[tokio::test]
        async fn a_spread_of_an_array_is_clean() {
            let errors =
                errors_of("void g(int a, int b) { } void f(int *xs) { g(1, xs...); }").await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn a_mixed_operand_may_be_spread() {
            let errors = errors_of("void g() { } void f(mixed x) { g(x...); }").await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn a_spread_needs_an_array() {
            let errors = errors_of("void g() { } void f(int x) { g(x...); }").await;
            assert_one_error(&errors, "`...` spreads an array, not int");
        }

        #[tokio::test]
        async fn a_spread_skips_the_argument_count_check() {
            let errors = errors_of("void g(int a) { } void f(int *xs) { g(xs...); }").await;
            assert!(errors.is_empty(), "{errors:?}");
            let errors = errors_of("void g(int a) { } void f(int *xs) { g(1, 2, xs...); }").await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn arguments_before_a_spread_are_type_checked() {
            let errors = errors_of(r#"void g(int a) { } void f(int *xs) { g("s", xs...); }"#).await;
            assert_one_error(&errors, "unexpected argument type to `g`");
        }

        #[tokio::test]
        async fn arguments_after_a_spread_are_not_type_checked() {
            let errors =
                errors_of(r#"void g(int a, int b) { } void f(int *xs) { g(xs..., "s"); }"#).await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn a_ref_cannot_follow_a_spread() {
            let errors =
                errors_of("void g(int ref a) { } void f(int *xs) { int y; g(xs..., ref y); }")
                    .await;
            assert_one_error(&errors, "a `ref` argument cannot follow a spread");
        }

        #[tokio::test]
        async fn a_ref_before_a_spread_is_checked_as_usual() {
            let errors =
                errors_of("void g(int ref a) { } void f(int *xs) { int y; g(ref y, xs...); }")
                    .await;
            assert!(errors.is_empty(), "{errors:?}");
            let errors =
                errors_of("void g(int a) { } void f(int *xs) { int y; g(ref y, xs...); }").await;
            assert_one_error(&errors, "`g` does not take argument 1 by reference");
        }

        #[tokio::test]
        async fn catch_and_sizeof_refuse_a_spread() {
            let errors = errors_of("void f(int *xs) { sizeof(xs...); }").await;
            assert_one_error(&errors, "`sizeof` takes one expression, not a spread");
            let errors = errors_of("void f(int *xs) { catch(xs...); }").await;
            assert_one_error(&errors, "`catch` takes one expression, not a spread");
        }

        #[tokio::test]
        async fn call_others_receiver_and_name_cannot_be_spread() {
            let errors = errors_of("void f(mixed *xs) { call_other(xs...); }").await;
            assert_one_error(
                &errors,
                "`call_other`'s receiver and function name cannot be spread",
            );
            let errors =
                errors_of(r#"void f(object o, mixed *xs) { call_other(o, "g", xs...); }"#).await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn a_receiver_call_checks_the_spread_operand() {
            let errors = errors_of("void f(object o, int x) { o->g(x...); }").await;
            assert_one_error(&errors, "`...` spreads an array, not int");
            let errors = errors_of("void f(object o, int *xs) { o->g(1, xs...); }").await;
            assert!(errors.is_empty(), "{errors:?}");
        }

        #[tokio::test]
        async fn pointer_and_chained_calls_may_spread() {
            let errors =
                errors_of("void f(function fp, int *xs) { fp(xs...); fp()(xs...); }").await;
            assert!(errors.is_empty(), "{errors:?}");
            let errors = errors_of("void f(function fp, int x) { fp()(x...); }").await;
            assert_one_error(&errors, "`...` spreads an array, not int");
        }

        #[tokio::test]
        async fn a_partial_application_cannot_take_a_ref() {
            // The grammar refuses it: no ref item in a partial argument list.
            assert!(
                walk_code("void g(int ref x) { } void f() { int y; function p = &g(ref y); }")
                    .await
                    .is_err()
            );
        }

        #[tokio::test]
        async fn a_chained_call_cannot_pass_a_ref() {
            let errors =
                errors_of("mixed get_fp() { return 0; } void f() { int y; get_fp()(ref y); }")
                    .await;
            assert_one_error(
                &errors,
                "a function pointer cannot take an argument by reference",
            );
        }

        #[tokio::test]
        async fn an_efun_ref_position_needs_a_variable() {
            let errors = errors_of(r#"void f() { sscanf("1", "%d", 3); }"#).await;
            assert_one_error(&errors, "argument 3 of `sscanf` must be a variable");
        }

        #[tokio::test]
        async fn a_bare_variable_at_an_efun_ref_position_is_clean() {
            let errors = errors_of(r#"void f() { int n; sscanf("1", "%d", n); }"#).await;
            assert!(errors.is_empty(), "{errors:?}");
        }
    }
}
