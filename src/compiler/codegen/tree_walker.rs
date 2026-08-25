use async_trait::async_trait;
use lpc_rs_core::ScopeId;
use lpc_rs_errors::Result;

use crate::compiler::{
    ast::{
        array_node::ArrayNode,
        assignment_node::AssignmentNode,
        ast_node::AstNodeTrait,
        binary_op_node::BinaryOpNode,
        block_node::BlockNode,
        break_node::BreakNode,
        call_node::{CallChain, CallNode},
        closure_node::ClosureNode,
        comma_expression_node::CommaExpressionNode,
        continue_node::ContinueNode,
        decl_node::DeclNode,
        do_while_node::DoWhileNode,
        float_node::FloatNode,
        for_each_node::{ForEachInit, ForEachNode},
        for_node::ForNode,
        function_def_node::FunctionDefNode,
        function_ptr_node::{FunctionPtrNode, FunctionPtrReceiver},
        if_node::IfNode,
        inherit_node::InheritNode,
        int_node::IntNode,
        label_node::LabelNode,
        labeled_statement_node::LabeledStatementNode,
        mapping_node::MappingNode,
        program_node::ProgramNode,
        range_node::RangeNode,
        ref_node::RefNode,
        return_node::ReturnNode,
        string_node::StringNode,
        switch_node::SwitchNode,
        ternary_node::TernaryNode,
        unary_op_node::UnaryOpNode,
        var_init_node::VarInitNode,
        var_node::VarNode,
        while_node::WhileNode,
    },
    compilation_context::CompilationContext,
    diagnostics::Diagnostics,
};

pub trait ContextHolder {
    /// Consume this walker, and return its `Context`.
    ///
    /// This is intended for use after a walker has completed processing, and
    /// you're ready to re-take ownership of the context for the next step.
    fn into_context(self) -> CompilationContext;
}

/// One pipeline pass over a program, run by [`apply`].
pub trait Pass: TreeWalker + ContextHolder + Send + Sized {
    /// Wrap `context` for this pass's walk.
    fn new(context: CompilationContext) -> Self;

    /// The diagnostics sink, while this pass still owns the context.
    fn diagnostics_mut(&mut self) -> &mut Diagnostics;
}

/// Run one pass over `program`, applying the pipeline's error policy: a walk
/// error is recorded and returned, and with `fatal`, any recorded error or bug
/// stops the pipeline here (warnings never do). Returns the walker, for its
/// products.
pub async fn apply<P: Pass>(
    program: &mut ProgramNode,
    context: CompilationContext,
    fatal: bool,
) -> Result<P> {
    let mut walker = P::new(context);
    let result = program.visit(&mut walker).await;

    if let Err(e) = result {
        return Err(walker.diagnostics_mut().finish_with(e));
    }

    if fatal && !walker.diagnostics_mut().is_clean() {
        return Err(walker
            .diagnostics_mut()
            .finish()
            .expect_err("not clean, so finish fails"));
    }

    Ok(walker)
}

// The `walk_*` functions below are the single statement of each node's
// child-visit order and error policy. A `visit_*` override that also wants the
// children calls the matching `walk_*` around its own work; the trait defaults
// delegate here. `let _ =` children are deliberate: those nodes keep walking
// their remaining children after a failed one, so one pass can report more
// than one error.

/// Visit each element of an array literal, in order.
pub async fn walk_array<W>(walker: &mut W, node: &mut ArrayNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for node in &mut node.value {
        node.visit(walker).await?;
    }

    Ok(())
}

/// Visit an assignment's left-hand side, then its right.
pub async fn walk_assignment<W>(walker: &mut W, node: &mut AssignmentNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    node.lhs.visit(walker).await?;
    node.rhs.visit(walker).await?;

    Ok(())
}

/// Visit a binary operation's left operand, then its right.
pub async fn walk_binary_op<W>(walker: &mut W, node: &mut BinaryOpNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    node.l.visit(walker).await?;
    node.r.visit(walker).await?;

    Ok(())
}

/// Visit each statement of a block, in order.
pub async fn walk_block<W>(walker: &mut W, node: &mut BlockNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for expr in &mut node.body {
        expr.visit(walker).await?;
    }

    Ok(())
}

/// Dispatch a call node to `visit_call_root` or `visit_call_chain` by its
/// chain kind.
pub async fn walk_call<W>(walker: &mut W, node: &mut CallNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    match &node.chain {
        CallChain::Root { .. } => walker.visit_call_root(node).await,
        CallChain::Node(_) => walker.visit_call_chain(node).await,
    }
}

/// Visit a root call's receiver (if any), then its arguments.
pub async fn walk_call_root<W>(walker: &mut W, node: &mut CallNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let CallChain::Root {
        receiver: Some(rcvr),
        ..
    } = &mut node.chain
    {
        rcvr.visit(walker).await?;
    }

    for argument in &mut node.arguments {
        argument.visit(walker).await?;
    }

    Ok(())
}

/// Visit a chained call's inner call, then the outer arguments.
pub async fn walk_call_chain<W>(walker: &mut W, node: &mut CallNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let CallChain::Node(chain_node) = &mut node.chain {
        chain_node.visit(walker).await?;
    }

    for argument in &mut node.arguments {
        argument.visit(walker).await?;
    }

    Ok(())
}

/// Visit a closure's parameters, then its body.
pub async fn walk_closure<W>(walker: &mut W, node: &mut ClosureNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(parameters) = &mut node.parameters {
        for param in parameters {
            param.visit(walker).await?;
        }
    }

    for expression in &mut node.body {
        expression.visit(walker).await?;
    }

    Ok(())
}

/// Visit each expression of a comma expression, in order.
pub async fn walk_comma_expression<W>(walker: &mut W, node: &mut CommaExpressionNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for expr in &mut node.value {
        let _ = expr.visit(walker).await;
    }

    Ok(())
}

/// Visit each initialization of a declaration, in order.
pub async fn walk_decl<W>(walker: &mut W, node: &mut DeclNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for init in &mut node.initializations {
        init.visit(walker).await?;
    }

    Ok(())
}

/// Visit a `do {} while` loop's body, then its condition.
pub async fn walk_do_while<W>(walker: &mut W, node: &mut DoWhileNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    let _ = node.body.visit(walker).await;
    let _ = node.condition.visit(walker).await;

    Ok(())
}

/// Visit a `for` loop's initializer, condition, body, then incrementer.
pub async fn walk_for<W>(walker: &mut W, node: &mut ForNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(n) = &mut *node.initializer {
        let _ = n.visit(walker).await;
    }
    if let Some(n) = &mut node.condition {
        let _ = n.visit(walker).await;
    }

    let _ = node.body.visit(walker).await;

    if let Some(n) = &mut node.incrementer {
        let _ = n.visit(walker).await;
    }

    Ok(())
}

/// Visit a `foreach` loop's initializer (key, then value, for a mapping),
/// collection, then body.
pub async fn walk_foreach<W>(walker: &mut W, node: &mut ForEachNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    match &mut node.initializer {
        ForEachInit::Array(init) | ForEachInit::String(init) => {
            let _ = init.visit(walker).await;
        }
        ForEachInit::Mapping { key, value } => {
            let _ = key.visit(walker).await;
            let _ = value.visit(walker).await;
        }
    }
    let _ = node.collection.visit(walker).await;
    let _ = node.body.visit(walker).await;

    Ok(())
}

/// Visit a function definition's parameters, then its body.
pub async fn walk_function_def<W>(walker: &mut W, node: &mut FunctionDefNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for parameter in &mut node.parameters {
        parameter.visit(walker).await?;
    }

    for expression in &mut node.body {
        expression.visit(walker).await?;
    }

    Ok(())
}

/// Visit a function pointer's static receiver (if any), then its present
/// arguments.
pub async fn walk_function_ptr<W>(walker: &mut W, node: &mut FunctionPtrNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(FunctionPtrReceiver::Static(rcvr)) = &mut node.receiver {
        rcvr.visit(walker).await?;
    }

    if let Some(args) = &mut node.arguments {
        for argument in args.iter_mut().flatten() {
            argument.visit(walker).await?;
        }
    }

    Ok(())
}

/// Visit an `if` statement's condition, body, then else clause.
pub async fn walk_if<W>(walker: &mut W, node: &mut IfNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    let _ = node.condition.visit(walker).await;
    let _ = node.body.visit(walker).await;
    if let Some(n) = &mut *node.else_clause {
        let _ = n.visit(walker).await;
    }

    Ok(())
}

/// Visit a case label's expression, if it has one.
pub async fn walk_label<W>(walker: &mut W, node: &mut LabelNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(expr) = &mut node.case {
        expr.visit(walker).await?;
    }

    Ok(())
}

/// Visit a labeled statement's label, then its statement.
pub async fn walk_labeled_statement<W>(
    walker: &mut W,
    node: &mut LabeledStatementNode,
) -> Result<()>
where
    W: TreeWalker + Send,
{
    node.label.visit(walker).await?;
    node.node.visit(walker).await
}

/// Visit each key of a mapping literal, then its value, in entry order.
pub async fn walk_mapping<W>(walker: &mut W, node: &mut MappingNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for (key, value) in &mut node.value {
        key.visit(walker).await?;
        value.visit(walker).await?;
    }

    Ok(())
}

/// Visit a program's inherits, then its body.
pub async fn walk_program<W>(walker: &mut W, node: &mut ProgramNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    for expr in &mut node.inherits {
        expr.visit(walker).await?;
    }

    for expr in &mut node.body {
        expr.visit(walker).await?;
    }

    Ok(())
}

/// Visit a range's left bound, then its right, skipping missing sides.
pub async fn walk_range<W>(walker: &mut W, node: &mut RangeNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(expr) = &mut *node.l {
        expr.visit(walker).await?;
    }

    if let Some(expr) = &mut *node.r {
        expr.visit(walker).await?;
    }

    Ok(())
}

/// Visit a return's value, if it has one.
pub async fn walk_return<W>(walker: &mut W, node: &mut ReturnNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(expression) = &mut node.value {
        expression.visit(walker).await?;
    }

    Ok(())
}

/// Visit a `switch` statement's expression, then its body.
pub async fn walk_switch<W>(walker: &mut W, node: &mut SwitchNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    node.expression.visit(walker).await?;
    node.body.visit(walker).await
}

/// Visit a ternary's condition, body, then else clause.
pub async fn walk_ternary<W>(walker: &mut W, node: &mut TernaryNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    let _ = node.condition.visit(walker).await;
    let _ = node.body.visit(walker).await;
    let _ = node.else_clause.visit(walker).await;

    Ok(())
}

/// Visit a unary operation's operand.
pub async fn walk_unary_op<W>(walker: &mut W, node: &mut UnaryOpNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    node.expr.visit(walker).await?;

    Ok(())
}

/// Visit a variable initialization's value, if it has one.
pub async fn walk_var_init<W>(walker: &mut W, node: &mut VarInitNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    if let Some(expr) = &mut node.value {
        expr.visit(walker).await?;
    }

    Ok(())
}

/// Visit a `while` loop's condition, then its body.
pub async fn walk_while<W>(walker: &mut W, node: &mut WhileNode) -> Result<()>
where
    W: TreeWalker + Send,
{
    let _ = node.condition.visit(walker).await;
    let _ = node.body.visit(walker).await;

    Ok(())
}

/// A trait for types that can walk abstract syntax trees
#[async_trait]
pub trait TreeWalker {
    /// Called by the node dispatch layer before visiting a node that owns a
    /// scope (block, closure, do_while, for, foreach, if, while), with the
    /// node's `scope_id`: the scope walker writes a fresh id through it, and
    /// every later pass reads it.
    fn enter_scope(&mut self, _scope_id: &mut Option<ScopeId>) {}

    /// Called after the visit returns — whether it succeeded or not.
    fn exit_scope(&mut self) {}

    /// Visit an array literal node
    async fn visit_array(&mut self, node: &mut ArrayNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_array(self, node).await
    }

    /// Visit an assignment node
    async fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_assignment(self, node).await
    }

    /// Visit a binary operation node
    async fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_binary_op(self, node).await
    }

    /// Visit a code block
    async fn visit_block(&mut self, node: &mut BlockNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_block(self, node).await
    }

    /// Visit a break node
    async fn visit_break(&mut self, _node: &mut BreakNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a function call node
    async fn visit_call(&mut self, node: &mut CallNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_call(self, node).await
    }

    /// Visit a call whose chain is a `CallChain::Root`. `visit_call` (via
    /// `walk_call`) dispatches here; nothing else calls it.
    async fn visit_call_root(&mut self, node: &mut CallNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_call_root(self, node).await
    }

    /// Visit a call whose chain is a `CallChain::Node`. `visit_call` (via
    /// `walk_call`) dispatches here; nothing else calls it.
    async fn visit_call_chain(&mut self, node: &mut CallNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_call_chain(self, node).await
    }

    /// Visit a closure node
    async fn visit_closure(&mut self, node: &mut ClosureNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_closure(self, node).await
    }

    /// Visit a comma expression
    async fn visit_comma_expression(&mut self, node: &mut CommaExpressionNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_comma_expression(self, node).await
    }

    /// Visit a continue node
    async fn visit_continue(&mut self, _node: &mut ContinueNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable declaration node
    async fn visit_decl(&mut self, node: &mut DeclNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_decl(self, node).await
    }

    /// Visit a `do {} while` loop
    async fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_do_while(self, node).await
    }

    /// Visit a float (literal) node
    async fn visit_float(&mut self, _node: &mut FloatNode) -> Result<()> {
        Ok(())
    }

    /// Visit a `for` loop
    async fn visit_for(&mut self, node: &mut ForNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_for(self, node).await
    }

    /// Visit a `foreach` loop
    async fn visit_foreach(&mut self, node: &mut ForEachNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_foreach(self, node).await
    }

    /// Visit a function definition node
    async fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_function_def(self, node).await
    }

    /// Visit a function pointer node
    async fn visit_function_ptr(&mut self, node: &mut FunctionPtrNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_function_ptr(self, node).await
    }

    /// Visit an `if` statement
    async fn visit_if(&mut self, node: &mut IfNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_if(self, node).await
    }

    /// Visit an `inherit` statement
    async fn visit_inherit(&mut self, _node: &mut InheritNode) -> Result<()> {
        Ok(())
    }

    /// Visit an int (literal) node
    async fn visit_int(&mut self, _node: &mut IntNode) -> Result<()> {
        Ok(())
    }

    /// Visit a case label
    async fn visit_label(&mut self, node: &mut LabelNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_label(self, node).await
    }

    /// Visit a labeled statement
    async fn visit_labeled_statement(&mut self, node: &mut LabeledStatementNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_labeled_statement(self, node).await
    }

    /// Visit a mapping literal node
    async fn visit_mapping(&mut self, node: &mut MappingNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_mapping(self, node).await
    }

    /// Visit a program node. This is the top-level translation unit.
    async fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_program(self, node).await
    }

    /// Visit a range literal
    async fn visit_range(&mut self, node: &mut RangeNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_range(self, node).await
    }

    /// Visit a function return node
    async fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_return(self, node).await
    }

    /// Visit a string (literal) node
    async fn visit_string(&mut self, _node: &mut StringNode) -> Result<()> {
        Ok(())
    }

    /// Visit a `switch` statement
    async fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_switch(self, node).await
    }

    /// Visit a ternary expression
    async fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_ternary(self, node).await
    }

    /// Visit a unary operation node
    async fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_unary_op(self, node).await
    }

    /// Visit a variable use node
    async fn visit_var(&mut self, _node: &mut VarNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a by-reference argument
    async fn visit_ref(&mut self, _node: &mut RefNode) -> Result<()>
    where
        Self: Sized,
    {
        Ok(())
    }

    /// Visit a variable initialization node
    async fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_var_init(self, node).await
    }

    /// Visit a `while` loop
    async fn visit_while(&mut self, node: &mut WhileNode) -> Result<()>
    where
        Self: Sized,
    {
        walk_while(self, node).await
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::{
        call_namespace::CallNamespace, function_flags::FunctionFlags, lpc_type::LpcType,
    };
    use lpc_rs_errors::lpc_error;
    use ustr::ustr;

    use super::*;
    use crate::compiler::ast::{
        ast_node::AstNode, decl_node::DeclNode, expression_node::ExpressionNode,
    };

    /// Records every leaf it reaches; the traversing defaults do the walking.
    #[derive(Default)]
    pub(super) struct SpyWalker {
        pub(super) visited: Vec<String>,
        poison: Option<&'static str>,
    }

    #[async_trait]
    impl TreeWalker for SpyWalker {
        fn enter_scope(&mut self, _scope_id: &mut Option<ScopeId>) {
            self.visited.push("enter".into());
        }

        fn exit_scope(&mut self) {
            self.visited.push("exit".into());
        }

        async fn visit_var(&mut self, node: &mut VarNode) -> Result<()> {
            self.visited.push(node.name.to_string());
            if self.poison == Some(node.name.as_str()) {
                return Err(lpc_error!("poisoned"));
            }
            Ok(())
        }

        async fn visit_int(&mut self, node: &mut IntNode) -> Result<()> {
            self.visited.push(node.value.to_string());
            Ok(())
        }

        async fn visit_inherit(&mut self, node: &mut InheritNode) -> Result<()> {
            self.visited.push(node.path.to_string());
            Ok(())
        }
    }

    pub(super) fn var(name: &str) -> ExpressionNode {
        ExpressionNode::Var(VarNode::new(name))
    }

    pub(super) fn stmt(name: &str) -> AstNode {
        AstNode::Expression(var(name))
    }

    pub(super) fn init(name: &str) -> VarInitNode {
        let mut node = VarInitNode::new(name, LpcType::Int(false));
        node.value = Some(var(name));
        node
    }

    pub(super) fn spy() -> SpyWalker {
        SpyWalker::default()
    }

    pub(super) fn poisoned(name: &'static str) -> SpyWalker {
        SpyWalker {
            poison: Some(name),
            ..SpyWalker::default()
        }
    }

    mod child_order {
        use super::*;

        #[tokio::test]
        async fn array_visits_elements_in_order() {
            let mut node = ArrayNode {
                value: vec![var("a"), var("b")],
                span: None,
            };
            let mut walker = spy();
            walker.visit_array(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn assignment_visits_lhs_then_rhs() {
            let mut node = AssignmentNode {
                lhs: var("a").into(),
                rhs: var("b").into(),
                span: None,
            };
            let mut walker = spy();
            walker.visit_assignment(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn binary_op_visits_left_then_right() {
            let mut node = BinaryOpNode {
                l: var("a").into(),
                r: var("b").into(),
                op: crate::compiler::ast::binary_op_node::BinaryOperation::Add,
                span: None,
            };
            let mut walker = spy();
            walker.visit_binary_op(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn block_visits_body_in_order() {
            let mut node = BlockNode::new(vec![stmt("a"), stmt("b")]);
            let mut walker = spy();
            walker.visit_block(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn call_root_visits_receiver_then_arguments() {
            let mut node = CallNode {
                chain: CallChain::Root {
                    receiver: Some(var("r").into()),
                    namespace: CallNamespace::default(),
                    name: ustr("f"),
                },
                arguments: vec![var("a"), var("b")],
                span: None,
            };
            let mut walker = spy();
            walker.visit_call(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["r", "a", "b"]);
        }

        #[tokio::test]
        async fn call_chain_visits_inner_call_then_arguments() {
            let inner = CallNode {
                chain: CallChain::Root {
                    receiver: None,
                    namespace: CallNamespace::default(),
                    name: ustr("f"),
                },
                arguments: vec![var("c")],
                span: None,
            };
            let mut node = CallNode {
                chain: CallChain::Node(Box::new(inner)),
                arguments: vec![var("a")],
                span: None,
            };
            let mut walker = spy();
            walker.visit_call(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["c", "a"]);
        }

        #[tokio::test]
        async fn closure_visits_parameters_then_body() {
            let mut node = ClosureNode {
                name: "closure-0".into(),
                return_type: LpcType::Mixed(false),
                flags: FunctionFlags::default(),
                parameters: Some(vec![init("p")]),
                body: vec![stmt("b1"), stmt("b2")],
                span: None,
                scope_id: None,
            };
            let mut walker = spy();
            walker.visit_closure(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["p", "b1", "b2"]);
        }

        #[tokio::test]
        async fn comma_expression_visits_values_in_order() {
            let mut node = CommaExpressionNode {
                value: vec![var("a"), var("b")],
                span: None,
            };
            let mut walker = spy();
            walker.visit_comma_expression(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn decl_visits_initializations_in_order() {
            let mut node = DeclNode {
                type_: LpcType::Int(false),
                initializations: vec![init("x"), init("y")],
            };
            let mut walker = spy();
            walker.visit_decl(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["x", "y"]);
        }

        #[tokio::test]
        async fn do_while_visits_body_then_condition() {
            let mut node = DoWhileNode::new(stmt("b"), var("c"), None);
            let mut walker = spy();
            walker.visit_do_while(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["b", "c"]);
        }

        #[tokio::test]
        async fn for_visits_initializer_condition_body_incrementer() {
            let mut node = ForNode::new(
                Some(stmt("i")),
                Some(var("c")),
                Some(var("n")),
                stmt("b"),
                None,
            );
            let mut walker = spy();
            walker.visit_for(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["i", "c", "b", "n"]);
        }

        #[tokio::test]
        async fn foreach_visits_initializer_collection_body() {
            let mut node =
                ForEachNode::new(ForEachInit::Array(init("k")), var("l"), stmt("b"), None);
            let mut walker = spy();
            walker.visit_foreach(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["k", "l", "b"]);
        }

        #[tokio::test]
        async fn foreach_mapping_visits_key_then_value_first() {
            let mut node = ForEachNode::new(
                ForEachInit::Mapping {
                    key: init("k"),
                    value: init("v"),
                },
                var("l"),
                stmt("b"),
                None,
            );
            let mut walker = spy();
            walker.visit_foreach(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["k", "v", "l", "b"]);
        }

        #[tokio::test]
        async fn function_def_visits_parameters_then_body() {
            let mut node = FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("f"),
                parameters: vec![init("p")],
                flags: FunctionFlags::default(),
                body: vec![stmt("b")],
                span: None,
            };
            let mut walker = spy();
            walker.visit_function_def(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["p", "b"]);
        }

        #[tokio::test]
        async fn function_ptr_visits_static_receiver_then_present_arguments() {
            let mut node = FunctionPtrNode {
                receiver: Some(FunctionPtrReceiver::Static(var("r").into())),
                arguments: Some(vec![Some(var("a")), None, Some(var("b"))]),
                name: ustr("f"),
                span: None,
            };
            let mut walker = spy();
            walker.visit_function_ptr(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["r", "a", "b"]);
        }

        #[tokio::test]
        async fn if_visits_condition_body_else() {
            let mut node = IfNode::new(var("c"), stmt("t"), Some(stmt("e")), None);
            let mut walker = spy();
            walker.visit_if(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["c", "t", "e"]);
        }

        #[tokio::test]
        async fn label_visits_its_case_expression() {
            let mut node = LabelNode::new(var("a"), None);
            let mut walker = spy();
            walker.visit_label(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a"]);

            let mut default_node = LabelNode::new_default(None);
            let mut walker = spy();
            walker.visit_label(&mut default_node).await.unwrap();
            assert!(walker.visited.is_empty());
        }

        #[tokio::test]
        async fn mapping_visits_each_key_then_its_value() {
            let mut node =
                MappingNode::new(vec![(var("k"), var("v")), (var("k2"), var("v2"))], None);
            let mut walker = spy();
            walker.visit_mapping(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["k", "v", "k2", "v2"]);
        }

        #[tokio::test]
        async fn program_visits_inherits_then_body() {
            let mut node = ProgramNode {
                inherits: vec![InheritNode {
                    path: ustr("/p"),
                    namespace: None,
                    span: None,
                }],
                body: vec![stmt("a")],
            };
            let mut walker = spy();
            walker.visit_program(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["/p", "a"]);
        }

        #[tokio::test]
        async fn range_visits_left_then_right() {
            let mut node = RangeNode::new(Some(var("a")), Some(var("b")), None);
            let mut walker = spy();
            walker.visit_range(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn return_visits_its_value() {
            let mut node = ReturnNode::new(Some(var("a")));
            let mut walker = spy();
            walker.visit_return(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a"]);

            let mut empty = ReturnNode::new(None);
            let mut walker = spy();
            walker.visit_return(&mut empty).await.unwrap();
            assert!(walker.visited.is_empty());
        }

        #[tokio::test]
        async fn switch_visits_expression_then_body() {
            let mut node = SwitchNode::new(var("e"), stmt("b"), None);
            let mut walker = spy();
            walker.visit_switch(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["e", "b"]);
        }

        #[tokio::test]
        async fn ternary_visits_condition_body_else() {
            let mut node = TernaryNode::new(var("c"), var("t"), var("e"), None);
            let mut walker = spy();
            walker.visit_ternary(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["c", "t", "e"]);
        }

        #[tokio::test]
        async fn unary_op_visits_its_expression() {
            let mut node = UnaryOpNode {
                expr: var("a").into(),
                op: crate::compiler::ast::unary_op_node::UnaryOperation::Bang,
                is_post: false,
                span: None,
            };
            let mut walker = spy();
            walker.visit_unary_op(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a"]);
        }

        #[tokio::test]
        async fn var_init_visits_its_value() {
            let mut node = init("a");
            let mut walker = spy();
            walker.visit_var_init(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["a"]);
        }

        #[tokio::test]
        async fn while_visits_condition_then_body() {
            let mut node = WhileNode::new(var("c"), stmt("b"), None);
            let mut walker = spy();
            walker.visit_while(&mut node).await.unwrap();
            assert_eq!(walker.visited, ["c", "b"]);
        }
    }

    mod error_policy {
        use super::*;

        #[tokio::test]
        async fn array_propagates_a_child_error_and_stops() {
            let mut node = ArrayNode {
                value: vec![var("a"), var("b")],
                span: None,
            };
            let mut walker = poisoned("a");
            assert!(walker.visit_array(&mut node).await.is_err());
            assert_eq!(walker.visited, ["a"]);
        }

        #[tokio::test]
        async fn block_propagates_a_child_error_and_stops() {
            let mut node = BlockNode::new(vec![stmt("a"), stmt("b")]);
            let mut walker = poisoned("a");
            assert!(walker.visit_block(&mut node).await.is_err());
            assert_eq!(walker.visited, ["a"]);
        }

        #[tokio::test]
        async fn function_def_propagates_a_parameter_error_and_stops() {
            let mut node = FunctionDefNode {
                return_type: LpcType::Void,
                name: ustr("f"),
                parameters: vec![init("p")],
                flags: FunctionFlags::default(),
                body: vec![stmt("b")],
                span: None,
            };
            let mut walker = poisoned("p");
            assert!(walker.visit_function_def(&mut node).await.is_err());
            assert_eq!(walker.visited, ["p"]);
        }

        #[tokio::test]
        async fn switch_propagates_its_expression_error_and_stops() {
            let mut node = SwitchNode::new(var("e"), stmt("b"), None);
            let mut walker = poisoned("e");
            assert!(walker.visit_switch(&mut node).await.is_err());
            assert_eq!(walker.visited, ["e"]);
        }

        #[tokio::test]
        async fn comma_expression_swallows_a_child_error_and_continues() {
            let mut node = CommaExpressionNode {
                value: vec![var("a"), var("b")],
                span: None,
            };
            let mut walker = poisoned("a");
            assert!(walker.visit_comma_expression(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["a", "b"]);
        }

        #[tokio::test]
        async fn do_while_swallows_a_child_error_and_continues() {
            let mut node = DoWhileNode::new(stmt("b"), var("c"), None);
            let mut walker = poisoned("b");
            assert!(walker.visit_do_while(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["b", "c"]);
        }

        #[tokio::test]
        async fn for_swallows_a_child_error_and_continues() {
            let mut node = ForNode::new(
                Some(stmt("i")),
                Some(var("c")),
                Some(var("n")),
                stmt("b"),
                None,
            );
            let mut walker = poisoned("i");
            assert!(walker.visit_for(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["i", "c", "b", "n"]);
        }

        #[tokio::test]
        async fn foreach_swallows_a_child_error_and_continues() {
            let mut node =
                ForEachNode::new(ForEachInit::Array(init("k")), var("l"), stmt("b"), None);
            let mut walker = poisoned("k");
            assert!(walker.visit_foreach(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["k", "l", "b"]);
        }

        #[tokio::test]
        async fn if_swallows_a_child_error_and_continues() {
            let mut node = IfNode::new(var("c"), stmt("t"), Some(stmt("e")), None);
            let mut walker = poisoned("c");
            assert!(walker.visit_if(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["c", "t", "e"]);
        }

        #[tokio::test]
        async fn ternary_swallows_a_child_error_and_continues() {
            let mut node = TernaryNode::new(var("c"), var("t"), var("e"), None);
            let mut walker = poisoned("c");
            assert!(walker.visit_ternary(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["c", "t", "e"]);
        }

        #[tokio::test]
        async fn while_swallows_a_child_error_and_continues() {
            let mut node = WhileNode::new(var("c"), stmt("b"), None);
            let mut walker = poisoned("c");
            assert!(walker.visit_while(&mut node).await.is_ok());
            assert_eq!(walker.visited, ["c", "b"]);
        }
    }
}

#[cfg(test)]
mod scope_hook_tests {
    use lpc_rs_core::{function_flags::FunctionFlags, lpc_type::LpcType};

    use super::{tests::*, *};
    use crate::compiler::ast::{ast_node::AstNode, expression_node::ExpressionNode};

    #[tokio::test]
    async fn a_block_is_entered_and_exited_around_its_children() {
        let mut node = AstNode::Block(BlockNode::new(vec![stmt("a"), stmt("b")]));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "a", "b", "exit"]);
    }

    #[tokio::test]
    async fn a_closure_is_entered_and_exited_around_its_children() {
        let mut node = ExpressionNode::Closure(ClosureNode {
            name: "closure-0".into(),
            return_type: LpcType::Mixed(false),
            flags: FunctionFlags::default(),
            parameters: Some(vec![init("p")]),
            body: vec![stmt("b1")],
            span: None,
            scope_id: None,
        });
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "p", "b1", "exit"]);
    }

    #[tokio::test]
    async fn a_do_while_is_entered_and_exited_around_its_children() {
        let mut node = AstNode::DoWhile(DoWhileNode::new(stmt("b"), var("c"), None));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "b", "c", "exit"]);
    }

    #[tokio::test]
    async fn a_for_is_entered_and_exited_around_its_children() {
        let mut node = AstNode::For(ForNode::new(
            Some(stmt("i")),
            Some(var("c")),
            Some(var("n")),
            stmt("b"),
            None,
        ));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "i", "c", "b", "n", "exit"]);
    }

    #[tokio::test]
    async fn a_foreach_is_entered_and_exited_around_its_children() {
        let mut node = AstNode::ForEach(Box::new(ForEachNode::new(
            ForEachInit::Array(init("k")),
            var("l"),
            stmt("b"),
            None,
        )));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "k", "l", "b", "exit"]);
    }

    #[tokio::test]
    async fn an_if_is_entered_and_exited_around_its_children() {
        let mut node = AstNode::If(IfNode::new(var("c"), stmt("t"), Some(stmt("e")), None));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "c", "t", "e", "exit"]);
    }

    #[tokio::test]
    async fn a_while_is_entered_and_exited_around_its_children() {
        let mut node = AstNode::While(WhileNode::new(var("c"), stmt("b"), None));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["enter", "c", "b", "exit"]);
    }

    #[tokio::test]
    async fn an_unscoped_node_fires_no_hooks() {
        let mut node = AstNode::Switch(SwitchNode::new(var("e"), stmt("b"), None));
        let mut walker = spy();
        node.visit(&mut walker).await.unwrap();
        assert_eq!(walker.visited, ["e", "b"]);
    }

    #[tokio::test]
    async fn exit_fires_even_when_a_child_errors() {
        let mut node = AstNode::Block(BlockNode::new(vec![stmt("a"), stmt("b")]));
        let mut walker = poisoned("a");
        assert!(node.visit(&mut walker).await.is_err());
        assert_eq!(walker.visited, ["enter", "a", "exit"]);
    }
}

#[cfg(test)]
mod apply_tests {
    use lpc_rs_errors::{lpc_error, lpc_warning};

    use super::*;

    macro_rules! stub_pass {
        ($name:ident, $body:expr) => {
            #[derive(Debug)]
            struct $name {
                context: CompilationContext,
            }

            #[async_trait]
            impl TreeWalker for $name {
                async fn visit_program(&mut self, _node: &mut ProgramNode) -> Result<()> {
                    let visit: fn(&mut $name) -> Result<()> = $body;
                    visit(self)
                }
            }

            impl ContextHolder for $name {
                fn into_context(self) -> CompilationContext {
                    self.context
                }
            }

            impl Pass for $name {
                fn new(context: CompilationContext) -> Self {
                    Self { context }
                }

                fn diagnostics_mut(&mut self) -> &mut Diagnostics {
                    &mut self.context.diagnostics
                }
            }
        };
    }

    stub_pass!(CleanPass, |_me| Ok(()));
    stub_pass!(FailingPass, |me| {
        Err(me.context.diagnostics.fail(lpc_error!("boom")))
    });
    stub_pass!(RecordingPass, |me| {
        me.context.diagnostics.record(lpc_error!("recorded"));
        Ok(())
    });
    stub_pass!(WarningPass, |me| {
        me.context.diagnostics.record(lpc_warning!("advisory"));
        Ok(())
    });

    #[tokio::test]
    async fn fatal_passes_a_clean_walk_through() {
        let mut program = ProgramNode::default();
        let walker: CleanPass = apply(&mut program, CompilationContext::default(), true)
            .await
            .unwrap();
        assert!(walker.into_context().diagnostics.errors().is_empty());
    }

    #[tokio::test]
    async fn a_walk_error_is_finished_and_returned() {
        let mut program = ProgramNode::default();
        let result: Result<FailingPass> =
            apply(&mut program, CompilationContext::default(), false).await;
        assert_eq!(result.unwrap_err().message(), "boom");
    }

    #[tokio::test]
    async fn fatal_stops_on_a_recorded_error() {
        let mut program = ProgramNode::default();
        let result: Result<RecordingPass> =
            apply(&mut program, CompilationContext::default(), true).await;
        assert_eq!(result.unwrap_err().message(), "recorded");
    }

    #[tokio::test]
    async fn lenient_flows_through_with_diagnostics_intact() {
        let mut program = ProgramNode::default();
        let walker: RecordingPass = apply(&mut program, CompilationContext::default(), false)
            .await
            .unwrap();
        let context = walker.into_context();
        assert_eq!(context.diagnostics.errors().len(), 1);
    }

    #[tokio::test]
    async fn fatal_lets_warnings_through() {
        let mut program = ProgramNode::default();
        let walker: WarningPass = apply(&mut program, CompilationContext::default(), true)
            .await
            .unwrap();
        assert_eq!(walker.into_context().diagnostics.errors().len(), 1);
    }
}
