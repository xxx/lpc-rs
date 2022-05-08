use if_chain::if_chain;

use crate::{
    ast::{
        assignment_node::AssignmentNode,
        ast_node::{AstNodeTrait, SpannedNode},
        binary_op_node::BinaryOpNode,
        block_node::BlockNode,
        break_node::BreakNode,
        call_node::CallNode,
        continue_node::ContinueNode,
        do_while_node::DoWhileNode,
        expression_node::ExpressionNode,
        for_node::ForNode,
        function_def_node::{FunctionDefNode, ARGV},
        int_node::IntNode,
        label_node::LabelNode,
        program_node::ProgramNode,
        range_node::RangeNode,
        return_node::ReturnNode,
        switch_node::SwitchNode,
        ternary_node::TernaryNode,
        unary_op_node::{UnaryOpNode, UnaryOperation},
        var_init_node::VarInitNode,
        while_node::WhileNode,
    },
    codegen::tree_walker::{ContextHolder, TreeWalker},
    compilation_context::CompilationContext,
    errors::LpcError,
    interpreter::efun::EFUN_PROTOTYPES,
    semantic::{
        lpc_type::LpcType,
        semantic_checks::{
            check_binary_operation_types, check_unary_operation_types, is_keyword, node_type,
        },
    },
    Result,
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
}

impl SemanticCheckWalker {
    pub fn new(context: CompilationContext) -> Self {
        Self {
            context,
            current_function: None,
            valid_jumps: Vec::new(),
            valid_labels: Vec::new(),
        }
    }

    fn allow_jumps(&mut self) {
        self.valid_jumps
            .push((BreakAllowed(true), ContinueAllowed(true)));
    }

    fn allow_breaks(&mut self) {
        self.valid_jumps
            .push((BreakAllowed(true), ContinueAllowed(false)));
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
        !self.valid_jumps.is_empty() && self.valid_jumps.last().unwrap().0 .0
    }

    fn can_continue(&self) -> bool {
        !self.valid_jumps.is_empty() && self.valid_jumps.last().unwrap().1 .0
    }

    fn can_use_labels(&self) -> bool {
        !self.valid_labels.is_empty() && self.valid_labels.last().unwrap().0
    }
}

impl ContextHolder for SemanticCheckWalker {
    fn into_context(self) -> CompilationContext {
        self.context
    }
}

impl TreeWalker for SemanticCheckWalker {
    fn visit_assignment(&mut self, node: &mut AssignmentNode) -> Result<()> {
        node.lhs.visit(self)?;
        node.rhs.visit(self)?;

        let left_type = node_type(&node.lhs, &self.context)?;
        let right_type = node_type(&node.rhs, &self.context)?;

        // The integer 0 is always a valid assignment.
        if left_type.matches_type(right_type)
            || matches!(*node.rhs, ExpressionNode::Int(IntNode { value: 0, .. }))
        {
            Ok(())
        } else {
            let e = LpcError::new(format!(
                "Mismatched types: `{}` ({}) = `{}` ({})",
                node.lhs, left_type, node.rhs, right_type
            ))
            .with_span(node.span);

            self.context.errors.push(e.clone());

            Err(e)
        }
    }

    fn visit_binary_op(&mut self, node: &mut BinaryOpNode) -> Result<()> {
        node.l.visit(self)?;
        node.r.visit(self)?;

        match check_binary_operation_types(node, &self.context) {
            Ok(_) => Ok(()),
            Err(err) => {
                self.context.errors.push(err.clone());
                Err(err)
            }
        }
    }

    fn visit_block(&mut self, node: &mut BlockNode) -> Result<()> {
        self.context.scopes.goto(node.scope_id);

        for stmt in &mut node.body {
            stmt.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    fn visit_break(&mut self, node: &mut BreakNode) -> Result<()> {
        if !self.can_break() {
            let e = LpcError::new("Invalid `break`.".to_string()).with_span(node.span);
            self.context.errors.push(e);

            // non-fatal
        }

        Ok(())
    }

    fn visit_call(&mut self, node: &mut CallNode) -> Result<()> {
        // call_other is not type checked
        if node.receiver.is_some() {
            return Ok(());
        }

        for argument in &mut node.arguments {
            argument.visit(self)?;
        }

        let lookup = self.context.scopes.lookup(&node.name);
        // Check function existence.
        if !self.context.function_prototypes.contains_key(&node.name)
            && !EFUN_PROTOTYPES.contains_key(node.name.as_str())
            // check for function pointers & closures
            && (lookup.is_none() || !lookup.unwrap().type_.matches_type(LpcType::Function(false)))
        {
            let e = LpcError::new(format!("Call to unknown function `{}`", node.name))
                .with_span(node.span);
            self.context.errors.push(e);
            // Non-fatal. Continue.
        }

        // Further checks require access to the function prototype for error messaging.
        let proto_opt = if let Some(prototype) = self.context.function_prototypes.get(&node.name) {
            Some(prototype)
        } else {
            EFUN_PROTOTYPES.get(node.name.as_str())
        };

        if let Some(prototype) = proto_opt {
            let arg_len = node.arguments.len();
            let valid = prototype.arity.is_valid(arg_len);

            // // Check function arity.
            // let minimum = if prototype.flags.varargs() {
            //     0
            // } else {
            //     prototype.num_args - prototype.num_default_args
            // };
            // let valid = (minimum..=prototype.num_args).contains(&arg_len)
            //     || (prototype.flags.ellipsis() && arg_len >= minimum);
            if !valid {
                let e = LpcError::new(format!(
                    "Incorrect argument count in call to `{}`: expected: {}, received: {}",
                    node.name, prototype.arity.num_args, arg_len
                ))
                .with_span(node.span)
                .with_label("Defined here", prototype.span);
                self.context.errors.push(e);
            }

            // Check argument types.
            for (index, ty) in prototype.arg_types.iter().enumerate() {
                if_chain! {
                    if let Some(arg) = node.arguments.get(index);
                    // Literal zero is always allowed
                    if !matches!(arg, ExpressionNode::Int(IntNode { value: 0, .. }));
                    let arg_type = node_type(arg, &self.context)?;
                    if !ty.matches_type(arg_type);
                    then {
                        let e = LpcError::new(format!(
                            "Unexpected argument type to `{}`: {}. Expected {}.",
                            node.name, arg_type, ty
                        ))
                        .with_span(arg.span())
                        .with_label("Declared here", prototype.arg_spans.get(index).cloned());

                        self.context.errors.push(e);
                    }
                }
            }
        }

        Ok(())
    }

    fn visit_continue(&mut self, node: &mut ContinueNode) -> Result<()> {
        if !self.can_continue() {
            let e = LpcError::new("Invalid `continue`.".to_string()).with_span(node.span);
            self.context.errors.push(e);

            // non-fatal
        }

        Ok(())
    }

    /// Visit a `do {} while` loop
    fn visit_do_while(&mut self, node: &mut DoWhileNode) -> Result<()> {
        self.allow_jumps();
        let _ = node.body.visit(self);
        let _ = node.condition.visit(self);

        self.prevent_jumps();
        Ok(())
    }

    fn visit_for(&mut self, node: &mut ForNode) -> Result<()> {
        self.allow_jumps();
        self.context.scopes.goto(node.scope_id);

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
        self.prevent_jumps();
        Ok(())
    }

    fn visit_function_def(&mut self, node: &mut FunctionDefNode) -> Result<()> {
        is_keyword(&node.name)?;

        let visibility_count = node.flags.public() as usize
            + node.flags.private() as usize
            + node.flags.protected() as usize;

        if visibility_count > 1 {
            let e = LpcError::new(
                "multiple visibilities specified. use one of `public`, `private`, or `protected` (or its alias, `static`), or leave unspecified for `public` visibility".to_string()
            ).with_span(node.span);
            self.context.errors.push(e);
        }

        self.context.scopes.goto_function(&node.name)?;
        self.current_function = Some(node.clone());

        for parameter in &mut node.parameters {
            parameter.visit(self)?;
        }

        for expression in &mut node.body {
            expression.visit(self)?;
        }

        self.context.scopes.pop();
        Ok(())
    }

    /// Visit a case label
    fn visit_label(&mut self, node: &mut LabelNode) -> Result<()> {
        if !self.can_use_labels() {
            let msg = if node.is_default() {
                "Invalid `default`."
            } else {
                "Invalid `case` statement."
            };

            let err = LpcError::new(msg).with_span(node.span);
            self.context.errors.push(err);
        }

        if let Some(expr) = &mut node.case {
            expr.visit(self)?;
        }

        Ok(())
    }

    fn visit_program(&mut self, node: &mut ProgramNode) -> Result<()> {
        self.context.scopes.goto_root();

        for expr in &mut node.body {
            expr.visit(self)?;
        }

        Ok(())
    }

    fn visit_range(&mut self, node: &mut RangeNode) -> Result<()> {
        if let Some(expr) = &mut *node.l {
            expr.visit(self)?;
        }

        if let Some(expr) = &mut *node.r {
            expr.visit(self)?;
        }

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
                format!("{}", node)
            } else {
                String::from("0")
            };

            let right_val = if let Some(node) = &*node.r {
                format!("{}", node)
            } else {
                String::from("-1")
            };

            let e = LpcError::new(format!(
                "Invalid range types: `{}` ({}) .. `{}` ({})",
                left_val, left_type, right_val, right_type
            ))
            .with_span(node.span);

            self.context.errors.push(e.clone());

            Err(e)
        }
    }

    fn visit_return(&mut self, node: &mut ReturnNode) -> Result<()> {
        if let Some(expression) = &mut node.value {
            expression.visit(self)?;
        }

        if let Some(function_def) = &self.current_function {
            if let Some(expression) = &node.value {
                if let ExpressionNode::Int(IntNode { value: 0, .. }) = expression {
                    // returning a literal 0 is allowable for any type, including void.
                } else {
                    let return_type = node_type(expression, &self.context)?;

                    if function_def.return_type == LpcType::Void
                        || !function_def.return_type.matches_type(return_type)
                    {
                        let error = LpcError::new(format!(
                            "Invalid return type {}. Expected {}.",
                            return_type, function_def.return_type
                        ))
                        .with_span(node.span)
                        .with_label("Defined here", function_def.span);

                        self.context.errors.push(error);
                    }
                }
            } else if function_def.return_type != LpcType::Void {
                let error = LpcError::new(format!(
                    "Invalid return type {}. Expected {}.",
                    LpcType::Void,
                    function_def.return_type
                ))
                .with_span(node.span)
                .with_label("Defined here", function_def.span);

                self.context.errors.push(error);
            }
        } // else warn?

        Ok(())
    }

    fn visit_switch(&mut self, node: &mut SwitchNode) -> Result<()> {
        self.allow_labels();
        self.allow_breaks();

        node.expression.visit(self)?;
        node.body.visit(self)?;

        self.prevent_jumps();
        self.prevent_labels();

        Ok(())
    }

    fn visit_ternary(&mut self, node: &mut TernaryNode) -> Result<()> {
        let _ = node.condition.visit(self);
        let _ = node.body.visit(self);
        let _ = node.else_clause.visit(self);

        let body_type = node_type(&*node.body, &self.context)?;
        let else_type = node_type(&*node.else_clause, &self.context)?;

        if body_type != else_type {
            let e = LpcError::new(format!(
                "differing types in ternary expression: `{}` and `{}`",
                body_type, else_type
            ))
            .with_span(node.span);
            self.context.errors.push(e);
        }

        Ok(())
    }

    fn visit_unary_op(&mut self, node: &mut UnaryOpNode) -> Result<()> {
        node.expr.visit(self)?;

        match check_unary_operation_types(node, &self.context) {
            Ok(_) => match node.op {
                UnaryOperation::Inc | UnaryOperation::Dec => {
                    if matches!(*node.expr, ExpressionNode::Int(_)) {
                        let err = LpcError::new("Invalid operation on `int` literal");
                        self.context.errors.push(err.clone());
                        Err(err)
                    } else {
                        Ok(())
                    }
                }
                _ => Ok(()),
            },
            Err(err) => {
                self.context.errors.push(err.clone());
                Err(err)
            }
        }
    }

    fn visit_var_init(&mut self, node: &mut VarInitNode) -> Result<()> {
        is_keyword(&node.name)?;

        if_chain! {
            if node.name == ARGV;
            if let Some(FunctionDefNode { flags, span, .. }) = self.current_function;
            if flags.ellipsis();
            then {
                let e = LpcError::new(
                    "Redeclaration of `argv` in a function with ellipsis arguments",
                )
                .with_span(node.span)
                .with_label("Declared here", span);
                self.context.errors.push(e.clone());
                return Err(e);
            }
        }

        if let Some(expression) = &mut node.value {
            expression.visit(self)?;

            let expr_type = node_type(expression, &self.context)?;

            // The integer 0 is always a valid assignment.
            let ret = if node.type_.matches_type(expr_type)
                || matches!(*expression, ExpressionNode::Int(IntNode { value: 0, .. }))
            {
                Ok(())
            } else {
                let e = LpcError::new(format!(
                    "Mismatched types: `{}` ({}) = `{}` ({})",
                    node.name, node.type_, expression, expr_type
                ))
                .with_span(node.span);

                self.context.errors.push(e.clone());

                Err(e)
            };

            return ret;
        }

        Ok(())
    }

    fn visit_while(&mut self, node: &mut WhileNode) -> Result<()> {
        self.allow_jumps();
        node.condition.visit(self)?;
        node.body.visit(self)?;

        self.prevent_jumps();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        apply_walker,
        ast::{ast_node::AstNode, expression_node::ExpressionNode, var_node::VarNode},
        codegen::{
            default_params_walker::DefaultParamsWalker, scope_walker::ScopeWalker,
            semantic_check_walker::SemanticCheckWalker,
        },
        compiler::{compiler_error::CompilerError, Compiler},
        errors,
        semantic::{
            function_prototype::FunctionPrototype, lpc_type::LpcType, scope_tree::ScopeTree,
            symbol::Symbol,
        },
        util::path_maker::LpcPath,
    };
    use claim::*;
    use std::{collections::HashMap, default::Default};

    fn empty_context() -> CompilationContext {
        let mut scopes = ScopeTree::default();
        scopes.push_new();
        CompilationContext {
            scopes,
            ..CompilationContext::default()
        }
    }

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

    fn walk_code(code: &str) -> std::result::Result<CompilationContext, CompilerError> {
        let compiler = Compiler::default();
        let (mut program, context) = compiler
            .parse_string(
                &LpcPath::new_in_game("/my_test.c", "/", "./tests/fixtures/code"),
                code,
            )
            .expect("failed to parse");

        let context = apply_walker!(ScopeWalker, program, context, false);
        let context = apply_walker!(DefaultParamsWalker, program, context, false);
        Ok(apply_walker!(SemanticCheckWalker, program, context, false))
    }

    mod test_visit_assignment {
        use crate::ast::binary_op_node::BinaryOperation;

        use super::*;

        #[test]
        fn validates_both_sides() -> Result<()> {
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
            node.visit(&mut walker)
        }

        #[test]
        fn always_allows_0() -> Result<()> {
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
            node.visit(&mut walker)
        }

        #[test]
        fn disallows_differing_types() {
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
            assert!(node.visit(&mut walker).is_err());
        }

        #[test]
        fn allows_mixed() {
            let mut init_node = VarInitNode {
                type_: LpcType::Mixed(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(324)),
                array: false,
                global: false,
                span: None,
                flags: None,
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
            let _ = init_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_array_items() {
            let mut init_node = VarInitNode {
                type_: LpcType::Int(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(324)),
                array: false,
                global: false,
                span: None,
                flags: None,
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
            let _ = init_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_array_ranges() {
            let mut init_node = VarInitNode {
                type_: LpcType::Int(true),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(vec![324])),
                array: false,
                global: false,
                span: None,
                flags: None,
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
            let _ = init_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());

            let _ = assignment_node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }
    }

    mod test_visit_binary_op {
        use crate::ast::binary_op_node::BinaryOperation;

        use super::*;

        #[test]
        fn validates_both_sides() -> Result<()> {
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
            node.visit(&mut walker)
        }

        #[test]
        fn disallows_differing_types() {
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
            assert!(node.visit(&mut walker).is_err());
        }
    }

    mod test_visit_break {
        use super::*;

        #[test]
        fn disallows_outside_of_loop_or_switch() {
            let code = "void create() { break; }";
            let context = walk_code(code).expect("failed to parse?");

            assert!(!context.errors.is_empty());
            assert_eq!(context.errors[0].to_string(), "Invalid `break`.");
        }

        #[test]
        fn allows_in_while_loop() {
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
            let context = walk_code(code).expect("failed to parse?");

            assert!(context.errors.is_empty());
        }

        #[test]
        fn allows_in_for_loop() {
            let code = r#"
                void create() {
                    for(int i = 0; i < 10; i += 1) {
                        if (i > 5) {
                            break;
                        }
                    }
                }"#;
            let context = walk_code(code).expect("failed to parse?");

            assert!(context.errors.is_empty());
        }

        #[test]
        fn allows_in_do_while_loop() {
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
            let context = walk_code(code).expect("failed to parse?");

            assert!(context.errors.is_empty());
        }

        #[test]
        fn allows_in_switch() {
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
            let context = walk_code(code).expect("failed to parse?");

            assert!(context.errors.is_empty());
        }
    }

    mod test_visit_call {
        use super::*;
        use crate::{
            interpreter::function_type::FunctionArity, semantic::function_flags::FunctionFlags,
        };

        #[test]
        fn allows_known_functions() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![],
                name: "known".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("known"),
                FunctionPrototype {
                    name: "known".into(),
                    return_type: LpcType::Int(false),
                    arity: FunctionArity::default(),
                    arg_types: vec![],
                    span: None,
                    arg_spans: vec![],
                    flags: FunctionFlags::default().with_ellipsis(false),
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_known_efuns() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "dump".to_string(),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_function_pointers() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "my_function_pointer".to_string(),
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("my_function_pointer", LpcType::Function(false));
            scopes.current_mut().unwrap().insert(sym);

            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_mixed_function_pointers() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "my_mixed_function_pointer".to_string(),
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("my_mixed_function_pointer", LpcType::Mixed(false));
            scopes.current_mut().unwrap().insert(sym);

            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn disallows_pointers_to_non_functions() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![ExpressionNode::from(IntNode::new(12))],
                name: "my_non_function_pointer".to_string(),
                span: None,
            });

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let sym = Symbol::new("my_non_function_pointer", LpcType::Int(false));
            scopes.current_mut().unwrap().insert(sym);

            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(!walker.context.errors.is_empty());
            assert_eq!(
                walker.context.errors.first().unwrap().to_string(),
                "Call to unknown function `my_non_function_pointer`"
            );
        }

        #[test]
        fn disallows_unknown_functions() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![],
                name: "unknown".to_string(),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.context.errors.len(), 1);
        }

        #[test]
        fn disallows_incorrect_function_arity() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![],
                name: "dump".to_string(),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.context.errors.len(), 1);
        }

        #[test]
        fn handles_ellipsis_argument_arity() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![
                    ExpressionNode::from("bar.c"),
                    ExpressionNode::from("my_function"),
                    ExpressionNode::from(123),
                    ExpressionNode::from(111),
                    ExpressionNode::from("sha256"),
                ], // `call_other` is specified as having 2 arguments, but we're passing more
                name: "call_other".to_string(),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn handles_varargs_argument_arity() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![],
                name: "my_function".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_function"),
                FunctionPrototype {
                    name: "my_function".into(),
                    return_type: LpcType::Int(false),
                    arity: FunctionArity {
                        num_args: 5,
                        varargs: true,
                        ..FunctionArity::default()
                    },
                    arg_types: vec![
                        LpcType::Int(false),
                        LpcType::Float(false),
                        LpcType::Int(false),
                        LpcType::String(false),
                        LpcType::Int(false),
                    ],
                    span: None,
                    arg_spans: vec![],
                    flags: FunctionFlags::default().with_varargs(true),
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();

            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn understands_argument_defaults() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![],
                name: "my_func".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: "my_func".into(),
                    return_type: LpcType::Int(false),
                    arity: FunctionArity {
                        num_args: 1,
                        num_default_args: 1,
                        ..FunctionArity::default()
                    },
                    arg_types: vec![LpcType::String(false)],
                    span: None,
                    arg_spans: vec![],
                    flags: FunctionFlags::default(),
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn disallows_invalid_arg_types() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![ExpressionNode::from(123)],
                name: "my_func".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: "my_func".into(),
                    return_type: LpcType::Int(false),
                    arity: FunctionArity {
                        num_args: 1,
                        ..FunctionArity::default()
                    },
                    arg_types: vec![LpcType::String(false)],
                    span: None,
                    arg_spans: vec![],
                    flags: FunctionFlags::default(),
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert_eq!(walker.context.errors.len(), 1);
        }

        #[test]
        fn allows_0() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: None,
                arguments: vec![ExpressionNode::from(0)],
                name: "my_func".to_string(),
                span: None,
            });

            let mut function_prototypes = HashMap::new();
            function_prototypes.insert(
                String::from("my_func"),
                FunctionPrototype {
                    name: "my_func".into(),
                    return_type: LpcType::String(false),
                    arity: FunctionArity {
                        num_args: 1,
                        ..FunctionArity::default()
                    },
                    arg_types: vec![LpcType::String(false)],
                    span: None,
                    arg_spans: vec![],
                    flags: FunctionFlags::default(),
                },
            );

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_bad_data_with_call_other() {
            let mut node = ExpressionNode::from(CallNode {
                receiver: Some(Box::new(ExpressionNode::from(23))),
                arguments: vec![],
                name: "dump".to_string(),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());
        }
    }

    mod test_visit_function_def {
        use super::*;
        use crate::{
            ast::{ast_node::AstNode, binary_op_node::BinaryOperation},
            codegen::scope_walker::ScopeWalker,
            semantic::function_flags::FunctionFlags,
        };

        #[test]
        fn handles_scopes() {
            let _global = VarInitNode {
                type_: LpcType::Int(false),
                name: "a".to_string(),
                value: Some(ExpressionNode::from(1)),
                array: false,
                global: true,
                span: None,
                flags: None,
            };

            let param1 = VarInitNode {
                type_: LpcType::String(false),
                name: "a".to_string(),
                value: Some(ExpressionNode::from("foo")),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let param2 = VarInitNode {
                type_: LpcType::Int(true),
                name: "a".to_string(),
                value: Some(ExpressionNode::from(vec![1, 2, 3, 4])),
                array: true,
                global: false,
                span: None,
                flags: None,
            };

            let mut function_def1 = FunctionDefNode {
                return_type: LpcType::Void,
                name: "foo".to_string(),
                parameters: vec![param1],
                flags: FunctionFlags::default(),
                body: vec![AstNode::from(ExpressionNode::BinaryOp(BinaryOpNode {
                    op: BinaryOperation::Add,
                    l: Box::new(ExpressionNode::from("foo")),
                    r: Box::new(ExpressionNode::Var(VarNode {
                        name: "a".to_string(),
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
                name: "snuh".to_string(),
                parameters: vec![param2],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };

            let context = empty_context();
            let mut scope_walker = ScopeWalker::new(context);
            let _ = scope_walker.visit_function_def(&mut function_def1);
            let _ = scope_walker.visit_function_def(&mut function_def2);

            let context = scope_walker.into_context();
            let mut walker = SemanticCheckWalker::new(context);

            let _ = walker.visit_function_def(&mut function_def1);
            let _ = walker.visit_function_def(&mut function_def2);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn disallows_keyword_name() {
            let mut node = FunctionDefNode {
                return_type: LpcType::Void,
                name: "while".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };
            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let result = walker.visit_function_def(&mut node);

            if let Err(e) = result {
                assert!(e.to_string().contains("is a keyword of the language"));
            } else {
                panic!("didn't error?")
            }
        }

        #[test]
        fn disallows_multiple_visibilities() {
            let code = r#"public private void create() { dump("ok"); }"#;
            let context = walk_code(code).expect("failed to parse?");

            assert!(!context.errors.is_empty());
            assert_eq!(
                context.errors[0].to_string(),
                "multiple visibilities specified. use one of `public`, `private`, or `protected` (or its alias, `static`), or leave unspecified for `public` visibility".to_string()
            );
        }
    }

    mod test_visit_label {
        use super::*;

        #[test]
        fn disallows_case_outside_of_switch() {
            let code = "void create() { case 12: 1; }";
            let context = walk_code(code).expect("failed to parse?");

            assert!(!context.errors.is_empty());
            assert_eq!(context.errors[0].to_string(), "Invalid `case` statement.");
        }

        #[test]
        fn disallows_default_outside_of_switch() {
            let code = "void create() { default: 1; }";
            let context = walk_code(code).expect("failed to parse?");

            assert!(!context.errors.is_empty());
            assert_eq!(context.errors[0].to_string(), "Invalid `default`.");
        }

        #[test]
        fn allows_in_switch() {
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
            let context = walk_code(code).expect("failed to parse?");
            assert!(context.errors.is_empty());
        }
    }

    mod test_visit_program {
        use super::*;

        #[test]
        fn checks_its_body() {
            let mut node = ProgramNode {
                inherits: Vec::new(),
                body: vec![AstNode::from(VarInitNode {
                    type_: LpcType::String(false),
                    name: "mapping".to_string(),
                    value: None,
                    array: false,
                    global: false,
                    span: None,
                    flags: None,
                })],
            };

            let mut walker = SemanticCheckWalker::new(empty_context());
            if let Err(e) = walker.visit_program(&mut node) {
                assert!(e.to_string().contains("is a keyword of the language"));
            } else {
                panic!("did not error?");
            }
        }
    }

    mod test_visit_range {
        use super::*;

        #[test]
        fn allows_ints() {
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
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn disallows_non_ints() {
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
            let _ = node.visit(&mut walker);

            println!("{:?}", walker.context.errors);
            assert!(!walker.context.errors.is_empty());
        }

        #[test]
        fn allows_start_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(Some(ExpressionNode::from(456))),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_end_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(Some(ExpressionNode::from(456))),
                r: Box::new(None),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_both_blank() {
            let mut node = ExpressionNode::from(RangeNode {
                l: Box::new(None),
                r: Box::new(None),
                span: None,
            });

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }
    }

    mod test_visit_return {
        use super::*;
        use crate::semantic::function_flags::FunctionFlags;

        #[test]
        fn test_visit_return() {
            let mut void_node = ReturnNode {
                value: None, // indicates a Void return value.
                span: None,
            };

            let mut int_node = ReturnNode {
                value: Some(ExpressionNode::from(100)),
                span: None,
            };

            let void_function_def = FunctionDefNode {
                return_type: LpcType::Void,
                name: "foo".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };

            let int_function_def = FunctionDefNode {
                return_type: LpcType::Int(false),
                name: "snuh".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };

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
            let _ = void_node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());

            // return void from non-void function
            walker.current_function = Some(int_function_def);
            let _ = void_node.visit(&mut walker);
            assert!(!walker.context.errors.is_empty());

            walker.context.errors = vec![];

            // return int from int function
            let _ = int_node.visit(&mut walker);
            assert!(walker.context.errors.is_empty());

            // return int from void function
            walker.current_function = Some(void_function_def);
            let _ = int_node.visit(&mut walker);
            assert!(!walker.context.errors.is_empty());
        }

        #[test]
        fn allows_0() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(0)),
                span: None,
            };

            let void_function_def = FunctionDefNode {
                return_type: LpcType::Void,
                name: "foo".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };

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
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn allows_mixed() {
            let mut node = ReturnNode {
                value: Some(ExpressionNode::from(123)),
                span: None,
            };

            let function_def = FunctionDefNode {
                return_type: LpcType::Mixed(false),
                name: "foo".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            };

            let function_prototypes = HashMap::new();
            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                function_prototypes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            walker.current_function = Some(function_def);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }
    }

    mod test_visit_unary_op {
        use crate::ast::unary_op_node::UnaryOperation;

        use super::*;

        mod test_negate {
            use super::*;

            #[test]
            fn works_allows_valid() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Negate,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::Int(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert!(node.visit(&mut walker).is_ok())
            }

            #[test]
            fn disallows_invalid() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Negate,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::String(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert!(node.visit(&mut walker).is_err());
            }
        }

        mod test_inc {
            use super::*;

            #[test]
            fn allows_vars() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Inc,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::Int(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert_ok!(node.visit(&mut walker));
            }

            #[test]
            fn disallows_literals() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::from(1)),
                    op: UnaryOperation::Inc,
                    is_post: false,
                    span: None,
                });

                let context = empty_context();
                let mut walker = SemanticCheckWalker::new(context);
                let result = node.visit(&mut walker);
                assert_err!(result.clone());
                assert_eq!(
                    result.unwrap_err().to_string().as_str(),
                    "Invalid operation on `int` literal"
                );
            }
        }

        mod test_dec {
            use super::*;

            #[test]
            fn allows_vars() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::Var(VarNode::new("foo"))),
                    op: UnaryOperation::Dec,
                    is_post: false,
                    span: None,
                });

                let context = context_with_var("foo", LpcType::Int(false));
                let mut walker = SemanticCheckWalker::new(context);
                assert_ok!(node.visit(&mut walker));
            }

            #[test]
            fn disallows_literals() {
                let mut node = ExpressionNode::from(UnaryOpNode {
                    expr: Box::new(ExpressionNode::from(1)),
                    op: UnaryOperation::Dec,
                    is_post: false,
                    span: None,
                });

                let context = empty_context();
                let mut walker = SemanticCheckWalker::new(context);
                let result = node.visit(&mut walker);
                assert_err!(result.clone());
                assert_eq!(
                    result.unwrap_err().to_string().as_str(),
                    "Invalid operation on `int` literal"
                );
            }
        }
    }

    mod test_visit_var_init {
        use super::*;
        use crate::semantic::function_flags::FunctionFlags;

        #[test]
        fn validates_both_sides() {
            let mut node = VarInitNode {
                name: "foo".to_string(),
                type_: LpcType::Int(false),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn always_allows_0() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(0)),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(walker.context.errors.is_empty());
        }

        #[test]
        fn disallows_differing_types() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: "foo".to_string(),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let mut scopes = ScopeTree::default();
            scopes.push_new();
            let context = CompilationContext {
                scopes,
                ..CompilationContext::default()
            };

            let mut walker = SemanticCheckWalker::new(context);
            let _ = node.visit(&mut walker);

            assert!(!walker.context.errors.is_empty());
        }

        #[test]
        fn disallows_keyword_name() {
            let mut node = VarInitNode {
                type_: LpcType::String(false),
                name: "switch".to_string(),
                value: Some(ExpressionNode::from(123)),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);
            let result = node.visit(&mut walker);

            if let Err(e) = result {
                assert!(e.to_string().contains("is a keyword of the language"));
            } else {
                panic!("didn't error?")
            }
        }

        #[test]
        fn disallows_argv_in_ellipsis_function() {
            let mut node = VarInitNode {
                type_: LpcType::Mixed(true),
                name: "argv".to_string(),
                value: Some(ExpressionNode::from(vec![ExpressionNode::from(11)])),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);

            // Fake it, as if we're currently walking a function def
            walker.current_function = Some(FunctionDefNode {
                return_type: LpcType::Void,
                name: "moop".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default().with_ellipsis(true),
                body: vec![],
                span: None,
            });

            let result = node.visit(&mut walker);

            if let Err(e) = result {
                assert!(e
                    .to_string()
                    .contains("Redeclaration of `argv` in a function with ellipsis arguments"));
            } else {
                panic!("didn't error?")
            }
        }

        #[test]
        fn allows_argv_in_non_ellipsis_function() {
            let mut node = VarInitNode {
                type_: LpcType::Mixed(true),
                name: "argv".to_string(),
                value: Some(ExpressionNode::from(vec![ExpressionNode::from(11)])),
                array: false,
                global: false,
                span: None,
                flags: None,
            };

            let context = empty_context();
            let mut walker = SemanticCheckWalker::new(context);

            // Fake it, as if we're currently walking a function def
            walker.current_function = Some(FunctionDefNode {
                return_type: LpcType::Void,
                name: "moop".to_string(),
                parameters: vec![],
                flags: FunctionFlags::default(),
                body: vec![],
                span: None,
            });

            let result = node.visit(&mut walker);

            assert!(result.is_ok());
        }
    }

    mod test_visit_ternary {
        use super::*;

        #[test]
        fn disallows_differing_types() {
            let mut node = ExpressionNode::from(TernaryNode {
                condition: Box::new(ExpressionNode::from(1)),
                body: Box::new(ExpressionNode::from(1)),
                else_clause: Box::new(ExpressionNode::from("foo")),
                span: None,
            });

            let mut walker = SemanticCheckWalker::new(CompilationContext::default());
            let _ = node.visit(&mut walker);

            assert!(!walker.context.errors.is_empty());

            assert_eq!(
                walker.context.errors.first().unwrap().to_string().as_str(),
                "differing types in ternary expression: `int` and `string`"
            );
        }
    }
}
