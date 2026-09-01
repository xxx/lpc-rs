//! The nesting cap: no node in a parsed file is taller than
//! [`MAX_NESTING_DEPTH`]. Every consumer of a tree — the walkers, the
//! derived `Clone`/`Debug`, `Display`, drop glue — recurses once per level,
//! so the bound is enforced where nodes are built: each grammar action that
//! owns children hands its node through [`guard`].

use lpc_rs_errors::{LpcError, lpc_error, span::Span};

use crate::{
    compile_time_config::MAX_NESTING_DEPTH,
    compiler::ast::{
        ast_node::{AstNode, SpannedNode},
        block_node::BlockNode,
        call_node::{CallChain, CallNode},
        decl_node::DeclNode,
        do_while_node::DoWhileNode,
        expression_node::ExpressionNode,
        for_each_node::ForEachInit,
        function_def_node::FunctionDefNode,
        function_ptr_node::FunctionPtrReceiver,
        label_node::LabelNode,
        program_node::ProgramNode,
        return_node::ReturnNode,
        var_init_node::VarInitNode,
    },
};

/// A borrowed node of any kind the height walk descends through — the two
/// enums, and every typed node a grammar rule builds before wrapping it.
#[derive(Debug, Clone, Copy)]
pub enum Child<'a> {
    Expr(&'a ExpressionNode),
    Stmt(&'a AstNode),
    Block(&'a BlockNode),
    FunctionDef(&'a FunctionDefNode),
    Decl(&'a DeclNode),
    Call(&'a CallNode),
    VarInit(&'a VarInitNode),
    Label(&'a LabelNode),
    Return(&'a ReturnNode),
    DoWhile(&'a DoWhileNode),
    Program(&'a ProgramNode),
}

macro_rules! child_from {
    ($($variant:ident => $ty:ty),+ $(,)?) => {$(
        impl<'a> From<&'a $ty> for Child<'a> {
            fn from(node: &'a $ty) -> Self {
                Child::$variant(node)
            }
        }
    )+};
}

child_from!(
    Expr => ExpressionNode,
    Stmt => AstNode,
    Block => BlockNode,
    FunctionDef => FunctionDefNode,
    Decl => DeclNode,
    Call => CallNode,
    VarInit => VarInitNode,
    Label => LabelNode,
    Return => ReturnNode,
    DoWhile => DoWhileNode,
    Program => ProgramNode,
);

impl Child<'_> {
    /// The node's own span.
    fn span(self) -> Option<Span> {
        match self {
            Child::Expr(e) => e.span(),
            Child::Stmt(s) => s.span(),
            Child::Block(_) | Child::Decl(_) | Child::Program(_) => None,
            Child::FunctionDef(n) => n.span,
            Child::Call(n) => n.span,
            Child::VarInit(n) => n.span,
            Child::Label(n) => n.span,
            Child::Return(n) => n.span,
            Child::DoWhile(n) => n.span,
        }
    }
}

/// The one statement of which nodes are a node's children, in source
/// order. A new child field on an existing node needs an arm here and a
/// shape pin in the tests below.
fn push_children<'a>(node: Child<'a>, out: &mut Vec<Child<'a>>) {
    match node {
        Child::Expr(e) => match e {
            ExpressionNode::Assignment(n) => {
                out.push(Child::Expr(&n.lhs));
                out.push(Child::Expr(&n.rhs));
            }
            ExpressionNode::BinaryOp(n) => {
                out.push(Child::Expr(&n.l));
                out.push(Child::Expr(&n.r));
            }
            ExpressionNode::Call(n) => push_children(Child::Call(n), out),
            ExpressionNode::Closure(n) => {
                out.extend(n.parameters.iter().flatten().map(Child::VarInit));
                out.extend(n.body.iter().map(Child::Stmt));
            }
            ExpressionNode::CommaExpression(n) => out.extend(n.value.iter().map(Child::Expr)),
            ExpressionNode::FunctionPtr(n) => {
                if let Some(FunctionPtrReceiver::Static(rcvr)) = &n.receiver {
                    out.push(Child::Expr(rcvr));
                }
                out.extend(n.arguments.iter().flatten().flatten().map(Child::Expr));
            }
            ExpressionNode::Range(n) => {
                out.extend(n.l.iter().map(Child::Expr));
                out.extend(n.r.iter().map(Child::Expr));
            }
            ExpressionNode::Ternary(n) => {
                out.push(Child::Expr(&n.condition));
                out.push(Child::Expr(&n.body));
                out.push(Child::Expr(&n.else_clause));
            }
            ExpressionNode::UnaryOp(n) => out.push(Child::Expr(&n.expr)),
            ExpressionNode::Array(n) => out.extend(n.value.iter().map(Child::Expr)),
            ExpressionNode::Mapping(n) => {
                for (key, value) in &n.value {
                    out.push(Child::Expr(key));
                    out.push(Child::Expr(value));
                }
            }
            ExpressionNode::Float(_)
            | ExpressionNode::Int(_)
            | ExpressionNode::String(_)
            | ExpressionNode::Var(_)
            | ExpressionNode::Ref(_) => {}
        },
        Child::Stmt(s) => match s {
            AstNode::Block(n) => push_children(Child::Block(n), out),
            AstNode::Call(n) => push_children(Child::Call(n), out),
            AstNode::Decl(n) => push_children(Child::Decl(n), out),
            AstNode::DoWhile(n) => push_children(Child::DoWhile(n), out),
            // Transparent: `exceeds` unwraps it before it reaches here.
            AstNode::Expression(e) => push_children(Child::Expr(e), out),
            AstNode::For(n) => {
                out.extend(n.initializer.iter().map(Child::Stmt));
                out.extend(n.condition.iter().map(Child::Expr));
                out.extend(n.incrementer.iter().map(Child::Expr));
                out.push(Child::Stmt(&n.body));
            }
            AstNode::ForEach(n) => {
                match &n.initializer {
                    ForEachInit::Array(init) | ForEachInit::String(init) => {
                        out.push(Child::VarInit(init));
                    }
                    ForEachInit::Mapping { key, value } => {
                        out.push(Child::VarInit(key));
                        out.push(Child::VarInit(value));
                    }
                }
                out.push(Child::Expr(&n.collection));
                out.push(Child::Stmt(&n.body));
            }
            AstNode::FunctionDef(n) => push_children(Child::FunctionDef(n), out),
            AstNode::If(n) => {
                out.push(Child::Expr(&n.condition));
                out.push(Child::Stmt(&n.body));
                out.extend(n.else_clause.iter().map(Child::Stmt));
            }
            AstNode::LabeledStatement(n) => {
                out.push(Child::Label(&n.label));
                out.push(Child::Stmt(&n.node));
            }
            AstNode::Program(n) => push_children(Child::Program(n), out),
            AstNode::Return(n) => push_children(Child::Return(n), out),
            AstNode::Switch(n) => {
                out.push(Child::Expr(&n.expression));
                out.push(Child::Stmt(&n.body));
            }
            AstNode::VarInit(n) => push_children(Child::VarInit(n), out),
            AstNode::While(n) => {
                out.push(Child::Expr(&n.condition));
                out.push(Child::Stmt(&n.body));
            }
            AstNode::Break(_) | AstNode::Continue(_) | AstNode::NoOp => {}
        },
        Child::Block(n) => out.extend(n.body.iter().map(Child::Stmt)),
        Child::FunctionDef(n) => {
            out.extend(n.parameters.iter().map(Child::VarInit));
            out.extend(n.body.iter().map(Child::Stmt));
        }
        Child::Decl(n) => out.extend(n.initializations.iter().map(Child::VarInit)),
        Child::Call(n) => {
            match &n.chain {
                CallChain::Root {
                    receiver: Some(rcvr),
                    ..
                } => out.push(Child::Expr(rcvr)),
                CallChain::Root { receiver: None, .. } => {}
                CallChain::Node(inner) => out.push(Child::Call(inner)),
            }
            out.extend(n.arguments.iter().map(Child::Expr));
        }
        Child::VarInit(n) => out.extend(n.value.iter().map(Child::Expr)),
        Child::Label(n) => out.extend(n.case.iter().map(Child::Expr)),
        Child::Return(n) => out.extend(n.value.iter().map(Child::Expr)),
        Child::DoWhile(n) => {
            out.push(Child::Stmt(&n.body));
            out.push(Child::Expr(&n.condition));
        }
        // Inherits are leaves.
        Child::Program(n) => out.extend(n.body.iter().map(Child::Stmt)),
    }
}

/// Where a tree crossed the cap: the first node found more than `max`
/// levels down, or the nearest ancestor with a span when it has none, or
/// the caller's fallback when the whole path is spanless.
#[derive(Debug, PartialEq, Eq)]
pub struct TooDeep(pub Option<Span>);

/// Does `root`'s height exceed `max`? Recursion-free, so it is safe on any
/// input; it stops at the first node past the cap, exploring children in
/// source order.
pub fn exceeds(root: Child<'_>, max: usize, fallback: Option<Span>) -> Result<(), TooDeep> {
    let mut stack = vec![(root, 1usize, root.span().or(fallback))];
    let mut children = Vec::new();
    while let Some((node, depth, nearest)) = stack.pop() {
        // The statement-position wrapper is not a level.
        let node = match node {
            Child::Stmt(AstNode::Expression(e)) => Child::Expr(e),
            other => other,
        };
        if depth > max {
            return Err(TooDeep(nearest));
        }
        children.clear();
        push_children(node, &mut children);
        // Reversed so the first child in source order pops first.
        for child in children.drain(..).rev() {
            stack.push((child, depth + 1, child.span().or(nearest)));
        }
    }
    Ok(())
}

/// The grammar's guard: hand `node` back unless it nests too deeply.
/// `span` is the node's own span as the action knows it — the fallback
/// location for a spanless path.
pub fn guard<T>(node: T, span: Option<Span>) -> Result<T, LpcError>
where
    for<'a> &'a T: Into<Child<'a>>,
{
    match exceeds((&node).into(), MAX_NESTING_DEPTH, span) {
        Ok(()) => Ok(node),
        Err(TooDeep(at)) => {
            Err(
                lpc_error!(at, "code nests too deeply (limit {})", MAX_NESTING_DEPTH)
                    .with_note("statements and expressions nest together; split the construct"),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::lpc_path::LpcPath;
    use lpc_rs_errors::span::Span;

    use super::*;
    use crate::{
        compiler::{Compiler, ast::program_node::ProgramNode},
        test_support::test_config,
    };

    async fn parsed(code: &str) -> ProgramNode {
        Compiler::new(test_config())
            .parse_string(&LpcPath::from("/nest.c"), code)
            .await
            .unwrap_or_else(|e| panic!("{code}: {e}"))
            .0
    }

    /// The smallest cap the program fits under — its height.
    fn height(prog: &ProgramNode) -> usize {
        (1..)
            .find(|&max| exceeds(Child::Program(prog), max, None).is_ok())
            .expect("finite tree")
    }

    async fn height_of(code: &str) -> usize {
        height(&parsed(code).await)
    }

    #[tokio::test]
    async fn expression_children_each_count_a_level() {
        // Program, FunctionDef, Return, then the expression.
        assert_eq!(height_of("mixed f(mixed a) { return a; }").await, 4);
        assert_eq!(height_of("mixed f(mixed a) { return a + a; }").await, 5);
        assert_eq!(
            height_of("mixed f(mixed a) { return a + (a + a); }").await,
            6
        );
        assert_eq!(height_of("mixed f(mixed a) { return ((a)); }").await, 4); // parens are free
        assert_eq!(height_of("mixed f(mixed a) { return a[0]; }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return a[1..2]; }").await, 6); // Index, Range, Int
        assert_eq!(height_of("mixed f(mixed a) { return !a; }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return a++; }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return a ? a : a; }").await, 5);
        assert_eq!(
            height_of("mixed f(mixed a) { return a ? (a ? a : a) : a; }").await,
            6
        );
        assert_eq!(height_of("mixed f(mixed a) { return a = 1; }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return a += 1; }").await, 6); // desugared BinaryOp
        assert_eq!(height_of("mixed f(mixed a) { return (a, a); }").await, 5);
        assert_eq!(
            height_of("mixed f(mixed a) { return (a, (a, a)); }").await,
            6
        );
        assert_eq!(height_of("mixed f(mixed a) { return f(a); }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return a->f(); }").await, 5); // receiver
        assert_eq!(height_of("mixed f(mixed a) { return f(a)(); }").await, 6); // Node, Root, Var
        assert_eq!(height_of("mixed f(mixed a) { return (: 1 :); }").await, 5);
        assert_eq!(
            height_of("mixed f(mixed a) { return (: [int p = 1] 2 :); }").await,
            6
        );
        assert_eq!(height_of("mixed f(mixed a) { return &f(a); }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return &(a)->f(); }").await, 5);
        assert_eq!(height_of("mixed f(mixed a) { return ({ 1 }); }").await, 5);
        assert_eq!(
            height_of("mixed f(mixed a) { return ([ 1 : 2 ]); }").await,
            5
        );
        assert_eq!(
            height_of("mixed f(mixed a) { return ([ ({ 1 }) : 2 ]); }").await,
            6
        );
    }

    #[tokio::test]
    async fn statement_children_each_count_a_level() {
        // Program, FunctionDef, then the statement.
        assert_eq!(height_of("void f(mixed a) { a; }").await, 3); // AstNode::Expression is transparent
        assert_eq!(height_of("void f(mixed a) { { } }").await, 3);
        assert_eq!(height_of("void f(mixed a) { { { } } }").await, 4);
        assert_eq!(height_of("void f(mixed a) { int x = a; }").await, 5); // Decl, VarInit, Var
        assert_eq!(height_of("void f(mixed a) { if (a) {} else {} }").await, 4);
        assert_eq!(
            height_of("void f(mixed a) { if (a) {} else if (a) {} }").await,
            5
        );
        assert_eq!(height_of("void f(mixed a) { while (a) {} }").await, 4);
        assert_eq!(height_of("void f(mixed a) { do {} while (a); }").await, 4);
        assert_eq!(height_of("void f(mixed a) { for (a; a; a) {} }").await, 4);
        assert_eq!(
            height_of("void f(mixed a) { for (int i = 1;;) {} }").await,
            6
        ); // Decl, VarInit, Int
        assert_eq!(height_of("void f(mixed a) { foreach (x : a) {} }").await, 4);
        assert_eq!(
            height_of("void f(mixed a) { foreach (k, v : a) {} }").await,
            4
        );
        assert_eq!(
            height_of("void f(mixed a) { switch (a) { case 1: {} } }").await,
            7
        ); // Switch, Block, Labeled, Label, Int
        assert_eq!(
            height_of("void f(mixed a) { switch (a) { default: {} } }").await,
            6
        );
        assert_eq!(height_of("void f(mixed a) { return; }").await, 3);
        assert_eq!(height_of("void f(int p = 1) {}").await, 4); // FunctionDef, VarInit, Int
        assert_eq!(height_of("int g = 1;").await, 4); // Decl, VarInit, Int
        assert_eq!(height_of("int g; int h;").await, 3);
        assert_eq!(height_of("inherit \"/std/object\";").await, 1);
    }

    #[tokio::test]
    async fn the_offending_span_is_the_deepest_node_in_source_order() {
        let prog = parsed("mixed f(mixed b, mixed c) { return b + c; }").await;
        let TooDeep(at) = exceeds(Child::Program(&prog), 4, None).expect_err("height 5");
        assert_eq!(at.and_then(|s| s.code()).as_deref(), Some("b"));
    }

    #[tokio::test]
    async fn a_spanless_deepest_node_reports_the_nearest_ancestor_with_one() {
        let prog = parsed("void f() { { { } } }").await;
        let TooDeep(at) = exceeds(Child::Program(&prog), 3, None).expect_err("height 4");
        assert_eq!(at.and_then(|s| s.code()).as_deref(), Some("void f"));
    }

    #[tokio::test]
    async fn an_all_spanless_path_reports_the_fallback() {
        let prog = parsed("void f() { { { } } }").await;
        let crate::compiler::ast::ast_node::AstNode::FunctionDef(def) = &prog.body[0] else {
            panic!("expected the function");
        };
        let crate::compiler::ast::ast_node::AstNode::Block(outer) = &def.body[0] else {
            panic!("expected the block");
        };
        let fallback = Span::new(0, 3..4);
        let TooDeep(at) = exceeds(Child::Block(outer), 1, Some(fallback)).expect_err("height 2");
        assert_eq!(at, Some(fallback));
    }

    // The guard's message is pinned by the boundary tests below.
    #[tokio::test]
    async fn guard_hands_the_node_back() {
        let prog = parsed("mixed f(mixed a) { return a; }").await;
        let prog = guard(prog, None).expect("height 4");
        assert_eq!(height(&prog), 4);
    }

    /// One nesting shape at size `n`. Expression shapes sit under
    /// `mixed f(mixed a) { return …; }` (three levels above the expression),
    /// statement shapes under `void f(mixed a) { … }` (two), the last two at
    /// program level.
    fn shape(kind: &str, n: usize) -> String {
        let rep = |s: &str| s.repeat(n);
        let chain = |m: usize| vec!["a"; m].join(" + ");
        let nest = |open: &str, close: &str, core: &str| {
            format!("{}{core}{}", open.repeat(n), close.repeat(n))
        };
        let expr = |x: String| format!("mixed f(mixed a) {{ return {x}; }}");
        let stmt = |s: String| format!("void f(mixed a) {{ {s} }}");
        match kind {
            "binary_left" => expr(chain(n)),
            "binary_right" => expr(format!("{}a{}", "a + (".repeat(n - 1), ")".repeat(n - 1))),
            "index" => expr(format!("a{}", rep("[0]"))),
            "range_l" => expr(nest("a[", "..]", "a")),
            "range_r" => expr(nest("a[..", "]", "a")),
            "unary" => expr(format!("{}a", rep("!"))),
            "postfix_inc" => expr(format!("a{}", rep("++"))),
            "ternary_else" => expr(format!("{}a", rep("a ? a : "))),
            "ternary_body" => expr(nest("a ? ", " : a", "a")),
            "ternary_cond" => expr(nest("(", ") ? a : a", "a")),
            "assign_rhs" => expr(format!("{}1", rep("a = "))),
            "assign_lhs" => expr(format!("a{} = 1", rep("[0]"))),
            "comma" => expr(nest("(a, ", ")", "a")),
            "call_args" => expr(nest("f(", ")", "a")),
            "call_receiver" => expr(format!("a{}", rep("->f()"))),
            "call_node" => expr(format!("f(a){}", "()".repeat(n - 1))),
            "closure_body" => expr(nest("(: ", " :)", "1")),
            "closure_param" => expr(nest("(: [int p = ", "] :)", "1")),
            "fptr_args" => expr(nest("&f(", ")", "1")),
            "fptr_receiver" => expr(nest("&(", ")->f()", "a")),
            "array" => expr(nest("({ ", " })", "1")),
            "mapping_value" => expr(nest("([ 1 : ", " ])", "1")),
            "mapping_key" => expr(nest("([ ", " : 1 ])", "1")),
            "block" => stmt(nest("{ ", " }", "")),
            "if" => stmt(format!("{}{{}}", rep("if (a) "))),
            "elseif" => stmt(format!("if (a) {{}}{}", " else if (a) {}".repeat(n - 1))),
            "while" => stmt(format!("{}{{}}", rep("while (a) "))),
            "do_while" => stmt(format!("{}{{}}{}", rep("do "), rep(" while (a);"))),
            "for" => stmt(format!("{}{{}}", rep("for (;;) "))),
            "foreach" => stmt(format!("{}{{}}", rep("foreach (x : a) "))),
            "switch" => stmt(nest("switch (a) { case 1: ", " }", "{}")),
            "expression_statement" => stmt(format!("{};", chain(n))),
            "decl" => stmt(format!("int x = {};", chain(n))),
            "label_case" => stmt(format!("switch (a) {{ case {}: {{}} }}", chain(n))),
            "for_init_expr" => stmt(format!("for ({};;) {{}}", chain(n))),
            "for_init_decl" => stmt(format!("for (int i = {};;) {{}}", chain(n))),
            "for_cond" => stmt(format!("for (;{};) {{}}", chain(n))),
            "for_inc" => stmt(format!("for (;;{}) {{}}", chain(n))),
            "foreach_collection" => stmt(format!("foreach (x : {}) {{}}", chain(n))),
            "switch_expr" => stmt(format!("switch ({}) {{}}", chain(n))),
            "while_cond" => stmt(format!("while ({}) {{}}", chain(n))),
            "if_cond" => stmt(format!("if ({}) {{}}", chain(n))),
            "do_while_cond" => stmt(format!("do {{}} while ({});", chain(n))),
            "global_decl" => format!("mixed a;\nmixed x = {};", chain(n)),
            "param_default" => format!("mixed a;\nvoid f(mixed p = {}) {{}}", chain(n)),
            other => panic!("unknown shape `{other}`"),
        }
    }

    /// Each shape's largest size whose tree is at most 256 high: program →
    /// function → return are three levels; a `[0]`, `!`, `?:`, `=`, call,
    /// closure, array, or mapping adds one per step above the innermost
    /// atom; a range or a closure parameter adds two; a `switch { case: }`
    /// adds three.
    const BOUNDARIES: &[(&str, usize)] = &[
        ("binary_left", 253),
        ("binary_right", 253),
        ("index", 252),
        ("range_l", 126),
        ("range_r", 126),
        ("unary", 252),
        ("postfix_inc", 252),
        ("ternary_else", 252),
        ("ternary_body", 252),
        ("ternary_cond", 252),
        ("assign_rhs", 252),
        ("assign_lhs", 251),
        ("comma", 252),
        ("call_args", 252),
        ("call_receiver", 252),
        ("call_node", 252),
        ("closure_body", 252),
        ("closure_param", 126),
        ("fptr_args", 252),
        ("fptr_receiver", 252),
        ("array", 252),
        ("mapping_value", 252),
        ("mapping_key", 252),
        ("block", 254),
        ("if", 253),
        ("elseif", 253),
        ("while", 253),
        ("do_while", 253),
        ("for", 253),
        ("foreach", 253),
        ("switch", 84),
        ("expression_statement", 254),
        ("decl", 252),
        ("label_case", 250),
        ("for_init_expr", 253),
        ("for_init_decl", 251),
        ("for_cond", 253),
        ("for_inc", 253),
        ("foreach_collection", 253),
        ("switch_expr", 253),
        ("while_cond", 253),
        ("if_cond", 253),
        ("do_while_cond", 253),
        ("global_decl", 253),
        ("param_default", 253),
    ];

    const TOO_DEEP: &str = "code nests too deeply (limit 256)";

    #[tokio::test]
    async fn every_shape_parses_at_the_cap_and_fails_one_past_it() {
        let compiler = Compiler::new(test_config());
        let path = LpcPath::from("/cap.c");
        for &(kind, ok) in BOUNDARIES {
            compiler
                .parse_string(&path, shape(kind, ok))
                .await
                .unwrap_or_else(|e| panic!("{kind} at {ok}: {e}"));
            let e = compiler
                .parse_string(&path, shape(kind, ok + 1))
                .await
                .map(|_| ())
                .expect_err(&format!("{kind} at {} parsed", ok + 1));
            assert_eq!(e.to_string(), TOO_DEEP, "{kind}");
        }
    }

    #[tokio::test]
    async fn the_error_lands_inside_the_construct() {
        let compiler = Compiler::new(test_config());
        let path = LpcPath::from("/cap.c");

        // A left-deep chain: the first operand.
        let code = shape("binary_left", 254);
        let e = compiler
            .parse_string(&path, &code)
            .await
            .map(|_| ())
            .expect_err("too deep");
        assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("a"));
        assert_eq!(
            e.span().map(|s| s.l()),
            Some(code.find("return a").unwrap() + 7)
        );

        // An `else if` chain: the innermost condition (its block has no span).
        let code = shape("elseif", 254);
        let e = compiler
            .parse_string(&path, &code)
            .await
            .map(|_| ())
            .expect_err("too deep");
        assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("a"));
        assert_eq!(
            e.span().map(|s| s.l()),
            Some(code.rfind("if (a)").unwrap() + 4)
        );

        // Bare blocks all the way down: the nearest ancestor with a span is
        // the function.
        let e = compiler
            .parse_string(&path, shape("block", 255))
            .await
            .map(|_| ())
            .expect_err("too deep");
        assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("void f"));
    }

    /// Shapes that do not type-check at any size (`range_*` need int
    /// bounds; `++` wants an int) stop before codegen; their cost classes
    /// run through `index` and `unary`.
    const NOT_TYPED: &[&str] = &["range_l", "range_r", "postfix_inc"];

    /// The safety property: a debug build on the 16 MiB test thread runs
    /// every walker, codegen included, over each shape at its boundary
    /// without overflowing. Nested closures are the costliest level
    /// (≈12 KB) and the load-bearing row.
    #[tokio::test]
    async fn every_shape_compiles_at_the_cap_without_overflowing() {
        let compiler = Compiler::new(test_config());
        for &(kind, ok) in BOUNDARIES {
            let result = compiler
                .compile_string("/cap.c", shape(kind, ok))
                .await
                .map(|_| ());
            if NOT_TYPED.contains(&kind) {
                if let Err(e) = result {
                    assert_ne!(e.to_string(), TOO_DEEP, "{kind}");
                }
            } else {
                result.unwrap_or_else(|e| panic!("{kind} at {ok}: {e}"));
            }
        }
    }

    #[tokio::test]
    async fn one_past_the_cap_is_a_compile_error_not_an_abort() {
        let compiler = Compiler::new(test_config());
        for kind in [
            "closure_body",
            "call_args",
            "binary_left",
            "elseif",
            "block",
        ] {
            let ok = BOUNDARIES.iter().find(|(k, _)| *k == kind).unwrap().1;
            let e = compiler
                .compile_string("/cap.c", shape(kind, ok + 1))
                .await
                .expect_err(&format!("{kind} compiled"));
            assert_eq!(e.to_string(), TOO_DEEP, "{kind}");
        }
    }

    /// Literal chains fold at parse time and parentheses build no node, so
    /// neither is nesting.
    #[tokio::test]
    async fn folded_literals_and_parentheses_are_flat() {
        let compiler = Compiler::new(test_config());
        let strings = vec!["\"s\""; 5_000].join(" + ");
        let ints = vec!["1"; 5_000].join(" + ");
        let parens = format!("{}a{}", "(".repeat(5_000), ")".repeat(5_000));
        compiler
            .compile_string(
                "/flat.c",
                format!("string s = {strings};\nint i = {ints};\nmixed f(mixed a) {{ return {parens}; }}"),
            )
            .await
            .expect("flat");
    }

    /// A too-deep macro body expanded into code meets the LPC guard at
    /// the use site — the expanded tokens carry the use span.
    #[tokio::test]
    async fn a_too_deep_macro_body_in_code_is_reported_at_the_use() {
        let body = vec!["a"; 257].join(" + ");
        let code = format!("#define A {body}\nmixed f(mixed a) {{ return A; }}\n");
        let e = Compiler::new(test_config())
            .compile_string("/use.c", code)
            .await
            .map(|_| ())
            .expect_err("too deep at the use");
        assert_eq!(e.to_string(), TOO_DEEP);
        assert_eq!(e.span().and_then(|s| s.code()).as_deref(), Some("A"));
    }
}
