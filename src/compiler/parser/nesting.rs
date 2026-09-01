//! The nesting cap: no node in a parsed file is taller than
//! [`MAX_NESTING_DEPTH`]. Every consumer of a tree — the walkers, the
//! derived `Clone`/`Debug`, `Display`, drop glue — recurses once per level,
//! so the bound is enforced where nodes are built: each grammar action that
//! owns children hands its node through [`guard`], which errors instead of
//! building the node that would cross the cap.

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
    /// The node's own span. Blocks, decls, labeled statements, and the
    /// program carry none.
    fn span(self) -> Option<Span> {
        match self {
            Child::Expr(e) => e.span(),
            Child::Stmt(s) => match s {
                AstNode::Block(_)
                | AstNode::Decl(_)
                | AstNode::LabeledStatement(_)
                | AstNode::Program(_)
                | AstNode::NoOp => None,
                AstNode::Break(n) => n.span,
                AstNode::Call(n) => n.span,
                AstNode::Continue(n) => n.span,
                AstNode::DoWhile(n) => n.span,
                AstNode::Expression(e) => e.span(),
                AstNode::For(n) => n.span,
                AstNode::ForEach(n) => n.span,
                AstNode::FunctionDef(n) => n.span,
                AstNode::If(n) => n.span,
                AstNode::Return(n) => n.span,
                AstNode::Switch(n) => n.span,
                AstNode::VarInit(n) => n.span,
                AstNode::While(n) => n.span,
            },
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
/// order. A new node variant fails to compile until it has an arm here; a
/// new child field on an existing node needs an arm and a shape pin in the
/// tests below.
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
#[derive(Debug, PartialEq)]
pub struct TooDeep(pub Option<Span>);

/// Does `root`'s height exceed `max`? Recursion-free, so it is safe on any
/// input; it stops at the first node past the cap, exploring children in
/// source order.
pub fn exceeds(root: Child<'_>, max: usize, fallback: Option<Span>) -> Result<(), TooDeep> {
    let mut stack = vec![(root, 1usize, root.span().or(fallback))];
    let mut children = Vec::new();
    while let Some((node, depth, nearest)) = stack.pop() {
        // The statement-position wrapper is not a level: its expression
        // takes its slot.
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
        Err(TooDeep(at)) => Err(lpc_error!(
            at,
            "code nests too deeply (limit {})",
            MAX_NESTING_DEPTH
        )),
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

    // The guard's message is pinned through the grammar in Task 2.
    #[tokio::test]
    async fn guard_hands_the_node_back() {
        let prog = parsed("mixed f(mixed a) { return a; }").await;
        let prog = guard(prog, None).expect("height 4");
        assert_eq!(height(&prog), 4);
    }
}
