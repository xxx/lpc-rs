//! `parse_string`, end to end: the result shape, actions and their
//! blocking, the memo, `nomatch`, the errors.

use indoc::indoc;

use super::{fails, run, s};
use crate::interpreter::lpc_ref::LpcRef;

/// `doc/efun/parse_string.md`'s example, verbatim.
const EXPRESSION: &str = indoc! { r#"
    string grammar = "
        whitespace = /[ \t]+/
        number = /[0-9]+/
        Expr: Term
        Expr: Expr '+' Term ? add
        Expr: Expr '-' Term ? subtract
        Term: Factor
        Term: Term '*' Factor ? multiply
        Factor: number ? value
        Factor: '(' Expr ')' ? group
    ";

    mixed *value(mixed *tree) { int n; sscanf(tree[0], "%d", n); return ({ n }); }
    mixed *add(mixed *tree) { return ({ tree[0] + tree[2] }); }
    mixed *subtract(mixed *tree) { return ({ tree[0] - tree[2] }); }
    mixed *multiply(mixed *tree) { return ({ tree[0] * tree[2] }); }
    mixed *group(mixed *tree) { return ({ tree[1] }); }

    mixed *evaluate(string s) { return parse_string(grammar, s); }
"# };

#[tokio::test]
async fn the_doc_example_evaluates_an_expression() {
    let main = format!("{EXPRESSION}\nmixed *create() {{ return evaluate(\"2 + 3 * (4 - 1)\"); }}");
    assert_eq!(run("", &[], &main).await, vec![LpcRef::from(11)]);
}

#[tokio::test]
async fn without_actions_the_result_is_the_flat_token_list() {
    let r = run("", &[], indoc! { r#"
        mixed *create() { return parse_string("whitespace = /[ ]+/ w = /[a-z]+/ S: w T T: w w", "a b c"); }
    "# }).await;
    assert_eq!(r, vec![s("a"), s("b"), s("c")]);
}

#[tokio::test]
async fn an_actions_array_is_spliced_and_a_nested_array_stays_nested() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed *two(mixed *t) { return ({ "x", "y" }); }
        mixed *nested(mixed *t) { return ({ ({ 1 }) }); }
        mixed *create() {
            mixed *flat = parse_string("w = /a/ S: T T T: w ? two", "aa");
            mixed *deep = parse_string("w = /a/ S: w ? nested", "a");
            return ({ sizeof(flat), flat[3], sizeof(deep), arrayp(deep[0]) });
        }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![LpcRef::from(4), s("y"), LpcRef::from(1), LpcRef::from(1)]
    );
}

#[tokio::test]
async fn a_blocked_derivation_yields_to_the_next() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed reject(mixed *t) { return 0; }
        mixed *keep(mixed *t) { return ({ "kept" }); }
        mixed *create() { return parse_string("w = /a/ S: A ? reject S: B ? keep A: w B: w", "a"); }
    "# },
    )
    .await;
    assert_eq!(r, vec![s("kept")]);
}

#[tokio::test]
async fn a_missing_action_function_blocks() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed *create() { return ({ parse_string("w = /a/ S: w ? nowhere", "a") }); }
    "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0)]);
}

#[tokio::test]
async fn an_action_error_propagates_as_itself() {
    let e = fails(
        "",
        &[],
        indoc! { r#"
        mixed *boom(mixed *t) { throw("boom"); return t; }
        mixed *create() { return parse_string("w = /a/ S: w ? boom", "a"); }
    "# },
    )
    .await;
    assert!(e.contains("boom"), "{e}");
    assert!(!e.contains("parse_string:"), "{e}");
}

#[tokio::test]
async fn a_subtree_shared_by_two_derivations_runs_its_action_once() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        int calls;
        mixed reject(mixed *t) { return 0; }
        mixed *keep(mixed *t) { return t; }
        mixed *count(mixed *t) { calls++; return t; }
        mixed *create() {
            parse_string("w = /a/ S: X ? reject S: X ? keep X: w ? count", "a");
            return ({ calls });
        }
    "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1)]);
}

#[tokio::test]
async fn a_blocked_child_blocks_its_parent() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        int calls;
        mixed reject(mixed *t) { calls++; return 0; }
        mixed *keep(mixed *t) { return t; }
        mixed *keep2(mixed *t) { return t; }
        mixed *create() {
            mixed first = parse_string("w = /a/ S: X ? keep X: w ? reject", "a");
            mixed second = parse_string(
                "w = /a/ S: X ? keep S: X ? keep2 X: w ? reject", "a"
            );
            return ({ first, second, calls });
        }
    "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(0), LpcRef::from(2)]);
}

#[tokio::test]
async fn nothing_parsing_and_untokenizable_input_return_zero() {
    let r = run("", &[], indoc! { r#"
        mixed *create() {
            return ({ parse_string("w = /[a-z]+/ S: w w", "a"), parse_string("w = /[a-z]+/ S: w w", "a b") });
        }
    "# }).await;
    assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(0)]);
}

#[tokio::test]
async fn a_nomatch_run_is_a_token() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed *create() { return parse_string("w = /[a-z]+/ rest = nomatch S: w rest", "ab!?"); }
    "# },
    )
    .await;
    assert_eq!(r, vec![s("ab"), s("!?")]);
}

#[tokio::test]
async fn alternatives_other_than_zero_are_rejected() {
    for value in ["1", "-1"] {
        let e = fails(
            "",
            &[],
            &format!(
                "mixed *create() {{ return parse_string(\"w = /a/ S: w\", \"a\", {value}); }}"
            ),
        )
        .await;
        assert!(
            e.contains("parse_string: alternatives are not supported"),
            "{e}"
        );
    }
}

#[tokio::test]
async fn zero_alternatives_is_the_default() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed *create() { return parse_string("w = /a/ S: w", "a", 0); }
    "# },
    )
    .await;
    assert_eq!(r, vec![s("a")]);
}

#[tokio::test]
async fn a_malformed_grammar_is_a_prefixed_error() {
    let e = fails(
        "",
        &[],
        indoc! { r#"
        mixed *create() { return parse_string("w = ", "a"); }
    "# },
    )
    .await;
    assert!(
        e.contains("parse_string: Rule 1: regular expression expected"),
        "{e}"
    );
}

/// `E: E E` over n words attempts about n³/6 chart adds; 200 words pass
/// `parse_string::LIMITS.max_steps` (2²⁰) while building the chart.
#[tokio::test]
async fn an_exhausted_budget_is_a_prefixed_error() {
    let e = fails(
        "",
        &[],
        indoc! { r#"
        mixed *create() {
            string input = "a";
            int i;
            for (i = 0; i < 200; i++) input += " a";
            return parse_string("whitespace = /[ ]+/ w = /a/ E: E E E: w", input);
        }
    "# },
    )
    .await;
    assert!(e.contains("parse_string: parse budget exhausted"), "{e}");
}

#[tokio::test]
async fn an_action_may_call_parse_string() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed *inner(mixed *t) { return parse_string("w = /[a-z]+/ S: w", t[0]); }
        mixed *create() { return parse_string("w = /[a-z]+/ S: w ? inner", "nested"); }
    "# },
    )
    .await;
    assert_eq!(r, vec![s("nested")]);
}

/// An action taken from another object's function pointer runs in that
/// object, not in the caller nor `TaskContext::process`'s original owner.
const ACTOR: (&str, &str) = (
    "/actor.c",
    indoc! { r#"
        mixed *act(mixed *t) { return ({ file_name(this_object()) }); }
        mixed *go() { return parse_string("w = /a/ S: w ? act", "a"); }
        function get_go() { return &go(); }
    "# },
);

#[tokio::test]
async fn an_action_runs_in_the_pointers_owner_not_the_caller() {
    let r = run(
        "",
        &[ACTOR],
        indoc! { r#"
        mixed create() {
            function f = "/actor"->get_go();
            return f();
        }
    "# },
    )
    .await;
    assert_eq!(r, vec![s("/actor")]);
}

/// A grammar whose derivation is one nonterminal level per `a`, with an
/// action at every level, over `n` of them. Left-recursive — a
/// right-recursive list's n²/2 chart items pass the step budget long before
/// the depth limit.
fn list_main(n: usize) -> String {
    format!(
        r#"
        string grammar = "word = /a/ List: List word ? act List: word ? act";
        mixed *act(mixed *t) {{ return ({{ sizeof(t) }}); }}
        mixed *create() {{
            string input = "a";
            int i;
            for (i = 1; i < {n}; i++) input += "a";
            return parse_string(grammar, input);
        }}
        "#
    )
}

/// Run `f` to completion on a current-thread runtime on a thread with half
/// the 2 MiB stack tokio's workers get.
fn on_a_1_mib_stack<T: Send + 'static>(
    f: impl std::future::Future<Output = T> + Send + 'static,
) -> T {
    std::thread::Builder::new()
        .stack_size(1 << 20)
        .spawn(move || {
            tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .expect("a current-thread runtime")
                .block_on(f)
        })
        .expect("a thread")
        .join()
        .expect("the thread finished")
}

#[test]
fn a_list_at_the_default_depth_evaluates_within_a_1_mib_stack() {
    let main = list_main(crate::command::grammar::DEFAULT_MAX_DEPTH);
    let r = on_a_1_mib_stack(async move { run("", &[], &main).await });
    // Every level above the bottom folds `({ child, "a" })` to `({ 2 })`.
    assert_eq!(r, vec![LpcRef::from(2)]);
}

#[test]
fn a_list_one_past_the_default_depth_is_too_deep() {
    let main = list_main(crate::command::grammar::DEFAULT_MAX_DEPTH + 1);
    let e = on_a_1_mib_stack(async move { fails("", &[], &main).await });
    assert!(e.contains("parse_string: parse deeper than 4096"), "{e}");
}
