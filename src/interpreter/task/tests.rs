use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use indexmap::IndexMap;
use indoc::indoc;
use lpc_rs_core::{LpcFloatInner, LpcIntInner, register::RegisterVariant};
use tokio::sync::mpsc;
use ustr::ustr;

use super::*;
use crate::{
    interpreter::{
        CommittedReader, lpc_ref::LpcRef, process::Process, vm::global_state::GlobalState,
    },
    test_support::{initialize_program, run_prog, try_run_prog, try_run_prog_with_config},
};

/// Committed global values by name, read through the committer.
fn committed_globals_by_name(gs: &Arc<GlobalState>, proc: &Process) -> HashMap<String, LpcRef> {
    proc.program
        .global_variables
        .iter()
        .filter_map(|(name, sym)| {
            let RegisterVariant::Global(reg) = sym.location? else {
                return None;
            };
            Some((name.clone(), gs.committed_global(proc, reg.index())))
        })
        .collect()
}

/// Run `code` and assert each named global's committed value.
async fn check_committed_globals(code: &str, expected: &[(&str, BareVal)]) {
    let task = run_prog(code).await;
    let gs = &task.context.global_state;
    let globals = committed_globals_by_name(gs, task.context.process());
    for (name, value) in expected {
        let actual = globals
            .get(*name)
            .unwrap_or_else(|| panic!("no global named `{name}`"));
        value.assert_equal(gs, actual);
    }
}

/// Run `code` and assert named variables in the last frame popped
/// (init-globals for file-scope code, the last user function otherwise).
async fn check_popped_vars(code: &str, expected: &[(&str, BareVal)]) {
    let task = run_prog(code).await;
    let frame = task.popped_frame.as_ref().expect("a frame was popped");
    let vars = frame.local_variables(task.context.txn());
    assert_named_vars(&task.context.global_state, &vars, expected);
}

/// Run `code` (which must call `debug("snapshot_stack")`) and assert named
/// variables in the snapshotted frame that made the call.
async fn check_snapshot_vars(code: &str, expected: &[(&str, BareVal)]) {
    let mut task = run_prog(code).await;
    let snapshot = &mut task.snapshots.pop().unwrap();
    snapshot.pop(); // pop off the init frame
    let frame = snapshot.pop().unwrap();
    let vars = frame.local_variables(task.context.txn());
    assert_named_vars(&task.context.global_state, &vars, expected);
}

/// Assert each `(name, value)` pair matches some variable of that name.
fn assert_named_vars(
    gs: &Arc<GlobalState>,
    vars: &[crate::interpreter::call_frame::LocalVariable],
    expected: &[(&str, BareVal)],
) {
    for (name, value) in expected {
        let found = vars.iter().filter(|v| &v.name == name).collect::<Vec<_>>();
        assert!(
            found.iter().any(|v| value.equal_to_lpc_ref(gs, &v.value)),
            "name: {name}, expected: {value}, found: {:?}",
            found.iter().map(|v| &v.value).collect::<Vec<_>>()
        );
    }
}

#[allow(dead_code)]
fn format_slice<I>(slice: &[I]) -> String
where
    I: Display,
{
    let mut ret = String::new();
    ret.push_str("[\n");

    for i in slice {
        ret.push_str(&format!("  {i},\n"));
    }

    ret.push(']');

    ret
}

#[allow(dead_code)]
fn format_map<'a, M, K, V>(map: M) -> String
where
    M: IntoIterator<Item = (&'a K, &'a V)>,
    K: Display + 'a,
    V: Display + 'a,
{
    let mut ret = String::new();
    ret.push_str("{\n");

    for (k, v) in map {
        ret.push_str(&format!("  {k}: {v},\n"));
    }

    ret.push('}');

    ret
}

/// A type to make it easier to set up test expectations for register contents
#[derive(Debug, Eq, Clone)]
enum BareVal {
    String(String),
    Int(LpcIntInner),
    Float(LpcFloatInner),
    Array(Vec<BareVal>),
    Mapping(HashMap<BareVal, BareVal>),
    Object(String),                         // Just the filename
    Function(String, Vec<Option<BareVal>>), // name and args
}

impl BareVal {
    pub fn from_lpc_ref(gs: &Arc<GlobalState>, lpc_ref: &LpcRef) -> Self {
        match lpc_ref {
            LpcRef::Float(x) => BareVal::Float(x.0),
            LpcRef::Int(x) => BareVal::Int(x.0),
            LpcRef::String(x) => BareVal::String(x.to_string()),
            LpcRef::Array(cell) => {
                let array = gs
                    .committed_array(cell.id)
                    .expect("array payload committed with its cell");
                let array = array
                    .iter()
                    .map(|r| BareVal::from_lpc_ref(gs, r))
                    .collect::<Vec<_>>();
                BareVal::Array(array)
            }
            LpcRef::Mapping(cell) => {
                let mapping = gs
                    .committed_mapping(cell.id)
                    .expect("mapping payload committed with its cell");
                let mapping = mapping
                    .iter()
                    .map(|(k, v)| (BareVal::from_lpc_ref(gs, k), BareVal::from_lpc_ref(gs, v)))
                    .collect::<HashMap<_, _>>();
                BareVal::Mapping(mapping)
            }
            LpcRef::Object(o) => {
                if let Some(o) = o.upgrade() {
                    let filename = o.filename().into_owned();
                    BareVal::Object(filename)
                } else {
                    BareVal::Int(0)
                }
            }
            LpcRef::Function(fp) => {
                let args = fp
                    .partial_args()
                    .iter()
                    .map(|item| item.as_ref().map(|r| BareVal::from_lpc_ref(gs, r)))
                    .collect::<Vec<_>>();

                BareVal::Function(fp.name().into(), args)
            }
        }
    }

    pub fn equal_to_lpc_ref(&self, gs: &Arc<GlobalState>, other: &LpcRef) -> bool {
        self == &BareVal::from_lpc_ref(gs, other)
    }

    pub fn assert_equal(&self, gs: &Arc<GlobalState>, other: &LpcRef) {
        assert_eq!(self, &BareVal::from_lpc_ref(gs, other));
    }

    pub fn assert_vec_equal(gs: &Arc<GlobalState>, a: &[BareVal], b: &[LpcRef]) {
        assert_eq!(
            a.len(),
            b.len(),
            "Vectors {:?} and {:?} are of different lengths",
            a,
            b
        );
        for (a, b) in a.iter().zip(b.iter()) {
            a.assert_equal(gs, b);
        }
    }
}

impl Hash for BareVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BareVal::Float(x) => x.hash(state),
            BareVal::Int(x) => x.hash(state),
            BareVal::String(x) => x.hash(state),
            BareVal::Array(x) => x.hash(state),
            // `HashMap` has no `Hash`; the length keeps equal mappings hashing equal.
            BareVal::Mapping(x) => x.len().hash(state),
            BareVal::Object(x) => x.hash(state),
            BareVal::Function(x, y) => {
                x.hash(state);
                y.hash(state);
            }
        }
    }
}

impl PartialEq for BareVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (BareVal::Float(x), BareVal::Float(y)) => x == y,
            (BareVal::Int(x), BareVal::Int(y)) => x == y,
            (BareVal::String(x), BareVal::String(y)) => x == y,
            (BareVal::Array(x), BareVal::Array(y)) => x == y,
            (BareVal::Mapping(x), BareVal::Mapping(y)) => x == y,
            (BareVal::Object(x), BareVal::Object(y)) => x == y,
            (BareVal::Function(x, y), BareVal::Function(a, b)) => x == a && y == b,
            _ => false,
        }
    }
}

impl Display for BareVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BareVal::Float(x) => write!(f, "{x}"),
            BareVal::Int(x) => write!(f, "{x}"),
            BareVal::String(x) => write!(f, "\"{x}\""),
            BareVal::Array(x) => write!(f, "{}", format_slice(x)),
            BareVal::Mapping(x) => write!(f, "{}", format_map(x)),
            BareVal::Object(x) => write!(f, "object({x})"),
            BareVal::Function(x, y) => {
                write!(f, "function({x}")?;
                for arg in y {
                    match arg {
                        Some(x) => write!(f, ", {x}")?,
                        None => write!(f, ", <partial>")?,
                    }
                }
                write!(f, ")")
            }
        }
    }
}

mod test_instructions {
    use super::*;
    mod test_aconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed *a = ({ 12, 4.3, "hello", ({ 1, 2, 3 }) });
                "##};
            check_committed_globals(
                code,
                &[(
                    "a",
                    BareVal::Array(vec![
                        BareVal::Int(12),
                        BareVal::Float(LpcFloatInner::from(4.3)),
                        BareVal::String("hello".into()),
                        BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                    ]),
                )],
            )
            .await;
        }
    }

    mod test_and {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 15 & 27;
                    mixed b = 0 & a;
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(11)), ("b", BareVal::Int(0))]).await;
        }
    }

    mod test_andand {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 123 && 333;
                    mixed b = 0;
                    mixed c = b && a;
                "##};

            check_committed_globals(
                code,
                &[
                    ("a", BareVal::Int(333)),
                    ("b", BareVal::Int(0)),
                    ("c", BareVal::Int(0)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn a_false_left_operand_yields_zero_in_a_loop_condition() {
            let code = indoc! { r##"
                    int hits = 0;
                    void create() {
                        int i;
                        for (i = 0; i < 4; i++) {
                            int a = i % 2;
                            if (a && 1) {
                                hits++;
                            }
                        }
                    }
                "##};

            check_committed_globals(code, &[("hits", BareVal::Int(2))]).await;
        }

        #[tokio::test]
        async fn a_false_left_operand_yields_zero_in_a_loop_assignment() {
            let code = indoc! { r##"
                    int hits = 0;
                    void create() {
                        int i;
                        for (i = 0; i < 4; i++) {
                            int a = i % 2;
                            int r = a && 1;
                            hits += r;
                        }
                    }
                "##};

            check_committed_globals(code, &[("hits", BareVal::Int(2))]).await;
        }

        #[tokio::test]
        async fn a_false_left_operand_yields_zero_in_a_loop_ternary() {
            let code = indoc! { r##"
                    int hits = 0;
                    void create() {
                        int i;
                        for (i = 0; i < 4; i++) {
                            int a = i % 2;
                            hits += (a && 1) ? 1 : 0;
                        }
                    }
                "##};

            check_committed_globals(code, &[("hits", BareVal::Int(2))]).await;
        }
    }

    mod test_condition_form {
        use super::*;

        #[tokio::test]
        async fn a_negated_andand_follows_de_morgan() {
            let code = indoc! { r##"
                    int hits = 0;
                    void create() {
                        int i;
                        for (i = 0; i < 4; i++) {
                            if (!(i % 2 && i > 1)) {
                                hits++;
                            }
                        }
                    }
                "##};

            check_committed_globals(code, &[("hits", BareVal::Int(3))]).await;
        }

        #[tokio::test]
        async fn a_do_while_oror_condition_runs_until_both_operands_are_false() {
            let code = indoc! { r##"
                    int hits = 0;
                    void create() {
                        int a = 2;
                        int b = 3;
                        do {
                            hits++;
                            if (a) a--;
                            else b--;
                        } while (a || b);
                    }
                "##};

            check_committed_globals(code, &[("hits", BareVal::Int(5))]).await;
        }

        #[tokio::test]
        async fn a_literal_true_while_leaves_by_break() {
            let code = indoc! { r##"
                    int hits = 0;
                    void create() {
                        while (1) {
                            hits++;
                            if (hits == 3) break;
                        }
                    }
                "##};

            check_committed_globals(code, &[("hits", BareVal::Int(3))]).await;
        }

        #[tokio::test]
        async fn a_dead_object_is_false_through_a_negated_condition() {
            let code = indoc! { r##"
                    int gone = 0;
                    void create() {
                        object ob = clone_object("/std/object");
                        destruct(ob);
                        if (!ob) gone = 1;
                    }
                "##};

            check_committed_globals(code, &[("gone", BareVal::Int(1))]).await;
        }
    }

    mod test_over_passed_arguments {
        use super::*;

        #[tokio::test]
        async fn call_other_extras_do_not_leak_into_an_uninitialized_local() {
            let code = indoc! { r##"
                    int r;
                    int f(int a) { int x; return x; }
                    void create() { r = this_object()->f(1, 2); }
                "##};

            check_committed_globals(code, &[("r", BareVal::Int(0))]).await;
        }

        #[tokio::test]
        async fn pointer_extras_do_not_leak_into_an_uninitialized_local() {
            let code = indoc! { r##"
                    int r;
                    int f(int a) { int x; return x; }
                    void create() { function fp = &f(); r = fp(1, 2); }
                "##};

            check_committed_globals(code, &[("r", BareVal::Int(0))]).await;
        }

        #[tokio::test]
        async fn ellipsis_extras_reach_argv_past_the_locals() {
            let code = indoc! { r##"
                    int r;
                    int f(int a, ...) { int x; int y; x = argv[0]; y = argv[1]; return a + x * 10 + y; }
                    void create() { r = f(1, 5, 6); }
                "##};

            check_committed_globals(code, &[("r", BareVal::Int(57))]).await;
        }

        #[tokio::test]
        async fn call_other_extras_reach_argv_past_the_locals() {
            let code = indoc! { r##"
                    int r;
                    int f(int a, ...) { int x; int y; x = argv[0]; y = argv[1]; return a + x * 10 + y; }
                    void create() { r = this_object()->f(1, 5, 6); }
                "##};

            check_committed_globals(code, &[("r", BareVal::Int(57))]).await;
        }
    }

    mod test_constant_pool {
        use super::*;

        #[tokio::test]
        async fn a_string_constant_survives_mutation_of_a_copy() {
            let code = indoc! { r##"
                    string s;
                    string t;
                    void create() {
                        int i;
                        for (i = 0; i < 2; i++) {
                            s = "ab";
                            t = s;
                            t += "c";
                        }
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    ("s", BareVal::String("ab".into())),
                    ("t", BareVal::String("abc".into())),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn string_literals_compare_equal_across_functions() {
            let code = indoc! { r##"
                    int r;
                    string g() { return "x"; }
                    void create() { r = g() == "x"; }
                "##};

            check_committed_globals(code, &[("r", BareVal::Int(1))]).await;
        }

        #[tokio::test]
        async fn call_other_by_a_literal_name_reaches_the_function() {
            let code = indoc! { r##"
                    int r;
                    int f() { return 42; }
                    void create() { r = this_object()->f(); }
                "##};

            check_committed_globals(code, &[("r", BareVal::Int(42))]).await;
        }
    }

    mod test_switch_ranges {
        use super::*;

        #[tokio::test]
        async fn a_range_case_matches_only_the_switch_value_inside_it() {
            let code = indoc! { r##"
                    int below; int inside; int above;
                    int f(int x) { switch (x) { case 10..200: return 1; default: return 0; } }
                    void create() { below = f(5); inside = f(50); above = f(500); }
                "##};

            check_committed_globals(
                code,
                &[
                    ("below", BareVal::Int(0)),
                    ("inside", BareVal::Int(1)),
                    ("above", BareVal::Int(0)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn an_open_range_case_is_bounded_on_one_side() {
            let code = indoc! { r##"
                    int low; int high;
                    int f(int x) { switch (x) { case ..9: return 1; case 100..: return 2; default: return 0; } }
                    void create() { low = f(-4) * 10 + f(9); high = f(100) * 10 + f(50); }
                "##};

            check_committed_globals(
                code,
                &[("low", BareVal::Int(11)), ("high", BareVal::Int(20))],
            )
            .await;
        }
    }

    mod test_bitwise_not {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    int a = ~0;
                    int b = 7;
                    int c = ~b;
                "##};

            check_committed_globals(
                code,
                &[
                    ("a", BareVal::Int(-1)),
                    ("b", BareVal::Int(7)),
                    ("c", BareVal::Int(-8)),
                ],
            )
            .await;
        }
    }

    mod test_call {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = tacos();
                    int tacos() { return 666; }
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(666))]).await;
        }

        #[tokio::test]
        async fn calls_correct_function() {
            let code = indoc! { r##"
                    inherit "/std/object";
                    mixed mine = public_function();
                    mixed parents = ::public_function();

                    string public_function() {
                        return "my public_function";
                    }
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let proc = ctx.process();
            let values = committed_globals_by_name(&ctx.global_state, proc);
            BareVal::String("my public_function".into())
                .assert_equal(&ctx.global_state, values.get("mine").unwrap());
            BareVal::String("/std/object public".into())
                .assert_equal(&ctx.global_state, values.get("parents").unwrap());
        }

        #[tokio::test]
        async fn calls_the_function_past_a_same_named_variable() {
            let code = indoc! { r##"
                    int foo = 1;
                    int foo() { return 2; }
                    int global_shadow = foo();
                    int local_shadow = bar();
                    int bar() { int foo = 1; return foo(); }
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let values = committed_globals_by_name(&ctx.global_state, ctx.process());
            BareVal::Int(2).assert_equal(&ctx.global_state, values.get("global_shadow").unwrap());
            BareVal::Int(2).assert_equal(&ctx.global_state, values.get("local_shadow").unwrap());
        }

        #[tokio::test]
        async fn calls_a_same_named_function_typed_variable() {
            let code = indoc! { r##"
                    function fp = (: "x" :);
                    int fp() { return 4; }
                    string s = fp();
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let values = committed_globals_by_name(&ctx.global_state, ctx.process());
            BareVal::String("x".into()).assert_equal(&ctx.global_state, values.get("s").unwrap());
        }

        #[tokio::test]
        async fn a_bare_function_name_is_a_reference() {
            let code = indoc! { r##"
                    int fp() { return 4; }
                    function fp2 = fp;
                    int v = fp2();
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let values = committed_globals_by_name(&ctx.global_state, ctx.process());
            BareVal::Int(4).assert_equal(&ctx.global_state, values.get("v").unwrap());
        }

        #[tokio::test]
        async fn calls_correct_function_with_efuns() {
            let code = indoc! { r##"
                    object ob = clone_object("/std/object");
                    mixed this_one = file_name(ob);
                    mixed efun_one = efun::file_name(ob);

                    string file_name(object ob) {
                        return "file_name_override";
                    }
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let proc = ctx.process();
            let values = committed_globals_by_name(&ctx.global_state, proc);
            BareVal::String("file_name_override".into())
                .assert_equal(&ctx.global_state, values.get("this_one").unwrap());
            BareVal::String("/std/object#0".into())
                .assert_equal(&ctx.global_state, values.get("efun_one").unwrap());
        }

        #[tokio::test]
        async fn calls_correct_function_with_simul_efuns() {
            let code = indoc! { r##"
                    string this_one = simul_efun("marf");
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let proc = ctx.process();
            let values = committed_globals_by_name(&ctx.global_state, proc);
            BareVal::String("this is a simul_efun: marf".into())
                .assert_equal(&ctx.global_state, values.get("this_one").unwrap());
        }
    }

    mod test_call_efun {
        use super::*;

        #[tokio::test]
        async fn accessor_errors_carry_a_span() {
            let code = r#"mixed x = 1; string s = implode(x, ",");"#;

            let error = try_run_prog(code)
                .await
                .expect_err("implode needs an array");

            assert!(error.span().is_some(), "{error:?}");
            assert_eq!(
                error.to_string(),
                "runtime error: invalid access. Expected array, actually int"
            );
        }

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = this_object();
                "##};

            check_committed_globals(code, &[("q", BareVal::Object("/my_file".into()))]).await;
        }
    }

    mod test_call_simul_efun {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = simul_efun("marf");
                "##};

            check_committed_globals(
                code,
                &[("q", BareVal::String("this is a simul_efun: marf".into()))],
            )
            .await;
        }
    }

    mod test_call_fp {
        use claims::assert_ok;

        use super::*;
        use crate::{
            interpreter::vm::Vm,
            test_support::{permissive_master, test_config},
        };

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    function q = tacos;
                    int a = q(666);
                    int tacos(int j) { return j + 1; }
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Function("tacos".into(), vec![])),
                    ("a", BareVal::Int(667)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_partial_applications() {
            let code = indoc! { r##"
                    function q = &tacos(, "adding some!");
                    int a = q(666, 4);
                    string tacos(int j, string s, int k) {
                        return s + " " +  (j + k);
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "q",
                        BareVal::Function(
                            "tacos".into(),
                            vec![None, Some(BareVal::String("adding some!".into()))],
                        ),
                    ),
                    ("a", BareVal::String("adding some! 670".into())),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_partial_applications_with_no_added_args() {
            let code = indoc! { r##"
                    function q = &tacos("my_string!");
                    int a = q();
                    string tacos(string s) {
                        return s + " awesome!" ;
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "q",
                        BareVal::Function(
                            "tacos".into(),
                            vec![Some(BareVal::String("my_string!".into()))],
                        ),
                    ),
                    ("a", BareVal::String("my_string! awesome!".into())),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_partial_applications_with_default_arguments() {
            let code = indoc! { r##"
                    function q = &tacos(, "adding some!");
                    int a = q(666, 4);
                    int b = q(123);
                    string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "q",
                        BareVal::Function(
                            "tacos".into(),
                            vec![None, Some(BareVal::String("adding some!".into()))],
                        ),
                    ),
                    ("a", BareVal::String("adding some! 670".into())),
                    ("b", BareVal::String("adding some! 223".into())),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_partial_applications_with_default_arguments_and_ellipsis() {
            let code = indoc! { r##"
                    function q = &tacos(, "adding some!", , 666, 123);
                    int a = q(42, 4, "should be in argv");
                    int b = q(69);
                    int tacos(int j, string s, int k = 100, ...) {
                        dump("argv!");
                        dump(argv);
                        return j + k;
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "q",
                        BareVal::Function(
                            "tacos".into(),
                            vec![
                                None,
                                Some(BareVal::String("adding some!".into())),
                                None,
                                Some(BareVal::Int(666)),
                                Some(BareVal::Int(123)),
                            ],
                        ),
                    ),
                    ("a", BareVal::Int(46)),
                    ("b", BareVal::Int(69)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_dynamic_receivers() {
            let code = indoc! { r##"
                    function q = &->name(, "awesome!");

                    int a = q(this_object(), 666);
                    int b = q(clone_object("/std/widget"), 42);

                    string name(int rank, string reaction) {
                        return "me: " + rank + ". " + reaction;
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "q",
                        BareVal::Function(
                            "name".into(),
                            vec![None, None, Some(BareVal::String("awesome!".into()))],
                        ),
                    ),
                    ("a", BareVal::String("me: 666. awesome!".into())),
                    ("b", BareVal::String("widget: 42. awesome!".into())),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn initializes_receiver_if_necessary() {
            let code = indoc! { r##"
                    function q = &->name(, "awesome!");

                    // object o = clone_object("/std/widget");
                    int a = q("/std/widget", 666); // should initialize /std/widget

                    string name(int rank, string reaction) {
                        return "me: " + rank + ". " + reaction;
                    }
                "##};

            let vm = Vm::new(test_config());
            permissive_master(&vm.global_state.object_space).await;
            let task = vm
                .initialize_process_from_code("doody.c", code)
                .await
                .unwrap();

            let object_space = task.context.object_space();

            let widget = object_space.lookup("/std/widget").unwrap();
            assert!(task.context.global_state.is_initialized(&widget));
        }

        #[tokio::test]
        async fn a_pointer_to_this_objects_private_function_fires() {
            let code = indoc! { r##"
                    function q = &(this_object())->tacos(, "adding some!");
                    string a = q(666, 4);
                    string b = q(123);
                    private string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, task.context.process());

            assert_eq!(globals["a"].to_string(), "adding some! 670");
            assert_eq!(globals["b"].to_string(), "adding some! 223");
        }

        async fn committed_r(code: &str) -> String {
            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, task.context.process());
            globals["r"].to_string()
        }

        /// A stored pointer outlives the resident it was made against.
        #[tokio::test]
        async fn a_pointer_call_without_a_resident_is_a_runtime_error() {
            use crate::{
                compile_time_config::MAX_CALL_STACK_SIZE,
                interpreter::{
                    call_frame::CallFrame,
                    function_type::{
                        function_address::FunctionAddress, function_ptr::FunctionPtrBuilder,
                    },
                    object_space::ObjectSpace,
                    task::task_template::TaskTemplate,
                },
                test_support::compile_prog,
            };
            use lpc_rs_core::register::Register;
            use thin_vec::ThinVec;

            let (program, config, _se_proc) = compile_prog("void create() { int x = 1; }").await;
            let (tx, _rx) = tokio::sync::mpsc::channel(128);
            let global_state = GlobalState::new(config, tx);
            let process = Arc::new(Process::new(program));
            ObjectSpace::insert_process_physical(&global_state.object_space, process.clone());
            let context = TaskTemplate::from(global_state).into_task_context(process.clone());
            assert!(context.simul_efuns().is_none());

            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context);
            let create = process.program.lookup_function("create").unwrap().clone();
            let mut frame = CallFrame::new(process.clone(), create, 0, None::<ThinVec<VarId>>);
            let ptr = FunctionPtrBuilder::default()
                .owner(Arc::downgrade(&process))
                .address(FunctionAddress::SimulEfun(ustr("nope")))
                .build()
                .unwrap();
            frame.registers[1] = ptr.into();
            task.stack.push(frame).unwrap();

            let e = task
                .handle_call_fp(Register(1).as_local())
                .await
                .unwrap_err();
            assert!(!e.is_bug(), "{e}");
            assert!(
                e.to_string().starts_with(
                    "runtime error: call to simul efun `nope`: no simul-efun object is loaded"
                ),
                "{e}"
            );
        }

        /// Codegen emits `Call` only for a name of the running program, so a
        /// miss is the driver's bug, never a simul-efun lookup.
        #[tokio::test]
        async fn a_call_missing_from_the_program_is_a_bug() {
            use crate::{
                compile_time_config::MAX_CALL_STACK_SIZE,
                interpreter::{
                    call_frame::CallFrame, object_space::ObjectSpace,
                    task::task_template::TaskTemplate,
                },
                test_support::compile_prog,
            };
            use thin_vec::ThinVec;

            let (program, config, _se_proc) = compile_prog("void create() { int x = 1; }").await;
            let (tx, _rx) = tokio::sync::mpsc::channel(128);
            let global_state = GlobalState::new(config, tx);
            let process = Arc::new(Process::new(program));
            ObjectSpace::insert_process_physical(&global_state.object_space, process.clone());
            let context = TaskTemplate::from(global_state).into_task_context(process.clone());
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(context);
            let create = process.program.lookup_function("create").unwrap().clone();
            let frame = CallFrame::new(process.clone(), create, 0, None::<ThinVec<VarId>>);
            task.stack.push(frame).unwrap();

            let e = task.handle_call(ustr("nope")).await.unwrap_err();
            assert!(e.is_bug(), "{e}");
            assert_eq!(
                e.to_string(),
                "runtime bug: call to unknown local function `nope`"
            );
        }

        #[tokio::test]
        async fn a_positional_arg_follows_a_captured_parameter() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    function f = (: [int x] function g = (: x :); return g() + $2; :);
                    r = f(10, 1);
                }
            "##};
            assert_eq!(committed_r(code).await, "11");
        }

        #[tokio::test]
        async fn a_positional_arg_aliases_a_declared_parameter() {
            let code = indoc! { r##"
                mixed r;
                void create() { function f = (: [int x] $1 * 2 :); r = f(21); }
            "##};
            assert_eq!(committed_r(code).await, "42");
        }

        #[tokio::test]
        async fn a_dynamic_pointer_without_a_receiver_is_an_error() {
            let code = indoc! { r##"
                mixed r;
                void create() { function f = &->sec(); r = catch(f()); }
            "##};
            let r = committed_r(code).await;
            assert!(r.contains("receiver"), "{r}");
        }

        #[tokio::test]
        async fn a_receiver_bound_by_papplyv_is_used() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    object o = clone_object("/ptr_target");
                    function f = papplyv(&->sec(), ({ o }));
                    r = f();
                }
            "##};
            assert_eq!(committed_r(code).await, "77");
        }

        #[tokio::test]
        async fn a_pointer_to_another_objects_private_function_cannot_be_taken() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    object o = clone_object("/ptr_target");
                    function f;
                    r = catch(f = &(o)->hidden());
                }
            "##};
            let r = committed_r(code).await;
            assert!(r.contains("private"), "{r}");
        }

        #[tokio::test]
        async fn a_private_pointer_taken_inside_its_object_fires_anywhere() {
            let code = indoc! { r##"
                mixed r;
                private int mine() { return 5; }
                void create() {
                    object o = clone_object("/ptr_target");
                    r = o->fire(&mine()) * 100 + o->fire(o->get_hidden());
                }
            "##};
            assert_eq!(committed_r(code).await, "542");
        }

        #[tokio::test]
        async fn a_pointer_into_a_destructed_object_is_an_error() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    object o = clone_object("/ptr_target");
                    function f = &(o)->sec();
                    destruct(o);
                    r = catch(f());
                }
            "##};
            let r = committed_r(code).await;
            assert!(r.contains("destructed"), "{r}");
        }

        #[tokio::test]
        async fn an_efun_pointer_runs_in_its_owner() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    object o = clone_object("/ptr_target");
                    r = o->fire(&this_object()) == this_object();
                }
            "##};
            assert_eq!(committed_r(code).await, "1");
        }

        #[tokio::test]
        async fn is_normal_call_for_local_private_functions() {
            let code = indoc! { r##"
                    function q = tacos;
                    int a = q(4);
                    private int tacos(int j) {
                        return j;
                    }
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Function("tacos".into(), vec![])),
                    ("a", BareVal::Int(4)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn checks_types() {
            let code = indoc! { r##"
                    function q = &tacos("foo");
                    int a = q();
                    private int tacos(int j) {
                        return j;
                    }
                "##};

            let result = try_run_prog(code).await;

            assert_eq!(
                result.unwrap_err().to_string(),
                "runtime error: unexpected argument type to `tacos`: string. expected int."
            );

            let code = indoc! { r##"
                    function q = &tacos(5, , 666);

                    int a = q(123.4);

                    private int tacos(int i, string s, int j) {
                        return i + j;
                    }
                "##};

            let result = try_run_prog(code).await;

            assert_eq!(
                result.unwrap_err().to_string(),
                "runtime error: unexpected argument type to `tacos`: float. expected string."
            );

            let code = indoc! { r##"
                    function f = taco_maker();

                    string name = f("carne asada");

                    private function taco_maker() {
                        return (: [string name, float price = 1.00] name :);
                    }
                "##};

            let result = try_run_prog(code).await;

            assert_ok!(result);
        }
    }

    mod test_call_other {
        use super::*;

        #[tokio::test]
        async fn errors_on_a_missing_path() {
            let code = r#"mixed q = "/no_such_file"->foo();"#;

            let error = try_run_prog(code)
                .await
                .expect_err("the receiver cannot load");

            assert!(error.to_string().contains("no_such_file"), "{error}");
        }

        #[tokio::test]
        async fn errors_on_an_uncompilable_path() {
            let code = r#"mixed q = "/broken"->foo();"#;

            let error = try_run_prog(code)
                .await
                .expect_err("the receiver cannot compile");

            assert_eq!(error.to_string(), "Unrecognized Token: ;");
        }

        #[tokio::test]
        async fn propagates_the_receivers_init_error() {
            let code = r#"mixed q = "/init_fails"->foo();"#;

            let error = try_run_prog(code)
                .await
                .expect_err("the receiver's initializer fails");

            assert_eq!(error.to_string(), "runtime error: Division by zero");
        }

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    int tacos() { return 666; }
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(666))]).await;
        }

        #[tokio::test]
        async fn returns_0_for_private_functions() {
            let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    private int tacos() { return 666; }
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(0))]).await;
        }

        #[tokio::test]
        async fn returns_0_for_protected_functions() {
            let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    protected int tacos() { return 666; }
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(0))]).await;
        }
    }

    mod test_catch {
        use super::*;

        #[tokio::test]
        async fn a_bug_is_not_caught() {
            let code = indoc! { r##"
                    mixed id = "a";
                    string e = catch(remove_call_out(id));
                "##};

            let error = try_run_prog(code)
                .await
                .expect_err("a bug must fail the task");

            assert!(error.is_bug());
            assert_eq!(
                error.to_string(),
                "runtime bug: non-int call out ID sent to `remove_call_out`"
            );
        }

        #[tokio::test]
        async fn stores_the_error_string() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        mixed e = catch(10 / j);
                    }
                "##};

            check_popped_vars(
                code,
                &[
                    ("j", BareVal::Int(0)),
                    (
                        "e",
                        BareVal::String("runtime error: Division by zero".into()),
                    ),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_zero_when_no_error() {
            let code = indoc! { r##"
                    void create() {
                        int j = 5;
                        mixed e = catch(10 / j);
                    }
                "##};

            check_popped_vars(code, &[("j", BareVal::Int(5)), ("e", BareVal::Int(0))]).await;
        }
    }

    mod test_error_locations {
        use super::*;

        #[tokio::test]
        async fn a_runtime_error_renders_with_its_stack_trace() {
            let code = indoc! { r##"
                void create() {
                    int j = 0;
                    int x = 10 / j;
                }
            "##};
            let e = try_run_prog(code).await.unwrap_err();
            assert_eq!(
                e.diagnostic_string(),
                "error: runtime error: Division by zero\n  ┌─ /my_file.c:3:13\n  │\n3 │     int x = 10 / j;\n  │             ^^^^^^\n  │\n  = Stack trace:\n    \n    /my_file.c:3:13 in create()\n    (unknown) in init-program()\n\n"
            );
        }

        fn stack_trace_of(e: &LpcError) -> String {
            e.to_diagnostics()[0]
                .notes
                .iter()
                .find(|n| n.starts_with("Stack trace"))
                .cloned()
                .unwrap_or_default()
        }

        #[tokio::test]
        async fn a_loaded_objects_compile_error_keeps_its_own_span() {
            let code = indoc! { r##"
                void create() {
                    clone_object("/broken");
                }
            "##};
            let e = try_run_prog(code).await.unwrap_err();
            let location = e.span().map(|s| s.to_string()).unwrap_or_default();
            assert!(location.contains("broken.c"), "{location}");
            let labels = &e.to_diagnostics()[0].labels;
            assert!(
                labels.iter().any(|l| l.message == "loaded from here"),
                "{labels:?}"
            );
        }

        #[tokio::test]
        async fn a_nested_trace_runs_from_the_entry_to_the_failing_frame() {
            let code = indoc! { r##"
                void create() {
                    object o = clone_object("/ptr_target");
                    o->fire((: 1 / 0 :));
                }
            "##};
            let e = try_run_prog(code).await.unwrap_err();
            let trace = stack_trace_of(&e);
            assert!(trace.contains(" in create()"), "{trace}");
            assert!(trace.contains(" in fire()"), "{trace}");
        }

        #[tokio::test]
        async fn call_other_on_an_array_propagates_an_error() {
            let code = indoc! { r##"
                void create() {
                    object o = clone_object("/ptr_target");
                    ({ o })->fire((: 1 / 0 :));
                }
            "##};
            let e = try_run_prog(code).await.unwrap_err();
            assert_eq!(e.to_string(), "runtime error: Division by zero");
        }

        #[tokio::test]
        async fn call_other_on_a_mapping_propagates_an_error() {
            let code = indoc! { r##"
                void create() {
                    object o = clone_object("/ptr_target");
                    ([ "o": o ])->fire((: 1 / 0 :));
                }
            "##};
            let e = try_run_prog(code).await.unwrap_err();
            assert_eq!(e.to_string(), "runtime error: Division by zero");
        }

        #[tokio::test]
        async fn a_timeout_carries_the_trace_it_interrupted() {
            use lpc_rs_utils::config::ConfigBuilder;

            let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};
            let config = crate::test_config_builder!()
                .max_execution_time(40_u64)
                .build()
                .unwrap();
            let e = try_run_prog_with_config(code, Arc::new(config))
                .await
                .unwrap_err();
            assert!(stack_trace_of(&e).contains(" in create()"), "{e:?}");
        }
    }

    mod test_catch_end {
        use super::*;

        #[tokio::test]
        async fn pops_the_catch_point() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(catch(catch(catch(10 / j))));
                    }
                "##};

            let task = run_prog(code).await;

            assert!(task.catch_points.is_empty());
        }
    }

    mod test_dec {
        use super::*;

        #[tokio::test]
        async fn stores_the_value_for_pre() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        int k = --j;
                    }
                "##};

            check_popped_vars(code, &[("j", BareVal::Int(-1)), ("k", BareVal::Int(-1))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_pre_when_global() {
            let code = indoc! { r##"
                    int j = 5;
                    int k = --j;
                "##};

            check_committed_globals(code, &[("j", BareVal::Int(4)), ("k", BareVal::Int(4))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_post() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        int k = j--;
                    }
                "##};

            check_popped_vars(code, &[("j", BareVal::Int(-1)), ("k", BareVal::Int(0))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_post_when_global() {
            let code = indoc! { r##"
                    int j = 5;
                    int k = j--;
                "##};

            check_committed_globals(code, &[("j", BareVal::Int(4)), ("k", BareVal::Int(5))]).await;
        }
    }

    mod test_eq_eq {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 2 == 2;
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(1))]).await;
        }
    }

    mod test_inheritance {
        use super::*;

        #[tokio::test]
        async fn an_inherited_function_variable_is_callable_by_name() {
            let code = indoc! { r##"
                inherit "/sibling_a";
                mixed r;
                void create() { r = fa(); }
            "##};

            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, task.context.process());

            assert_eq!(globals["r"].to_string(), "from-a");
        }

        #[tokio::test]
        async fn a_reference_resolves_to_the_visible_parents_global() {
            for order in [
                r#"inherit "/visible"; inherit "/hidden";"#,
                r#"inherit "/hidden"; inherit "/visible";"#,
            ] {
                let code = format!("{order}\nmixed r;\nvoid create() {{ r = shared; }}");
                let task = run_prog(&code).await;
                let globals =
                    committed_globals_by_name(&task.context.global_state, task.context.process());

                assert_eq!(globals["r"].to_string(), "1", "{order}");
            }
        }

        #[tokio::test]
        async fn a_protected_parents_global_is_readable_from_the_child() {
            let code = indoc! { r##"
                inherit "/guarded";
                mixed r;
                void create() { r = shared; }
            "##};

            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, task.context.process());

            assert_eq!(globals["r"].to_string(), "3");
        }

        #[tokio::test]
        async fn a_program_reached_through_two_parents_has_one_set_of_globals() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    object o = clone_object("/diamond_child");
                    mixed before = o->both();
                    o->set_left_a(7);
                    r = before + o->both();
                }
            "##};

            let task = run_prog(code).await;
            let gs = &task.context.global_state;
            let globals = committed_globals_by_name(gs, task.context.process());
            let LpcRef::Array(cell) = &globals["r"] else {
                panic!("both() returns an array");
            };
            let values: Vec<String> = gs
                .committed_array(cell.id)
                .unwrap()
                .iter()
                .map(|v| v.to_string())
                .collect();

            assert_eq!(values, ["123", "123", "123", "7", "7", "7"]);
        }

        #[tokio::test]
        async fn sibling_parents_keep_their_strings_calls_and_globals() {
            let code = indoc! { r##"
                mixed r;
                void create() { object o = clone_object("/sibling_child"); r = o->snapshot(); }
            "##};

            let task = run_prog(code).await;
            let gs = &task.context.global_state;
            let globals = committed_globals_by_name(gs, task.context.process());
            let LpcRef::Array(cell) = &globals["r"] else {
                panic!("snapshot is an array");
            };
            let snapshot: Vec<String> = gs
                .committed_array(cell.id)
                .unwrap()
                .iter()
                .map(|v| v.to_string())
                .collect();

            assert_eq!(
                snapshot,
                [
                    "alpha", "beta", "from-a", "from-b", "1", "2", "3", "child", "1", "0", "0",
                    "1", "2"
                ]
            );
        }
    }

    mod test_functionptrconst {
        use super::*;

        #[tokio::test]
        async fn an_inherited_closure_keeps_its_own_body() {
            let code = indoc! { r##"
                mixed r;
                void create() {
                    object o = clone_object("/inherit_closure_child");
                    function pc = o->parent_closure();
                    function cc = o->child_closure();
                    r = o->parent_direct() + pc() * 1000 + cc() * 1000000;
                }
            "##};

            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, task.context.process());

            assert_eq!(globals["r"], LpcRef::Int(LpcInt(2_000_101_101)));
        }

        #[tokio::test]
        async fn stores_the_value_for_efuns() {
            let code = indoc! { r##"
                    function f = dump;
                "##};

            check_committed_globals(code, &[("f", BareVal::Function("dump".into(), vec![]))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_simul_efuns() {
            let code = indoc! { r##"
                    function f = simul_efun;
                "##};

            check_committed_globals(
                code,
                &[("f", BareVal::Function("simul_efun".into(), vec![]))],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_call_other() {
            let code = indoc! { r##"
                    function f = &(this_object())->tacco();

                    void tacco() {
                        dump("tacco!");
                    }
                "##};

            check_committed_globals(code, &[("f", BareVal::Function("tacco".into(), vec![]))])
                .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_call_other_string_receiver() {
            let code = indoc! { r##"
                    function f = &("/secure/simul_efuns")->simul_efun();
                "##};

            check_committed_globals(
                code,
                &[("f", BareVal::Function("simul_efun".into(), vec![]))],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_with_args() {
            let code = indoc! { r##"
                    function f = &tacco(1, 666);

                    void tacco(int a, int b) {
                        dump(a + b);
                    }
                "##};

            check_committed_globals(
                code,
                &[(
                    "f",
                    BareVal::Function(
                        "tacco".into(),
                        vec![Some(BareVal::Int(1)), Some(BareVal::Int(666))],
                    ),
                )],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_with_partial_applications() {
            let code = indoc! { r##"
                    function f = &tacco(1, , , 42, );

                    void tacco(int a, int b, int c, int d, int e) {
                        dump(a + b - c * (d + e));
                    }
                "##};

            check_committed_globals(
                code,
                &[(
                    "f",
                    BareVal::Function(
                        "tacco".into(),
                        vec![
                            Some(BareVal::Int(1)),
                            None,
                            None,
                            Some(BareVal::Int(42)),
                            None,
                        ],
                    ),
                )],
            )
            .await;
        }

        #[tokio::test]
        async fn stores_the_value_for_closures() {
            let code = indoc! { r##"
                    function f = maker();

                    function maker() {
                        int i = 666;
                        return (: i + $1 :);
                    }
                "##};

            check_committed_globals(
                code,
                &[("f", BareVal::Function("closure-0".into(), vec![]))],
            )
            .await;
        }
    }

    mod test_gt {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 1200 > 1199;
                    mixed r = 1199 > 1200;
                    mixed s = 1200 > 1200;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(1)),
                    ("r", BareVal::Int(0)),
                    ("s", BareVal::Int(0)),
                ],
            )
            .await;
        }
    }

    mod test_gte {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 1200 >= 1199;
                    mixed r = 1199 >= 1200;
                    mixed s = 1200 >= 1200;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(1)),
                    ("r", BareVal::Int(0)),
                    ("s", BareVal::Int(1)),
                ],
            )
            .await;
        }
    }

    mod test_iadd {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    int q = 16 + 34;
                    int r = 12 + -4;
                    int s = q + r;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(50)),
                    ("r", BareVal::Int(8)),
                    ("s", BareVal::Int(58)),
                ],
            )
            .await;
        }
    }

    mod test_literals {
        use super::*;

        #[tokio::test]
        async fn stores_a_float() {
            let code = indoc! { r##"
                    float π = 4.13;
                "##};

            check_committed_globals(code, &[("π", BareVal::Float(4.13.into()))]).await;
        }

        #[tokio::test]
        async fn stores_an_int() {
            let code = indoc! { r##"
                    mixed q = 666;
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(666))]).await;
        }

        #[tokio::test]
        async fn stores_zero() {
            let code = indoc! { r##"
                    mixed q = 0;
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(0))]).await;
        }

        #[tokio::test]
        async fn stores_one() {
            let code = indoc! { r##"
                    mixed q = 1;
                "##};

            check_committed_globals(code, &[("q", BareVal::Int(1))]).await;
        }

        #[tokio::test]
        async fn stores_a_string() {
            let code = indoc! { r##"
                    string foo = "lolwut";
                "##};

            check_committed_globals(code, &[("foo", BareVal::String("lolwut".into()))]).await;
        }
    }

    mod test_idiv {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 16 / 2;
                    mixed r = 12 / -4;
                    mixed s = q / r;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(8)),
                    ("r", BareVal::Int(-3)),
                    ("s", BareVal::Int(-2)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn errors_on_division_by_zero() {
            let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q / r;
                "##};

            let r = try_run_prog(code).await;

            assert_eq!(
                r.unwrap_err().to_string(),
                "runtime error: Division by zero"
            )
        }
    }

    mod test_imod {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 16 % 7;
                    mixed r = 12 % -7;
                    mixed s = q % r;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(2)),
                    ("r", BareVal::Int(5)),
                    ("s", BareVal::Int(2)),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn errors_on_division_by_zero() {
            let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q % r;
                "##};

            let r = try_run_prog(code).await;

            assert_eq!(
                r.unwrap_err().to_string(),
                "runtime error: Remainder division by zero"
            )
        }
    }

    mod test_imul {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    int q = 16 * 2;
                    int r = 12 * -4;
                    int s = q * r;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(32)),
                    ("r", BareVal::Int(-48)),
                    ("s", BareVal::Int(-1536)),
                ],
            )
            .await;
        }
    }

    mod test_inc {
        use super::*;

        #[tokio::test]
        async fn errors_on_a_non_int_global() {
            let code = indoc! { r##"
                    mixed f() { return "x"; }
                    int j = f();
                    void create() { j++; }
                "##};

            let error = try_run_prog(code)
                .await
                .expect_err("++ on a string must error");
            assert_eq!(error.to_string(), "runtime error: invalid increment");
        }

        #[tokio::test]
        async fn wraps_at_the_int_maximum() {
            let code = indoc! { r##"
                    int j = 9223372036854775807;
                    void create() { j++; }
                "##};

            check_committed_globals(code, &[("j", BareVal::Int(i64::MIN))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_pre() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        int k = ++j;
                    }
                "##};

            check_popped_vars(code, &[("j", BareVal::Int(1)), ("k", BareVal::Int(1))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_pre_when_global() {
            let code = indoc! { r##"
                    int j = 0;
                    int k = ++j;
                "##};

            check_committed_globals(code, &[("j", BareVal::Int(1)), ("k", BareVal::Int(1))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_post() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        int k = j++;
                    }
                "##};

            check_popped_vars(code, &[("j", BareVal::Int(1)), ("k", BareVal::Int(0))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_post_when_global() {
            let code = indoc! { r##"
                    int j = 5;
                    int k = j++;
                "##};

            check_committed_globals(code, &[("j", BareVal::Int(6)), ("k", BareVal::Int(5))]).await;
        }
    }

    mod test_isub {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    int q = 16 - 2;
                    int r = 12 - -4;
                    int s = q - r;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(14)),
                    ("r", BareVal::Int(16)),
                    ("s", BareVal::Int(-2)),
                ],
            )
            .await;
        }
    }

    mod test_jmp {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    void create() {
                        mixed j;
                        int i = 12;
                        if (i > 10) {
                            j = 69;
                        } else {
                            j = 3;
                        }
                    }
                "##};

            check_popped_vars(code, &[("i", BareVal::Int(12)), ("j", BareVal::Int(69))]).await;
        }
    }

    mod test_jnz {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    void create() {
                        int j;
                        do {
                            j += 1;
                        } while(j < 8);
                    }
                "##};

            check_popped_vars(code, &[("j", BareVal::Int(8))]).await;
        }
    }

    mod test_jz {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                            int i = 12;
                            int j = i > 12 ? 10 : 1000;
                        "##};

            check_committed_globals(code, &[("i", BareVal::Int(12)), ("j", BareVal::Int(1000))])
                .await;
        }
    }

    mod test_load {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    int *i = ({ 1, 2, 3 });
                    int j = i[1];
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "i",
                        BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                    ),
                    ("j", BareVal::Int(2)),
                ],
            )
            .await;
        }
    }

    mod test_lt {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 1200 < 1199;
                    mixed r = 1199 < 1200;
                    mixed s = 1200 < 1200;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(0)),
                    ("r", BareVal::Int(1)),
                    ("s", BareVal::Int(0)),
                ],
            )
            .await;
        }
    }

    mod test_lte {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 1200 <= 1199;
                    mixed r = 1199 <= 1200;
                    mixed s = 1200 <= 1200;
                "##};

            check_committed_globals(
                code,
                &[
                    ("q", BareVal::Int(0)),
                    ("r", BareVal::Int(1)),
                    ("s", BareVal::Int(1)),
                ],
            )
            .await;
        }
    }

    mod test_mapconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = ([
                        "asdf": 123,
                        456: 4.13
                    ]);
                "##};

            let mut hashmap = HashMap::new();
            hashmap.insert(BareVal::String("asdf".into()), BareVal::Int(123));
            hashmap.insert(BareVal::Int(456), BareVal::Float(4.13.into()));
            check_committed_globals(code, &[("q", BareVal::Mapping(hashmap))]).await;
        }
    }

    mod test_madd {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = "abc";
                    mixed b = 123;
                    mixed c = a + b;
                "##};

            check_committed_globals(
                code,
                &[
                    ("a", BareVal::String("abc".into())),
                    ("b", BareVal::Int(123)),
                    ("c", BareVal::String("abc123".into())),
                ],
            )
            .await;
        }
    }

    mod test_mmul {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = "abc";
                    mixed b = 4;
                    mixed c = a * b;
                "##};

            check_committed_globals(
                code,
                &[
                    ("a", BareVal::String("abc".into())),
                    ("b", BareVal::Int(4)),
                    ("c", BareVal::String("abcabcabcabc".into())),
                ],
            )
            .await;
        }
    }

    mod test_msub {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = ({ 1, 1, 2, 3 });
                    mixed b = a - ({ 1 });
                "##};

            check_committed_globals(
                code,
                &[
                    (
                        "a",
                        BareVal::Array(vec![
                            BareVal::Int(1),
                            BareVal::Int(1),
                            BareVal::Int(2),
                            BareVal::Int(3),
                        ]),
                    ),
                    ("b", BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)])),
                ],
            )
            .await;
        }
    }

    mod test_not {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = !2;
                    mixed b = !!4;
                    int c = !0.00;
                    int d = !0.01;
                    int e = !"";
                    int f = !"asdf";
                "##};

            check_committed_globals(
                code,
                &[
                    ("a", BareVal::Int(0)),
                    ("b", BareVal::Int(1)),
                    ("c", BareVal::Int(1)),
                    ("d", BareVal::Int(0)),
                    ("e", BareVal::Int(0)),
                    ("f", BareVal::Int(0)),
                ],
            )
            .await;
        }
    }

    mod test_or {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 15 | 27;
                    mixed b = 0 | a;
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(31)), ("b", BareVal::Int(31))])
                .await;
        }
    }

    mod test_oror {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 123 || 333;
                    mixed b = 0;
                    mixed c = b || a;
                "##};

            check_committed_globals(
                code,
                &[
                    ("a", BareVal::Int(123)),
                    ("b", BareVal::Int(0)),
                    ("c", BareVal::Int(123)),
                ],
            )
            .await;
        }
    }

    mod test_populate_argv {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    void create() {
                        do_thing(1, 2, 3, "foo", ({ "bar", "baz", 4.13 }), ([ "a": 123 ]));
                    }

                    void do_thing(int a, int b, ...) {
                        dump(argv);
                        debug("snapshot_stack");
                    }
                "##};

            let mut mapping = HashMap::new();
            mapping.insert(BareVal::String("a".into()), BareVal::Int(123));
            check_snapshot_vars(
                code,
                &[
                    ("a", BareVal::Int(1)),
                    ("b", BareVal::Int(2)),
                    (
                        "argv",
                        BareVal::Array(vec![
                            BareVal::Int(3),
                            BareVal::String("foo".into()),
                            BareVal::Array(vec![
                                BareVal::String("bar".into()),
                                BareVal::String("baz".into()),
                                BareVal::Float(4.13.into()),
                            ]),
                            BareVal::Mapping(mapping),
                        ]),
                    ),
                ],
            )
            .await;
        }

        #[tokio::test]
        async fn test_creates_empty_array() {
            let code = indoc! { r##"
                    void create() {
                        function f = (: [int i = 69, ...] dump(i, argv); argv :);
                        f();
                    }
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;
            BareVal::Array(vec![]).assert_equal(&ctx.global_state, &ctx.result().unwrap());
        }

        #[tokio::test]
        async fn argv_is_a_named_local() {
            let code = indoc! { r##"
                    void create() {
                        do_thing(1, 2, 3, "foo");
                    }

                    void do_thing(int a, int b, ...) {
                        debug("snapshot_stack");
                    }
                "##};

            check_snapshot_vars(
                code,
                &[(
                    "argv",
                    BareVal::Array(vec![BareVal::Int(3), BareVal::String("foo".into())]),
                )],
            )
            .await;
        }
    }

    mod test_populate_defaults {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    void create() {
                        do_thing(45, 34, 7.77);
                    }

                    void do_thing(int a, int b, float d = 6.66, string s = "snuh", mixed *muh = ({ "a string", 3, 2.44 })) {
                        debug("snapshot_stack");
                    }
                "##};

            check_snapshot_vars(
                code,
                &[
                    ("a", BareVal::Int(45)),
                    ("b", BareVal::Int(34)),
                    ("d", BareVal::Float(7.77.into())),
                    ("s", BareVal::String("snuh".into())),
                    (
                        "muh",
                        BareVal::Array(vec![
                            BareVal::String("a string".into()),
                            BareVal::Int(3),
                            BareVal::Float(2.44.into()),
                        ]),
                    ),
                ],
            )
            .await;
        }
    }

    mod test_range {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = ({ 1, 2, 3 })[1..];
                "##};

            check_committed_globals(
                code,
                &[("a", BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)]))],
            )
            .await;
        }
    }

    mod test_regcopy {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 4;
                    mixed b = a;
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(4)), ("b", BareVal::Int(4))]).await;
        }
    }

    mod test_ret {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    int create() { return 666; }
                "##};

            let task = run_prog(code).await;
            BareVal::Int(666)
                .assert_equal(&task.context.global_state, &task.context.result().unwrap());
        }
    }

    mod test_shl {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 12345 << 6;
                    mixed b = 0 << a;
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(790080)), ("b", BareVal::Int(0))])
                .await;
        }
    }

    mod test_shr {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 12345 >> 6;
                    mixed b = 0 >> a;
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(192)), ("b", BareVal::Int(0))])
                .await;
        }
    }

    mod test_destructed_mapping_keys {
        use super::*;

        async fn globals(code: &str) -> (Arc<GlobalState>, HashMap<String, LpcRef>) {
            let task = run_prog(code).await;
            let gs = task.context.global_state.clone();
            let globals = committed_globals_by_name(&gs, &task.context.process);
            (gs, globals)
        }

        fn committed_len(gs: &Arc<GlobalState>, mapping: &LpcRef) -> usize {
            let LpcRef::Mapping(cell) = mapping else {
                panic!("not a mapping: {mapping}");
            };
            gs.committed_mapping(cell.id).unwrap().len()
        }

        #[tokio::test]
        async fn a_dead_key_reads_as_zero() {
            let code = indoc! { r##"
                mapping m = ([ 0: 7 ]);
                int got;
                void create() {
                    object ob = clone_object("/clone_target");
                    m[ob] = 1;
                    destruct(ob);
                    got = m[ob];
                }
            "##};

            let (_, globals) = globals(code).await;
            assert_eq!(globals["got"], LpcRef::from(7));
        }

        #[tokio::test]
        async fn a_dead_key_stores_as_zero() {
            let code = indoc! { r##"
                mapping m = ([]);
                int at_zero;
                int size;
                void create() {
                    object ob = clone_object("/clone_target");
                    destruct(ob);
                    m[ob] = 9;
                    at_zero = m[0];
                    size = sizeof(m);
                }
            "##};

            let (_, globals) = globals(code).await;
            assert_eq!(globals["at_zero"], LpcRef::from(9));
            assert_eq!(globals["size"], LpcRef::from(1));
        }

        #[tokio::test]
        async fn a_dead_key_in_a_literal_is_zero() {
            let code = indoc! { r##"
                mapping m;
                int got;
                void create() {
                    object ob = clone_object("/clone_target");
                    destruct(ob);
                    m = ([ ob: 5 ]);
                    got = m[0];
                }
            "##};

            let (_, globals) = globals(code).await;
            assert_eq!(globals["got"], LpcRef::from(5));
        }

        #[tokio::test]
        async fn sizeof_drops_entries_whose_key_died() {
            let code = indoc! { r##"
                mapping m = ([ "x": 2 ]);
                int size;
                void create() {
                    object ob = clone_object("/clone_target");
                    m[ob] = 1;
                    destruct(ob);
                    size = sizeof(m);
                }
            "##};

            let (gs, globals) = globals(code).await;
            assert_eq!(globals["size"], LpcRef::from(1));
            assert_eq!(committed_len(&gs, &globals["m"]), 1);
        }

        #[tokio::test]
        async fn foreach_skips_entries_whose_key_died() {
            let code = indoc! { r##"
                mapping m = ([ "x": 2 ]);
                int seen;
                void create() {
                    object ob = clone_object("/clone_target");
                    m[ob] = 1;
                    destruct(ob);
                    foreach(k, v: m) { seen++; }
                }
            "##};

            let (_, globals) = globals(code).await;
            assert_eq!(globals["seen"], LpcRef::from(1));
        }

        #[tokio::test]
        async fn an_untraversed_mapping_is_not_written() {
            let code = indoc! { r##"
                mapping m = ([ "x": 2 ]);
                void create() {
                    object ob = clone_object("/clone_target");
                    m[ob] = 1;
                    destruct(ob);
                }
            "##};

            let (gs, globals) = globals(code).await;
            assert_eq!(committed_len(&gs, &globals["m"]), 2);
        }
    }

    mod test_sizeof {
        use std::sync::Arc;

        use lpc_rs_asm::instruction::Instruction::{Ret, Sizeof};
        use lpc_rs_core::{INIT_GLOBALS, lpc_path::LpcPath, lpc_type::LpcType};
        use lpc_rs_function_support::{
            constant::LpcConstant, function_prototype::FunctionPrototypeBuilder,
        };
        use lpc_rs_utils::lpc_string::LpcString;

        use super::*;
        use crate::interpreter::program::Program;
        use crate::test_support::test_config;

        #[tokio::test]
        async fn stores_the_value_for_arrays() {
            let code = indoc! { r##"
                    int a = sizeof(({ 1, 2, 3 }));
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(3))]).await;
        }

        #[tokio::test]
        async fn the_size_reaches_the_assignment() {
            let code = indoc! { r##"
                    mapping m = ([ "a": 1, 'b': 2 ]);
                    int a = sizeof(({ 1, 2, 3 }));
                    int b = sizeof(m);
                    int c;
                    void create() { c = sizeof(m) + sizeof(({ 1 })); }
                "##};

            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, &task.context.process);

            assert_eq!(globals["a"], LpcRef::from(3));
            assert_eq!(globals["b"], LpcRef::from(2));
            assert_eq!(globals["c"], LpcRef::from(3));
        }

        #[tokio::test]
        async fn stores_the_value_for_mappings() {
            let code = indoc! { r##"
                    int a = sizeof(([ "a": 1, 'b': 2, 3: ({ 4, 5, 6 }), 0: 0 ]));
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(4))]).await;
        }

        #[tokio::test]
        async fn stores_the_value_for_strings() {
            let config = Arc::new(test_config());
            let path = Arc::new(LpcPath::new_in_game("/my_file.c", "/", &*config.lib_dir));

            let prototype = FunctionPrototypeBuilder::default()
                .name(INIT_GLOBALS)
                .filename(path.clone())
                .return_type(LpcType::Void)
                .build()
                .unwrap();
            let initializer = ProgramFunction {
                prototype,
                num_locals: 1,
                num_upvalues: 0,
                instructions: vec![
                    Sizeof(Register(0).as_constant(), Register(1).as_local()),
                    Ret,
                ],
                debug_spans: vec![None, None],
                labels: Some(HashMap::new()),
                local_variables: Default::default(),
                arg_locations: Default::default(),
                constants: vec![LpcConstant::String(Arc::new(LpcString::Static(ustr(
                    "Hello, world!",
                ))))],
            }
            .into();

            let program = Program {
                filename: path,
                functions: Box::new(IndexMap::default()),
                initializer: Some(initializer),
                // num_init_registers: 2,
                ..Default::default()
            };

            let (tx, _rx) = mpsc::channel(128);
            let global_state = GlobalState::new(config, tx);

            let task = initialize_program::<20>(program, global_state)
                .await
                .expect("failed to initialize");

            let registers = &task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::Int(13)];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, registers);
        }
    }

    mod test_staging {
        use std::sync::Arc;

        use lpc_rs_asm::instruction::Instruction::{self, *};
        use lpc_rs_core::{
            INIT_GLOBALS, function_arity::FunctionArity, function_receiver::FunctionReceiver,
            lpc_path::LpcPath, lpc_type::LpcType,
        };
        use lpc_rs_function_support::{
            constant::LpcConstant, function_prototype::FunctionPrototypeBuilder,
        };
        use lpc_rs_utils::lpc_string::LpcString;

        use super::*;
        use crate::{
            interpreter::{efun::EFUN_PROTOTYPES, program::Program},
            test_support::test_config,
        };

        fn local(n: RegisterSize) -> RegisterVariant {
            Register(n).as_local()
        }

        fn constant(n: RegisterSize) -> RegisterVariant {
            Register(n).as_constant()
        }

        fn string(s: &str) -> LpcConstant {
            LpcConstant::String(Arc::new(LpcString::Static(ustr(s))))
        }

        /// `second(a, b)` returns `b`.
        fn second(path: &Arc<LpcPath>) -> ProgramFunction {
            let prototype = FunctionPrototypeBuilder::default()
                .name("second")
                .filename(path.clone())
                .return_type(LpcType::Mixed(false))
                .arity(FunctionArity::new(2))
                .arg_types(vec![LpcType::Mixed(false), LpcType::Mixed(false)])
                .build()
                .unwrap();
            let mut func = ProgramFunction::new(prototype, 0);
            func.push_instruction(Copy(local(2), local(0)), None);
            func.push_instruction(Ret, None);
            func
        }

        /// Run a hand-assembled initializer that never clears a staging
        /// vector, alongside `second`, and return its registers.
        async fn run(
            instructions: Vec<Instruction>,
            num_locals: RegisterSize,
            constants: Vec<LpcConstant>,
        ) -> (Arc<GlobalState>, Vec<LpcRef>) {
            let config = Arc::new(test_config());
            let path = Arc::new(LpcPath::new_in_game("/my_file.c", "/", &*config.lib_dir));

            let prototype = FunctionPrototypeBuilder::default()
                .name(INIT_GLOBALS)
                .filename(path.clone())
                .return_type(LpcType::Void)
                .build()
                .unwrap();
            let mut initializer = ProgramFunction::new(prototype, num_locals);
            for instruction in instructions {
                initializer.push_instruction(instruction, None);
            }
            initializer.constants = constants;

            let mut functions = IndexMap::default();
            functions.insert("second".to_string(), Arc::new(second(&path)));
            let program = Program {
                filename: path,
                functions: Box::new(functions),
                initializer: Some(initializer.into()),
                ..Default::default()
            };

            let (tx, _rx) = mpsc::channel(128);
            let global_state = GlobalState::new(config, tx);
            let task = initialize_program::<20>(program, global_state)
                .await
                .expect("failed to initialize");
            let registers = task.popped_frame.unwrap().registers.to_vec();
            (task.context.global_state, registers)
        }

        fn stringp() -> Instruction {
            CallEfun(u8::try_from(EFUN_PROTOTYPES.get_index_of("stringp").unwrap()).unwrap())
        }

        #[tokio::test]
        async fn an_efun_call_starts_with_only_its_own_arguments() {
            let (gs, registers) = run(
                vec![
                    PushArg(constant(0)),
                    stringp(),
                    Copy(local(0), local(1)),
                    PushArg(constant(1)),
                    stringp(),
                    Copy(local(0), local(2)),
                    Ret,
                ],
                2,
                vec![LpcConstant::Int(7), string("cde")],
            )
            .await;

            BareVal::Int(1).assert_equal(&gs, &registers[2]);
        }

        #[tokio::test]
        async fn a_local_call_starts_with_only_its_own_arguments() {
            let (gs, registers) = run(
                vec![
                    PushArg(constant(0)),
                    Call(ustr("second")),
                    Copy(local(0), local(1)),
                    PushArg(constant(1)),
                    Call(ustr("second")),
                    Copy(local(0), local(2)),
                    Ret,
                ],
                2,
                vec![string("ab"), string("cde")],
            )
            .await;

            BareVal::Int(0).assert_equal(&gs, &registers[2]);
        }

        #[tokio::test]
        async fn a_pointer_call_starts_with_only_its_own_arguments() {
            let (gs, registers) = run(
                vec![
                    FunctionPtrConst {
                        location: local(1),
                        receiver: FunctionReceiver::Local,
                        name: ustr("second"),
                    },
                    PushArg(constant(0)),
                    CallFp(local(1)),
                    Copy(local(0), local(2)),
                    PushArg(constant(1)),
                    CallFp(local(1)),
                    Copy(local(0), local(3)),
                    Ret,
                ],
                3,
                vec![string("ab"), string("cde")],
            )
            .await;

            BareVal::Int(0).assert_equal(&gs, &registers[3]);
        }

        #[tokio::test]
        async fn an_array_starts_with_only_its_own_items() {
            let (gs, registers) = run(
                vec![
                    PushArrayItem(constant(0)),
                    AConst(local(1)),
                    PushArrayItem(constant(1)),
                    AConst(local(2)),
                    Ret,
                ],
                2,
                vec![string("ab"), string("cde")],
            )
            .await;

            BareVal::Array(vec![BareVal::String("cde".into())]).assert_equal(&gs, &registers[2]);
        }

        #[tokio::test]
        async fn a_mapping_starts_with_only_its_own_pairs() {
            let (gs, registers) = run(
                vec![
                    PushArrayItem(constant(0)),
                    PushArrayItem(constant(2)),
                    MapConst(local(1)),
                    PushArrayItem(constant(1)),
                    PushArrayItem(constant(3)),
                    MapConst(local(2)),
                    Ret,
                ],
                2,
                vec![
                    string("ab"),
                    string("cde"),
                    LpcConstant::Int(1),
                    LpcConstant::Int(2),
                ],
            )
            .await;

            let expected = HashMap::from([(BareVal::String("cde".into()), BareVal::Int(2))]);
            BareVal::Mapping(expected).assert_equal(&gs, &registers[2]);
        }

        #[tokio::test]
        async fn a_pointer_starts_with_only_its_own_partial_arguments() {
            let (gs, registers) = run(
                vec![
                    PushPartialArg(Some(constant(0))),
                    FunctionPtrConst {
                        location: local(1),
                        receiver: FunctionReceiver::Local,
                        name: ustr("second"),
                    },
                    PushPartialArg(Some(constant(1))),
                    FunctionPtrConst {
                        location: local(2),
                        receiver: FunctionReceiver::Local,
                        name: ustr("second"),
                    },
                    Ret,
                ],
                2,
                vec![string("ab"), string("cde")],
            )
            .await;

            BareVal::Function("second".into(), vec![Some(BareVal::String("cde".into()))])
                .assert_equal(&gs, &registers[2]);
        }
    }

    mod test_store {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    void create() {
                        mixed a = ({ 1, 2, 3 });
                        a[2] = 678;
                    }
                "##};

            check_popped_vars(
                code,
                &[(
                    "a",
                    BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(678)]),
                )],
            )
            .await;
        }
    }

    mod test_xor {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = 15 ^ 27;
                    mixed b = 0 ^ a;
                "##};

            check_committed_globals(code, &[("a", BareVal::Int(20)), ("b", BareVal::Int(20))])
                .await;
        }
    }
}

mod test_limits {

    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::test_config_builder;

    #[tokio::test]
    async fn errors_on_stack_overflow() {
        let code = indoc! { r##"
                int kab00m = marf();

                int marf() {
                    return marf();
                }
            "##};

        let r = try_run_prog(code).await;

        assert_eq!(r.unwrap_err().to_string(), "stack overflow");
    }

    #[tokio::test]
    async fn errors_on_too_long_evaluation() {
        let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};

        let config = test_config_builder!()
            .max_execution_time(40_u64)
            .build()
            .unwrap();

        let r = try_run_prog_with_config(code, Arc::new(config)).await;

        assert_eq!(
            r.unwrap_err().to_string(),
            "evaluation limit of 40ms has been reached"
        );
    }
}

mod test_globals {
    use super::*;
    use crate::interpreter::task::tests::BareVal::*;

    #[tokio::test]
    async fn test_frame_globals() {
        let code = indoc! { r##"
                int i = 0;
                function inc = (: i++ :);
                int j = inc();
                int k = inc();
            "##};

        check_committed_globals(
            code,
            &[
                ("i", Int(2)),
                ("inc", Function("closure-0".to_string(), vec![])),
                ("j", Int(0)),
                ("k", Int(1)),
            ],
        )
        .await;
    }
}

mod test_upvalues {
    use super::*;
    use crate::interpreter::task::tests::BareVal::*;

    async fn check_frame_upvalue_ptrs<T>(code: &str, upvalue_ptrs: &[T])
    where
        T: Into<Register> + Copy,
    {
        let mut task = run_prog(code).await;

        let snapshot = &mut task.snapshots.pop().unwrap();
        snapshot.pop(); // pop off the init frame

        let frame = snapshot.pop().unwrap();

        // Cells are identities; only how many the frame holds is observable.
        assert_eq!(upvalue_ptrs.len(), frame.upvalue_ptrs.len());
    }

    #[tokio::test]
    async fn test_local_captures() {
        let code = indoc! { r##"
                void create() {
                    int i = 0;
                    function inc = (: i++; debug("snapshot_stack"); i :);
                    int j = inc();
                    int k = inc();
                }
            "##};

        let expected = vec![Register(0)];
        check_frame_upvalue_ptrs(code, &expected).await;

        check_snapshot_vars(code, &[]).await;
    }

    #[tokio::test]
    async fn upvalue_writes_survive_gc() {
        // a closure captures a *local*, so `i++` and `return i`
        // route through the cell arm of `CallFrame::bump_in_location` /
        // `CallFrame::get_location` via the transaction. The cell is committed
        // during the eval; after the frame that held the closure is gone, the
        // cell is no longer marked (no live `FunctionPtr` references it), so
        // a sweep drops its `VarId` out of the committer's world. The txn
        // must remain usable and consistent afterwards — no torn cell.
        let code = indoc! { r##"
                int bump() {
                    int i = 0;
                    function inc = (: i++ :);
                    inc();
                    inc();
                    return i;
                }
            "##};

        let mut task = run_prog(code).await;
        let f = task
            .context
            .process()
            .program
            .unmangled_functions
            .get("bump")
            .cloned()
            .expect("bump");
        task.timed_eval(f, &[], 0).await.unwrap();
        assert_eq!(
            task.result(),
            Some(LpcRef::from(2)),
            "bump() should have incremented the captured local twice"
        );

        // The frame holding the closure is gone, so its cell is unreachable.
        let report = task.context.global_state.gc().await.unwrap().unwrap();
        assert_eq!(report.reclaimed, 1, "the dead cell is reclaimed");
    }

    #[tokio::test]
    async fn test_shared_captures() {
        let code = indoc! { r##"
                void create() {
                    int i = 0;
                    function inc = (: i++ :);
                    int j = inc();
                    int k = inc();
                    debug("snapshot_stack");
                }
            "##};

        let expected = [
            ("i", Int(2)),
            ("j", Int(0)),
            ("k", Int(1)),
            ("inc", Function("closure-0".to_string(), vec![])),
        ];

        check_snapshot_vars(code, &expected).await;
    }

    #[tokio::test]
    async fn test_arg_captures() {
        let code = indoc! { r##"
                void create() {
                    function add = make_adder(10);
                    int j = add(5);
                    int k = add(-20);
                    int l = add();
                    function add2 = make_adder(666);
                    int m = add2(1);
                    int n = add2();
                    debug("snapshot_stack");
                }

                function make_adder(int i) {
                    return (: [int j = i] j + $1 :);
                }
            "##};

        let expected = [
            ("j", Int(10)),
            ("k", Int(-40)),
            ("l", Int(20)),
            ("m", Int(2)),
            ("n", Int(1332)),
            ("add", Function("closure-0".into(), vec![])),
            ("add2", Function("closure-0".into(), vec![])),
        ];
        check_snapshot_vars(code, &expected).await;
    }

    #[tokio::test]
    async fn test_higher_order() {
        let code = indoc! { r##"
                void create() {
                    function make_counter = make_make_counter(0);

                    function counter1 = make_counter();
                    function counter2 = make_counter();
                    function counter3 = make_counter(100);

                    int c1 = counter1();
                    int c2 = counter2(4);
                    int c3 = counter3();

                    debug("snapshot_stack");
                }

                function make_make_counter(int default_value) {
                    int counter = default_value;
                    return (: [int count_by = 1]
                        return (: [int j = count_by] counter += j :);
                    :);
                }
            "##};

        let expected = [
            ("c1", Int(1)),
            ("c2", Int(5)),
            ("c3", Int(105)),
            ("make_counter", Function("closure-1".into(), vec![])),
            ("counter1", Function("closure-0".into(), vec![])),
            ("counter2", Function("closure-0".into(), vec![])),
            ("counter3", Function("closure-0".into(), vec![])),
        ];

        check_snapshot_vars(code, &expected).await;
    }

    #[tokio::test]
    async fn test_higher_order_with_implicit_vars() {
        let code = indoc! { r##"
                void create() {
                    function make = make_maker();

                    function made1 = make(1);
                    function made2 = make(2);

                    int c1 = made1();
                    int c2 = made2(69);

                    debug("snapshot_stack");
                }

                function make_maker() {
                    return (: [int i]
                        return (: $1 :); // This should *not* capture `i`
                    :);
                }
            "##};

        let expected = [
            ("c1", Int(0)),
            ("c2", Int(69)),
            ("make", Function("closure-1".into(), vec![])),
            ("made1", Function("closure-0".into(), vec![])),
            ("made2", Function("closure-0".into(), vec![])),
        ];

        check_snapshot_vars(code, &expected).await;
    }

    #[tokio::test]
    async fn test_higher_order_with_partial_application() {
        let code = indoc! { r##"
                void create() {
                    function partial = &make_maker(,666);

                    function maker = partial("hello");

                    function made1 = maker(1, 2);
                    function made2 = (: maker(3, $1) :); // closure-0
                    made2 = made2(77);

                    int c1 = made1(-4);
                    int c2 = made2(69);

                    debug("snapshot_stack");
                }

                function make_maker(string str, int i) {
                    return (: [int j, int k] // closure-2
                        return (: [int l] str + i + " " + j + " " + k + " " + l :); // closure-1
                    :);
                }
            "##};

        let expected = [
            ("c1", String("hello666 1 2 -4".into())),
            ("c2", String("hello666 3 77 69".into())),
            (
                "partial",
                Function("make_maker".into(), vec![None, Some(Int(666))]),
            ),
            ("maker", Function("closure-2".into(), vec![])),
            ("made1", Function("closure-1".into(), vec![])),
            ("made2", Function("closure-1".into(), vec![])),
        ];

        check_snapshot_vars(code, &expected).await;
    }

    #[tokio::test]
    async fn test_upvalued_ellipsis() {
        let code = indoc! { r##"
                void create() {
                    function partial = &make_maker(,666);

                    function maker = partial("hello");

                    function made1 = maker(123, 456);
                    function made2 = (: maker("world", $1) :); // closure-0
                    made2 = made2(77);

                    int c1 = made1(0);
                    int c2 = made2(1);

                    debug("snapshot_stack");
                }

                function make_maker(string str, int _i) {
                    return (: [...] // closure-2
                        dump("maker", argv);
                        return (: [int i] dump(str, argv[i]); argv[i] :); // closure-1
                    :);
                }
            "##};

        let expected = [
            ("c1", Int(123)),
            ("c2", Int(77)),
            (
                "partial",
                Function("make_maker".into(), vec![None, Some(Int(666))]),
            ),
            ("maker", Function("closure-2".into(), vec![])),
            ("made1", Function("closure-1".into(), vec![])),
            ("made2", Function("closure-1".into(), vec![])),
        ];

        check_snapshot_vars(code, &expected).await;
    }

    mod layout {
        use super::*;

        async fn committed_r(code: &str) -> LpcRef {
            let task = run_prog(code).await;
            let globals =
                committed_globals_by_name(&task.context.global_state, task.context.process());
            globals["r"].clone()
        }

        #[tokio::test]
        async fn a_declaration_without_initializer_gets_a_cell() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function fact;
                    fact = (: if ($1 <= 1) return 1; return $1 * fact($1 - 1); :);
                    r = fact(5);
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(120)));
        }

        #[tokio::test]
        async fn sibling_closures_each_own_their_captures() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function a = (: int j = 1; function x = (: j :); return x(); :);
                    function b = (: int k = 2; function y = (: k :); return y(); :);
                    r = a() * 10 + b();
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(12)));
        }

        #[tokio::test]
        async fn a_capture_declared_after_a_capturing_closure_gets_a_cell() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function a = (: int j = 1; function x = (: j :); return x(); :);
                    int later = 5;
                    function g = (: later :);
                    r = a() * 10 + g();
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(15)));
        }

        #[tokio::test]
        async fn a_declaration_in_a_loop_body_gets_a_cell_per_iteration() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function *fs = ({});
                    for (int i = 0; i < 3; i++) {
                        int j = i * 10;
                        function f = (: j :);
                        fs += ({ f });
                    }
                    function f0 = fs[0];
                    function f1 = fs[1];
                    function f2 = fs[2];
                    r = f0() * 100 + f1() * 10 + f2();
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(120)));
        }

        #[tokio::test]
        async fn a_variable_declared_once_is_shared_by_every_iteration() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function *fs = ({});
                    for (int i = 0; i < 3; i++) {
                        function f = (: i :);
                        fs += ({ f });
                    }
                    function f0 = fs[0];
                    function f1 = fs[1];
                    function f2 = fs[2];
                    r = f0() * 100 + f1() * 10 + f2();
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(333)));
        }

        #[tokio::test]
        async fn a_while_body_declaration_gets_a_cell_per_iteration() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function *fs = ({});
                    int i = 0;
                    while (i < 3) {
                        int j = i;
                        function f = (: j :);
                        fs += ({ f });
                        i++;
                    }
                    function f0 = fs[0];
                    function f1 = fs[1];
                    function f2 = fs[2];
                    r = f0() * 100 + f1() * 10 + f2();
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(12)));
        }

        #[tokio::test]
        async fn an_earlier_closure_keeps_the_cell_it_captured() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function a;
                    function b;
                    for (int k = 0; k < 2; k++) {
                        int j = k;
                        if (k == 0) { a = (: j :); } else { b = (: j :); }
                    }
                    r = a() * 10 + b();
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(1)));
        }

        #[tokio::test]
        async fn a_closure_may_capture_the_variable_it_initializes() {
            let code = indoc! { r##"
                int r;
                void create() {
                    function fact = (: if ($1 <= 1) return 1; return $1 * fact($1 - 1); :);
                    r = fact(5);
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(120)));
        }

        #[tokio::test]
        async fn a_variable_read_in_its_own_initializer_reads_zero() {
            let code = indoc! { r##"
                int g = g + 10;
                int r;
                void create() {
                    int x = x + 1;
                    r = x * 100 + g;
                }
            "## };
            assert_eq!(committed_r(code).await, LpcRef::Int(LpcInt(110)));
        }
    }
}

mod test_gc {
    use super::*;

    #[tokio::test]
    async fn test_gc_is_accurate() {
        let code = indoc! { r##"
                int k = 0;

                void create() {
                    function stored = store();
                    function stored2 = store();
                    function stored3 = store();

                    int i = stored();
                    int j = stored2();
                    int l = stored3();
                }

                function store() {
                    int i = k++;

                    return (: i :);
                }
            "##};

        let task = run_prog(code).await;
        let ctx = &task.context;

        // Three `store()` frames each minted a cell; every closure over them is gone.
        let report = ctx.global_state.gc().await.unwrap().unwrap();
        assert_eq!(report.reclaimed, 3);
    }
}

mod object_identity {
    use super::*;

    #[tokio::test]
    async fn objects_compare_and_key_by_identity() {
        let code = indoc! { r##"
            object o1 = this_object();
            object o2 = this_object();
            object w1 = clone_object("/std/widget");
            object w2 = clone_object("/std/widget");
            int same = o1 == o2;
            int self_same = o1 == o1;
            int diff = o1 != o2;
            int other = o1 == w1;
            int clones = w1 == w2;
            mapping m = ([ o1: 1, w1: 2 ]);
            int by_other_handle = m[o2];
            int by_same_handle = m[o1];
            int by_clone = m[w1];
            int missing_clone = m[w2];
        "##};

        let task = run_prog(code).await;
        let ctx = task.context;
        let values = committed_globals_by_name(&ctx.global_state, ctx.process());
        let expect = |name: &str, value: i64| {
            BareVal::Int(value).assert_equal(&ctx.global_state, values.get(name).unwrap());
        };

        expect("same", 1);
        expect("self_same", 1);
        expect("diff", 0);
        expect("other", 0);
        expect("clones", 0);
        expect("by_other_handle", 1);
        expect("by_same_handle", 1);
        expect("by_clone", 2);
        expect("missing_clone", 0);
    }
}

mod destructed_refs {
    use super::*;
    use crate::{
        compile_time_config::MAX_CALL_STACK_SIZE,
        interpreter::{
            object_space::ObjectSpace, stm::TxnHandle, task::task_template::TaskTemplate,
        },
        test_support::compile_prog,
        util::process_builder::process_insert_and_initialize_program,
    };

    #[tokio::test]
    async fn reads_as_zero_in_the_destructing_attempt() {
        let code = indoc! { r##"
            object ob;
            object alias;
            int is_zero, ne_zero, not_ob, objp, branch, alias_eq, named;

            void create() {
                ob = clone_object("/std/widget");
                alias = ob;
                destruct(ob);
                is_zero = ob == 0;
                ne_zero = ob != 0;
                not_ob = !ob;
                objp = objectp(ob);
                if (ob) branch = 1; else branch = 2;
                alias_eq = ob == alias;
                named = file_name(ob) == 0;
            }
        "##};

        let task = run_prog(code).await;
        let ctx = task.context;
        let values = committed_globals_by_name(&ctx.global_state, ctx.process());
        let expect = |name: &str, value: i64| {
            BareVal::Int(value).assert_equal(&ctx.global_state, values.get(name).unwrap());
        };

        expect("is_zero", 1);
        expect("ne_zero", 0);
        expect("not_ob", 1);
        expect("objp", 0);
        expect("branch", 2);
        expect("alias_eq", 1);
        expect("named", 1);
    }

    #[tokio::test]
    async fn a_discarded_attempt_leaves_the_object_live() {
        let code = indoc! { r##"
            void create() {
                destruct(this_object());
                throw("boom");
            }
        "##};

        let (program, config, se_proc) = compile_prog(code).await;
        let (tx, _rx) = tokio::sync::mpsc::channel(128);
        let global_state = Arc::new(GlobalState::new(config, tx));
        ObjectSpace::insert_process_physical(&global_state.object_space, se_proc);
        let process = Arc::new(Process::new(program));

        let result = process_insert_and_initialize_program::<MAX_CALL_STACK_SIZE>(
            process.clone(),
            TaskTemplate::from(global_state.clone()),
        )
        .await;

        assert!(result.is_err());
        assert!(global_state.object_space.lookup("/my_file").is_some());
        let lpc_ref = LpcRef::from(Arc::downgrade(&process));
        assert!(lpc_ref.live_object(&TxnHandle::empty()).is_some());
    }

    #[tokio::test]
    async fn this_object_reads_as_zero_after_self_destruct() {
        let code = indoc! { r##"
            int this_zero;

            void create() {
                destruct(this_object());
                this_zero = this_object() == 0;
            }
        "##};

        let task = run_prog(code).await;
        let ctx = task.context;
        let values = committed_globals_by_name(&ctx.global_state, ctx.process());
        BareVal::Int(1).assert_equal(&ctx.global_state, values.get("this_zero").unwrap());
    }
}

mod bare_val {
    use super::*;

    #[test]
    fn separately_built_keys_find_each_other() {
        let mut map = HashMap::new();
        map.insert(BareVal::Object("/my_file".into()), BareVal::Int(1));
        map.insert(BareVal::Array(vec![BareVal::Int(2)]), BareVal::Int(2));
        map.insert(BareVal::Mapping(HashMap::new()), BareVal::Int(3));

        assert_eq!(
            map.get(&BareVal::Object("/my_file".into())),
            Some(&BareVal::Int(1))
        );
        assert_eq!(
            map.get(&BareVal::Array(vec![BareVal::Int(2)])),
            Some(&BareVal::Int(2))
        );
        assert_eq!(
            map.get(&BareVal::Mapping(HashMap::new())),
            Some(&BareVal::Int(3))
        );
    }
}
