use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use decorum::Total;
use indexmap::IndexMap;
use indoc::indoc;
use lpc_rs_core::{LpcFloatInner, LpcIntInner, RegisterSize, register::RegisterVariant};
use tokio::sync::mpsc;

use super::*;
use crate::{
    interpreter::{
        CommittedReader,
        lpc_ref::{LpcRef, NULL},
        process::Process,
        vm::global_state::GlobalState,
    },
    test_support::{initialize_program, run_prog, try_run_prog, try_run_prog_with_config},
};

/// Committed global values for a process, read through the committer.
fn committed_global_values(gs: &Arc<GlobalState>, proc: &Process) -> Vec<LpcRef> {
    let mut values = Vec::new();
    for i in 0..gs.global_slot_count(proc) {
        values.push(gs.committed_global(proc, i as RegisterSize));
    }
    values
}

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
    use crate::interpreter::bank::RefBank;

    async fn snapshot_registers(code: &str) -> (Arc<GlobalState>, RefBank) {
        let mut task = run_prog(code).await;
        let gs = task.context.global_state.clone();
        let mut stack = task.snapshots.pop().unwrap();

        // The top of the stack in the snapshot is the object initialization frame,
        // which is not what we care about here, so we get the second-to-top frame
        // instead.
        let index = stack.len() - 2;

        (gs, std::mem::take(&mut stack[index].registers))
    }

    mod test_aconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed *a = ({ 12, 4.3, "hello", ({ 1, 2, 3 }) });
                "##};
            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(12),
                BareVal::Float(LpcFloatInner::from(4.3)),
                BareVal::String("hello".into()),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                BareVal::Array(vec![
                    BareVal::Int(12),
                    BareVal::Float(LpcFloatInner::from(4.3)),
                    BareVal::String("hello".into()),
                    BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                ]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(11),
                BareVal::Int(0),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(123),
                BareVal::Int(333),
                BareVal::Int(333),
                BareVal::Int(0),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(-1),
                BareVal::Int(7),
                BareVal::Int(-8),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(666), BareVal::Int(666)];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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
            // this is deprecated behavior that emits a warning, but probably won't ever be removed completely.
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
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = this_object();
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Object("/my_file".into()),
                BareVal::Object("/my_file".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_call_simul_efun {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = simul_efun("marf");
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::String("this is a simul_efun: marf".into()),
                BareVal::String("marf".into()),
                BareVal::String("this is a simul_efun: marf".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_call_fp {
        use claims::assert_ok;

        use super::*;
        use crate::{interpreter::vm::Vm, test_support::test_config};

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    function q = tacos;
                    int a = q(666);
                    int tacos(int j) { return j + 1; }
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(667),
                BareVal::Function("tacos".into(), vec![]),
                BareVal::Int(666),
                BareVal::Int(667),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::String("adding some! 670".into()),
                BareVal::String("adding some!".into()),
                BareVal::Function(
                    "tacos".into(),
                    vec![None, Some(BareVal::String("adding some!".into()))],
                ),
                BareVal::Int(666),
                BareVal::Int(4),
                BareVal::String("adding some! 670".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::String("my_string! awesome!".into()),
                BareVal::String("my_string!".into()),
                BareVal::Function(
                    "tacos".into(),
                    vec![Some(BareVal::String("my_string!".into()))],
                ),
                BareVal::String("my_string! awesome!".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::String("adding some! 223".into()),
                BareVal::String("adding some!".into()),
                BareVal::Function(
                    "tacos".into(),
                    vec![None, Some(BareVal::String("adding some!".into()))],
                ),
                BareVal::Int(666),
                BareVal::Int(4),
                BareVal::String("adding some! 670".into()),
                BareVal::Int(123),
                BareVal::String("adding some! 223".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(69),
                BareVal::String("adding some!".into()),
                BareVal::Int(666),
                BareVal::Int(123),
                BareVal::Function(
                    "tacos".to_string(),
                    vec![
                        None,
                        Some(BareVal::String("adding some!".into())),
                        None,
                        Some(BareVal::Int(666)),
                        Some(BareVal::Int(123)),
                    ],
                ),
                BareVal::Int(42),
                BareVal::Int(4),
                BareVal::String("should be in argv".into()),
                BareVal::Int(46),
                BareVal::Int(69),
                BareVal::Int(69),
            ];
            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::String("widget: 42. awesome!".into()),
                BareVal::String("awesome!".into()),
                BareVal::Function(
                    "name".into(),
                    vec![None, Some(BareVal::String("awesome!".into()))],
                ),
                BareVal::Object("/my_file".into()),
                BareVal::Int(666),
                BareVal::String("me: 666. awesome!".into()),
                BareVal::String("/std/widget".into()),
                BareVal::Object("/std/widget#0".into()),
                BareVal::Int(42),
                BareVal::String("widget: 42. awesome!".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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
            let task = vm
                .initialize_process_from_code("doody.c", code)
                .await
                .unwrap();

            let object_space = task.context.object_space();

            let widget = object_space.lookup("/std/widget").unwrap();
            assert!(widget.flags.test(ObjectFlags::Initialized));
        }

        #[tokio::test]
        async fn is_0_for_call_other_private_functions() {
            let code = indoc! { r##"
                    function q = &(this_object())->tacos(, "adding some!");
                    int a = q(666, 4);
                    int b = q(123);
                    private string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("adding some!".into()),
                BareVal::Object("/my_file".into()),
                BareVal::Function(
                    "tacos".into(),
                    vec![None, Some(BareVal::String("adding some!".into()))],
                ),
                BareVal::Int(666),
                BareVal::Int(4),
                BareVal::Int(0),
                BareVal::Int(123),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(4),
                BareVal::Function("tacos".into(), vec![]),
                BareVal::Int(4),
                BareVal::Int(4),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    int tacos() { return 666; }
                "##};

            let task = run_prog(code).await;
            let registers = &task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(666),
                BareVal::Object("/my_file".into()),
                BareVal::String("tacos".into()),
                BareVal::Int(666),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, registers);
        }

        #[tokio::test]
        async fn returns_0_for_private_functions() {
            let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    private int tacos() { return 666; }
                "##};

            let task = run_prog(code).await;
            let registers = &task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Object("/my_file".into()),
                BareVal::String("tacos".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, registers);
        }

        #[tokio::test]
        async fn returns_0_for_protected_functions() {
            let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    protected int tacos() { return 666; }
                "##};

            let task = run_prog(code).await;
            let registers = &task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Object("/my_file".into()),
                BareVal::String("tacos".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, registers);
        }

        #[tokio::test]
        async fn returns_0_for_unknown_receiver() {
            let code = indoc! { r##"
                    mixed q = "/foobarbaz"->tacos();
                "##};

            let task = run_prog(code).await;
            let registers = &task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("/foobarbaz".into()),
                BareVal::String("tacos".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, registers);
        }
    }

    mod test_catch {
        use super::*;

        #[tokio::test]
        async fn stores_the_error_string() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(10 / j);

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::String("Runtime Error: Division by zero".into()),
                BareVal::Int(10),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_zero_when_no_error() {
            let code = indoc! { r##"
                    void create() {
                        int j = 5;
                        catch(10 / j);

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(5),
                BareVal::Int(0),
                BareVal::Int(10),
                BareVal::Int(2),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
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
                        --j;

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(-1),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_pre_when_global() {
            let code = indoc! { r##"
                    int j = 5;
                    int k = --j;
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let expected = vec![BareVal::Int(4), BareVal::Int(4)];

            let proc = ctx.process();

            BareVal::assert_vec_equal(
                &ctx.global_state,
                &expected,
                &committed_global_values(&ctx.global_state, proc),
            );
        }

        #[tokio::test]
        async fn stores_the_value_for_post() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        j--;

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(-1),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_post_when_global() {
            let code = indoc! { r##"
                    int j = 5;
                    int k = j--;
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let expected = vec![BareVal::Int(4), BareVal::Int(5)];

            let proc = ctx.process();

            BareVal::assert_vec_equal(
                &ctx.global_state,
                &expected,
                &committed_global_values(&ctx.global_state, proc),
            );
        }
    }

    mod test_eq_eq {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 2 == 2;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(2),
                BareVal::Int(2),
                BareVal::Int(1),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_fconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    float π = 4.13;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::Float(4.13.into())];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_functionptrconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value_for_efuns() {
            let code = indoc! { r##"
                    function f = dump;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Function("dump".to_string(), vec![]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_simul_efuns() {
            let code = indoc! { r##"
                    function f = simul_efun;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Function("simul_efun".to_string(), vec![]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_call_other() {
            let code = indoc! { r##"
                    function f = &(this_object())->tacco();

                    void tacco() {
                        dump("tacco!");
                    }
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Object("/my_file".into()),
                BareVal::Object("/my_file".into()),
                BareVal::Function("tacco".to_string(), vec![]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_call_other_string_receiver() {
            let code = indoc! { r##"
                    function f = &("/secure/simul_efuns")->simul_efun();
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("/secure/simul_efuns".into()),
                BareVal::Function("simul_efun".to_string(), vec![]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_with_args() {
            let code = indoc! { r##"
                    function f = &tacco(1, 666);

                    void tacco(int a, int b) {
                        dump(a + b);
                    }
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(666),
                BareVal::Function(
                    "tacco".to_string(),
                    vec![Some(BareVal::Int(1)), Some(BareVal::Int(666))],
                ),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_with_partial_applications() {
            let code = indoc! { r##"
                    function f = &tacco(1, , , 42, );

                    void tacco(int a, int b, int c, int d, int e) {
                        dump(a + b - c * (d + e));
                    }
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(42),
                BareVal::Function(
                    "tacco".to_string(),
                    vec![
                        Some(BareVal::Int(1)),
                        None,
                        None,
                        Some(BareVal::Int(42)),
                        None,
                    ],
                ),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Function("closure-0".to_string(), vec![]),
                BareVal::Function("closure-0".to_string(), vec![]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1200),
                BareVal::Int(1199),
                BareVal::Int(1),
                BareVal::Int(1199),
                BareVal::Int(1200),
                BareVal::Int(0),
                BareVal::Int(1200),
                BareVal::Int(1200),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1200),
                BareVal::Int(1199),
                BareVal::Int(1),
                BareVal::Int(1199),
                BareVal::Int(1200),
                BareVal::Int(0),
                BareVal::Int(1200),
                BareVal::Int(1200),
                BareVal::Int(1),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                // the constant expressions are folded at parse time
                BareVal::Int(50),
                BareVal::Int(8),
                BareVal::Int(58),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_iconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 666;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::Int(666)];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_iconst0 {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 0;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::Int(0)];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_iconst1 {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed q = 1;
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::Int(1)];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                // the constant expressions are folded at parse time
                BareVal::Int(8),
                BareVal::Int(-3),
                BareVal::Int(-2),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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
                "Runtime Error: Division by zero"
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                // the constant expressions are folded at parse time
                BareVal::Int(2),
                BareVal::Int(5),
                BareVal::Int(2),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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
                "Runtime Error: Remainder division by zero"
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(32),
                BareVal::Int(-48),
                BareVal::Int(-1536),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_inc {
        use super::*;

        #[tokio::test]
        async fn stores_the_value_for_pre() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        ++j;

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_pre_when_global() {
            let code = indoc! { r##"
                    int j = 0;
                    int k = ++j;
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let expected = vec![BareVal::Int(1), BareVal::Int(1)];

            let proc = ctx.process();

            BareVal::assert_vec_equal(
                &ctx.global_state,
                &expected,
                &committed_global_values(&ctx.global_state, proc),
            );
        }

        #[tokio::test]
        async fn stores_the_value_for_post() {
            let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        j++;

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_post_when_global() {
            let code = indoc! { r##"
                    int j = 5;
                    int k = j++;
                "##};

            let task = run_prog(code).await;
            let ctx = task.context;

            let expected = vec![BareVal::Int(6), BareVal::Int(5)];

            let proc = ctx.process();

            BareVal::assert_vec_equal(
                &ctx.global_state,
                &expected,
                &committed_global_values(&ctx.global_state, proc),
            );
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(14),
                BareVal::Int(16),
                BareVal::Int(-2),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

                        // Store a snapshot, so we can test this even though this stack
                        // frame would otherwise have been popped off into the aether.
                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(69),
                BareVal::Int(12),
                BareVal::Int(10),
                BareVal::Int(1),
                BareVal::Int(69),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
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

                        // Store a snapshot, so we can test this even though this stack
                        // frame would otherwise have been popped off into the aether.
                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(8),
                BareVal::Int(1),
                BareVal::Int(8),
                BareVal::Int(8),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(12),
                BareVal::Int(1000),
                BareVal::Int(12),
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::Int(1000),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                BareVal::Int(1),
                BareVal::Int(2),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1200),
                BareVal::Int(1199),
                BareVal::Int(0),
                BareVal::Int(1199),
                BareVal::Int(1200),
                BareVal::Int(1),
                BareVal::Int(1200),
                BareVal::Int(1200),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1200),
                BareVal::Int(1199),
                BareVal::Int(0),
                BareVal::Int(1199),
                BareVal::Int(1200),
                BareVal::Int(1),
                BareVal::Int(1200),
                BareVal::Int(1200),
                BareVal::Int(1),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let mut hashmap = HashMap::new();
            hashmap.insert(BareVal::String("asdf".into()), BareVal::Int(123));
            hashmap.insert(BareVal::Int(456), BareVal::Float(4.13.into()));

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("asdf".into()),
                BareVal::Int(123),
                BareVal::Int(456),
                BareVal::Float(4.13.into()),
                BareVal::Mapping(hashmap),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("abc".into()),
                BareVal::Int(123),
                BareVal::String("abc123".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("abc".into()),
                BareVal::Int(4),
                BareVal::String("abcabcabcabc".into()),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Array(vec![
                    BareVal::Int(1),
                    BareVal::Int(1),
                    BareVal::Int(2),
                    BareVal::Int(3),
                ]),
                BareVal::Int(1),
                BareVal::Array(vec![BareVal::Int(1)]),
                BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(2),
                BareVal::Int(0),
                BareVal::Int(4),
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Float(Total::from(0.0)),
                BareVal::Int(1),
                BareVal::Float(Total::from(0.01)),
                BareVal::Int(0),
                BareVal::String("".into()),
                BareVal::Int(0),
                BareVal::String("asdf".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(31),
                BareVal::Int(0),
                BareVal::Int(31),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(123),
                BareVal::Int(123),
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::Int(123),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let (gs, registers) = snapshot_registers(code).await;

            let mut mapping = HashMap::new();
            mapping.insert(BareVal::String("a".into()), BareVal::Int(123));

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Array(vec![
                    BareVal::Int(3),
                    BareVal::String("foo".into()),
                    BareVal::Array(vec![
                        BareVal::String("bar".into()),
                        BareVal::String("baz".into()),
                        BareVal::Float(4.13.into()),
                    ]),
                    BareVal::Mapping(mapping.clone()),
                ]),
                BareVal::String("snapshot_stack".into()),
                BareVal::Array(vec![
                    BareVal::String("bar".into()),
                    BareVal::String("baz".into()),
                    BareVal::Float(4.13.into()),
                ]),
                BareVal::Mapping(mapping),
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
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

            let (gs, registers) = snapshot_registers(code).await;

            let mut mapping = HashMap::new();
            mapping.insert(BareVal::String("a".into()), BareVal::Int(123));

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(45),
                BareVal::Int(34),
                BareVal::Float(7.77.into()),
                BareVal::String("snuh".into()),
                BareVal::Array(vec![
                    BareVal::String("a string".into()),
                    BareVal::Int(3),
                    BareVal::Float(2.44.into()),
                ]),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::String("snuh".into()),
                BareVal::String("a string".into()),
                BareVal::Int(3),
                BareVal::Float(2.44.into()),
                BareVal::Array(vec![
                    BareVal::String("a string".into()),
                    BareVal::Int(3),
                    BareVal::Float(2.44.into()),
                ]),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }
    }

    mod test_range {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = ({ 1, 2, 3 })[1..];
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                BareVal::Int(1),
                BareVal::Int(-1),
                BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)]),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::Int(4)];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(666), // return value from create()
                BareVal::Int(666), // The copy of the call return value into its own register
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(790080),
                BareVal::Int(0),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(192),
                BareVal::Int(0),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }
    }

    mod test_sizeof {
        use std::sync::Arc;

        use lpc_rs_asm::instruction::Instruction::{Ret, SConst, Sizeof};
        use lpc_rs_core::{INIT_PROGRAM, lpc_path::LpcPath, lpc_type::LpcType};
        use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;
        use once_cell::sync::OnceCell;
        use string_interner::StringInterner;

        use super::*;
        use crate::interpreter::program::Program;
        use crate::test_support::test_config;

        #[tokio::test]
        async fn stores_the_value_for_arrays() {
            let code = indoc! { r##"
                    int a = sizeof(({ 1, 2, 3 }));
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)]),
                BareVal::Int(3),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_mappings() {
            let code = indoc! { r##"
                    int a = sizeof(([ "a": 1, 'b': 2, 3: ({ 4, 5, 6 }), 0: 0 ]));
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let mut mapping = HashMap::new();
            mapping.insert(BareVal::String("a".into()), BareVal::Int(1));
            mapping.insert(BareVal::Int(98), BareVal::Int(2));
            mapping.insert(
                BareVal::Int(3),
                BareVal::Array(vec![BareVal::Int(4), BareVal::Int(5), BareVal::Int(6)]),
            );
            mapping.insert(BareVal::Int(0), BareVal::Int(0));

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("a".into()),
                BareVal::Int(1),
                BareVal::Int(98),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Int(4),
                BareVal::Int(5),
                BareVal::Int(6),
                BareVal::Array(vec![BareVal::Int(4), BareVal::Int(5), BareVal::Int(6)]),
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::Mapping(mapping),
                BareVal::Int(4),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
        }

        #[tokio::test]
        async fn stores_the_value_for_strings() {
            let config = Arc::new(test_config());
            let path = Arc::new(LpcPath::new_in_game("/my_file.c", "/", &*config.lib_dir));

            let prototype = FunctionPrototypeBuilder::default()
                .name(INIT_PROGRAM)
                .filename(path.clone())
                .return_type(LpcType::Void)
                .build()
                .unwrap();
            let initializer = ProgramFunction {
                prototype,
                num_locals: 2,
                num_upvalues: 0,
                instructions: vec![
                    SConst(Register(1).as_local(), 0),
                    Sizeof(Register(1).as_local(), Register(2).as_local()),
                    Ret,
                ],
                debug_spans: vec![None, None],
                labels: Some(HashMap::new()),
                local_variables: Default::default(),
                arg_locations: Default::default(),
                strings: OnceCell::with_value(
                    StringInterner::from_iter(["Hello, world!"].into_iter()).into(),
                ),
            }
            .into();

            let program = Program {
                filename: path,
                functions: Box::new(IndexMap::new()),
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

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("Hello, world!".into()),
                BareVal::Int(13),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, registers);
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

                        debug("snapshot_stack");
                    }
                "##};

            let (gs, registers) = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(2),
                BareVal::Int(3),
                BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(678)]),
                BareVal::Int(678),
                BareVal::Int(2),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&gs, &expected, &registers);
        }
    }

    mod test_sconst {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    string foo = "lolwut";
                "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![BareVal::Int(0), BareVal::String("lolwut".into())];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(20),
                BareVal::Int(0),
                BareVal::Int(20),
            ];

            BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);
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

        let task = run_prog(code).await;
        let registers = task.popped_frame.unwrap().registers;

        let expected = vec![
            Int(1),
            Int(0),
            Function("closure-0".to_string(), vec![]),
            Int(0),
            Int(1),
        ];

        BareVal::assert_vec_equal(&task.context.global_state, &expected, &registers);

        let ctx = &task.context;
        let proc = ctx.process();

        let expected = vec![
            Int(2),
            Function("closure-0".to_string(), vec![]),
            Int(0),
            Int(1),
        ];
        BareVal::assert_vec_equal(
            &ctx.global_state,
            &expected,
            &committed_global_values(&ctx.global_state, proc),
        );
    }
}

mod test_upvalues {
    use super::*;
    use crate::interpreter::task::tests::BareVal::*;

    async fn check_local_vars<T>(code: &str, vars: &IndexMap<&str, T>)
    where
        T: Into<BareVal> + Clone,
    {
        let mut task = run_prog(code).await;

        let snapshot = &mut task.snapshots.pop().unwrap();
        snapshot.pop(); // pop off the init frame

        let frame = snapshot.pop().unwrap();

        let frame_vars = frame.local_variables(task.context.txn());

        for (k, v) in vars {
            let v: BareVal = v.clone().into();
            let found = frame_vars
                .iter()
                .filter(|v| &v.name == k)
                .collect::<Vec<_>>();
            assert!(
                found
                    .iter()
                    .any(|local| v.equal_to_lpc_ref(&task.context.global_state, &local.value)),
                "key: {k}, value: {v}, found: {:?}",
                found.iter().map(|v| &v.value).collect::<Vec<_>>()
            );
            // assert_eq!(&v, frame_vars.get(*k).unwrap(), "key: {}", k);
        }
    }

    async fn check_vm_upvalues<T>(code: &str, upvalues: &[T])
    where
        T: Into<BareVal> + Clone,
    {
        let mut task = run_prog(code).await;

        let snapshot = &mut task.snapshots.pop().unwrap();
        snapshot.pop(); // pop off the init frame

        let frame = snapshot.pop().unwrap();

        let expected: Vec<BareVal> = upvalues.iter().map(|i| i.clone().into()).collect();

        // The frame's upvalue cells hold transactional identities, not the
        // values themselves: each slot is a `VarId` whose committed
        // value is read through the frame's transaction. Slot order still
        // matches upvalue-creation order, so the original position-based
        // assertions apply to the committed reads.
        frame.with_upvalues(|uv| {
            let values: Vec<LpcRef> = (0..uv.len())
                .map(|i| {
                    task.context
                        .txn()
                        .with(|t| t.read(uv[i]).unwrap_or_else(|| NULL.clone()))
                })
                .collect();

            assert_eq!(
                expected.len(),
                values.len(),
                "expected upvalues: {:?}\nvm upvalues: {:?}\nbank: {:?}\n",
                expected,
                values,
                (0..uv.len())
                    .map(|i| format!("{:?}", uv[i]))
                    .collect::<Vec<_>>()
            );

            for (v, ev) in values.iter().zip(&expected) {
                ev.assert_equal(&task.context.global_state, v);
            }
        });
    }

    async fn check_frame_upvalue_ptrs<T>(code: &str, upvalue_ptrs: &[T])
    where
        T: Into<Register> + Copy,
    {
        let mut task = run_prog(code).await;

        let snapshot = &mut task.snapshots.pop().unwrap();
        snapshot.pop(); // pop off the init frame

        let frame = snapshot.pop().unwrap();

        assert_eq!(upvalue_ptrs.len(), frame.upvalue_ptrs.len());

        for (i, v) in upvalue_ptrs.iter().enumerate() {
            let v: Register = (*v).into();
            assert_eq!(v, frame.upvalue_ptrs[i], "index: {i}");
        }
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

        let expected = vec![Int(2)];
        check_vm_upvalues(code, &expected).await;

        let expected = vec![Register(0)];
        check_frame_upvalue_ptrs(code, &expected).await;

        let expected: IndexMap<&str, BareVal> = IndexMap::new();
        check_local_vars(code, &expected).await;
    }

    #[tokio::test]
    async fn upvalue_writes_survive_gc() {
        // a closure captures a *local*, so `i++` and `return i`
        // route through the cell arm of `CallFrame::apply_in_location` /
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

        // The frame holding the closure is gone; the full pass drops the dead
        // cell's VarId from the committer's world.
        task.context.global_state.gc().await.unwrap();

        // The txn remains usable after the sweep: a fresh read through the
        // committer for the (now-swept) cell falls back to NULL, and no
        // panic or inconsistency occurs. The dead cell is gone from the
        // shared bank.
        task.context.global_state.with_upvalues(|uv| {
            assert_eq!(uv.len(), 0, "swept cell should be removed from the bank")
        });
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

        let expected = IndexMap::from([
            ("i", Int(2)),
            ("j", Int(0)),
            ("k", Int(1)),
            ("inc", Function("closure-0".to_string(), vec![])),
        ]);

        check_local_vars(code, &expected).await;
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

        let expected = vec![Int(10), Int(666)];
        check_vm_upvalues(code, &expected).await;

        let expected = IndexMap::from([
            ("j", Int(10)),
            ("k", Int(-40)),
            ("l", Int(20)),
            ("m", Int(2)),
            ("n", Int(1332)),
            ("add", Function("closure-0".into(), vec![])),
            ("add2", Function("closure-0".into(), vec![])),
        ]);
        check_local_vars(code, &expected).await;
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

        // let expected = vec![Int(105), Int(1), Int(1), Int(100)];
        // check_vm_upvalues(code, &expected);

        let expected = IndexMap::from([
            ("c1", Int(1)),
            ("c2", Int(5)),
            ("c3", Int(105)),
            ("make_counter", Function("closure-1".into(), vec![])),
            ("counter1", Function("closure-0".into(), vec![])),
            ("counter2", Function("closure-0".into(), vec![])),
            ("counter3", Function("closure-0".into(), vec![])),
        ]);

        check_local_vars(code, &expected).await;
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

        let expected: Vec<BareVal> = vec![];
        check_vm_upvalues(code, &expected).await;

        let expected = IndexMap::from([
            ("c1", Int(0)),
            ("c2", Int(69)),
            ("make", Function("closure-1".into(), vec![])),
            ("made1", Function("closure-0".into(), vec![])),
            ("made2", Function("closure-0".into(), vec![])),
        ]);

        check_local_vars(code, &expected).await;
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

        let expected: Vec<BareVal> = vec![
            Function("closure-2".into(), vec![]),
            String("hello".into()),
            Int(666),
            Int(1),
            Int(2),
            Int(3),
            Int(77),
        ];
        check_vm_upvalues(code, &expected).await;

        let expected = IndexMap::from([
            ("c1", String("hello666 1 2 -4".into())),
            ("c2", String("hello666 3 77 69".into())),
            (
                "partial",
                Function("make_maker".into(), vec![None, Some(Int(666))]),
            ),
            ("maker", Function("closure-2".into(), vec![])),
            ("made1", Function("closure-1".into(), vec![])),
            ("made2", Function("closure-1".into(), vec![])),
        ]);

        check_local_vars(code, &expected).await;
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

        let expected: Vec<BareVal> = vec![
            Function("closure-2".into(), vec![]),
            String("hello".into()),
            Array(vec![Int(123), Int(456)]),
            Array(vec![String("world".into()), Int(77)]),
        ];
        check_vm_upvalues(code, &expected).await;

        let expected = IndexMap::from([
            ("c1", Int(123)),
            ("c2", Int(77)),
            (
                "partial",
                Function("make_maker".into(), vec![None, Some(Int(666))]),
            ),
            ("maker", Function("closure-2".into(), vec![])),
            ("made1", Function("closure-1".into(), vec![])),
            ("made2", Function("closure-1".into(), vec![])),
        ]);

        check_local_vars(code, &expected).await;
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
        ctx.global_state.with_upvalues(|uv| {
            assert!(uv.len() > 0);
        });

        // The full pass drops the dead cells' VarIds out of the committer's
        // world.
        ctx.global_state.gc().await.unwrap();
        ctx.global_state.with_upvalues(|uv| {
            assert_eq!(uv.len(), 0);
        });
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
