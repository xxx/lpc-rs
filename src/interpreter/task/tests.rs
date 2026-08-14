use std::{
    collections::HashMap,
    fmt::{Display, Formatter},
    hash::{Hash, Hasher},
};

use decorum::Total;
use indexmap::IndexMap;
use indoc::indoc;
use lpc_rs_core::{LpcFloatInner, LpcIntInner};
use tokio::sync::mpsc;

use super::*;
use crate::{
    interpreter::{lpc_ref::LpcRef, object_space::ObjectSpace},
    test_support::{compile_prog, run_prog},
};

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
    pub fn from_lpc_ref(lpc_ref: &LpcRef) -> Self {
        match lpc_ref {
            LpcRef::Float(x) => BareVal::Float(x.0),
            LpcRef::Int(x) => BareVal::Int(x.0),
            LpcRef::String(x) => {
                let s = x.read();
                BareVal::String(s.to_string())
            }
            LpcRef::Array(x) => {
                let a = x.read();
                let array = a.iter().map(BareVal::from_lpc_ref).collect::<Vec<_>>();
                BareVal::Array(array)
            }
            LpcRef::Mapping(x) => {
                let m = x.read();
                let mapping = m
                    .iter()
                    .map(|(k, v)| (BareVal::from_lpc_ref(k), BareVal::from_lpc_ref(v)))
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
                let args = fp.with_partial_args(|pa| {
                    pa.iter()
                        .map(|item| item.as_ref().map(BareVal::from_lpc_ref))
                        .collect::<Vec<_>>()
                });

                BareVal::Function(fp.name().into(), args)
            }
        }
    }

    pub fn equal_to_lpc_ref(&self, other: &LpcRef) -> bool {
        self == &BareVal::from_lpc_ref(other)
    }

    pub fn assert_equal(&self, other: &LpcRef) {
        assert_eq!(self, &BareVal::from_lpc_ref(other));
    }

    pub fn assert_vec_equal(a: &[BareVal], b: &[LpcRef]) {
        assert_eq!(
            a.len(),
            b.len(),
            "Vectors {:?} and {:?} are of different lengths",
            a,
            b
        );
        for (a, b) in a.iter().zip(b.iter()) {
            a.assert_equal(b);
        }
    }
}

impl PartialEq<&LpcRef> for BareVal {
    fn eq(&self, lpc_ref: &&LpcRef) -> bool {
        &BareVal::from_lpc_ref(lpc_ref) == self
    }
}

impl Hash for BareVal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            BareVal::Float(x) => x.hash(state),
            BareVal::Int(x) => x.hash(state),
            BareVal::String(x) => x.hash(state),
            BareVal::Array(x) => std::ptr::hash(&**x, state),
            BareVal::Mapping(x) => std::ptr::hash(x, state),
            BareVal::Object(x) => std::ptr::hash(x, state),
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

    async fn snapshot_registers(code: &str) -> RefBank {
        let mut task = run_prog(code).await;
        let mut stack = task.snapshots.pop().unwrap();

        // The top of the stack in the snapshot is the object initialization frame,
        // which is not what we care about here, so we get the second-to-top frame
        // instead.
        let index = stack.len() - 2;

        std::mem::take(&mut stack[index].registers)
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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
            let values = proc.global_variable_values();
            BareVal::String("my public_function".into()).assert_equal(values.get("mine").unwrap());
            BareVal::String("/std/object public".into())
                .assert_equal(values.get("parents").unwrap());
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
            let values = proc.global_variable_values();
            BareVal::String("file_name_override".into())
                .assert_equal(values.get("this_one").unwrap());
            BareVal::String("/std/object#0".into()).assert_equal(values.get("efun_one").unwrap());
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
            let values = proc.global_variable_values();
            BareVal::String("this is a simul_efun: marf".into())
                .assert_equal(values.get("this_one").unwrap());
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
        }
    }

    mod test_call_fp {
        use claims::assert_ok;
        use tokio::sync::mpsc;

        use super::*;
        use crate::{
            interpreter::{task::initialize_program::InitializeProgramBuilder, vm::Vm},
            test_support::test_config,
            util::process_builder::ProcessInitializer,
        };

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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
            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            let (program, config, process) = compile_prog(code).await;
            let (tx, _rx) = mpsc::channel(128);
            let global_state = Arc::new(GlobalState::new(config, tx));

            ObjectSpace::insert_process(&global_state.object_space, process);

            let result = InitializeProgramBuilder::<32>::default()
                .program(program)
                .global_state(global_state.clone())
                .build()
                .await;

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

            let (program, _config, process) = compile_prog(code).await;
            ObjectSpace::insert_process(&global_state.object_space, process);

            let result = InitializeProgramBuilder::<10>::default()
                .program(program)
                .global_state(global_state.clone())
                .build()
                .await;

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

            let (program, _config, process) = compile_prog(code).await;
            let object_space = ObjectSpace::default();
            let space_cell = object_space;
            ObjectSpace::insert_process(&space_cell, process);

            let result = InitializeProgramBuilder::<20>::default()
                .program(program)
                .global_state(global_state.clone())
                .build()
                .await;

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

            BareVal::assert_vec_equal(&expected, registers);
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

            BareVal::assert_vec_equal(&expected, registers);
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

            BareVal::assert_vec_equal(&expected, registers);
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

            BareVal::assert_vec_equal(&expected, registers);
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

            let registers = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(0),
                BareVal::String("Runtime Error: Division by zero".into()),
                BareVal::Int(10),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&expected, &registers);
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

            let registers = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(5),
                BareVal::Int(0),
                BareVal::Int(10),
                BareVal::Int(2),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&expected, &registers);
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

            let registers = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(-1),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&expected, &registers);
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
                &expected,
                &proc.with_globals(|g| g.iter().cloned().collect::<Vec<_>>()),
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

            let registers = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(-1),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&expected, &registers);
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
                &expected,
                &proc.with_globals(|g| g.iter().cloned().collect::<Vec<_>>()),
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
        }
    }

    mod test_idiv {
        use super::*;
        use crate::interpreter::task::initialize_program::InitializeProgramBuilder;

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

            BareVal::assert_vec_equal(&expected, &registers);
        }

        #[tokio::test]
        async fn errors_on_division_by_zero() {
            let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q / r;
                "##};

            let (program, config, _) = compile_prog(code).await;
            let (tx, _rx) = mpsc::channel(128);
            let global_state = GlobalState::new(config, tx);

            let r = InitializeProgramBuilder::<10>::default()
                .global_state(global_state)
                .program(program)
                .build()
                .await;

            assert_eq!(
                r.unwrap_err().to_string(),
                "Runtime Error: Division by zero"
            )
        }
    }

    mod test_imod {
        use super::*;
        use crate::interpreter::task::initialize_program::InitializeProgramBuilder;

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

            BareVal::assert_vec_equal(&expected, &registers);
        }

        #[tokio::test]
        async fn errors_on_division_by_zero() {
            let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q % r;
                "##};

            let (program, config, _) = compile_prog(code).await;
            let (tx, _rx) = mpsc::channel(128);
            let global_state = GlobalState::new(config, tx);

            let r = InitializeProgramBuilder::<20>::default()
                .global_state(global_state)
                .program(program)
                .build()
                .await;

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            let registers = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&expected, &registers);
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
                &expected,
                &proc.with_globals(|g| g.iter().cloned().collect::<Vec<_>>()),
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

            let registers = snapshot_registers(code).await;

            let expected = vec![
                BareVal::Int(0),
                BareVal::Int(1),
                BareVal::Int(0),
                BareVal::String("snapshot_stack".into()),
                BareVal::Int(0),
            ];

            BareVal::assert_vec_equal(&expected, &registers);
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
                &expected,
                &proc.with_globals(|g| g.iter().cloned().collect::<Vec<_>>()),
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            let registers = snapshot_registers(code).await;

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            let registers = snapshot_registers(code).await;

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
        }
    }

    mod test_not {
        use super::*;

        #[tokio::test]
        async fn stores_the_value() {
            let code = indoc! { r##"
                    mixed a = !2;
                    mixed b = !!4;
                    float c = !0.00;
                    float d = !0.01;
                    string e = !"";
                    string f = !"asdf";
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            let registers = snapshot_registers(code).await;

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

            BareVal::assert_vec_equal(&expected, &registers);
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
            BareVal::Array(vec![]).assert_equal(ctx.result().unwrap());
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

            let registers = snapshot_registers(code).await;

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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
        use crate::{
            interpreter::task::initialize_program::InitializeProgramBuilder,
            test_support::test_config,
        };

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            let task = InitializeProgramBuilder::<20>::default()
                .program(program)
                .global_state(global_state)
                .build()
                .await
                .expect("failed to initialize");

            let registers = &task.popped_frame.unwrap().registers;

            let expected = vec![
                BareVal::Int(0),
                BareVal::String("Hello, world!".into()),
                BareVal::Int(13),
            ];

            BareVal::assert_vec_equal(&expected, registers);
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

            let registers = snapshot_registers(code).await;

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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
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

            BareVal::assert_vec_equal(&expected, &registers);
        }
    }
}

mod test_limits {

    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::{
        interpreter::task::initialize_program::InitializeProgramBuilder, test_config_builder,
    };

    #[tokio::test]
    async fn errors_on_stack_overflow() {
        let code = indoc! { r##"
                int kab00m = marf();

                int marf() {
                    return marf();
                }
            "##};

        let (program, config, _) = compile_prog(code).await;
        let (tx, _rx) = mpsc::channel(128);
        let global_state = GlobalState::new(config, tx);

        let r = InitializeProgramBuilder::<20>::default()
            .program(program)
            .global_state(global_state)
            .build()
            .await;

        assert_eq!(r.unwrap_err().to_string(), "stack overflow");
    }

    #[tokio::test]
    async fn errors_on_too_long_evaluation() {
        let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};

        let (program, _, _) = compile_prog(code).await;
        let (tx, _rx) = mpsc::channel(128);

        let config = test_config_builder!()
            .max_execution_time(40_u64)
            .build()
            .unwrap();

        let global_state = GlobalState::new(config, tx);

        let r = InitializeProgramBuilder::<20>::default()
            .program(program)
            .global_state(global_state)
            .build()
            .await;

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

        BareVal::assert_vec_equal(&expected, &registers);

        let proc = task.context.process();

        let expected = vec![
            Int(2),
            Function("closure-0".to_string(), vec![]),
            Int(0),
            Int(1),
        ];
        proc.with_globals(|g| BareVal::assert_vec_equal(&expected, g));
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

        let frame_vars = frame.local_variables();

        for (k, v) in vars {
            let v: BareVal = v.clone().into();
            let found = frame_vars
                .iter()
                .filter(|v| &v.name == k)
                .collect::<Vec<_>>();
            assert!(
                found.iter().any(|local| v.equal_to_lpc_ref(&local.value)),
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

        assert_eq!(
            upvalues.len(),
            frame.with_upvalues(|uv| uv.len()),
            "frame upvalues: {:?}\nvm upvalues: {:?}",
            upvalues
                .iter()
                .map(|i| i.clone().into())
                .collect::<Vec<BareVal>>(),
            frame.with_upvalues(|uv| format!("{:?}", uv.iter().collect::<Vec<_>>()))
        );

        frame.with_upvalues(|uv| {
            for (i, v) in upvalues.iter().enumerate() {
                let v: BareVal = v.clone().into();
                v.assert_equal(&uv[i]);
            }
        })
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
    use crate::interpreter::gc::sweep::Sweep;

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
        ctx.with_upvalues(|uv| {
            assert!(!uv.is_empty());
        });

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        task.mark(&mut marked, &mut processed).unwrap();
        ctx.with_upvalues_mut(|uv| {
            uv.sweep(&marked).unwrap();

            assert!(uv.is_empty());
        });
    }
}
