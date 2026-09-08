//! A call from a parent's code reaches the object's most-derived definition
//! of the name; `::` reaches the parent's own (CD, LDMud, FluffOS, DGD;
//! survey local/bench-drivers/ovprobe).

use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, vm::Vm},
    test_support::lib_holding,
};

const MASTER: &str = "int valid_load(mixed a, mixed b, mixed c, mixed d) { return 1; } \
    int valid_inherit(mixed a, mixed b, mixed c, mixed d) { return 1; }";

/// `f()` calls `g()` and `x()` reads what `g` set.
const PARENT: &str = "int x;\nvoid g() { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n";

/// Clone `/child.c` from `files` plus the master, call `f()`, return `x()`.
async fn x_after_f(files: &[(&str, &str)]) -> i64 {
    let root = lib_holding("inheritance", files);
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.initialize_process_from_code("/secure/master.c", MASTER)
        .await
        .unwrap();
    let main = r#"int create() { object o = clone_object("/child"); o->f(); return o->x(); }"#;
    let task = vm
        .initialize_process_from_code("/main.c", main)
        .await
        .unwrap();
    let LpcRef::Int(LpcInt(x)) = task.result().expect("create() returns") else {
        panic!("create() did not return an int");
    };
    x
}

#[tokio::test]
async fn a_parents_call_reaches_the_childs_redefinition() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        ("child.c", "inherit \"/parent\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_qualified_call_reaches_the_parents_own() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        (
            "child.c",
            "inherit \"/parent\";\nvoid g() { ::g(); x += 10; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 11);
}

#[tokio::test]
async fn a_private_parent_function_is_not_overridden() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nprivate void g() { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n",
        ),
        ("child.c", "inherit \"/parent\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 1);
}

#[tokio::test]
async fn a_private_redefinition_intercepts() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        (
            "child.c",
            "inherit \"/parent\";\nprivate void g() { x = 2; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_static_redefinition_intercepts() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nstatic void g() { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n",
        ),
        (
            "child.c",
            "inherit \"/parent\";\nstatic void g() { x = 2; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_later_siblings_definition_intercepts_an_earlier_siblings_call() {
    // CD and FluffOS; LDMud keeps the earlier sibling's own, DGD refuses the file.
    let x = x_after_f(&[
        ("parent.c", PARENT),
        ("sibling.c", "int y;\nvoid g() { y = 3; }\nint y() { return y; }\n"),
        (
            "child.c",
            "inherit \"/parent\" p;\ninherit \"/sibling\";\nint x() { return p::x() * 10 + y(); }\n",
        ),
    ])
    .await;
    assert_eq!(x, 3);
}

#[tokio::test]
async fn a_pointer_taken_in_the_parent_reaches_the_childs_redefinition() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nvoid g() { x = 1; }\nvoid f() { function fp = &g(); fp(); }\nint x() { return x; }\n",
        ),
        ("child.c", "inherit \"/parent\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_short_call_through_a_varargs_parent_runs_the_redefinition_with_zeros() {
    let x = x_after_f(&[
        (
            "parent.c",
            "int x;\nvarargs void g(int n) { x = 1; }\nvoid f() { g(); }\nint x() { return x; }\n",
        ),
        (
            "child.c",
            "inherit \"/parent\";\nvoid g(int n, int m) { x = 2 + n + m; }\n",
        ),
    ])
    .await;
    assert_eq!(x, 2);
}

#[tokio::test]
async fn a_grandparents_call_reaches_the_grandchilds_redefinition() {
    let x = x_after_f(&[
        ("parent.c", PARENT),
        ("middle.c", "inherit \"/parent\";\n"),
        ("child.c", "inherit \"/middle\";\nvoid g() { x = 2; }\n"),
    ])
    .await;
    assert_eq!(x, 2);
}
