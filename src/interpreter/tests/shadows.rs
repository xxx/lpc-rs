//! `shadow(ob, flag)`: the chain query, the attach, its refusals, and —
//! from the dispatch tasks on — where an external call enters a chain.
//! Cases follow `local/bench-drivers/shprobe/results.md` (c01–c23).

use indoc::indoc;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        tests::{fails, run},
        vm::Vm,
    },
    test_support::lib_holding,
};

/// A master that allows every shadow and records who asked.
const ALLOWING: &str = indoc! { r#"
    string asker;
    int query_allow_shadow(object ob) { asker = file_name(previous_object()); return 1; }
    string asker() { return asker; }
"# };

/// A master that refuses every shadow.
const REFUSING: &str = "int query_allow_shadow(object ob) { return 0; }";

/// A master with no shadow apply at all.
const SILENT: &str = "void nothing() {}";

/// Any object that can shadow, be shadowed, move and die.
const S: &str = indoc! { r#"
    object go(object o) { return shadow(o, 1); }
    void enter(object e) { move_object(e); }
    void die() { destruct(this_object()); }
    string f() { return "s.f"; }
"# };

/// A target that opts out with the pragma.
const NO_SHADOW: &str = "#pragma no_shadow\nstring f() { return \"tn.f\"; }";

/// A target with a nomask function.
const NOMASK: &str =
    "nomask string n() { return \"t.n\"; }\nobject go(object o) { return shadow(o, 1); }";

/// A shadow that defines the nomask function's name.
const DEFINES_N: &str =
    "object go(object o) { return shadow(o, 1); }\nstring n() { return \"sn.n\"; }";

fn ints(values: &[LpcRef]) -> Vec<i64> {
    values
        .iter()
        .map(|v| match v {
            LpcRef::Int(LpcInt(i)) => *i,
            other => panic!("not an int: {other}"),
        })
        .collect()
}

#[tokio::test]
async fn the_query_walks_the_chain_inner_to_outer() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/s");
            object s1 = clone_object("/s");
            object s2 = clone_object("/s");
            int before = shadow(t, 0);
            int first = s1->go(t) == t;
            int after_one = shadow(t, 0) == s1;
            int second = s2->go(t) == t;
            return ({ before, first, after_one, second,
                      shadow(t, 0) == s1, shadow(s1, 0) == s2, shadow(s2, 0) });
        }
    "# };
    let result = run(ALLOWING, &[("/s.c", S)], main).await;
    assert_eq!(ints(&result), vec![0, 1, 1, 1, 1, 1, 0]);
}

#[tokio::test]
async fn the_master_hears_the_shadow_as_previous_object() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/s");
            object s1 = clone_object("/s");
            s1->go(t);
            return ({ find_object("/secure/master")->asker() == file_name(s1) });
        }
    "# };
    assert_eq!(ints(&run(ALLOWING, &[("/s.c", S)], main).await), vec![1]);
}

/// `main` under `ALLOWING`, with `/s.c` and `extra` loaded; the error text.
async fn refused(extra: &[(&str, &str)], main: &str) -> String {
    let mut objects = vec![("/s.c", S)];
    objects.extend_from_slice(extra);
    fails(ALLOWING, &objects, main).await
}

#[tokio::test]
async fn a_shadow_cannot_shadow_twice() {
    let main = r#"void create() { object t = clone_object("/s"); object u = clone_object("/s"); object s = clone_object("/s"); s->go(t); s->go(u); }"#;
    assert!(refused(&[], main).await.contains("Already shadowing."));
}

#[tokio::test]
async fn a_shadowed_object_cannot_shadow() {
    let main = r#"void create() { object t = clone_object("/s"); object u = clone_object("/s"); object s = clone_object("/s"); s->go(t); t->go(u); }"#;
    assert!(
        refused(&[], main)
            .await
            .contains("Can't shadow when shadowed.")
    );
}

#[tokio::test]
async fn a_shadow_must_have_no_environment() {
    let main = r#"void create() { object t = clone_object("/s"); object s = clone_object("/s"); s->enter(t); s->go(t); }"#;
    assert!(
        refused(&[], main)
            .await
            .contains("The shadow must not reside inside another object.")
    );
}

#[tokio::test]
async fn a_shadow_cannot_be_shadowed_nor_shadow_itself() {
    let chain = r#"void create() { object t = clone_object("/s"); object s = clone_object("/s"); object s3 = clone_object("/s"); s->go(t); s3->go(s); }"#;
    assert!(refused(&[], chain).await.contains("Can't shadow a shadow."));
    let self_ = r#"void create() { object s = clone_object("/s"); s->go(s); }"#;
    assert!(refused(&[], self_).await.contains("Can't shadow a shadow."));
}

#[tokio::test]
async fn the_master_cannot_be_shadowed() {
    let main =
        r#"void create() { object s = clone_object("/s"); s->go(find_object("/secure/master")); }"#;
    assert!(
        refused(&[], main)
            .await
            .contains("Can't shadow the master.")
    );
}

#[tokio::test]
async fn the_simul_efun_object_cannot_be_shadowed() {
    let root = lib_holding(
        "shadow-sefun",
        &[("secure/simul_efuns.c", "int one() { return 1; }")],
    );
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .simul_efun_file("/secure/simul_efuns")
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.global_state
        .initialize_simul_efuns()
        .await
        .expect("configured")
        .expect("compiles");
    vm.initialize_process_from_code("/secure/master.c", ALLOWING)
        .await
        .unwrap();
    vm.initialize_process_from_code("/s.c", S).await.unwrap();
    let main = r#"void create() { object s = clone_object("/s"); s->go(find_object("/secure/simul_efuns")); }"#;
    let err = vm
        .initialize_process_from_code("/main.c", main)
        .await
        .unwrap_err()
        .to_string();
    assert!(err.contains("Can't shadow the simul-efun object."), "{err}");
}

#[tokio::test]
async fn a_no_shadow_program_cannot_be_shadowed() {
    let main = r#"void create() { object tn = clone_object("/tn"); object s = clone_object("/s"); s->go(tn); }"#;
    assert!(
        refused(&[("/tn.c", NO_SHADOW)], main)
            .await
            .contains("Can't shadow a 'no_shadow' program.")
    );
}

#[tokio::test]
async fn a_nomask_function_cannot_be_shadowed() {
    let main = r#"void create() { object t = clone_object("/tm"); object sn = clone_object("/sn"); sn->go(t); }"#;
    let err = refused(&[("/tm.c", NOMASK), ("/sn.c", DEFINES_N)], main).await;
    assert!(
        err.contains("Illegal to shadow 'nomask' function 'n'."),
        "{err}"
    );
}

#[tokio::test]
async fn a_master_without_the_apply_disables_shadowing() {
    let main = r#"void create() { object t = clone_object("/s"); object s = clone_object("/s"); s->go(t); }"#;
    let err = fails(SILENT, &[("/s.c", S)], main).await;
    assert!(
        err.contains("Shadowing is disabled: the master defines no query_allow_shadow."),
        "{err}"
    );
}

#[tokio::test]
async fn a_refusing_master_refuses() {
    let main = r#"void create() { object t = clone_object("/s"); object s = clone_object("/s"); s->go(t); }"#;
    let err = fails(REFUSING, &[("/s.c", S)], main).await;
    assert!(err.contains("The master refused the shadow."), "{err}");
}
