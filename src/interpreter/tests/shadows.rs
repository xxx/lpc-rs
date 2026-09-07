//! `shadow(ob, flag)`: the chain query, the attach, its refusals, and,
//! through the doors, where an external call enters a chain.
//! Cases follow `local/bench-drivers/shprobe/results.md` (c01–c23).

use indoc::indoc;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
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

/// `S` with its death function renamed, for an outer shadow that must not
/// answer `die`.
const SB: &str = indoc! { r#"
    object go(object o) { return shadow(o, 1); }
    void enter(object e) { move_object(e); }
    void die_b() { destruct(this_object()); }
    string f() { return "sb.f"; }
"# };

/// A shadow that attaches through `attach` and defines nothing else.
const SG: &str = "object attach(object o) { return shadow(o, 1); }";

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
            mixed before = shadow(t, 0);
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
    // `s` attaches through `attach`, not `go`, so the outer call below still
    // reaches `t`'s own `go` instead of being caught by the shadow.
    let main = r#"void create() { object t = clone_object("/s"); object u = clone_object("/s"); object s = clone_object("/sg"); s->attach(t); t->go(u); }"#;
    assert!(
        refused(&[("/sg.c", SG)], main)
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
async fn a_no_shadow_program_protects_what_inherits_it() {
    let root = lib_holding(
        "shadow-inherits",
        &[
            ("tn.c", NO_SHADOW),
            ("tni.c", "inherit \"/tn\";\nstring g() { return \"g\"; }"),
        ],
    );
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.initialize_process_from_code("/secure/master.c", ALLOWING)
        .await
        .unwrap();
    vm.initialize_process_from_code("/s.c", S).await.unwrap();
    vm.initialize_process_from_code("/tnii.c", "inherit \"/tni\";\nstring k() { return \"k\"; }")
        .await
        .unwrap();
    let main = r#"void create() { object s = clone_object("/s"); s->go(find_object("/tnii")); }"#;
    let err = vm
        .initialize_process_from_code("/main.c", main)
        .await
        .unwrap_err()
        .to_string();
    assert!(err.contains("Can't shadow a 'no_shadow' program."), "{err}");
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

#[tokio::test]
async fn the_query_needs_a_live_object() {
    let main = indoc! { r#"
        mixed *create() {
            object s = clone_object("/s");
            s->die();
            return ({ catch(shadow(0, 0)), catch(shadow(s, 0)), catch(shadow(s, 1)) });
        }
    "# };
    let got = strings(&run(ALLOWING, &[("/s.c", S)], main).await);
    assert!(
        got[0].contains("the target must be an object"),
        "{}",
        got[0]
    );
    assert!(
        got[1].contains("the target has been destructed"),
        "{}",
        got[1]
    );
    assert!(
        got[2].contains("the target has been destructed"),
        "{}",
        got[2]
    );
}

/// The walk skips a chain entry whose object died: an invariant guard, so
/// the state is forced here rather than reached.
#[tokio::test]
async fn a_dead_entry_is_skipped() {
    use std::sync::Arc;

    use crate::{
        interpreter::{
            process::shadow::ShadowEntry,
            stm::{Transaction, TxnHandle, start_txn},
        },
        test_support::committed_global,
    };

    let vm = Vm::new(crate::test_support::test_config());
    for (path, code) in [("/secure/master.c", ALLOWING), ("/t.c", T), ("/s.c", S)] {
        vm.initialize_process_from_code(path, code).await.unwrap();
    }
    let main = r#"object t, s; void create() { t = clone_object("/t"); s = clone_object("/s"); s->go(t); }"#;
    let task = vm
        .initialize_process_from_code("/main.c", main)
        .await
        .unwrap();
    let object = |name: &str| match committed_global(&task, name) {
        LpcRef::Object(weak) => weak.upgrade().unwrap(),
        other => panic!("{name}: {other:?}"),
    };
    let (t, s) = (object("t"), object("s"));

    let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
    let txn = TxnHandle::new(Transaction::new(live.inner.clone()));
    assert!(
        matches!(Process::shadow_entry(&txn, &t, "f", &t), ShadowEntry::Found(ref found, _) if Arc::ptr_eq(found, &s)),
        "the live shadow answers f"
    );
    txn.with(|inner| inner.drop_var(*s.cell.get().unwrap()));
    assert!(
        matches!(Process::shadow_entry(&txn, &t, "f", &t), ShadowEntry::Fallback(ref real) if Arc::ptr_eq(real, &t)),
        "the dead shadow is passed over"
    );
    drop(live);
}

#[tokio::test]
async fn destructing_an_inner_shadow_closes_the_chain() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/s");
            object s1 = clone_object("/s");
            object s2 = clone_object("/sb");
            s1->go(t);
            s2->go(t);
            s1->die();
            return ({ shadow(t, 0) == s2, shadow(s2, 0) });
        }
    "# };
    assert_eq!(
        ints(&run(ALLOWING, &[("/s.c", S), ("/sb.c", SB)], main).await),
        vec![1, 0]
    );
}

#[tokio::test]
async fn destructing_the_target_frees_its_shadows() {
    // `s1` is `/sh`, which defines no `die`, so the call below reaches `t`'s
    // own `die` instead of the shadow's.
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/s");
            object u = clone_object("/s");
            object s1 = clone_object("/sh"); s1->set_tag("s1");
            s1->go(t);
            t->die();
            string dead_forward = s1->fwd();
            return ({ objectp(s1), s1->go(u) == u, shadow(u, 0) == s1,
                      dead_forward == "s1.fwd->0" });
        }
    "# };
    assert_eq!(
        ints(&run(ALLOWING, &[("/s.c", S), ("/sh.c", SH)], main).await),
        vec![1, 1, 1, 1]
    );
}

#[tokio::test]
async fn a_shadowing_object_cannot_be_moved() {
    let main = r#"void create() { object t = clone_object("/s"); object r = clone_object("/s"); object s = clone_object("/s"); s->go(t); s->enter(r); }"#;
    assert!(
        refused(&[], main)
            .await
            .contains("Can't move an object that is shadowing.")
    );
}

/// A target with an internal call, a self call through `this_object()`, a
/// public `p`, a way to die, a way to move, and a way to enable commands.
const T: &str = indoc! { r#"
    string f() { return "t.f prev=" + file_name(previous_object()); }
    string g() { return "t.g"; }
    string h() { return "t.h->" + f(); }
    string self() { return "t.self->" + this_object()->f(); }
    string p() { return "t.p"; }
    void die_t() { destruct(this_object()); }
    void enter(object e) { move_object(e); }
    void wake() { enable_commands(); }
"# };

/// A shadow with a tag, a forward to its target, a self call, a static `p`.
const SH: &str = indoc! { r#"
    string tag; object who;
    void set_tag(string s) { tag = s; }
    object go(object o) { who = o; return shadow(o, 1); }
    string f() { return tag + ".f prev=" + file_name(previous_object()) + " this=" + file_name(this_object()); }
    string fwd() { return tag + ".fwd->" + who->f(); }
    string selfcall() { return tag + ".self->" + this_object()->f(); }
    static string p() { return tag + ".p"; }
"# };

/// The dispatch table, one shadow (c04–c09) and two (c12–c16): the names of
/// main, t, s1, s2, then each probe's answer.
const DISPATCH_MAIN: &str = indoc! { r#"
    mixed *create() {
        object t = clone_object("/t");
        object s1 = clone_object("/sh"); s1->set_tag("s1");
        object s2 = clone_object("/sh"); s2->set_tag("s2");
        string me = file_name(this_object());
        s1->go(t);
        string c04 = t->f();
        string c05 = t->g();
        string c06 = t->h();
        string c07 = t->self();
        string c08 = s1->fwd();
        string c09 = t->p();
        s2->go(t);
        return ({ me, file_name(t), file_name(s1), file_name(s2),
                  c04, c05, c06, c07, c08, c09,
                  t->f(), s1->f(), s2->fwd(), s1->fwd(), s1->selfcall() });
    }
"# };

fn strings(values: &[LpcRef]) -> Vec<String> {
    values.iter().map(|v| v.to_string()).collect()
}

#[tokio::test]
async fn external_calls_enter_at_the_outermost_shadow_and_fall_inward() {
    let got = strings(&run(ALLOWING, &[("/t.c", T), ("/sh.c", SH)], DISPATCH_MAIN).await);
    let (me, t, s1, s2) = (&got[0], &got[1], &got[2], &got[3]);
    let expected = vec![
        format!("s1.f prev={me} this={s1}"),          // c04 t->f
        "t.g".to_string(),                            // c05 not in s1
        format!("t.h->t.f prev={me}"),                // c06 internal stays internal
        format!("t.self->s1.f prev={t} this={s1}"),   // c07 this_object()->f() sees the shadow
        format!("s1.fwd->t.f prev={s1}"),             // c08 the shadow reaches its target
        "t.p".to_string(),                            // c09 static in the shadow is skipped
        format!("s2.f prev={me} this={s2}"),          // c12 two shadows: outermost first
        format!("s2.f prev={me} this={s2}"),          // c13 a call on s1 from outside starts at s2
        format!("s2.fwd->s1.f prev={s2} this={s1}"),  // c14 from s2, t->f enters just inside s2
        format!("s2.fwd->s1.f prev={s2} this={s1}"),  // c15 s1->fwd from outside starts at s2
        format!("s2.self->s2.f prev={s2} this={s2}"), // c16 this_object()->f in s1 starts at s2
    ];
    assert_eq!(got[4..].to_vec(), expected);
}

#[tokio::test]
async fn a_collection_call_enters_each_receivers_chain() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/t");
            object s1 = clone_object("/sh"); s1->set_tag("s1");
            s1->go(t);
            return ({ file_name(this_object()), file_name(s1) }) + ({ t })->f();
        }
    "# };
    let got = strings(&run(ALLOWING, &[("/t.c", T), ("/sh.c", SH)], main).await);
    assert_eq!(got[2], format!("s1.f prev={} this={}", got[0], got[1]));
}

/// A shadow that answers `id` and records what it is told, through either
/// `catch_tell` or `process_input`.
const LISTENING: &str = indoc! { r#"
    string heard;
    object go(object o) { return shadow(o, 1); }
    int id(string s) { return s == "shade"; }
    void catch_tell(string msg) { heard = msg; }
    int process_input(string s) { heard = s; return 1; }
    string heard() { return heard; }
    string f() { return "shade.f"; }
"# };

#[tokio::test]
async fn a_dynamic_pointer_enters_the_chain() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/t");
            object s = clone_object("/ls");
            s->go(t);
            function fp = &->f();
            return ({ fp(t) });
        }
    "# };
    let got = strings(&run(ALLOWING, &[("/t.c", T), ("/ls.c", LISTENING)], main).await);
    assert_eq!(got, vec!["shade.f".to_string()]);
}

#[tokio::test]
async fn present_asks_the_shadow_for_id() {
    let main = indoc! { r#"
        mixed *create() {
            object room = clone_object("/s");
            object t = clone_object("/t");
            object s = clone_object("/ls");
            t->enter(room);
            s->go(t);
            return ({ present("shade", room) == t });
        }
    "# };
    let got = run(
        ALLOWING,
        &[("/s.c", S), ("/t.c", T), ("/ls.c", LISTENING)],
        main,
    )
    .await;
    assert_eq!(ints(&got), vec![1]);
}

#[tokio::test]
async fn a_parser_id_apply_enters_the_chain() {
    let main = indoc! { r#"
        mixed *create() {
            object room = clone_object("/s");
            object t = clone_object("/t");
            object s = clone_object("/ls");
            t->enter(room);
            s->go(t);
            mixed *items;
            int r = parse_command("shade", room, "%i", items);
            return ({ r, items[1] == t });
        }
    "# };
    let got = run(
        ALLOWING,
        &[("/s.c", S), ("/t.c", T), ("/ls.c", LISTENING)],
        main,
    )
    .await;
    assert_eq!(ints(&got), vec![1, 1]);
}

#[tokio::test]
async fn a_driver_hook_enters_the_chain() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/t");
            object s = clone_object("/ls");
            s->go(t);
            tell_object(t, "psst");
            return ({ s->heard() });
        }
    "# };
    let got = strings(&run(ALLOWING, &[("/t.c", T), ("/ls.c", LISTENING)], main).await);
    assert_eq!(got, vec!["psst".to_string()]);
}

#[tokio::test]
async fn a_command_hook_enters_the_chain() {
    let main = indoc! { r#"
        mixed *create() {
            object t = clone_object("/t");
            object s = clone_object("/ls");
            s->go(t);
            t->wake();
            command("psst", t);
            return ({ s->heard() });
        }
    "# };
    let got = strings(&run(ALLOWING, &[("/t.c", T), ("/ls.c", LISTENING)], main).await);
    assert_eq!(got, vec!["psst".to_string()]);
}

/// A shadow with a prompt, for the applies the connection makes by name.
const PROMPTING: &str = indoc! { r#"
    object go(object o) { return shadow(o, 1); }
    string write_prompt() { return "shade> " + file_name(this_object()); }
"# };

/// The body of `/main.c`'s chain and the template to apply on it.
async fn shadowed_body() -> (
    std::sync::Arc<crate::interpreter::process::Process>,
    TaskTemplate,
) {
    let vm = Vm::new(crate::test_support::test_config());
    for (path, code) in [
        ("/secure/master.c", ALLOWING),
        ("/t.c", T),
        ("/ps.c", PROMPTING),
    ] {
        vm.initialize_process_from_code(path, code).await.unwrap();
    }
    let main = vm
        .initialize_process_from_code(
            "/main.c",
            indoc! { r#"
                object t;
                void create() { object s = clone_object("/ps"); t = clone_object("/t"); s->go(t); }
                object t() { return t; }
            "# },
        )
        .await
        .unwrap()
        .context
        .process;
    let template = TaskTemplate::from(vm.global_state.clone());
    let body = match apply_function_by_name("t", &[], main, template.clone(), None).await {
        Some(Ok(LpcRef::Object(weak))) => weak.upgrade().unwrap(),
        other => panic!("the body: {other:?}"),
    };
    (body, template)
}

#[tokio::test]
async fn a_body_apply_enters_the_chain() {
    let (body, template) = shadowed_body().await;
    let got = apply_function_by_name("write_prompt", &[], body, template, None)
        .await
        .unwrap()
        .unwrap();
    let LpcRef::String(s) = got else {
        panic!("a string: {got:?}");
    };
    assert!(s.to_str().starts_with("shade> /ps#"), "{s}");
}

#[tokio::test]
async fn a_body_apply_falls_back_to_the_body() {
    let (body, template) = shadowed_body().await;
    let got = apply_function_by_name("g", &[], body, template, None)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(got, LpcRef::from("t.g"));
}

#[tokio::test]
async fn a_body_apply_nothing_defines_is_absent() {
    let (body, template) = shadowed_body().await;
    let got = apply_function_by_name("nothing_here", &[], body, template, None).await;
    assert!(got.is_none(), "{got:?}");
}
