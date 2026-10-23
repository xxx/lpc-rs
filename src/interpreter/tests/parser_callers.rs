//! Parser callbacks retain the invoking LPC frames behind the actor.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::run_with;
use crate::{
    interpreter::{lpc_ref::LpcRef, vm::Vm},
    test_support::lib_holding,
};

const TRACE: &str = r#"
    #define TRACE(label) "/log"->record(label, previous_object(-1), calling_object(-1), calling_function(-1), calling_program(-1), this_player(), origin())
"#;

const LOG: &str = r#"
    mapping records = ([]);
    string describe(mixed *values) {
        string *names = ({});
        foreach (mixed value in values) {
            names += ({ objectp(value) ? file_name(value) : stringp(value) ? value : "_" });
        }
        return implode(names, " ");
    }
    void record(string label, object *previous, object *frames, string *functions,
                string *programs, object player, string how) {
        records[label] = ({ describe(previous), describe(frames), describe(functions),
                            describe(programs), player, how });
    }
    int check(string label, string previous, string frames, string functions,
              string programs, object player) {
        mixed *got = records[label];
        if (!got) throw("missing callback: " + label);
        mixed *want = ({ previous, frames, functions, programs, player, "driver" });
        for (int i = 0; i < sizeof(want); i++) {
            if (got[i] != want[i]) throw(label + " field " + i + ": " + got[i]);
        }
        return 1;
    }
"#;

const MASTER: &str = r#"
    int valid_load() { return 1; }
    int valid_inherit() { return 1; }
    string *parse_command_id_list() { TRACE("master ids"); return ({}); }
    object *parse_command_users() { TRACE("users"); return ({ find_object("/bob") }); }
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        TRACE("error"); return "unresolved";
    }
"#;

const ACTOR: &str = r#"
    object denied;
    void create() { enable_commands(); }
    void forbid(object caller) { denied = caller; }
    int can_verb_rule() {
        TRACE("can");
        return !denied || member_array(denied, previous_object(-1)) < 0;
    }
"#;

const ITEM: &str = r#"
    void move(object destination) { move_object(destination); }
    string *parse_command_id_list() { TRACE("ids"); return ({ "gem" }); }
    int direct_verb_rule() { TRACE("direct"); return 1; }
"#;

const BOB: &str = r#"
    void create() { enable_commands(); }
    string *parse_command_id_list() { return ({ "bob" }); }
    int indirect_verb_rule() { TRACE("indirect"); return 1; }
"#;

const BOX: &str = r#"
    void move(object destination) { move_object(destination); }
    int inventory_visible() { TRACE("visible"); return 1; }
    int inventory_accessible() { TRACE("accessible"); return 1; }
"#;

const OWNER: &str = r#"
    void enable() { enable_commands(); }
    void create() {
        parse_init();
        parse_add_rule("inspect", "OBJ to LIV");
    }
    void do_inspect_obj_to_liv() { TRACE("do"); }
    mixed parse_local(int restricted, string line) {
        if (restricted) return parse_my_rules(find_object("/actor"), line, restricted == 1);
        return parse_sentence(line);
    }
    mixed enter(int restricted, string line) { return parse_local(restricted, line); }
"#;

const BRIDGE: &str = r#"
    mixed local_enter(int restricted, string line) { return "/owner"->enter(restricted, line); }
    mixed enter(int restricted, string line) { return local_enter(restricted, line); }
"#;

async fn check(owner: &str, bridge: &str, body: &str) {
    let sources = [
        ("/log.c", LOG),
        ("/actor.c", ACTOR),
        ("/other.c", ACTOR),
        ("/box.c", BOX),
        ("/item.c", ITEM),
        ("/bob.c", BOB),
        ("/owner.c", owner),
        ("/bridge.c", bridge),
    ]
    .map(|(path, code)| (path, format!("{TRACE}\n{code}")));
    let sources: Vec<_> = sources
        .iter()
        .map(|(path, code)| (*path, code.as_str()))
        .collect();
    let main = format!(
        r#"
        mixed *create() {{
            object actor = find_object("/actor");
            object log = find_object("/log");
            set_this_player(actor);
            "/box"->move(actor);
            "/item"->move(find_object("/box"));
            {body}
        }}
    "#
    );
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        assert_eq!(
            run_with(config, &format!("{TRACE}\n{MASTER}"), &sources, &main).await,
            vec![LpcRef::from(1)],
            "{mode:?}"
        );
    }
}

#[tokio::test]
async fn every_parser_callback_retains_external_and_local_callers() {
    check(OWNER, BRIDGE, r#"
        for (int restricted = 0; restricted < 3; restricted++) {
            mixed result = "/bridge"->enter(restricted, "inspect gem to bob");
            if (restricted == 2 ? !arrayp(result) : result != 1) return ({0});
            string *labels = ({ "visible", "accessible", "users", "master ids", "ids", "can", "direct", "indirect" });
            if (restricted != 2) labels += ({ "do" });
            foreach (string label in labels) {
                log->check(label, "/actor /owner /bridge /main",
                    "/actor /owner /owner /bridge /bridge /main",
                    "_ parse_local enter local_enter enter create",
                    "_ /owner.c /owner.c /bridge.c /bridge.c /main.c", actor);
            }
            if ("/bridge"->enter(restricted, "inspect missing to bob") != "unresolved") return ({0});
            log->check("error", "/actor /owner /bridge /main",
                "/actor /owner /owner /bridge /bridge /main",
                "_ parse_local enter local_enter enter create",
                "_ /owner.c /owner.c /bridge.c /bridge.c /main.c", actor);
        }
        return ({ this_player() == actor && !previous_object() && !query_command() });
    "#).await;
}

#[tokio::test]
async fn parser_actor_and_invoking_object_are_not_deduplicated() {
    check(
        OWNER,
        BRIDGE,
        r#"
        object owner = find_object("/owner");
        owner->enable();
        "/box"->move(owner);
        set_this_player(owner);
        if (owner->enter(0, "inspect gem to bob") != 1) return ({0});
        log->check("do", "/owner /owner /main", "/owner /owner /owner /main",
            "_ parse_local enter create", "_ /owner.c /owner.c /main.c", owner);
        return ({1});
    "#,
    )
    .await;
}

#[tokio::test]
async fn actor_permissions_can_reject_an_untrusted_invoking_caller() {
    check(
        OWNER,
        BRIDGE,
        r#"
        actor->forbid(find_object("/bridge"));
        for (int restricted = 0; restricted < 2; restricted++) {
            if ("/owner"->enter(restricted, "inspect gem to bob") != 1) return ({0});
            if ("/bridge"->enter(restricted, "inspect gem to bob") != -2) return ({0});
        }
        return ({1});
    "#,
    )
    .await;
}

#[tokio::test]
async fn foreign_efun_pointers_retain_owner_firer_and_calling_frames() {
    let owner = format!(
        r#"{OWNER}
        function sentence() {{ return parse_sentence; }}
        function owned() {{ return parse_my_rules; }}
    "#
    );
    let bridge = r#"
        mixed enter(int restricted, string line) {
            function parser = restricted ? "/owner"->owned() : "/owner"->sentence();
            if (restricted) return parser(find_object("/actor"), line, 1);
            return parser(line);
        }
    "#;
    check(
        &owner,
        bridge,
        r#"
        for (int restricted = 0; restricted < 2; restricted++) {
            if ("/bridge"->enter(restricted, "inspect gem to bob") != 1) return ({0});
            log->check("do", "/actor /owner /bridge /main", "/actor /owner /bridge /main",
                "_ _ enter create", "_ _ /bridge.c /main.c", actor);
        }
        return ({ this_player() == actor && !previous_object() && !query_command() });
    "#,
    )
    .await;
}

#[tokio::test]
async fn nested_parses_retain_outer_frames_and_restore_after_exceptions() {
    let owner = r#"
        int calls;
        void create() {
            parse_init(); parse_add_rule("outer", "");
            parse_add_rule("inner", ""); parse_add_rule("boom", "");
        }
        void do_inner() { calls++; TRACE("inner"); }
        void do_boom() { calls++; TRACE("boom"); throw("parser failure"); }
        void do_outer() {
            TRACE("outer before");
            object other = find_object("/other");
            if (parse_my_rules(other, "inner", 1) != 1) throw("inner did not run");
            if (!catch(parse_my_rules(other, "boom", 1))) throw("exception missing");
            TRACE("outer after");
            if (query_command() != "outer" || query_verb() != "outer") throw("command leaked");
        }
        int calls() { return calls; }
        mixed parse_local(int restricted, string line) {
            if (restricted) return parse_my_rules(find_object("/actor"), line, 1);
            return parse_sentence(line);
        }
        mixed enter(int restricted, string line) { return parse_local(restricted, line); }
    "#;
    check(owner, BRIDGE, r#"
        for (int restricted = 0; restricted < 2; restricted++) {
            if ("/bridge"->enter(restricted, "outer") != 1) return ({0});
            foreach (string label in ({ "outer before", "outer after" })) {
                log->check(label, "/actor /owner /bridge /main",
                    "/actor /owner /owner /bridge /bridge /main",
                    "_ parse_local enter local_enter enter create",
                    "_ /owner.c /owner.c /bridge.c /bridge.c /main.c", actor);
            }
            foreach (string label in ({ "inner", "boom" })) {
                log->check(label, "/other /owner /actor /owner /bridge /main",
                    "/other /owner /actor /owner /owner /bridge /bridge /main",
                    "_ do_outer _ parse_local enter local_enter enter create",
                    "_ /owner.c _ /owner.c /owner.c /bridge.c /bridge.c /main.c", find_object("/other"));
            }
            if ("/owner"->calls() != 3 * restricted + 2) return ({0});
            if (!catch("/bridge"->enter(restricted, "boom"))) return ({0});
            if (this_player() != actor || previous_object() || query_command()) return ({0});
        }
        return ({1});
    "#).await;
}

#[tokio::test]
async fn simul_and_inherited_frames_keep_their_defining_programs() {
    let base = r#"
        mixed parse_local(int restricted) {
            if (restricted) return parse_my_rules(find_object("/actor"), "inspect", 1);
            return parse_sentence("inspect");
        }
    "#;
    let simul = format!(
        r#"{TRACE}
        inherit "/base";
        void create() {{ parse_init(); parse_add_rule("inspect", ""); }}
        void do_inspect() {{ TRACE("do"); }}
        mixed simul_parse(int restricted) {{ return parse_local(restricted); }}
    "#
    );
    let root = lib_holding("parser-callers", &[("base.c", base), ("simul.c", &simul)]);
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .simul_efun_file("/simul.c")
            .null_mode(mode)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.initialize_simul_efuns().await.unwrap().unwrap();
        for (path, code) in [("/log.c", LOG), ("/actor.c", ACTOR)] {
            vm.initialize_process_from_code(path, &format!("{TRACE}\n{code}"))
                .await
                .unwrap();
        }
        let task = vm
            .initialize_process_from_code(
                "/main.c",
                r#"
            int create() {
                object actor = find_object("/actor");
                set_this_player(actor);
                for (int restricted = 0; restricted < 2; restricted++) {
                    if (simul_parse(restricted) != 1) return 0;
                    "/log"->check("do", "/actor /simul /main", "/actor /simul /simul /main",
                        "_ parse_local simul_parse create", "_ /base.c /simul.c /main.c", actor);
                }
                return this_player() == actor && !previous_object() && !query_command();
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode:?}");
    }
}
