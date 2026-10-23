//! Independent parser actors and command givers, through real LPC callbacks.

use std::sync::Arc;

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    compiler::CompilerBuilder,
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::{Vm, global_state::GlobalState},
    },
    test_support::connect,
};

const NOTE: &str = r#"
    #define NOTE(label) "/log"->note(label, this_player(), previous_object(-1), this_interactive(), origin(), query_command())
"#;

const LOG: &str = r#"
    mapping expected = ([]), hits = ([]);
    string failure;
    void expect(string line, object actor, object giver, object interactive_user, object invoker) {
        expected[line] = ({ actor, giver, interactive_user, invoker });
    }
    void reset() { hits = ([]); }
    void fail_at(string label) { failure = label; }
    int seen(string label) { return hits[label] ? hits[label] : 0; }
    void note(string label, object player, object *callers, object interactive_user, string how, string line) {
        mixed *want = expected[line];
        if (!want) throw("unexpected command: " + line);
        if (player != want[1]) throw(label + ": wrong player");
        if (sizeof(callers) < 2 || callers[0] != want[0] || callers[1] != want[3]) throw(label + ": wrong callers");
        if (interactive_user != want[2]) throw(label + ": wrong interactive");
        if (how != "driver") throw(label + ": wrong origin");
        hits[label] = seen(label) + 1;
        if (failure == label) throw("callback failure: " + label);
    }
"#;

const MASTER: &str = r#"
    int valid_load() { return 1; }
    int valid_destruct() { return 1; }
    string *parse_command_id_list() { NOTE("master ids"); return ({}); }
    string *parse_command_plural_id_list() { NOTE("master plurals"); return ({}); }
    string *parse_command_adjectiv_id_list() { NOTE("master adjectives"); return ({}); }
    string *parse_command_prepos_list() { NOTE("prepositions"); return ({"to"}); }
    string parse_command_all_word() { NOTE("all word"); return "all"; }
    int parse_command_numeral(string word) { NOTE("numeral"); return 0; }
    string *parse_command_pluralize(string *words) { NOTE("pluralize"); return ({"gems"}); }
    object *parse_command_users() { NOTE("users"); return ({find_object("/bob")}); }
    string parser_error_message(int kind, object ob, mixed arg, int flag) { NOTE("error"); return "unresolved"; }
"#;

const ACTOR: &str = r#"
    int denied;
    void create() { enable_commands(); }
    void deny(int value) { denied = value; }
    int can_verb_rule() { NOTE("can"); return !denied; }
"#;

const BOX: &str = r#"
    void move(object destination) { move_object(destination); }
    int inventory_visible() { NOTE("visible"); return 1; }
    int inventory_accessible() { NOTE("accessible"); return 1; }
"#;

const ITEM: &str = r#"
    int mutate;
    void change_player(int value) { mutate = value; }
    void move(object destination) { move_object(destination); }
    string *parse_command_id_list() { NOTE("ids"); if (mutate) set_this_player(nil); return ({"gem"}); }
    string *parse_command_plural_id_list() { NOTE("plurals"); return nil; }
    string *parse_command_adjectiv_id_list() { NOTE("adjectives"); return ({}); }
    int direct_verb_rule() { NOTE("direct"); return 1; }
"#;

const COIN: &str = r#"
    void move(object destination) { move_object(destination); }
    int id(string word) { NOTE("id"); return word == "coin"; }
    int direct_verb_rule() { NOTE("direct"); return 1; }
"#;

const BOB: &str = r#"
    void create() { enable_commands(); }
    string *parse_command_id_list() { return ({"bob"}); }
    int indirect_verb_rule() { NOTE("indirect"); return 1; }
"#;

const OWNER: &str = r#"
    void create() {
        parse_init(); parse_add_rule("inspect", "OBJ to LIV");
        parse_add_rule("ping", ""); parse_add_rule("outer", "");
        parse_add_rule("recurse", "");
    }
    void do_inspect_obj_to_liv() { NOTE("action"); }
    void do_ping() { NOTE("action"); }
    mixed mine(object actor, string line, int execute, object giver) { return parse_my_rules(actor, line, execute, giver); }
    mixed short_mine(object actor, string line) { return parse_my_rules(actor, line); }
    mixed pointer_mine(object actor, string line, object giver) {
        function f = parse_my_rules;
        return f(actor, line, 1, giver);
    }
    mixed spread_mine(mixed *args) { return parse_my_rules(args...); }
    mixed short_pointer_mine(object actor, string line) {
        function f = parse_my_rules;
        return f(actor, line);
    }
    void do_recurse() {
        "/log"->expect("recurse", previous_object(), this_player(), this_interactive(), this_object());
        parse_sentence("recurse", 0, nil, nil, previous_object(), this_player());
    }
    void do_outer() {
        NOTE("outer before");
        object other = find_object("/other");
        "/log"->expect("ping", other, other, this_interactive(), this_object());
        if (parse_sentence("ping", 0, nil, nil, other) != 1) throw("nested default giver");
        "/log"->expect("ping", other, nil, this_interactive(), this_object());
        if (parse_sentence("ping", 0, nil, nil, other, nil) != 1) throw("nested result");
        "/log"->fail_at("action");
        mixed error = catch(parse_my_rules(other, "ping", 1, nil));
        "/log"->fail_at(nil);
        if (!stringp(error) || strindex(error, "callback failure: action") < 0) throw("nested error");
        if (query_command() != "outer" || query_verb() != "outer") throw("lost command frame");
        NOTE("outer after");
    }
"#;

async fn seed(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, &format!("{NOTE}\n{code}"))
        .await
        .unwrap_or_else(|error| panic!("{path}: {}", error.diagnostic_string()))
        .context
        .process
}

async fn check(body: &str) {
    check_with_entry(body, true).await;
}

async fn check_with_entry(body: &str, has_entry: bool) {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        for (path, source) in [
            ("/log.c", LOG),
            ("/secure/master.c", MASTER),
            ("/actor.c", ACTOR),
            ("/other.c", ACTOR),
            ("/box.c", BOX),
            ("/gem.c", ITEM),
            ("/coin.c", COIN),
            ("/bob.c", BOB),
            ("/owner.c", OWNER),
            ("/npc.c", ""),
            ("/dead.c", ""),
        ] {
            seed(&vm, path, source).await;
        }
        let user = seed(&vm, "/user.c", "").await;
        let _connected = connect(&vm, &user).await;
        let main = seed(&vm, "/main.c", &format!(r#"
            #pragma no_warnings
            int run() {{
                object user = find_object("/user"), actor = find_object("/actor"), other = find_object("/other");
                object owner = find_object("/owner"), log = find_object("/log"), npc = find_object("/npc");
                "/box"->move(actor); "/gem"->move(find_object("/box")); "/coin"->move(actor);
                {body}
            }}
        "#)).await;
        let template = TaskTemplate::from(vm.global_state.clone());
        template.set_this_player(has_entry.then_some(user));
        let result = apply_function_by_name("run", &[], main, template, Some(10_000))
            .await
            .unwrap()
            .unwrap_or_else(|error| panic!("{mode:?}: {}", error.diagnostic_string()));
        assert_eq!(result, LpcRef::from(1), "{mode:?}");
    }
}

#[tokio::test]
async fn explicit_identities_reach_every_callback_and_both_restricted_modes() {
    check(r#"
        string line = "inspect gem to bob";
        for (int mode = 0; mode < 3; mode++) {
            log->expect(line, actor, user, user, mode ? owner : this_object()); log->reset();
            mixed result = mode ? owner->mine(actor, line, mode == 1, user) : parse_sentence(line, 0, nil, nil, actor, user);
            if (mode == 2 ? !arrayp(result) : result != 1) throw("parse result");
            foreach (string label in ({ "visible", "accessible", "users", "master ids", "master plurals",
                "master adjectives", "prepositions", "all word", "numeral", "pluralize", "ids", "plurals",
                "adjectives", "can", "direct", "indirect" })) {
                if (!log->seen(label)) throw("missing callback: " + label);
            }
            if ((mode == 2) != !log->seen("action")) return 0;
            if (this_player() != user || this_interactive() != user || query_command()) return 0;
        }
        return 1;
    "#).await;
}

#[tokio::test]
async fn omitted_givers_default_to_actor_and_explicit_null_clears_them() {
    check(r#"
        set_this_player(actor);
        log->expect("ping", actor, actor, user, this_object());
        if (parse_sentence("ping") != 1 || parse_sentence("ping", 0, nil, nil, actor) != 1) return 0;
        log->expect("ping", actor, actor, user, owner);
        if (!arrayp(owner->short_mine(actor, "ping"))) return 0;
        set_this_player(user);
        foreach (object giver in ({ user, npc, other, nil })) {
            log->expect("ping", actor, giver, user, this_object());
            if (parse_sentence("ping", 0, nil, nil, actor, giver) != 1) return 0;
            log->expect("ping", actor, giver, user, owner);
            if (owner->mine(actor, "ping", 1, giver) != 1) return 0;
            if (this_player() != user || this_interactive() != user) return 0;
        }
        return !query_command();
    "#).await;
}

#[tokio::test]
async fn spread_and_pointer_calls_distinguish_omission_from_null() {
    check(
        r#"
        function sentence = parse_sentence;
        foreach (object giver in ({ user, nil })) {
            mixed *args = ({"ping", 0, nil, nil, actor, giver});
            log->expect("ping", actor, giver, user, this_object());
            if (parse_sentence(args...) != 1 || sentence(args...) != 1) return 0;
            log->expect("ping", actor, giver, user, owner);
            if (owner->pointer_mine(actor, "ping", giver) != 1) return 0;
            if (owner->spread_mine(({actor, "ping", 1, giver})) != 1) return 0;
        }
        set_this_player(actor);
        log->expect("ping", actor, actor, user, this_object());
        if (sentence("ping") != 1 || sentence("ping", 0, nil, nil, actor) != 1) return 0;
        log->expect("ping", actor, actor, user, owner);
        if (!arrayp(owner->short_pointer_mine(actor, "ping")) || !arrayp(owner->spread_mine(({actor, "ping"})))) return 0;
        return this_player() == actor && this_interactive() == user;
    "#,
    )
    .await;
}

#[tokio::test]
async fn supplied_scope_nicknames_and_id_fallback_keep_selected_context() {
    check(r#"
        string line = "inspect jewel to bob";
        mixed *scope = ({ find_object("/box"), ({find_object("/gem")}) });
        log->expect(line, actor, nil, user, this_object());
        if (parse_sentence(line, 0, scope, (["jewel":find_object("/gem")]), actor, nil) != 1) return 0;
        if (!log->seen("visible") || !log->seen("accessible")) return 0;
        line = "inspect coin to bob";
        log->expect(line, actor, npc, user, this_object()); log->reset();
        if (parse_sentence(line, 0, nil, nil, actor, npc) != 1 || !log->seen("id")) return 0;
        return this_player() == user && this_interactive() == user;
    "#).await;
}

#[tokio::test]
async fn actor_permissions_and_owned_rules_are_independent_of_the_giver() {
    check(
        r#"
        log->expect("ping", actor, user, user, this_object());
        actor->deny(1);
        if (parse_sentence("ping", 0, nil, nil, actor, user) != -2 || log->seen("action")) return 0;
        actor->deny(0);
        log->expect("ping", actor, user, user, owner);
        if (owner->mine(actor, "ping", 1, user) != 1) return 0;
        if (parse_my_rules(actor, "ping", 1, user) != 0) return 0;
        return this_player() == user;
    "#,
    )
    .await;
}

#[tokio::test]
async fn errors_and_callback_player_mutation_preserve_selected_and_outer_context() {
    check(r#"
        string line = "inspect gem to bob";
        "/gem"->change_player(1);
        for (int restricted = 0; restricted < 2; restricted++) {
            log->expect(line, actor, npc, user, restricted ? owner : this_object());
            foreach (string label in ({ "visible", "users", "master ids", "ids", "can", "direct", "indirect", "action" })) {
                log->reset(); log->fail_at(label);
                mixed error = catch(restricted ? owner->mine(actor, line, 1, npc) : parse_sentence(line, 0, nil, nil, actor, npc));
                log->fail_at(nil);
                if (!stringp(error) || strindex(error, "callback failure: " + label) < 0) throw("missing failure: " + label);
                if (!log->seen(label) || this_player() != user || query_command() || query_verb()) return 0;
            }
            if ((restricted ? owner->mine(actor, line, 1, npc) : parse_sentence(line, 0, nil, nil, actor, npc)) != 1) return 0;
            string missing = "inspect missing to bob";
            log->expect(missing, actor, nil, user, restricted ? owner : this_object()); log->reset();
            mixed result = restricted ? owner->mine(actor, missing, 1, nil) : parse_sentence(missing, 0, nil, nil, actor, nil);
            if (result != "unresolved" || !log->seen("error")) return 0;
            log->fail_at("error");
            mixed error = catch(restricted ? owner->mine(actor, missing, 1, nil) : parse_sentence(missing, 0, nil, nil, actor, nil));
            log->fail_at(nil);
            if (!stringp(error) || strindex(error, "callback failure: error") < 0) return 0;
        }
        return this_player() == user && this_interactive() == user && !query_command();
    "#).await;
}

#[tokio::test]
async fn nested_parses_restore_their_outer_actor_giver_and_command_frame() {
    check(r#"
        log->expect("outer", actor, npc, user, this_object());
        log->expect("ping", other, nil, user, owner);
        if (parse_sentence("outer", 0, nil, nil, actor, npc) != 1) return 0;
        if (log->seen("outer before") != 1 || log->seen("outer after") != 1 || log->seen("action") != 3) return 0;
        return this_player() == user && this_interactive() == user && !query_command() && !query_verb();
    "#).await;
}

#[tokio::test]
async fn invalid_identities_fail_before_callbacks_and_dead_givers_clear() {
    check(r#"
        mixed error = catch(parse_sentence("ping"));
        if (!stringp(error) || strindex(error, "this_player() is not a living") < 0) return 0;
        object dead = find_object("/dead"); destruct(dead);
        foreach (mixed bad in ({ nil, dead, user, 7, "wrong", ({}), ([]) })) {
            error = catch(parse_sentence("ping", 0, nil, nil, bad, user));
            if (!stringp(error) || strindex(error, "actor is not a living") < 0) return 0;
            error = catch(parse_my_rules(bad, "ping", 1, user));
            if (!stringp(error) || strindex(error, "actor is not a living") < 0) return 0;
        }
        foreach (mixed bad in ({ 7, "wrong", ({}), ([]) })) {
            error = catch(parse_sentence("ping", 0, nil, nil, actor, bad));
            if (!stringp(error) || strindex(error, "command giver must be an object or null") < 0) return 0;
            error = catch(parse_my_rules(actor, "ping", 1, bad));
            if (!stringp(error) || strindex(error, "command giver must be an object or null") < 0) return 0;
        }
        if (log->seen("can") || log->seen("action") || log->seen("visible")) return 0;
        log->expect("ping", actor, nil, user, this_object());
        if (parse_sentence("ping", 0, nil, nil, actor, dead) != 1) return 0;
        log->expect("ping", actor, nil, user, owner);
        if (owner->mine(actor, "ping", 1, dead) != 1) return 0;
        mixed zero = 0;
        log->expect("ping", actor, nil, user, this_object());
        error = catch(parse_sentence("ping", 0, nil, nil, actor, zero));
        if (nil == 0 ? stringp(error) : !stringp(error)) return 0;
        log->expect("ping", actor, nil, user, owner);
        error = catch(owner->mine(actor, "ping", 1, zero));
        if (nil == 0 ? stringp(error) : !stringp(error)) return 0;
        return this_player() == user && this_interactive() == user && !query_command();
    "#).await;
}

#[tokio::test]
async fn background_parsing_does_not_gain_interactivity_from_a_connected_giver() {
    check_with_entry(
        r#"
        log->expect("inspect gem to bob", actor, user, nil, this_object());
        if (parse_sentence("inspect gem to bob", 0, nil, nil, actor, user) != 1) return 0;
        log->expect("ping", actor, user, nil, owner);
        if (owner->mine(actor, "ping", 1, user) != 1) return 0;
        return !this_player() && !this_interactive() && !query_command();
    "#,
        false,
    )
    .await;
}

#[test]
fn depth_errors_restore_explicit_parser_context() {
    std::thread::Builder::new().stack_size(16 * 1024 * 1024).spawn(|| {
        tokio::runtime::Builder::new_current_thread().enable_all().build().unwrap().block_on(check(r#"
            log->expect("recurse", actor, npc, user, this_object());
            mixed error = catch(parse_sentence("recurse", 0, nil, nil, actor, npc));
            if (!stringp(error) || strindex(error, "nesting deeper than 16") < 0) throw("missing depth error");
            return this_player() == user && this_interactive() == user && !query_command() && !query_verb();
        "#));
    }).unwrap().join().unwrap();
}

#[tokio::test]
async fn parser_retries_reselect_the_giver_and_commit_callback_writes_once() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir("tests/fixtures/code")
            .null_mode(mode)
            .build()
            .unwrap();
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(Arc::new(config), tx, 2));
        state
            .object_space
            .create_process_from_code("/giver.c", "")
            .await
            .unwrap();
        let task = state.initialize_process_from_code("/test.c", r#"
            int calls;
            object expected;
            void do_ping() {
                if (this_player() != expected || previous_object() != this_object() || this_interactive()) throw("callback context");
                calls++;
            }
            int create() {
                enable_commands(); parse_init(); parse_add_rule("ping", "");
                set_this_player(this_object());
                expected = find_object("/giver");
                if (parse_sentence("ping", 0, nil, nil, this_object(), expected) != 1) return 0;
                expected = nil;
                if (parse_my_rules(this_object(), "ping", 1, nil) != 1) return 0;
                return calls == 2 && this_player() == this_object() && !query_command();
            }
        "#).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode:?}");
        assert_eq!(state.attempt_telemetry().conflicts, 2);
        assert_eq!(
            state.committed_global(&task.context.process, 0),
            LpcRef::from(2)
        );
    }
}

#[tokio::test]
async fn uncaught_parser_errors_roll_back_callback_writes() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        let process = seed(
            &vm,
            "/owner.c",
            r#"
            int calls;
            void create() { enable_commands(); parse_init(); parse_add_rule("ping", ""); }
            void do_ping() { calls++; throw("action failed"); }
            void run() { parse_my_rules(this_object(), "ping", 1, nil); }
        "#,
        )
        .await;
        let error = apply_function_by_name(
            "run",
            &[],
            process.clone(),
            TaskTemplate::from(vm.global_state.clone()),
            Some(10_000),
        )
        .await
        .unwrap()
        .unwrap_err();
        assert!(error.to_string().contains("action failed"));
        assert_eq!(
            vm.global_state.committed_global(&process, 0),
            LpcRef::from(0)
        );
    }
}

#[tokio::test]
async fn concurrent_parser_tasks_keep_their_selected_givers_separate() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        let source = r#"
            int calls;
            object expected;
            void create() { enable_commands(); parse_init(); parse_add_rule("ping", ""); }
            void do_ping() {
                if (this_player() != expected || previous_object() != this_object()) throw("crossed identities");
                calls++;
            }
            int run(object giver) {
                expected = giver;
                for (int i = 0; i < 20; i++) {
                    if (parse_my_rules(this_object(), "ping", 1, giver) != 1 || this_player() || query_command()) return 0;
                }
                return calls;
            }
        "#;
        let first = seed(&vm, "/first.c", source).await;
        let second = seed(&vm, "/second.c", source).await;
        let first_args = [LpcRef::from(Arc::downgrade(&second))];
        let second_args = [LpcRef::from(Arc::downgrade(&first))];
        let (a, b) = tokio::join!(
            apply_function_by_name(
                "run",
                &first_args,
                first,
                TaskTemplate::from(vm.global_state.clone()),
                Some(10_000)
            ),
            apply_function_by_name(
                "run",
                &second_args,
                second,
                TaskTemplate::from(vm.global_state.clone()),
                Some(10_000)
            ),
        );
        assert_eq!(a.unwrap().unwrap(), LpcRef::from(20), "{mode:?}");
        assert_eq!(b.unwrap().unwrap(), LpcRef::from(20), "{mode:?}");
    }
}

#[tokio::test]
async fn parser_identity_parameters_reject_statically_wrong_types() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir("tests/fixtures/code")
            .null_mode(mode)
            .build()
            .unwrap();
        let compiler = CompilerBuilder::default().config(config).build().unwrap();
        for expression in [
            r#"parse_sentence("ping", 0, nil, nil, "bad")"#,
            r#"parse_sentence("ping", 0, nil, nil, this_object(), "bad")"#,
            r#"parse_my_rules(this_object(), "ping", 1, "bad")"#,
        ] {
            let error = compiler
                .compile_string("/bad.c", &format!("void run() {{ {expression}; }}"))
                .await
                .unwrap_err();
            let diagnostic = error.diagnostic_string();
            assert!(
                diagnostic.contains("string") && diagnostic.contains("object"),
                "{mode:?}: {diagnostic}"
            );
        }
    }
}

#[tokio::test]
async fn a_giver_destroyed_during_parsing_does_not_fall_back_to_the_actor() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        seed(
            &vm,
            "/secure/master.c",
            "int valid_load() { return 1; } int valid_destruct() { return 1; }",
        )
        .await;
        seed(&vm, "/giver.c", "").await;
        let process = seed(&vm, "/owner.c", r#"
            int called;
            void create() { enable_commands(); parse_init(); parse_add_rule("ping", ""); }
            int can_ping() {
                if (this_player() != find_object("/giver")) throw("missing giver before destruction");
                destruct(this_player()); return 1;
            }
            void do_ping() {
                if (this_player() || previous_object() != this_object()) throw("dead giver fallback");
                called++;
            }
            int run() {
                return parse_sentence("ping", 0, nil, nil, this_object(), find_object("/giver")) == 1 && called == 1 && !this_player();
            }
        "#).await;
        let result = apply_function_by_name(
            "run",
            &[],
            process,
            TaskTemplate::from(vm.global_state.clone()),
            Some(10_000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from(1), "{mode:?}");
    }
}
