//! Command vocabulary uses the actor while preserving the entering player context.

use std::sync::Arc;

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    },
    test_support::connect,
};

const NOTE: &str = r#"
    #define NOTE(label) "/log"->note(label, this_player(), previous_object(), this_interactive(), origin(), query_command())
"#;

const LOG: &str = r#"
    mapping expected = ([]), hits = ([]);
    string failure;
    string key(string line) { return line ? line : "outside"; }
    void expect(string line, object player, object caller, object interactive_user) {
        expected[key(line)] = ({ player, caller, interactive_user });
    }
    void reset() { hits = ([]); }
    void fail_at(string label) { failure = label; }
    int seen(string label) { return hits[label] ? hits[label] : 0; }
    void note(string label, object player, object caller, object interactive_user,
              string how, string line) {
        mixed *want = expected[key(line)];
        if (!want) throw("unexpected command: " + key(line));
        if (player != want[0]) throw(label + ": wrong player");
        if (caller != want[1]) throw(label + ": wrong caller");
        if (interactive_user != want[2]) throw(label + ": wrong interactive");
        if (how != "driver") throw(label + ": wrong origin");
        hits[label] = seen(label) + 1;
        if (failure == label) throw("vocabulary failure: " + label);
    }
"#;

const MASTER: &str = r#"
    int valid_load() { return 1; }
    string *parse_command_id_list() { NOTE("master ids"); return ({}); }
    string *parse_command_plural_id_list() { NOTE("master plurals"); return ({}); }
    string *parse_command_adjectiv_id_list() { NOTE("master adjectives"); return ({}); }
    string *parse_command_prepos_list() { NOTE("prepositions"); return ({"at"}); }
    string parse_command_all_word() { NOTE("all word"); return "all"; }
    int parse_command_numeral(string word) { NOTE("numeral"); return 0; }
    string *parse_command_pluralize(string *words) { NOTE("pluralize"); return ({"gems"}); }
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        NOTE("error"); return "no match";
    }
"#;

const BODY: &str = r#"
    void create() {
        enable_commands(); set_this_player(this_object());
        add_rule("'native' %o", native_action);
        add_rule("'fallback' %o", decline);
    }
    int native_action(object ob) { NOTE("native action"); return 1; }
    int decline(object ob) { NOTE("decline"); return 0; }
    int local_command(string line) { return command(line); }
    int can_verb_rule() { NOTE("can"); return 1; }
    void catch_tell(string message) { NOTE("output"); }
"#;

const ITEM: &str = r#"
    int mutate;
    void change_player(int value) { mutate = value; }
    void move(object destination) { move_object(destination); }
    string *parse_command_id_list() {
        NOTE("ids");
        if (mutate) set_this_player(nil);
        return ({"gem"});
    }
    string *parse_command_plural_id_list() { NOTE("plurals"); return nil; }
    string *parse_command_adjectiv_id_list() { NOTE("adjectives"); return ({}); }
    int direct_verb_rule() { NOTE("direct"); return 1; }
"#;

const COIN: &str = r#"
    void move(object destination) { move_object(destination); }
    int id(string word) { NOTE("id"); return word == "coin"; }
    int direct_verb_rule() { NOTE("direct"); return 1; }
"#;

const VERB: &str = r#"
    void create() {
        parse_init(); parse_add_rule("touch", "OBJ");
        parse_add_rule("fallback", "OBJ"); parse_add_rule("outer", "");
    }
    void do_touch_obj() { NOTE("action"); }
    void do_fallback_obj() { NOTE("action"); }
    void do_outer() {
        NOTE("outer before");
        object other = find_object("/other_body");
        if (command("native gem", other) != 1) throw("native nested command");
        if (command("touch gem", other) != 1) throw("parser nested command");
        "/log"->fail_at("ids");
        mixed error = catch(command("native gem", other));
        "/log"->fail_at(nil);
        if (!stringp(error) || strindex(error, "vocabulary failure: ids") < 0) throw("missing error");
        if (query_verb() != "outer" || query_command() != "outer") throw("command context leaked");
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
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = ConfigBuilder::default()
            .lib_dir("tests/fixtures/code")
            .null_mode(mode)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        for (path, source) in [
            ("/log.c", LOG),
            ("/secure/master.c", MASTER),
            ("/body.c", BODY),
            ("/other_body.c", BODY),
            ("/gem.c", ITEM),
            ("/other_gem.c", ITEM),
            ("/coin.c", COIN),
            ("/verb.c", VERB),
        ] {
            seed(&vm, path, source).await;
        }
        let user = seed(&vm, "/user.c", "").await;
        let _connected = connect(&vm, &user).await;
        let code = format!(
            r#"
            int run() {{
                object user = find_object("/user"), body = find_object("/body");
                object other = find_object("/other_body"), log = find_object("/log");
                "/gem"->move(body); "/coin"->move(body); "/other_gem"->move(other);
                {body}
            }}
        "#
        );
        let main = seed(&vm, "/main.c", &code).await;
        let template = TaskTemplate::from(vm.global_state.clone());
        template.set_this_player(Some(user));
        let result = apply_function_by_name("run", &[], main, template, Some(10_000))
            .await
            .unwrap()
            .unwrap_or_else(|error| panic!("{mode:?}: {}", error.diagnostic_string()));
        assert_eq!(result, LpcRef::from(1), "{mode:?}");
    }
}

#[tokio::test]
async fn parser_vocabulary_and_handlers_use_the_command_actor() {
    check(r#"
        log->expect("touch gem", body, body, user);
        foreach (object player in ({ user, body, other, nil })) {
            set_this_player(player); log->reset();
            if (command("touch gem", body) != 1) return 0;
            foreach (string stage in ({ "master ids", "master plurals", "master adjectives",
                "prepositions", "all word", "numeral", "ids", "plurals", "pluralize",
                "adjectives", "can", "direct", "action" })) {
                if (!log->seen(stage)) throw("missing callback: " + stage);
            }
            if (this_player() != player || this_interactive() != user || query_command()) return 0;
        }
        log->expect("touch missing", body, body, user);
        if (command("touch missing", body) != 1 || !log->seen("error") || !log->seen("output")) return 0;
        return !this_player() && this_interactive() == user && !query_command();
    "#).await;
}

#[tokio::test]
async fn native_vocabulary_and_id_fallback_use_the_default_actor() {
    check(r#"
        foreach (object player in ({ user, body, nil })) {
            set_this_player(player);
            foreach (string noun in ({ "gem", "coin" })) {
                string line = "native " + noun;
                log->expect(line, body, body, user); log->reset();
                if (body->local_command(line) != 1) return 0;
                if (!log->seen(noun == "gem" ? "ids" : "id") || !log->seen("native action")) return 0;
                if (this_player() != player || this_interactive() != user || query_command()) return 0;
            }
        }
        return 1;
    "#).await;
}

#[tokio::test]
async fn native_refusal_keeps_actor_context_for_parser_fallback() {
    check(
        r#"
        log->expect("fallback gem", body, body, user);
        if (command("fallback gem", body) != 1) return 0;
        foreach (string stage in ({ "ids", "decline", "can", "direct", "action" })) {
            if (!log->seen(stage)) throw("missing callback: " + stage);
        }
        return this_player() == user && this_interactive() == user && !query_command();
    "#,
    )
    .await;
}

#[tokio::test]
async fn direct_parse_command_retains_its_entering_player() {
    check(
        r#"
        object gem = find_object("/gem");
        foreach (object player in ({ user, body, nil })) {
            set_this_player(player);
            log->expect(nil, player, this_object(), user); log->reset();
            object matched;
            if (parse_command("gem", ({ gem }), "%o", matched) != 1 || matched != gem) return 0;
            if (!log->seen("ids") || !log->seen("pluralize")) return 0;
            if (this_player() != player || this_interactive() != user || query_command()) return 0;
        }
        return 1;
    "#,
    )
    .await;
}

#[tokio::test]
async fn caught_vocabulary_errors_restore_context_and_retain_writes() {
    check(r#"
        foreach (string prefix in ({ "native", "touch" })) {
            foreach (string stage in ({ "all word", "master ids", "ids", "pluralize", "id" })) {
                string line = prefix + (stage == "id" ? " coin" : " gem");
                log->expect(line, body, body, user); log->reset(); log->fail_at(stage);
                mixed error = catch(command(line, body));
                log->fail_at(nil);
                if (!stringp(error) || strindex(error, "vocabulary failure: " + stage) < 0) {
                    throw("missing failure: " + stage);
                }
                if (!log->seen(stage) || log->seen("action") || log->seen("native action")) return 0;
                if (this_player() != user || this_interactive() != user || query_command() || query_verb()) return 0;
            }
        }
        return 1;
    "#).await;
}

#[tokio::test]
async fn nested_commands_restore_the_outer_actor_and_command_frame() {
    check(r#"
        log->expect("outer", body, body, user);
        log->expect("native gem", other, other, user);
        log->expect("touch gem", other, other, user);
        if (command("outer", body) != 1) return 0;
        foreach (string stage in ({ "outer before", "native action", "action", "outer after" })) {
            if (log->seen(stage) != 1) throw("missing or repeated callback: " + stage);
        }
        return this_player() == user && this_interactive() == user && !query_command() && !query_verb();
    "#).await;
}

#[tokio::test]
async fn vocabulary_player_changes_do_not_leak_to_later_callbacks() {
    check(r#"
        "/gem"->change_player(1);
        foreach (object player in ({ user, body })) {
            set_this_player(player);
            foreach (string line in ({ "native gem", "touch gem" })) {
                log->expect(line, body, body, user); log->reset();
                if (command(line, body) != 1) return 0;
                foreach (string stage in ({ "ids", "plurals", "pluralize", "adjectives" })) {
                    if (!log->seen(stage)) throw("missing callback: " + stage);
                }
                if (!log->seen(line == "native gem" ? "native action" : "action")) return 0;
                if (this_player() != player || this_interactive() != user || query_command()) return 0;
            }
        }
        return 1;
    "#).await;
}
