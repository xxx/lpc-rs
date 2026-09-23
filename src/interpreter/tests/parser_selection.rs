//! Rule selection defers diagnostics until every registered alternative has failed.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run_with, s};
use crate::interpreter::lpc_ref::LpcRef;

async fn check(master: &str, objects: &[(&str, &str)], body: &str, expected: Vec<LpcRef>) {
    let main = format!(
        r##"
        string heard = "";
        void catch_tell(string message) {{ heard += message; }}
        mixed *create() {{
            enable_commands();
            set_this_player(this_object());
            {body}
        }}
        "##
    );
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        assert_eq!(
            run_with(config, master, objects, &main).await,
            expected,
            "{mode:?}"
        );
    }
}

#[tokio::test]
async fn later_success_skips_error_hooks_and_preserves_permission_effects() {
    check(
        r##"string parser_error_message() { throw("premature error hook"); }"##,
        &[
            ("/item.c", r##"
                int rejected;
                string *parse_command_id_list() { return ({ "sword" }); }
                mixed direct_look_str_obj(string part, object ob) { rejected++; return "not there"; }
                int direct_look_at_obj(object ob) { return 1; }
                void move(object dest) { move_object(dest); }
                int count() { return rejected; }
            "##),
            ("/verb.c", r##"
                int rejected, acted;
                void create() {
                    parse_init();
                    parse_add_rule("look", "STR OBJ");
                    parse_add_rule("look", "STR");
                    parse_add_rule("look", "OBJ");
                    parse_add_rule("look", "at OBJ");
                    parse_add_rule("look", "at STR");
                }
                mixed can_look_str(string text) { rejected++; return "not possible"; }
                void do_look_at_obj(object ob) { acted++; }
                int can_look_at_str(string text) { throw("tried past success"); }
                int refusals() { return rejected; }
                int actions() { return acted; }
            "##),
        ],
        r##"
            "/item"->move(this_object());
            mixed parsed = parse_sentence("look at sword");
            int dispatched = command("look at sword");
            return ({ parsed, dispatched, "/verb"->actions(), "/verb"->refusals(),
                "/item"->count(), heard });
        "##,
        vec![1.into(), 1.into(), 2.into(), 2.into(), 2.into(), s("")],
    ).await;
}

#[tokio::test]
async fn a_failure_on_one_owner_does_not_hide_another_owners_success() {
    check(
        r##"string parser_error_message() { throw("premature error hook"); }"##,
        &[
            (
                "/first.c",
                r##"
                void create() { parse_init(); parse_add_rule("look", "STR"); }
                string can_look_str(string text) { return "first owner refused"; }
            "##,
            ),
            (
                "/second.c",
                r##"
                int acted;
                void create() { parse_init(); parse_add_rule("look", "STR"); }
                void do_look_str(string text) { acted++; }
                int count() { return acted; }
            "##,
            ),
        ],
        r##"
            mixed parsed = parse_sentence("look around");
            int dispatched = command("look around");
            return ({ parsed, dispatched, "/second"->count(), heard });
        "##,
        vec![1.into(), 1.into(), 2.into(), s("")],
    )
    .await;
}

#[tokio::test]
async fn the_first_success_wins_even_when_a_later_rule_is_more_specific() {
    check(
        "",
        &[(
            "/verb.c",
            r##"
            int broad, specific;
            void create() {
                parse_init();
                parse_add_rule("look", "STR");
                parse_add_rule("look", "around");
            }
            void do_look_str(string text) { broad++; }
            void do_look_around() { specific++; }
            int broad_count() { return broad; }
            int specific_count() { return specific; }
        "##,
        )],
        r##"
            mixed parsed = parse_sentence("look around");
            int dispatched = command("look around");
            return ({ parsed, dispatched, "/verb"->broad_count(), "/verb"->specific_count() });
        "##,
        vec![1.into(), 1.into(), 2.into(), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn the_first_message_wins_only_after_every_rule_has_failed() {
    check(
        r##"
            int reports;
            string parser_error_message(int kind, object ob, mixed arg, int flag) {
                reports++;
                if ("/first"->count() != "/second"->count()) throw("rules still pending");
                if (arg != "first") throw("reported past first message");
                return arg;
            }
            int count() { return reports; }
        "##,
        &[
            (
                "/first.c",
                r##"
                int tried;
                void create() { parse_init(); parse_add_rule("look", "STR"); }
                string can_look_str(string text) { tried++; return "first"; }
                int count() { return tried; }
            "##,
            ),
            (
                "/second.c",
                r##"
                int tried;
                void create() { parse_init(); parse_add_rule("look", "STR"); }
                string can_look_str(string text) { tried++; return "second"; }
                int count() { return tried; }
            "##,
            ),
        ],
        r##"
            mixed parsed = parse_sentence("look around");
            int dispatched = command("look around");
            return ({ parsed, dispatched, heard, "/secure/master"->count(), "/second"->count() });
        "##,
        vec![s("first"), 1.into(), s("first"), 2.into(), 2.into()],
    )
    .await;
}

#[tokio::test]
async fn silent_reports_yield_to_later_messages_and_plain_refusals_skip_the_hook() {
    check(
        r##"
            string reports = "";
            string parser_error_message(int kind, object ob, mixed arg, int flag) {
                reports += arg;
                if (arg == "second") return "message";
                return nil;
            }
            string seen() { return reports; }
        "##,
        &[(
            "/verb.c",
            r##"
            void create() {
                parse_init();
                parse_add_rule("look", "STR");
                parse_add_rule("look", "elsewhere");
                parse_add_rule("look", "WRD");
                parse_add_rule("look", "around");
            }
            int can_look_str(string text) { return 0; }
            string can_look_wrd(string text) { return "#first"; }
            string can_look_around() { return "second"; }
        "##,
        )],
        r##"
            mixed parsed = parse_sentence("look around");
            int dispatched = command("look around");
            return ({ parsed, dispatched, heard, "/secure/master"->seen() });
        "##,
        vec![
            s("message"),
            1.into(),
            s("message"),
            s("firstsecondfirstsecond"),
        ],
    )
    .await;
}

#[tokio::test]
async fn silent_failures_keep_severity_and_dispatch_fallback() {
    check(
        r##"
            int reports, fallbacks;
            string parser_error_message(int kind, object ob, mixed arg, int flag) { reports++; return nil; }
            string command_not_found(object actor, string line) { fallbacks++; return "fallback"; }
            int count() { return reports; }
            int fallback_count() { return fallbacks; }
        "##,
        &[("/verb.c", r##"
            void create() {
                parse_init();
                parse_add_rule("look", "OBJ");
                parse_add_rule("look", "STR");
                parse_add_rule("look", "elsewhere");
                parse_add_rule("wait", "STR");
                parse_add_rule("wait", "elsewhere");
            }
            string can_look_str(string text) { return "refused"; }
            string can_wait_str(string text) { return "refused"; }
        "##)],
        r##"
            mixed unresolved = parse_sentence("look nowhere");
            mixed refused = parse_sentence("wait here");
            mixed no_parse = parse_sentence("look");
            mixed no_verb = parse_sentence("unknown");
            int dispatched = command("look nowhere");
            return ({ unresolved, refused, no_parse, no_verb, dispatched, heard,
                "/secure/master"->count(), "/secure/master"->fallback_count() });
        "##,
        vec![(-3).into(), (-2).into(), (-1).into(), 0.into(), 0.into(), s("fallback"), 5.into(), 1.into()],
    ).await;
}

#[tokio::test]
async fn deferred_ambiguity_keeps_its_candidates_when_a_later_rule_changes_scope() {
    check(
        r##"
            int valid_destruct() { return 1; }
            string parser_error_message(int kind, object ob, mixed arg, int flag) {
                if (kind != 4 || ob || flag || sizeof(arg) != 2) throw("wrong ambiguity");
                if (arg[0] != find_object("/first") || arg[1]) throw("wrong candidates");
                return "ambiguous";
            }
        "##,
        &[
            (
                "/first.c",
                r##"string *parse_command_id_list() { return ({ "sword" }); }
                int direct_look_obj(object ob) { return 1; }"##,
            ),
            (
                "/second.c",
                r##"string *parse_command_id_list() { return ({ "sword" }); }
                int direct_look_obj(object ob) { return 1; }"##,
            ),
            (
                "/verb.c",
                r##"
                void create() {
                    parse_init();
                    parse_add_rule("look", "OBJ");
                    parse_add_rule("look", "STR");
                }
                int can_look_str(string text) { destruct(find_object("/second")); return 0; }
            "##,
            ),
        ],
        r##"
            mixed result = parse_sentence("look sword", 0,
                ({ find_object("/first"), find_object("/second") }));
            return ({ result, !find_object("/second"), heard });
        "##,
        vec![s("ambiguous"), 1.into(), s("")],
    )
    .await;
}

#[tokio::test]
async fn an_error_in_a_later_rule_propagates_before_diagnostics() {
    check(
        r##"string parser_error_message() { throw("premature error hook"); }"##,
        &[(
            "/verb.c",
            r##"
            void create() {
                parse_init();
                parse_add_rule("look", "STR");
                parse_add_rule("look", "around");
            }
            string can_look_str(string text) { return "refused"; }
            int can_look_around() { throw("handler boom"); }
        "##,
        )],
        r##"
            mixed parsed = catch(parse_sentence("look around"));
            mixed dispatched = catch(command("look around"));
            return ({ strindex(parsed, "handler boom") >= 0,
                strindex(dispatched, "handler boom") >= 0, !query_verb(), heard });
        "##,
        vec![1.into(), 1.into(), 1.into(), s("")],
    )
    .await;
}

#[tokio::test]
async fn an_error_hook_exception_propagates_after_every_rule_fails() {
    check(
        r##"
            string parser_error_message() {
                if (!"/verb"->tried_last()) throw("rules still pending");
                throw("diagnostic boom");
            }
        "##,
        &[(
            "/verb.c",
            r##"
            int last;
            void create() {
                parse_init();
                parse_add_rule("look", "STR");
                parse_add_rule("look", "around");
            }
            string can_look_str(string text) { last = 0; return "refused"; }
            int can_look_around() { last = 1; return 0; }
            int tried_last() { return last; }
        "##,
        )],
        r##"
            mixed parsed = catch(parse_sentence("look around"));
            mixed dispatched = catch(command("look around"));
            return ({ strindex(parsed, "diagnostic boom") >= 0,
                strindex(dispatched, "diagnostic boom") >= 0, !query_verb(), heard });
        "##,
        vec![1.into(), 1.into(), 1.into(), s("")],
    )
    .await;
}

#[tokio::test]
async fn deferred_hooks_keep_actor_and_synonym_context_across_nested_parsing() {
    check(
        r##"
            string parser_error_message(int kind, object ob, mixed arg, int flag) {
                if (this_player() != find_object("/main") || previous_object() != this_player())
                    throw("wrong actor");
                if (origin() != "driver" || query_verb() != "x") throw("wrong entry context");
                if (kind != 6 || ob != find_object("/verb") || arg != "refused" || flag)
                    throw("wrong refusal");
                if (parse_sentence("ping") != 1 || query_verb() != "x") throw("lost outer verb");
                return "message";
            }
        "##,
        &[(
            "/verb.c",
            r##"
            int pings;
            void create() {
                parse_init();
                parse_add_rule("look", "STR");
                parse_add_rule("look", "around");
                parse_add_synonym("x", "look");
                parse_add_rule("ping", "");
            }
            string can_look_str(string text) { return "refused"; }
            int can_look_around() { return 0; }
            void do_ping() { pings++; }
            int count() { return pings; }
        "##,
        )],
        r##"
            mixed parsed = parse_sentence("x around");
            int dispatched = command("x around");
            return ({ parsed, dispatched, heard, "/verb"->count(), !query_verb() });
        "##,
        vec![s("message"), 1.into(), s("message"), 2.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn a_later_many_rule_can_succeed_after_single_object_ambiguity() {
    let item = r#"
        string *parse_command_id_list() { return ({ "sword" }); }
        int direct_look_obj(mixed ob) { return 1; }
        void move(object dest) { move_object(dest); }
    "#;
    check(
        r#"string parser_error_message() { throw("premature ambiguity report"); }"#,
        &[
            ("/first.c", item),
            ("/second.c", item),
            (
                "/verb.c",
                r#"
                int actions;
                void create() {
                    parse_init();
                    parse_add_rule("look", "OBJ");
                    parse_add_rule("look", "OBS");
                }
                void do_look_obj(object ob) { throw("ambiguous single slot accepted"); }
                void do_look_obs(mixed *obs) { actions++; }
                int count() { return actions; }
            "#,
            ),
        ],
        r#"
            "/first"->move(this_object());
            "/second"->move(this_object());
            mixed parsed = parse_sentence("look sword");
            int dispatched = command("look sword");
            return ({ parsed, dispatched, "/verb"->count(), heard });
        "#,
        vec![1.into(), 1.into(), 2.into(), s("")],
    )
    .await;
}

#[tokio::test]
async fn long_repeated_separators_retry_refused_splits_in_order() {
    let words: Vec<_> = (0..17).map(|i| format!("w{i}")).collect();
    let input = words.join(" at ");
    let target = words[1..].join(" at ");
    let prefixes = (1..words.len())
        .rev()
        .map(|end| words[..end].join(" at "))
        .collect::<Vec<_>>()
        .join("|");
    let item = format!(
        r#"
        string *parse_command_id_list() {{ return ({{ "{target}" }}); }}
        int direct_cast_str_at_obj() {{ return 1; }}
        void move(object dest) {{ move_object(dest); }}
    "#
    );
    let verb = format!(
        r#"
        string *seen = ({{}});
        int acted;
        void create() {{ parse_init(); parse_add_rule("cast", "STR at OBJ"); }}
        int can_cast_str_at_obj(string spell) {{
            seen += ({{ spell }});
            return spell == "w0";
        }}
        void do_cast_str_at_obj(string spell, object ob, string raw_spell, string raw_target) {{
            if (spell != "w0" || raw_spell != spell || raw_target != "{target}" ||
                ob != find_object("/item") || query_verb() != "cast" ||
                query_command() != "cast {input}") throw("wrong successful split");
            acted++;
        }}
        string trace() {{ return implode(seen, "|"); }}
        int actions() {{ return acted; }}
    "#
    );
    check(
        r#"string parser_error_message() { throw("premature error hook"); }"#,
        &[("/item.c", &item), ("/verb.c", &verb)],
        &format!(
            r#"
            "/item"->move(this_object());
            mixed parsed = parse_sentence("cast {input}");
            int dispatched = command("cast {input}");
            return ({{ parsed, dispatched, "/verb"->actions(),
                "/verb"->trace() == "{prefixes}|{prefixes}", heard }});
        "#
        ),
        vec![1.into(), 1.into(), 2.into(), 1.into(), s("")],
    )
    .await;
}
