//! The parser package, end to end: registration, the handler protocol,
//! parse_sentence's codes, and the master's messages.

use indoc::indoc;

use super::{fails, run, s};
use crate::interpreter::lpc_ref::LpcRef;

const VERB: (&str, &str) = (
    "/verbs/look.c",
    indoc! { r#"
        void create() {
            parse_init();
            parse_add_rule("look", "");
            parse_add_rule("look", "at OBJ");
            parse_add_rule("look", "at OBS with OBJ");
            parse_add_synonym("examine", "look", "at OBJ");
        }
        void drop() { parse_remove("look"); }
        string *rules() { return parse_my_rules(); }
    "# },
);

#[tokio::test]
async fn my_rules_lists_verb_and_rule_in_registration_order() {
    // `mixed`, not `mixed *`: a call_other's static type is always the
    // scalar `mixed` wildcard, which `mixed *` never matches.
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed create() { return "/verbs/look"->rules(); }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            s("look "),
            s("look at OBJ"),
            s("look at OBS with OBJ"),
            s("examine at OBJ")
        ]
    );
}

#[tokio::test]
async fn parse_dump_lists_every_rule_with_its_owner() {
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed *create() { return ({ parse_dump() }); }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![s(
            "look   (/verbs/look)\nlook at OBJ  (/verbs/look)\nlook at OBS with OBJ  (/verbs/look)\nexamine at OBJ  (/verbs/look)\n"
        )]
    );
}

#[tokio::test]
async fn parse_remove_drops_a_verb_and_its_synonyms() {
    // `mixed`, not `mixed *`: see the previous test.
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed create() { "/verbs/look"->drop(); return "/verbs/look"->rules(); }
    "# },
    )
    .await;
    assert_eq!(r, Vec::<LpcRef>::new());
}

#[tokio::test]
async fn add_rule_without_init_and_bad_rules_are_errors() {
    assert!(
        fails(
            "",
            &[],
            r#"mixed *create() { parse_add_rule("x", "OBJ"); return ({}); }"#
        )
        .await
        .contains("parse_add_rule: parse_init() has not been called")
    );
    assert!(
        fails(
            "",
            &[],
            r#"mixed *create() { parse_init(); parse_add_rule("x", "STR STR"); return ({}); }"#
        )
        .await
        .contains("parse_add_rule: two STR tokens in 'STR STR'")
    );
    assert!(
        fails(
            "",
            &[],
            r#"mixed *create() { parse_init(); parse_add_synonym("y", "x"); return ({}); }"#
        )
        .await
        .contains("parse_add_synonym: this_object() has no rules for 'x'")
    );
}

#[tokio::test]
async fn parse_refresh_is_accepted() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { parse_refresh(); return ({ 1 }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1)]);
}

// The parser package's protocol, end to end: dispatch trying a verb
// object's rules after the actor's own, and `parse_sentence` running the
// same rules directly. An English vocabulary and messages sit in the
// fixtures below as test data only.

const MASTER: &str = indoc! { r#"
    string *parse_command_id_list() { return ({}); }
    string *parse_command_adjectiv_id_list() { return ({ "the", "a" }); }
    string parse_command_all_word() { return "all"; }
    int parse_command_numeral(string w) {
        if (w == "first") return -1; if (w == "second") return -2; if (w == "third") return -3; if (w == "two") return 2; return 0;
    }
    string *parse_command_pluralize(string *s) { string *p = ({}); int i; for (i = 0; i < sizeof(s); i++) p += ({ s[i] + "s" }); return p; }
    object *parse_command_users() { return ({ find_object("/bob") }); }
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        switch (kind) {
            case 4: return "ambiguous " + sizeof(arg);
            case 5: return "only " + arg;
            case 6: return "reason " + arg;
            case 7: return "no " + arg;
            case 8: return "one at a time";
            case 3: return "cannot reach " + arg;
            case 2: return "not alive " + arg;
        }
        return 0;
    }
"# };

const SWORD: (&str, &str) = (
    "/sword.c",
    indoc! { r##"
    string *parse_command_id_list() { return ({ "sword" }); }
    string *parse_command_adjectiv_id_list() { return ({ "red" }); }
    mixed direct_take_obj(object ob, string w) { return 1; }
    mixed direct_kick_obj(object ob, string w) { return "#too heavy"; }
    mixed direct_give_obj_to_liv(object what, object whom, string a, string b) { return 1; }
    void go(object d) { move_object(d); }
"## },
);
const SWORD2: (&str, &str) = (
    "/sword2.c",
    indoc! { r##"
    string *parse_command_id_list() { return ({ "sword" }); }
    string *parse_command_adjectiv_id_list() { return ({ "red" }); }
    mixed direct_take_obj(object ob, string w) { return 1; }
    mixed direct_kick_obj(object ob, string w) { return "#too heavy"; }
    void go(object d) { move_object(d); }
"## },
);
const KNIFE: (&str, &str) = (
    "/knife.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "knife", "sword" }); }
    mixed direct_take_obj(object ob, string w) { return 0; }
    mixed direct_kick_obj(object ob, string w) { return "not that one"; }
    void go(object d) { move_object(d); }
"# },
);
const BOB: (&str, &str) = (
    "/bob.c",
    indoc! { r#"
    void create() { enable_commands(); }
    string *parse_command_id_list() { return ({ "bob" }); }
    mixed indirect_give_obj_to_liv(object what, object whom, string a, string b) { return 1; }
    void go(object d) { move_object(d); }
"# },
);
const VERBS: (&str, &str) = (
    "/verbs.c",
    indoc! { r#"
    string log;
    string typed_verb;
    void create() {
        parse_init();
        parse_add_rule("take", "OBJ");
        parse_add_rule("take", "OBS");
        parse_add_rule("kick", "OBJ");
        parse_add_rule("give", "OBJ to LIV");
        parse_add_rule("look", "");
        parse_add_rule("say", "STR");
        parse_add_rule("fail", "OBJ");
        parse_add_synonym("get", "take");
    }
    mixed can_take_obj(object o, string w) { return 1; }
    void do_take_obj(object o, string w) { log = "took " + w + " " + file_name(o); typed_verb = query_verb(); }
    void do_take_obs(mixed *obs, string w) { log = "took " + sizeof(obs) + " " + w; }
    mixed can_kick_obj(object o, string w) { return 1; }
    void do_kick_obj(object o, string w) { throw("kick handler error"); }
    void do_give_obj_to_liv(object o, object l, string a, string b) { log = "gave " + a + " to " + b + " " + query_verb(); }
    void do_look() { log = "looked"; }
    mixed can_say_str(string s) { if (s == "nothing") return "say something"; return 1; }
    void do_say_str(string s) { log = "said " + s; }
    mixed can_fail_obj(object o, string w) { return 0; }
    string query_log() { return log; }
    string query_typed_verb() { return typed_verb; }
"# },
);
const ROOM: (&str, &str) = ("/room.c", "");

/// A player in `/room` with the sword, the knife and bob beside it.
const PLAYER_SETUP: &str = indoc! { r#"
    object room = find_object("/room");
    move_object(room);
    "/sword"->go(room); "/knife"->go(room); "/bob"->go(room);
"# };

/// `/main.c`: `enable_commands()`, `set_this_player()`, a `catch_tell`
/// capturing the last message into `heard`, then `body`.
fn custom_main(body: &str) -> String {
    format!(
        r#"
        string heard;
        void catch_tell(string m) {{ heard = m; }}
        mixed *create() {{
            enable_commands();
            set_this_player(this_object());
{body}
        }}
        "#
    )
}

/// [`custom_main`] with [`PLAYER_SETUP`] (the sword, the knife and bob
/// beside the player in `/room`) before `body`.
fn main_c(body: &str) -> String {
    custom_main(&format!("{PLAYER_SETUP}\n{body}"))
}

#[tokio::test]
async fn a_parser_rule_handles_a_typed_line() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int r = command("take red sword");
            return ({ r, "/verbs"->query_log() });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), s("took red sword /sword")]);
}

#[tokio::test]
async fn direct_filters_choose_the_qualifying_object() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            command("take sword");
            return ({ "/verbs"->query_log() });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![s("took sword /sword")]);
}

#[tokio::test]
async fn two_qualifiers_are_ambiguous_and_the_master_says_so() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        "/sword2"->go(room);
        "/knife"->go(room);
        int r = command("take sword");
        return ({ r, heard });
    "#;
    let r = run(
        MASTER,
        &[SWORD, SWORD2, KNIFE, VERBS, ROOM],
        &custom_main(body),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), s("ambiguous 2")]);
}

#[tokio::test]
async fn an_ordinal_picks_and_past_the_end_reports_the_count() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        "/sword2"->go(room);
        int r1 = command("take second sword");
        string picked = "/verbs"->query_log();
        int r2 = command("take third sword");
        return ({ r1, picked, r2, heard });
    "#;
    let r = run(MASTER, &[SWORD, SWORD2, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("took second sword /sword2"),
            LpcRef::from(1),
            s("only 2")
        ]
    );
}

const CRATE_A: (&str, &str) = (
    "/crate_a.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "crate" }); }
    mixed direct_kick_obj(object ob, string w) { return 1; }
    void go(object d) { move_object(d); }
"# },
);
const CRATE_B: (&str, &str) = (
    "/crate_b.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "crate" }); }
    mixed direct_kick_obj(object ob, string w) { return 1; }
    void go(object d) { move_object(d); }
"# },
);

#[tokio::test]
async fn all_on_a_single_slot_is_bad_multiple_and_on_obs_takes_them() {
    // "kick" registers only a single-object rule; "all" on it is bad
    // multiple, and the master's message reports it.
    let kick_body = r#"
        object room = find_object("/room");
        move_object(room);
        "/crate_a"->go(room);
        "/crate_b"->go(room);
        int r = command("kick all crates");
        return ({ r, heard });
    "#;
    let kick = run(
        MASTER,
        &[VERBS, CRATE_A, CRATE_B, ROOM],
        &custom_main(kick_body),
    )
    .await;
    assert_eq!(kick, vec![LpcRef::from(1), s("one at a time")]);

    // "take" registers a single-object rule and a many-object rule; without
    // a master apply to describe the single slot's bad-multiple failure, it
    // falls through silently to the many-object rule, which takes them all.
    let half_master = indoc! { r#"
        string parse_command_all_word() { return "all"; }
        string *parse_command_pluralize(string *s) { string *p = ({}); int i; for (i = 0; i < sizeof(s); i++) p += ({ s[i] + "s" }); return p; }
    "# };
    let take_body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        "/sword2"->go(room);
        command("take all swords");
        return ({ "/verbs"->query_log() });
    "#;
    let take = run(
        half_master,
        &[SWORD, SWORD2, VERBS, ROOM],
        &custom_main(take_body),
    )
    .await;
    assert_eq!(take, vec![s("took 2 all swords")]);
}

#[tokio::test]
async fn a_plain_reason_beats_a_soft_one() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int r = command("kick sword");
            return ({ r, heard });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), s("reason not that one")]);
}

#[tokio::test]
async fn a_soft_reason_is_reported_when_alone() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        int r = command("kick sword");
        return ({ r, heard });
    "#;
    let r = run(MASTER, &[SWORD, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![LpcRef::from(1), s("reason too heavy")]);
}

#[tokio::test]
async fn can_refusing_is_minus_two_and_a_can_reason_is_reported() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int fail = parse_sentence("fail sword");
            int say = parse_sentence("say nothing");
            return ({ fail, say });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(-2), s("reason say something")]);
}

#[tokio::test]
async fn liv_resolves_through_parse_command_users() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        "/bob"->go(find_object("/elsewhere"));
        int r = command("give sword to bob");
        return ({ r, "/verbs"->query_log() });
    "#;
    let elsewhere = ("/elsewhere.c", "");
    let r = run(
        MASTER,
        &[SWORD, BOB, VERBS, ROOM, elsewhere],
        &custom_main(body),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), s("gave sword to bob give")]);
}

#[tokio::test]
async fn a_synonym_reports_the_typed_verb() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int r = command("get sword");
            return ({ r, "/verbs"->query_typed_verb() });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), s("get")]);
}

const BOX: (&str, &str) = (
    "/box.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "box" }); }
    int inventory_visible() { return 1; }
    int inventory_accessible() { return 0; }
    void go(object d) { move_object(d); }
"# },
);
const GEM: (&str, &str) = (
    "/gem.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "gem" }); }
    mixed direct_take_obj(object ob, string w) { return 1; }
    void go(object d) { move_object(d); }
"# },
);

#[tokio::test]
async fn a_closed_container_is_visible_but_not_reachable() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/box"->go(room);
        "/gem"->go(find_object("/box"));
        int r = command("take gem");
        return ({ r, heard });
    "#;
    let r = run(MASTER, &[BOX, GEM, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![LpcRef::from(1), s("cannot reach gem")]);
}

#[tokio::test]
async fn nicknames_name_objects_in_scope() {
    // "it" is not one of `/sword`'s own ids; only the nickname names it, and
    // only because `/sword` is already in the actor's scope.
    let r = run(
        "",
        &[SWORD, VERBS],
        r#"mixed *create() {
            enable_commands();
            set_this_player(this_object());
            "/sword"->go(this_object());
            int r = parse_sentence("take it", 0, 0, ([ "it": find_object("/sword") ]));
            return ({ r });
        }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1)]);
}

#[tokio::test]
async fn parse_sentence_codes() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int dance = parse_sentence("dance");
            int take = parse_sentence("take");
            int fail = parse_sentence("fail sword");
            return ({ dance, take, fail });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(-1), LpcRef::from(-2)]);

    // No `parser_error_message`: an unresolved noun falls back to its silent
    // verdict instead of a message.
    let r = run(
        "",
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int r = parse_sentence("take nothing");
            return ({ r });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(-3)]);
}

#[tokio::test]
async fn the_dispatcher_falls_through_to_command_not_found_on_a_silent_refusal() {
    let master = indoc! { r#"
        string parser_error_message(int kind, object ob, mixed arg, int flag) { return 0; }
        string command_not_found(object who, string line) { return "what?"; }
    "# };
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        int r = command("fail sword");
        return ({ r, heard });
    "#;
    let r = run(master, &[SWORD, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![LpcRef::from(0), s("what?")]);
}

#[tokio::test]
async fn a_handler_error_propagates() {
    // A single crate qualifies for "kick", reaching `do_kick_obj`, which
    // throws.
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/crate_a"->go(room);
        command("kick crate");
        return ({});
    "#;
    let err = fails("", &[VERBS, CRATE_A, ROOM], &custom_main(body)).await;
    assert!(err.contains("kick handler error"), "{err}");
}

#[tokio::test]
async fn str_needs_at_least_one_word() {
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int r = parse_sentence("say");
            return ({ r });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(-1)]);
}

const CANDLE: (&str, &str) = (
    "/candle.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "candle" }); }
    int seen_count; int seen_is_self;
    mixed direct_grab_obj(mixed items, string w) {
        seen_count = arrayp(items) ? sizeof(items) : -1;
        seen_is_self = arrayp(items) && sizeof(items) > 0 && items[0] == this_object();
        return 1;
    }
    int query_seen_count() { return seen_count; }
    int query_seen_is_self() { return seen_is_self; }
    void go(object d) { move_object(d); }
"# },
);
const STUB_CANDLE: (&str, &str) = (
    "/stub_candle.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "candle" }); }
    mixed direct_grab_obj(mixed items, string w) { return "nope"; }
    void go(object d) { move_object(d); }
"# },
);
const GRABBER: (&str, &str) = (
    "/grabber.c",
    indoc! { r#"
    mixed *obs_log;
    void create() { parse_init(); parse_add_rule("grab", "OBS"); }
    void do_grab_obs(mixed *obs) { obs_log = obs; }
    int query_len() { return sizeof(obs_log); }
    int query_first_is_object() { return objectp(obs_log[0]); }
    int query_second_is_string() { return stringp(obs_log[1]); }
"# },
);

/// A many slot's re-ask (`direct_`/`indirect_`) sees an object-only array,
/// while `do_` sees the qualifying objects followed by the plain reason.
#[tokio::test]
async fn a_many_slot_shows_objects_only_before_do_and_the_reason_only_inside_do() {
    let r = run(
        "",
        &[CANDLE, STUB_CANDLE, GRABBER],
        indoc! { r#"
        mixed *create() {
            enable_commands();
            set_this_player(this_object());
            "/candle"->go(this_object());
            "/stub_candle"->go(this_object());
            command("grab candle");
            return ({
                "/candle"->query_seen_count(),
                "/candle"->query_seen_is_self(),
                "/grabber"->query_len(),
                "/grabber"->query_first_is_object(),
                "/grabber"->query_second_is_string()
            });
        }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(1),
            LpcRef::from(1)
        ]
    );
}
