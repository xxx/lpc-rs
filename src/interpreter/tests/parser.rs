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
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed *create() { return "/verbs/look"->rules(); }
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
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed *create() { "/verbs/look"->drop(); return "/verbs/look"->rules(); }
    "# },
    )
    .await;
    assert_eq!(r, Vec::<LpcRef>::new());
}

/// `give` with a synonym, a synonym of that synonym, and a second synonym
/// of the base registered after the first.
const CHAIN: (&str, &str) = (
    "/verbs/give.c",
    indoc! { r#"
        void create() {
            parse_init();
            parse_add_rule("give", "OBJ to LIV");
            parse_add_synonym("g", "give");
            parse_add_synonym("gv", "g");
            parse_add_synonym("x", "give");
        }
        void drop() { parse_remove("give"); }
        string *rules() { return parse_my_rules(); }
    "# },
);

#[tokio::test]
async fn a_synonym_is_minted_once_per_base_rule_not_once_per_sibling() {
    // `x` is a synonym of `give` registered when `g` and `gv` already
    // exist; matching on a rule's own verb mints one `x` rule, not three.
    let r = run(
        "",
        &[CHAIN],
        indoc! { r#"
        mixed *create() { return "/verbs/give"->rules(); }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            s("give OBJ to LIV"),
            s("g OBJ to LIV"),
            s("gv OBJ to LIV"),
            s("x OBJ to LIV")
        ]
    );
}

#[tokio::test]
async fn parse_remove_of_the_base_drops_a_synonym_of_a_synonym() {
    let r = run(
        "",
        &[CHAIN],
        indoc! { r#"
        mixed *create() { "/verbs/give"->drop(); return "/verbs/give"->rules(); }
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
        parse_add_synonym("g", "give");
        parse_add_synonym("gv", "g");
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

#[tokio::test]
async fn ordinals_count_the_actors_own_things_before_the_rooms() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword2"->go(room);
        "/sword"->go(this_object());
        int r1 = command("take first sword");
        string first = "/verbs"->query_log();
        int r2 = command("take second sword");
        string second = "/verbs"->query_log();
        return ({ r1, first, r2, second });
    "#;
    let r = run(MASTER, &[SWORD, SWORD2, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("took first sword /sword"),
            LpcRef::from(1),
            s("took second sword /sword2")
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
async fn a_living_slot_given_a_thing_is_not_alive() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        "/bob"->go(room);
        return ({ parse_sentence("give sword to sword") });
    "#;
    let r = run(MASTER, &[SWORD, BOB, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![s("not alive sword")]);
}

const LOG_SWORD: (&str, &str) = (
    "/log_sword.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "sword" }); }
    string *log = ({});
    string describe(mixed v) {
        if (v == 0) return "0";
        if (v == this_object()) return "self";
        return "other";
    }
    mixed direct_give_obj_to_liv(mixed a, mixed b, string x, string y) {
        log += ({ describe(a) + " " + describe(b) + " " + x + " " + y });
        return 1;
    }
    string *query_log() { return log; }
    void go(object d) { move_object(d); }
"# },
);
const LOG_BOB: (&str, &str) = (
    "/log_bob.c",
    indoc! { r#"
    void create() { enable_commands(); }
    string *parse_command_id_list() { return ({ "bob" }); }
    string *log = ({});
    string describe(mixed v) {
        if (v == 0) return "0";
        if (v == this_object()) return "self";
        return "other";
    }
    mixed indirect_give_obj_to_liv(mixed a, mixed b, string x, string y) {
        log += ({ describe(a) + " " + describe(b) + " " + x + " " + y });
        return 1;
    }
    string *query_log() { return log; }
    void go(object d) { move_object(d); }
"# },
);

#[tokio::test]
async fn a_candidates_own_slot_holds_itself_while_filtered_and_reverts_to_zero() {
    // Q7's sequence for `give OBJ to LIV`: filtering the direct slot puts
    // the sword candidate in its own (still-empty) slot; filtering the
    // indirect slot puts bob in its, with the direct slot already chosen;
    // the all-filled re-ask hands both handlers every object.
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/log_sword"->go(room);
        "/log_bob"->go(room);
        int r = command("give sword to bob");
        mixed *sword_log = "/log_sword"->query_log();
        mixed *bob_log = "/log_bob"->query_log();
        return ({
            r,
            sword_log[0], sword_log[1],
            bob_log[0], bob_log[1],
            "/verbs"->query_log()
        });
    "#;
    let r = run(
        MASTER,
        &[LOG_SWORD, LOG_BOB, VERBS, ROOM],
        &custom_main(body),
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("self 0 sword bob"),
            s("self other sword bob"),
            s("other self sword bob"),
            s("other self sword bob"),
            s("gave sword to bob give"),
        ]
    );
}

const OBS_FILTER_CRATE: (&str, &str) = (
    "/obs_filter_crate.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "crate" }); }
    int seen_object; int recorded;
    mixed direct_stash_obj(mixed a, string w) {
        // Only the filter-time call (before the all-filled re-ask, which
        // hands a many slot the object array) is of interest here.
        if (!recorded) {
            seen_object = objectp(a);
            recorded = 1;
        }
        return 1;
    }
    int query_seen_object() { return seen_object; }
    void go(object d) { move_object(d); }
"# },
);
/// A verb whose `OBS` slot's chosen array is reported to it by each
/// candidate the re-ask asks.
const ARRAY_VERB: (&str, &str) = (
    "/array_verb.c",
    indoc! { r#"
    mixed *seen; int notes; int same;
    void create() { parse_init(); parse_add_rule("stack", "OBS"); }
    void note(mixed *obs) {
        notes = notes + 1;
        if (notes == 1) seen = obs; else same = (obs == seen);
    }
    void do_stack_obs(mixed *obs, string w) { }
    int query_notes() { return notes; }
    int query_same() { return same; }
"# },
);
const ARRAY_CRATE_A: (&str, &str) = (
    "/array_crate_a.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "crate" }); }
    mixed direct_stack_obj(mixed a, string w) {
        if (!objectp(a)) "/array_verb"->note(a);
        return 1;
    }
    void go(object d) { move_object(d); }
"# },
);
const ARRAY_CRATE_B: (&str, &str) = (
    "/array_crate_b.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "crate" }); }
    mixed direct_stack_obj(mixed a, string w) {
        if (!objectp(a)) "/array_verb"->note(a);
        return 1;
    }
    void go(object d) { move_object(d); }
"# },
);

#[tokio::test]
async fn a_many_slot_hands_the_re_ask_the_same_array() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/array_crate_a"->go(room);
        "/array_crate_b"->go(room);
        command("stack all crates");
        return ({ "/array_verb"->query_notes(), "/array_verb"->query_same() });
    "#;
    let r = run(
        MASTER,
        &[ARRAY_VERB, ARRAY_CRATE_A, ARRAY_CRATE_B, ROOM],
        &custom_main(body),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(2), LpcRef::from(1)]);
}

const STASH_VERB: (&str, &str) = (
    "/stash_verb.c",
    indoc! { r#"
    void create() { parse_init(); parse_add_rule("stash", "OBS"); }
    void do_stash_obs(mixed *obs) { }
"# },
);

#[tokio::test]
async fn a_many_slot_filter_call_gets_a_bare_object_not_an_array() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/obs_filter_crate"->go(room);
        int r = command("stash crate");
        return ({ r, "/obs_filter_crate"->query_seen_object() });
    "#;
    let r = run(
        "",
        &[OBS_FILTER_CRATE, STASH_VERB, ROOM],
        &custom_main(body),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(1)]);
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

#[tokio::test]
async fn a_synonym_of_a_synonym_matches_the_typed_verb_not_the_base() {
    // `VERBS` registers `"g"` as a synonym of `"give"`, then `"gv"` as a
    // synonym of `"g"`; the second `parse_add_synonym` call must match `"g"`
    // against the sibling `Rule`'s own verb, not `give`'s `ParserRule.verb`.
    let r = run(
        MASTER,
        &[SWORD, KNIFE, BOB, VERBS, ROOM],
        &main_c(
            r#"
            int r = command("gv sword to bob");
            return ({ r, "/verbs"->query_log() });
        "#,
        ),
    )
    .await;
    // `do_give_obj_to_liv` logs `query_verb()`; it reports "gv", not "give".
    assert_eq!(r, vec![LpcRef::from(1), s("gave sword to bob gv")]);
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

const APPLE: (&str, &str) = (
    "/apple.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "apple" }); }
    mixed direct_take_obj(object ob, string w) { return 1; }
"# },
);
const PEAR: (&str, &str) = (
    "/pear.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "pear" }); }
    mixed direct_take_obj(object ob, string w) { return 1; }
"# },
);

#[tokio::test]
async fn a_nested_scope_array_flattens_and_skips_non_objects() {
    let dead = ("/dead_thing.c", "");
    let r = run(
        "",
        &[APPLE, PEAR, VERBS, dead],
        r#"mixed *create() {
            enable_commands();
            set_this_player(this_object());
            object apple = find_object("/apple");
            object pear = find_object("/pear");
            object gone = clone_object("/dead_thing");
            destruct(gone);
            mixed *scope = ({ apple, ({ pear, gone, "not an object" }) });
            int top = parse_sentence("take apple", 0, scope);
            int nested = parse_sentence("take pear", 0, scope);
            return ({ top, nested });
        }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(1)]);
}

#[tokio::test]
async fn a_non_array_scope_is_an_error() {
    let err = fails(
        "",
        &[],
        r#"mixed *create() {
            enable_commands();
            set_this_player(this_object());
            mixed bad = "not an array";
            parse_sentence("take it", 0, bad);
            return ({});
        }"#,
    )
    .await;
    assert!(
        err.contains("parse_sentence: the scope must be an array of objects"),
        "{err}"
    );
}

#[tokio::test]
async fn a_self_referential_scope_is_bounded() {
    let err = fails(
        "",
        &[],
        r#"mixed *create() {
            enable_commands();
            set_this_player(this_object());
            mixed *a = ({ 0 });
            a[0] = a;
            parse_sentence("take it", 0, a);
            return ({});
        }"#,
    )
    .await;
    assert!(
        err.contains("parse_sentence: the scope nests deeper than 20"),
        "{err}"
    );
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

#[tokio::test]
async fn a_noun_less_parser_rule_asks_the_master_no_vocabulary() {
    let master = indoc! { r#"
        int calls;
        string *parse_command_id_list() { calls++; return ({}); }
        int query_calls() { return calls; }
        string parser_error_message(int kind, object ob, mixed arg, int flag) { return 0; }
    "# };
    let verb = (
        "/say.c",
        indoc! { r#"
        string log;
        void create() { parse_init(); parse_add_rule("say", "STR"); }
        mixed can_say_str(string s) { return 1; }
        void do_say_str(string s) { log = s; }
        string query_log() { return log; }
    "# },
    );
    let r = run(
        master,
        &[verb],
        &custom_main(
            r#"
            int r = command("say hello there");
            return ({ r, "/say"->query_log(), "/secure/master"->query_calls() });
        "#,
        ),
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), s("hello there"), LpcRef::from(0)]);
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

// Q8's unpinned behaviours: ordering against the actor's own rules, a
// destructed verb object's rules disappearing from both dispatch and
// `parse_dump`, the all-filled re-ask refusing after filtering qualified,
// and the generic fallback handlers' `verb, rule` prefix.

#[tokio::test]
async fn the_actors_own_rule_beats_a_parser_rule_for_the_same_verb() {
    let main = indoc! { r#"
        mixed *create() {
            enable_commands();
            set_this_player(this_object());
            object room = find_object("/room");
            move_object(room);
            "/sword"->go(room);
            add_action("do_take", "take");
            int r = command("take sword");
            return ({ r, "/verbs"->query_log() });
        }
        int do_take(string arg) { return 1; }
    "# };
    let r = run(MASTER, &[SWORD, VERBS, ROOM], main).await;
    // The actor's own `do_take` handled it; the parser rule's `do_take_obj`
    // never ran, so `/verbs`' log stays at its unset default.
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(0)]);
}

#[tokio::test]
async fn a_zero_from_the_actors_own_rule_falls_through_to_the_parser_rule() {
    let main = indoc! { r#"
        mixed *create() {
            enable_commands();
            set_this_player(this_object());
            object room = find_object("/room");
            move_object(room);
            "/sword"->go(room);
            add_action("do_take", "take");
            int r = command("take sword");
            return ({ r, "/verbs"->query_log() });
        }
        int do_take(string arg) { return 0; }
    "# };
    let r = run(MASTER, &[SWORD, VERBS, ROOM], main).await;
    assert_eq!(r, vec![LpcRef::from(1), s("took sword /sword")]);
}

#[tokio::test]
async fn a_destructed_verb_objects_rules_vanish_from_dispatch_and_the_dump() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/sword"->go(room);
        destruct(find_object("/verbs"));
        int commanded = command("take sword");
        int sentenced = parse_sentence("take sword");
        string dump = parse_dump();
        return ({ commanded, sentenced, dump });
    "#;
    let r = run(MASTER, &[SWORD, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![LpcRef::from(0), LpcRef::from(0), s("")]);
}

#[tokio::test]
async fn the_all_filled_reask_can_still_refuse_after_filtering_qualified() {
    let refuses = (
        "/reask_refuses.c",
        indoc! { r#"
        string *parse_command_id_list() { return ({ "sword" }); }
        mixed direct_give_obj_to_liv(mixed a, mixed b, string x, string y) {
            return b == 0 ? 1 : 0;
        }
        void go(object d) { move_object(d); }
    "# },
    );
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/reask_refuses"->go(room);
        "/bob"->go(room);
        int r = parse_sentence("give sword to bob");
        return ({ r });
    "#;
    let r = run(MASTER, &[refuses, BOB, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![LpcRef::from(-2)]);
}

#[tokio::test]
async fn the_all_filled_reask_can_still_be_a_reason_after_filtering_qualified() {
    let reasons = (
        "/reask_reasons.c",
        indoc! { r#"
        string *parse_command_id_list() { return ({ "sword" }); }
        mixed direct_give_obj_to_liv(mixed a, mixed b, string x, string y) {
            if (b == 0) return 1;
            return "not now";
        }
        void go(object d) { move_object(d); }
    "# },
    );
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/reask_reasons"->go(room);
        "/bob"->go(room);
        int r = parse_sentence("give sword to bob");
        return ({ r });
    "#;
    let r = run(MASTER, &[reasons, BOB, VERBS, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![s("reason not now")]);
}

const GENERIC_VERB: (&str, &str) = (
    "/generic_verb.c",
    indoc! { r#"
    string seen_verb; string seen_rule;
    void create() { parse_init(); parse_add_rule("poke", "OBJ"); }
    mixed can_verb_rule(string verb, string rule, mixed o, string w) {
        seen_verb = verb; seen_rule = rule;
        return 1;
    }
    void do_verb_rule(string verb, string rule, mixed o, string w) { }
    string query_seen_verb() { return seen_verb; }
    string query_seen_rule() { return seen_rule; }
"# },
);
const GENERIC_TARGET: (&str, &str) = (
    "/generic_target.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "thing" }); }
    string seen_verb; string seen_rule;
    mixed direct_verb_rule(string verb, string rule, mixed o, string w) {
        seen_verb = verb; seen_rule = rule;
        return 1;
    }
    string query_seen_verb() { return seen_verb; }
    string query_seen_rule() { return seen_rule; }
    void go(object d) { move_object(d); }
"# },
);

#[tokio::test]
async fn generic_fallback_handlers_receive_the_verb_and_rule_prefix() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/generic_target"->go(room);
        int r = command("poke thing");
        return ({
            r,
            "/generic_verb"->query_seen_verb(), "/generic_verb"->query_seen_rule(),
            "/generic_target"->query_seen_verb(), "/generic_target"->query_seen_rule()
        });
    "#;
    let r = run(
        "",
        &[GENERIC_VERB, GENERIC_TARGET, ROOM],
        &custom_main(body),
    )
    .await;
    assert_eq!(
        r,
        vec![LpcRef::from(1), s("poke"), s("OBJ"), s("poke"), s("OBJ"),]
    );
}

const QUOTED_VERB: (&str, &str) = (
    "/quoted_verb.c",
    indoc! { r#"
    string seen_rule;
    void create() { parse_init(); parse_add_rule("look", "at bob's OBJ"); }
    mixed can_verb_rule(string verb, string rule, mixed o, string w) { return 1; }
    void do_verb_rule(string verb, string rule, mixed o, string w) { seen_rule = rule + " " + file_name(o); }
    string query_seen_rule() { return seen_rule; }
"# },
);

#[tokio::test]
async fn a_literal_with_a_quote_reaches_the_generic_handlers() {
    let body = r#"
        object room = find_object("/room");
        move_object(room);
        "/generic_target"->go(room);
        int r = command("look at bob's thing");
        return ({ r, "/quoted_verb"->query_seen_rule() });
    "#;
    let r = run("", &[QUOTED_VERB, GENERIC_TARGET, ROOM], &custom_main(body)).await;
    assert_eq!(r, vec![LpcRef::from(1), s("at bob's OBJ /generic_target")]);
}
