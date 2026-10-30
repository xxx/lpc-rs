//! Pair permissions, selection, and action arguments in both null modes.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run_with, s};
use crate::interpreter::lpc_ref::LpcRef;

const MASTER: &str = r#"
    int reports;
    int valid_destruct() { return 1; }
    string parse_command_all_word() { return "all"; }
    int parse_command_numeral(string word) {
        if (word == "first") return -1;
        if (word == "second") return -2;
        if (word == "third") return -3;
        if (word == "two") return 2;
        return 0;
    }
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        reports++;
        if (kind == 4) return "ambiguous:" + sizeof(arg);
        if (kind == 5) return "ordinal:" + arg;
        if (kind == 6) return file_name(ob) + ":" + arg;
        return "kind:" + kind;
    }
    int count() { return reports; }
"#;

const ITEM: &str = r#"
    int calls;
    string *parse_command_id_list() { return ({ "sword" }); }
    string *parse_command_plural_id_list() { return ({ "swords" }); }
    void move(object dest) { move_object(dest); }
    int count() { return calls; }
"#;

const BOX: &str = r#"
    int calls;
    string *parse_command_id_list() { return ({ "box" }); }
    string *parse_command_plural_id_list() { return ({ "boxes" }); }
    int count() { return calls; }
    void wake() { enable_commands(); }
"#;

const DIRECT: &str = r#"
    int direct_get_obj_from_obj(object item, object box) {
        calls++;
        return environment(item) == box;
    }
"#;

const INDIRECT: &str = r#"
    int indirect_get_obj_from_obj(object item, object box) { calls++; return 1; }
"#;

async fn check(rule: &str, item: &str, container: &str, body: &str, expected: Vec<LpcRef>) {
    check_with(rule, item, container, "", body, expected).await;
}

async fn check_with(
    rule: &str,
    item: &str,
    container: &str,
    verb_extra: &str,
    body: &str,
    expected: Vec<LpcRef>,
) {
    let item = format!("{ITEM}\n{item}");
    let container = format!("{BOX}\n{container}");
    let verb = format!(
        r#"
        int acted;
        mixed first, second;
        string log = "";
        void create() {{ parse_init(); parse_add_rule("get", "{rule}"); }}
        void do_verb_rule(string verb, string rule, mixed args...) {{
            acted++; first = args[0]; second = args[1];
        }}
        int count() {{ return acted; }}
        mixed one() {{ return first; }}
        mixed two() {{ return second; }}
        void note(string entry) {{ log += entry; }}
        string seen() {{ return log; }}
        {verb_extra}
    "#
    );
    let main = format!(
        r#"
        mixed *create() {{
            enable_commands();
            set_this_player(this_object());
            object a = find_object("/a"), b = find_object("/b");
            object box = find_object("/box"), other_box = find_object("/other_box");
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
            run_with(
                config,
                MASTER,
                &[
                    ("/a.c", &item),
                    ("/b.c", &item),
                    ("/box.c", &container),
                    ("/other_box.c", &container),
                    ("/verb.c", &verb),
                ],
                &main
            )
            .await,
            expected,
            "{mode:?}, {rule}"
        );
    }
}

#[tokio::test]
async fn the_container_disambiguates_the_item_before_selection() {
    check(
        "OBJ from OBJ",
        DIRECT,
        INDIRECT,
        r#"
        a->move(other_box); b->move(box);
        mixed result = parse_sentence("get sword from box", 0, ({ a, b, box }));
        return ({ result, "/verb"->one() == b, "/verb"->two() == box,
            a->count(), b->count(), box->count(), "/secure/master"->count() });
    "#,
        vec![
            1.into(),
            1.into(),
            1.into(),
            1.into(),
            2.into(),
            2.into(),
            0.into(),
        ],
    )
    .await;
}

#[tokio::test]
async fn the_item_disambiguates_the_container_before_selection() {
    check(
        "OBJ from OBJ",
        DIRECT,
        INDIRECT,
        r#"
        a->move(box);
        mixed result = parse_sentence("get sword from box", 0, ({ a, other_box, box }));
        return ({ result, "/verb"->two() == box, a->count(),
            other_box->count(), box->count() });
    "#,
        vec![1.into(), 1.into(), 3.into(), 0.into(), 2.into()],
    )
    .await;
}

#[tokio::test]
async fn multiple_compatible_singular_targets_still_report_ambiguity() {
    check(
        "OBJ from OBJ",
        DIRECT,
        INDIRECT,
        r#"
        a->move(box); b->move(box);
        return ({ parse_sentence("get sword from box", 0, ({ a, b, box })),
            "/verb"->count() });
    "#,
        vec![s("ambiguous:2"), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn plural_counts_are_applied_after_relation_filtering() {
    check(
        "OBS from OBJ",
        DIRECT,
        INDIRECT,
        r#"
        a->move(other_box); b->move(box);
        mixed one = parse_sentence("get sword from box", 0, ({ a, b, box }));
        mixed two = parse_sentence("get two swords from box", 0, ({ a, b, box }));
        mixed all = parse_sentence("get all swords from box", 0, ({ a, b, box }));
        mixed *items = "/verb"->one();
        return ({ one, two, all, sizeof(items), items[0] == b,
            "/verb"->two() == box, "/secure/master"->count() });
    "#,
        vec![
            1.into(),
            1.into(),
            1.into(),
            1.into(),
            1.into(),
            1.into(),
            0.into(),
        ],
    )
    .await;
}

#[tokio::test]
async fn the_reverse_plural_orientation_receives_scalar_permissions_and_an_action_array() {
    check(
        "OBJ from OBS",
        r#"
        int direct_get_obj_from_obj(object item, object box) { return 1; }
    "#,
        r#"
        mixed indirect_get_obj_from_obj(object item, object box) {
            if (file_name(box) == "/box") return "locked";
            return 1;
        }
    "#,
        r#"
        mixed result = parse_sentence("get sword from all boxes", 0, ({ a, box, other_box }));
        mixed *boxes = "/verb"->two();
        return ({ result, "/verb"->one() == a, sizeof(boxes),
            boxes[0] == other_box, boxes[1], "/secure/master"->count() });
    "#,
        vec![
            1.into(),
            1.into(),
            2.into(),
            1.into(),
            s("locked"),
            0.into(),
        ],
    )
    .await;
}

#[tokio::test]
async fn a_plural_slot_does_not_choose_between_compatible_singular_partners() {
    check(
        "OBS from OBJ",
        DIRECT,
        INDIRECT,
        r#"
        a->move(box); b->move(other_box);
        return ({ parse_sentence("get sword from box", 0, ({ a, b, box, other_box })),
            "/verb"->count() });
    "#,
        vec![s("ambiguous:2"), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn direct_ordinals_pin_targets_before_permissions_including_for_plural_rules() {
    for rule in ["OBJ from OBJ", "OBS from OBJ"] {
        check(
            rule,
            DIRECT,
            INDIRECT,
            r#"
            a->move(other_box); b->move(box);
            mixed first = parse_sentence("get first sword from box", 0, ({ a, b, box }));
            mixed second = parse_sentence("get second sword from box", 0, ({ a, b, box }));
            mixed third = parse_sentence("get third sword from box", 0, ({ a, b, box }));
            mixed item = "/verb"->one();
            if (arrayp(item)) item = item[0];
            return ({ first, second, third, item == b, "/verb"->count() });
        "#,
            vec![(-2).into(), 1.into(), s("ordinal:2"), 1.into(), 1.into()],
        )
        .await;
    }
}

#[tokio::test]
async fn indirect_ordinals_do_not_substitute_a_compatible_container() {
    for rule in ["OBJ from OBJ", "OBJ from OBS"] {
        check(
            rule,
            DIRECT,
            INDIRECT,
            r#"
            a->move(box);
            mixed second = parse_sentence("get sword from second box", 0, ({ a, box, other_box }));
            mixed first = parse_sentence("get sword from first box", 0, ({ a, box, other_box }));
            mixed third = parse_sentence("get sword from third box", 0, ({ a, box, other_box }));
            mixed container = "/verb"->two();
            if (arrayp(container)) container = container[0];
            return ({ second, first, third, container == box, "/verb"->count() });
        "#,
            vec![(-2).into(), 1.into(), s("ordinal:2"), 1.into(), 1.into()],
        )
        .await;
    }
}

#[tokio::test]
async fn ordinal_positions_include_permission_rejections() {
    check(
        "OBJ from OBJ",
        r#"
        mixed direct_get_obj_from_obj(object item, object box) {
            if (file_name(item) == "/a") return "denied";
            return 1;
        }
    "#,
        INDIRECT,
        r#"
        mixed first = parse_sentence("get first sword from box", 0, ({ a, b, box }));
        mixed second = parse_sentence("get second sword from box", 0, ({ a, b, box }));
        return ({ first, second, "/verb"->one() == b });
    "#,
        vec![s("/a:denied"), 1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn plain_reasons_beat_soft_reasons_with_the_refusing_object_retained() {
    check(
        "OBS from OBJ",
        r##"
        mixed direct_get_obj_from_obj(object item, object box) {
            if (file_name(item) == "/a") return "#not mine";
            return "locked";
        }
    "##,
        INDIRECT,
        r#"
        return ({ parse_sentence("get all swords from box", 0, ({ a, b, box })) });
    "#,
        vec![s("/b:locked")],
    )
    .await;
}

#[tokio::test]
async fn plural_reasons_only_describe_the_selected_partner() {
    check(
        "OBS from OBJ",
        r##"
        mixed direct_get_obj_from_obj(object item, object box) {
            if (file_name(box) == "/other_box") return "wrong source";
            if (file_name(item) == "/b") return "#not mine";
            return 1;
        }
    "##,
        INDIRECT,
        r#"
        mixed result = parse_sentence("get all swords from box", 0, ({ a, b, other_box, box }));
        mixed *items = "/verb"->one();
        return ({ result, sizeof(items), items[0] == a, "/verb"->two() == box });
    "#,
        vec![1.into(), 1.into(), 1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn a_late_direct_refusal_preserves_other_plural_members() {
    check(
        "OBS from OBJ",
        r#"
        mixed direct_get_obj_from_obj(object item, object box) {
            if (++calls == 2 && file_name(item) == "/a") return "too heavy";
            return 1;
        }
    "#,
        INDIRECT,
        r#"
        mixed result = parse_sentence("get all swords from box", 0, ({ a, b, box }));
        mixed *items = "/verb"->one();
        return ({ result, sizeof(items), items[0] == b, items[1],
            box->count(), "/secure/master"->count() });
    "#,
        vec![
            1.into(),
            2.into(),
            1.into(),
            s("too heavy"),
            3.into(),
            0.into(),
        ],
    )
    .await;
}

#[tokio::test]
async fn a_late_indirect_refusal_preserves_other_plural_members() {
    check(
        "OBJ from OBS",
        r#"
        int direct_get_obj_from_obj(object item, object box) { return 1; }
    "#,
        r#"
        mixed indirect_get_obj_from_obj(object item, object box) {
            if (++calls == 2 && file_name(box) == "/box") return "closed";
            return 1;
        }
    "#,
        r#"
        mixed result = parse_sentence("get sword from all boxes", 0, ({ a, box, other_box }));
        mixed *boxes = "/verb"->two();
        return ({ result, sizeof(boxes), boxes[0] == other_box, boxes[1],
            "/secure/master"->count() });
    "#,
        vec![1.into(), 2.into(), 1.into(), s("closed"), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn a_late_refusal_does_not_replace_an_already_selected_count() {
    check(
        "OBS from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) { return ++calls == 1; }
    "#,
        INDIRECT,
        r#"
        mixed result = parse_sentence("get sword from box", 0, ({ a, b, box }));
        return ({ result, a->count(), b->count(), "/verb"->count() });
    "#,
        vec![(-2).into(), 2.into(), 1.into(), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn a_partner_change_during_search_is_seen_by_the_final_recheck() {
    check(
        "OBS from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) {
            if (file_name(item) == "/b") "/a"->move(find_object("/other_box"));
            return environment(item) == box;
        }
    "#,
        INDIRECT,
        r#"
        a->move(box); b->move(box);
        mixed result = parse_sentence("get all swords from box", 0, ({ a, b, box }));
        mixed *items = "/verb"->one();
        return ({ result, sizeof(items), items[0] == b });
    "#,
        vec![1.into(), 1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn a_destructed_partner_is_never_called_and_cannot_reach_the_action() {
    check(
        "OBJ from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) { destruct(box); return 1; }
    "#,
        r#"
        int indirect_get_obj_from_obj(object item, object box) { throw("called dead partner"); }
    "#,
        r#"
        mixed result = parse_sentence("get sword from box", 0, ({ a, box }));
        return ({ result, !find_object("/box"), "/verb"->count() });
    "#,
        vec![(-2).into(), 1.into(), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn destruction_during_the_last_recheck_removes_an_earlier_accepted_member() {
    check(
        "OBS from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) { return 1; }
    "#,
        r#"
        int indirect_get_obj_from_obj(object item, object box) {
            if (++calls == 4) destruct(find_object("/a"));
            return 1;
        }
    "#,
        r#"
        mixed result = parse_sentence("get all swords from box", 0, ({ a, b, box }));
        mixed *items = "/verb"->one();
        return ({ result, !a, sizeof(items), items[0] == b });
    "#,
        vec![1.into(), 1.into(), 1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn a_permission_can_destruct_an_object_that_has_not_been_checked_yet() {
    check(
        "OBS from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) {
            if (file_name(item) == "/b") throw("called destructed neighbour");
            object neighbour = find_object("/b");
            if (neighbour) destruct(neighbour);
            return 1;
        }
    "#,
        INDIRECT,
        r#"
        mixed result = parse_sentence("get all swords from box", 0, ({ a, b, box }));
        mixed *items = "/verb"->one();
        return ({ result, !b, sizeof(items), items[0] == a });
    "#,
        vec![1.into(), 1.into(), 1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn pair_calls_preserve_capture_layout_generic_fallback_and_actor_context() {
    check_with(
        "WRD OBJ from OBJ STR",
        r#"
        int direct_verb_rule(string verb, string rule, string style, object item,
            object box, string tail, string typed_verb, string style_words, string item_words, string box_words, string tail_words) {
            if (verb != "get" || rule != "WRD OBJ from OBJ STR" || style != "carefully"
                || tail != "now" || item_words != "blade" || box_words != "box"
                || typed_verb != "take" || style_words != "carefully" || tail_words != "now")
                throw("wrong direct arguments");
            if (this_player() != find_object("/main") || previous_object() != this_player()
                || origin() != "driver" || query_verb() != "take") throw("wrong context");
            "/verb"->note("D" + file_name(item) + file_name(box) + ";");
            return environment(item) == box;
        }
    "#,
        r#"
        int indirect_verb_rule(string verb, string rule, string style, object item,
            object box, string tail, string typed_verb, string style_words, string item_words, string box_words, string tail_words) {
            if (verb != "get" || item_words != "blade" || box_words != "box"
                || typed_verb != "take" || style_words != "carefully" || tail_words != "now")
                throw("wrong indirect arguments");
            "/verb"->note("I" + file_name(item) + file_name(box) + ";");
            return 1;
        }
    "#,
        r#"
        void synonyms() { parse_add_synonym("take", "get"); }
        int can_verb_rule(string verb, string rule, string style, object item,
            object box, string tail, string typed_verb, string style_words, string item_words, string box_words, string tail_words) {
            if (item || box || style != "carefully" || tail != "now"
                || item_words != "blade" || box_words != "box"
                || typed_verb != "take" || style_words != "carefully" || tail_words != "now") throw("wrong can arguments");
            log += "C;";
            return 1;
        }
        void do_get_wrd_obj_from_obj_str(string style, object item, object box,
            string tail, string style_words, string item_words, string box_words, string tail_words) {
            first = item; second = box; acted++;
            log += "A;";
        }
    "#,
        r#"
        a->move(box);
        "/verb"->synonyms();
        mixed result = parse_sentence("take carefully blade from box now", 0,
            ({ a, other_box, box }), ([ "blade": a ]));
        return ({ result, "/verb"->one() == a, "/verb"->two() == box, "/verb"->seen() });
    "#,
        vec![
            1.into(),
            1.into(),
            1.into(),
            s("C;D/a/other_box;D/a/box;I/a/box;D/a/box;I/a/box;A;"),
        ],
    )
    .await;
}

#[tokio::test]
async fn an_exception_stops_pair_search_and_propagates_without_error_reporting() {
    check(
        "OBS from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) {
            "/verb"->note(file_name(item));
            throw("permission failed");
        }
    "#,
        INDIRECT,
        r#"
        mixed error = catch(parse_sentence("get all swords from box", 0, ({ a, b, box })));
        return ({ stringp(error), strindex(error, "permission failed") >= 0,
            "/verb"->seen(), box->count(), "/verb"->count(), "/secure/master"->count() });
    "#,
        vec![1.into(), 1.into(), s("/a"), 0.into(), 0.into(), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn a_later_rule_can_succeed_after_all_pairs_are_refused() {
    check_with(
        "OBJ from OBJ",
        r#"
        string direct_get_obj_from_obj(object item, object box) { return "refused"; }
    "#,
        INDIRECT,
        r#"
        void fallback() { parse_add_rule("get", "STR"); }
        void do_get_str(string line) { acted++; }
    "#,
        r#"
        "/verb"->fallback();
        mixed result = parse_sentence("get sword from box", 0, ({ a, box }));
        return ({ result, "/verb"->count(), "/secure/master"->count() });
    "#,
        vec![1.into(), 1.into(), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn two_plural_slots_keep_the_existing_aggregate_protocol() {
    check("OBS from OBS", r#"
        int direct_get_obj_from_obj(mixed items, mixed boxes) {
            if (++calls == 1) {
                if (items != this_object() || boxes) throw("wrong initial direct arguments");
            } else if (!arrayp(items) || !arrayp(boxes)) throw("wrong aggregate recheck");
            return 1;
        }
    "#, r#"
        int indirect_get_obj_from_obj(mixed items, mixed boxes) {
            if (!arrayp(items)) throw("missing selected direct array");
            if (++calls == 1) {
                if (boxes != this_object()) throw("wrong initial indirect argument");
            } else if (!arrayp(boxes)) throw("wrong aggregate recheck");
            return 1;
        }
    "#, r#"
        mixed result = parse_sentence("get all swords from all boxes", 0, ({ a, b, box, other_box }));
        mixed *items = "/verb"->one(), *boxes = "/verb"->two();
        return ({ result, sizeof(items), sizeof(boxes), items[0] == a, items[1] == b,
            boxes[0] == box, boxes[1] == other_box });
    "#, vec![1.into(), 2.into(), 2.into(), 1.into(), 1.into(), 1.into(), 1.into()]).await;
}

#[tokio::test]
async fn missing_permissions_on_both_objects_and_owner_exclude_pairs() {
    check_with(
        "OBJ from OBJ",
        "",
        INDIRECT,
        "",
        r#"
        return ({ parse_sentence("get sword from box", 0, ({ a, box })), box->count(),
            "/verb"->count() });
    "#,
        vec![(-2).into(), 0.into(), 0.into()],
    )
    .await;
}

#[tokio::test]
async fn living_slots_use_pair_permissions_with_both_plural_orientations() {
    for rule in ["OBS from LIV", "OBJ from LVS"] {
        check(
            rule,
            r#"
            int direct_get_obj_from_liv(object item, object box) {
                return environment(item) == box;
            }
        "#,
            r#"
            int indirect_get_obj_from_liv(object item, object box) { return 1; }
        "#,
            r#"
            a->move(box);
            mixed nonliving = parse_sentence("get sword from box", 0, ({ a, box }));
            box->wake();
            mixed result = parse_sentence("get sword from box", 0, ({ a, box }));
            mixed item = "/verb"->one(), container = "/verb"->two();
            if (arrayp(item)) item = item[0];
            if (arrayp(container)) container = container[0];
            return ({ nonliving, result, item == a, container == box });
        "#,
            vec![s("kind:2"), 1.into(), 1.into(), 1.into()],
        )
        .await;
    }
}

#[tokio::test]
async fn unreachable_objects_are_excluded_before_ordinal_selection() {
    check(
        "OBJ from OBJ",
        r#"
        int direct_get_obj_from_obj(object item, object box) { calls++; return 1; }
    "#,
        r#"
        int inventory_accessible() { return 0; }
        int indirect_get_obj_from_obj(object item, object box) { calls++; return 1; }
        void move(object dest) { move_object(dest); }
    "#,
        r#"
        box->move(this_object()); a->move(box); b->move(this_object());
        mixed second = parse_sentence("get second sword from box");
        mixed first = parse_sentence("get first sword from box");
        return ({ second, first, "/verb"->one() == b, a->count() });
    "#,
        vec![s("ordinal:1"), 1.into(), 1.into(), 0.into()],
    )
    .await;
}
