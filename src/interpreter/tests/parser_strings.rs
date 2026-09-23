//! Multiple text captures, handler arguments and split retries in both entry paths.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::run_with;
use crate::interpreter::lpc_ref::LpcRef;

const MASTER: &str = r#"
    int reports; mixed last;
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        reports++; last=arg; return "error:"+kind;
    }
    int reports() { return reports; }
    mixed last() { return last; }
"#;
const ITEM: &str = r#"
    string *parse_command_id_list() { return ({"gem"}); }
    string *parse_command_plural_id_list() { return ({"gems"}); }
    void move(object dest) { move_object(dest); }
"#;
const BOX: &str = r#"
    string *parse_command_id_list() { return ({"box"}); }
    void move(object dest) { move_object(dest); }
"#;
const RECORD: &str = r#"
    mixed *seen=({}); int actions;
    void do_verb_rule(string base, string rule, mixed args...) {
        actions++; seen=({base,rule,args});
    }
    mixed *seen() { return seen; }
    int actions() { return actions; }
"#;

async fn check(objects: &[(&str, &str)], extra: &str, body: &str) {
    for entry in ["parse_sentence", "command"] {
        let main = format!(
            r#"
            {extra}
            mixed submit(string text) {{ return {entry}(text); }}
            mixed *create() {{
                enable_commands(); set_this_player(this_object());
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
                run_with(config, MASTER, objects, &main).await,
                vec![LpcRef::from(1)],
                "{entry} {mode:?}"
            );
        }
    }
}

async fn mixed_layout(tier: usize) {
    let rule = "STR from OBJ  about STR to OBJ:v";
    let names = [
        "cast_str_from_obj_about_str_to_obj",
        "cast_str_word_obj_word_str_word_obj",
        "verb_str_word_obj_word_str_word_obj",
        "verb_rule",
    ];
    let mut sources = [String::new(), ITEM.to_owned(), BOX.to_owned()];
    for source in &mut sources {
        source.push_str("int calls; int calls() { return calls; }");
    }
    for (family, target) in [("can", 0), ("direct", 1), ("indirect", 2), ("do", 0)] {
        let mut args: Vec<&str> = Vec::new();
        if tier >= 2 {
            args.push("\"cast\"");
        }
        let registered_rule = format!("\"{rule}\"");
        if tier == 3 {
            args.push(&registered_rule);
        }
        let first = if family == "can" {
            "nil"
        } else {
            "find_object(\"/item\")"
        };
        let second = if family == "can" {
            "nil"
        } else {
            "find_object(\"/box\")"
        };
        if tier == 1 || tier == 2 {
            args.extend([
                "\"fire ball\"",
                "\"from\"",
                first,
                "\"about\"",
                "\"far away\"",
                "\"to\"",
                second,
            ]);
        } else {
            args.extend(["\"fire ball\"", first, "\"far away\"", second]);
        }
        if tier >= 2 {
            args.push("\"cast  now\"");
        }
        args.extend(["\"fire ball\"", "\"gem\"", "\"far away\"", "\"box\""]);
        let mut conditions = vec![format!("sizeof(args)=={}", args.len())];
        conditions.extend(
            args.iter()
                .enumerate()
                .map(|(i, value)| format!("args[{i}]=={value}")),
        );
        sources[target].push_str(&format!(
            r#"
            int {family}_{name}(mixed args...) {{
                if (!({conditions})) throw("{family} layout");
                if (this_player()!=find_object("/main") || previous_object()!=this_player()
                    || origin()!="driver" || query_verb()!="cast now"
                    || query_verb(1)!="cast  now") throw("{family} context");
                calls++; return 1;
            }}
        "#,
            name = names[tier],
            conditions = conditions.join(" && ")
        ));
        for name in &names[tier + 1..] {
            sources[target].push_str(&format!(
                "int {family}_{name}() {{ throw(\"wrong tier\"); return 0; }}"
            ));
        }
    }
    sources[0].push_str(&format!(r#"
        void create() {{ parse_init(); parse_add_rule("cast","{rule}"); parse_add_synonym("cast now","cast"); }}
    "#));
    check(
        &[
            ("/verb.c", &sources[0]),
            ("/item.c", &sources[1]),
            ("/box.c", &sources[2]),
        ],
        "",
        r#"
        "/item"->move(this_object()); "/box"->move(this_object());
        return ({submit("cast  now fire ball from gem about far away to box")==1
            && "/verb"->calls()==2 && "/item"->calls()==2 && "/box"->calls()==2
            && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn multiple_strings_keep_specific_handler_and_object_permission_positions() {
    mixed_layout(0).await;
}

#[tokio::test]
async fn multiple_strings_keep_literal_word_handler_positions() {
    mixed_layout(1).await;
}

#[tokio::test]
async fn multiple_strings_keep_generic_shape_handler_positions() {
    mixed_layout(2).await;
}

#[tokio::test]
async fn multiple_strings_keep_generic_rule_handler_positions() {
    mixed_layout(3).await;
}

#[tokio::test]
async fn text_only_rules_call_no_object_permissions_and_duplicate_source_strings() {
    let verb = r#"
        int correct;
        void create() { parse_init(); parse_add_rule("cast","STR at STR"); }
        int can_cast_str_at_str(string left,string right,string raw_left,string raw_right) {
            if(left!="spark at dusk" || right!="tower" || raw_left!=left || raw_right!=right)
                throw("can arguments");
            return 1;
        }
        int direct_verb_rule() { throw("unexpected direct"); return 0; }
        int indirect_verb_rule() { throw("unexpected indirect"); return 0; }
        void do_cast_str_at_str(string left,string right,string raw_left,string raw_right) {
            correct=left=="spark at dusk" && right=="tower" && raw_left==left && raw_right==right;
        }
        int correct() { return correct; }
    "#;
    check(
        &[("/verb.c", verb)],
        "",
        r#"
        return ({submit("cast spark at dusk at tower")==1 && "/verb"->correct()});
    "#,
    )
    .await;
}

#[tokio::test]
async fn adjacent_three_and_four_strings_reach_handlers() {
    for (rule, input, condition) in [
        (
            "STR STR STR",
            "a b c d",
            r#"a[0]=="a b" && a[1]=="c" && a[2]=="d" && a[4]==a[0] && a[5]==a[1] && a[6]==a[2] && sizeof(a)==7"#,
        ),
        (
            "STR STR STR STR",
            "a b c d",
            r#"a[0]=="a" && a[1]=="b" && a[2]=="c" && a[3]=="d" && a[5]==a[0] && a[6]==a[1] && a[7]==a[2] && a[8]==a[3] && sizeof(a)==9"#,
        ),
    ] {
        let verb = format!(
            r#"void create() {{ parse_init(); parse_add_rule("cast","{rule}"); }} {RECORD}"#
        );
        check(
            &[("/verb.c", &verb)],
            "",
            &format!(
                r#"
            if(submit("cast {input}")!=1) return ({{0}});
            mixed *a="/verb"->seen()[2]; return ({{{condition}}});
        "#
            ),
        )
        .await;
    }
}

#[tokio::test]
async fn unicode_source_strings_keep_internal_whitespace_and_equal_slots() {
    let verb = format!(
        r#"void create() {{ parse_init(); parse_add_rule("cast","STR at STR"); }} {RECORD}"#
    );
    check(&[("/verb.c",&verb)], "", "
        if(submit(\"cast Éclair!\\t bleu at 遠い\u{2003}塔\")!=1) return ({0});
        mixed *a=\"/verb\"->seen()[2];
        if(a[0]!=\"Éclair!\\t bleu\" || a[1]!=\"遠い\u{2003}塔\" || a[3]!=a[0] || a[4]!=a[1]) return ({0});
        if(submit(\"cast same at same\")!=1) return ({0});
        a=\"/verb\"->seen()[2];
        return ({sizeof(a)==5 && a[0]==\"same\" && a[1]==a[0] && a[3]==a[0] && a[4]==a[0]});
    ").await;
}

#[tokio::test]
async fn refused_splits_keep_effects_and_retry_before_a_later_rule() {
    let verb = format!(
        r#"
        string *tried=({{}});
        void create() {{ parse_init(); parse_add_rule("cast","STR at STR"); parse_add_rule("cast","STR"); }}
        mixed can_cast_str_at_str(string a,string b) {{ tried+=({{a}}); return a=="spark" ? 1 : "first refusal"; }}
        int can_cast_str() {{ throw("tried later rule"); return 0; }}
        string trace() {{ return implode(tried,"|"); }}
        {RECORD}
    "#
    );
    check(
        &[("/verb.c", &verb)],
        "",
        r#"
        if(submit("cast spark at dusk at tower")!=1) return ({0});
        mixed *a="/verb"->seen()[2];
        return ({"/verb"->trace()=="spark at dusk|spark" && a[0]=="spark" && a[1]=="dusk at tower"
            && "/verb"->actions()==1 && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn exhausting_string_splits_reports_the_first_tied_reason_once() {
    let verb = r#"
        int calls;
        void create() { parse_init(); parse_add_rule("cast","STR at STR"); }
        string can_cast_str_at_str(string left) { calls++; return left; }
        int calls() { return calls; }
    "#;
    check(
        &[("/verb.c", verb)],
        "",
        r#"
        submit("cast spark at dusk at tower");
        return ({"/verb"->calls()==2 && "/secure/master"->reports()==1
            && "/secure/master"->last()=="spark at dusk"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn a_string_permission_exception_stops_split_and_rule_retries() {
    let verb = r#"
        int calls;
        void create() { parse_init(); parse_add_rule("cast","STR at STR"); parse_add_rule("cast","STR"); }
        int can_cast_str_at_str() { calls++; throw("stop splits"); return 0; }
        int can_cast_str() { throw("unexpected later rule"); return 0; }
        int calls() { return calls; }
    "#;
    check(
        &[("/verb.c", verb)],
        "",
        r#"
        mixed error=catch(submit("cast spark at dusk at tower"));
        return ({stringp(error) && strindex(error,"stop splits")>=0
            && "/verb"->calls()==1 && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn a_later_rule_can_succeed_after_every_string_split_refuses() {
    let verb = format!(
        r#"
        int calls;
        void create() {{ parse_init(); parse_add_rule("cast","STR at STR"); parse_add_rule("cast","STR"); }}
        string can_cast_str_at_str() {{ calls++; return "refused"; }}
        int calls() {{ return calls; }}
        {RECORD}
    "#
    );
    check(
        &[("/verb.c", &verb)],
        "",
        r#"
        if(submit("cast spark at dusk at tower")!=1) return ({0});
        mixed *seen="/verb"->seen();
        return ({"/verb"->calls()==2 && seen[1]=="STR" && seen[2][0]=="spark at dusk at tower"
            && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn an_earlier_successful_catchall_keeps_precedence_over_multiple_strings() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("cast","STR"); parse_add_rule("cast","STR at STR"); }}
        int can_cast_str_at_str() {{ throw("tried past success"); return 0; }}
        {RECORD}
    "#
    );
    check(
        &[("/verb.c", &verb)],
        "",
        r#"
        return ({submit("cast spark at tower")==1 && "/verb"->seen()[1]=="STR"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn long_string_rules_stop_at_the_existing_parse_count_cap() {
    let words: Vec<_> = (0..41).map(|i| format!("w{i}")).collect();
    let input = words.join(" at ");
    let verb = r#"
        int calls;
        void create() { parse_init(); parse_add_rule("cast","STR at STR"); }
        mixed can_cast_str_at_str(string left) { calls++; return left=="w0" ? 1 : "not this split"; }
        int calls() { return calls; }
        void do_cast_str_at_str() { throw("split past the cap"); }
    "#;
    check(
        &[("/verb.c", verb)],
        "",
        &format!(
            r#"
        submit("cast {input}"); return ({{"/verb"->calls()==32 && "/secure/master"->reports()==1}});
    "#
        ),
    )
    .await;
}

#[tokio::test]
async fn copied_string_rules_preserve_alias_arguments_and_remove_with_the_base() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("cast","STR at STR"); parse_add_synonym("aim at","cast","STR at STR"); }}
        string *rules() {{ return parse_my_rules(); }}
        void drop() {{ parse_remove("cast"); }}
        {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)], "", r#"
        if(submit("aim at spark at tower")!=1) return ({0});
        mixed *seen="/verb"->seen();
        if(seen[0]!="cast" || seen[1]!="STR at STR" || seen[2][2]!="aim at" || sizeof("/verb"->rules())!=2) return ({0});
        "/verb"->drop(); return ({sizeof("/verb"->rules())==0 && !command("aim at spark at tower")});
    "#).await;
}

#[tokio::test]
async fn invalid_object_and_text_modifiers_leave_registered_string_rules_intact() {
    check(
        &[],
        "",
        r#"
        parse_init(); parse_add_rule("cast","STR at STR");
        mixed objects=catch(parse_add_rule("cast","STR OBJ STR OBJ STR OBJ"));
        mixed modifier=catch(parse_add_rule("cast","STR STR:v"));
        return ({stringp(objects) && strindex(objects,"more than two object slots")>=0
            && stringp(modifier) && strindex(modifier,"unsupported parser modifier")>=0
            && sizeof(parse_my_rules())==1 && parse_my_rules()[0]=="cast STR at STR"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn strings_around_a_plural_slot_keep_permission_and_action_array_layouts() {
    let verb = r#"
        int checks, correct;
        void create() { parse_init(); parse_add_rule("cast","STR OBS STR"); }
        int can_cast_str_obj_str(string left, mixed items, string right) {
            return left=="fire" && right=="glow";
        }
        int direct_cast_str_obj_str(string left, mixed items, string right, string raw_left, string raw_items, string raw_right) {
            if(left!="fire" || right!="glow" || raw_left!=left || raw_right!=right || raw_items!="gems")
                throw("plural permission layout");
            checks++;
            return arrayp(items) ? sizeof(items)==2 : objectp(items);
        }
        int indirect_verb_rule() { throw("unexpected indirect slot"); return 0; }
        void do_cast_str_obs_str(string left, mixed *items, string right, string raw_left, string raw_items, string raw_right) {
            correct=left=="fire" && right=="glow" && raw_left==left && raw_right==right
                && raw_items=="gems" && sizeof(items)==2 && member_array(find_object("/one"),items)>=0
                && member_array(find_object("/two"),items)>=0;
        }
        int correct() { return correct && checks==4; }
    "#;
    check(
        &[("/verb.c", verb), ("/one.c", ITEM), ("/two.c", ITEM)],
        "",
        r#"
        "/one"->move(this_object()); "/two"->move(this_object());
        return ({submit("cast fire gems glow")==1 && "/verb"->correct()});
    "#,
    )
    .await;
}

#[tokio::test]
async fn the_ulib_text_example_retries_and_prints_the_accepted_captures() {
    let verb = include_str!("../../../ulib/examples/verb.c").replace("#if 0\n", "#if 1\n");
    check(
        &[("/verb.c", &verb)],
        r#"
        string heard="";
        void catch_tell(string message) { heard+=message; }
    "#,
        r#"
        return ({submit("cast spark at dusk at tower")==1
            && heard=="The grammar matched spark at dusk at tower.\n"
            && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}
