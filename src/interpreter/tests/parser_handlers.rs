//! Handler tiers, target fallback and source arguments in both null modes.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run_with, s};
use crate::interpreter::lpc_ref::LpcRef;

const MASTER: &str = r#"
    int reports;
    int valid_destruct() { return 1; }
    string parse_command_all_word() { return "all"; }
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        reports++;
        if(kind==6) return file_name(ob)+":"+arg;
        return "kind:"+kind;
    }
    int count() { return reports; }
"#;

const ITEM: &str = r#"
    string *parse_command_id_list() { return ({"book"}); }
    string *parse_command_plural_id_list() { return ({"books"}); }
    void move(object dest) { move_object(dest); }
"#;

const BOX: &str = r#"
    string *parse_command_id_list() { return ({"box"}); }
"#;

async fn check(objects: &[(&str, &str)], globals: &str, body: &str, expected: Vec<LpcRef>) {
    let main = format!(
        r#"
        {globals}
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
            expected,
            "{mode:?}"
        );
    }
}

async fn layouts(tier: usize) {
    let rule = "about STR  from OBJ to OBJ";
    let names = [
        "read_about_str_from_obj_to_obj",
        "read_word_str_word_obj_word_obj",
        "verb_word_str_word_obj_word_obj",
        "verb_rule",
    ];
    let mut sources = [String::new(), ITEM.to_owned(), BOX.to_owned()];
    for source in &mut sources {
        source.push_str("int calls; int count() { return calls; }");
    }
    for (family, target) in [("can", 0), ("direct", 1), ("indirect", 2), ("do", 0)] {
        let mut expected: Vec<String> = Vec::new();
        if tier >= 2 {
            expected.push("\"read\"".into());
        }
        if tier == 3 {
            expected.push(format!("\"{rule}\""));
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
            expected.extend(
                [
                    "\"about\"",
                    "\"ancient history\"",
                    "\"from\"",
                    first,
                    "\"to\"",
                    second,
                ]
                .map(str::to_owned),
            );
        } else {
            expected.extend(["\"ancient history\"", first, second].map(str::to_owned));
        }
        if tier >= 2 {
            expected.push("\"consult\"".into());
        }
        expected.extend(["\"ancient history\"", "\"book\"", "\"box\""].map(str::to_owned));
        let mut conditions = vec![format!("sizeof(args)=={}", expected.len())];
        conditions.extend(
            expected
                .iter()
                .enumerate()
                .map(|(i, value)| format!("args[{i}]=={value}")),
        );
        sources[target].push_str(&format!(
            r#"
            int {family}_{name}(mixed args...) {{
                if(!({conditions})) throw("{family} argument layout");
                if(this_player()!=find_object("/main") || previous_object()!=this_player()
                    || origin()!="driver" || query_verb()!="consult") throw("{family} context");
                calls++; return 1;
            }}
        "#,
            name = names[tier],
            conditions = conditions.join(" && ")
        ));
        for name in &names[tier + 1..] {
            sources[target].push_str(&format!(
                "int {family}_{name}() {{ throw(\"unexpected lower tier\"); return 0; }}\n"
            ));
        }
    }
    sources[0].push_str(&format!(r#"
        void create() {{ parse_init(); parse_add_rule("read","{rule}"); parse_add_synonym("consult","read"); }}
    "#));
    check(
        &[
            ("/verb.c", &sources[0]),
            ("/item.c", &sources[1]),
            ("/box.c", &sources[2]),
        ],
        "",
        r#"
        mixed result=parse_sentence("consult about ancient history from book to box",0,
            ({find_object("/item"),find_object("/box")}));
        return ({result,"/verb"->count(),"/item"->count(),"/box"->count()});
    "#,
        [1, 2, 2, 2].map(LpcRef::from).to_vec(),
    )
    .await;
}

#[tokio::test]
async fn exact_handlers_receive_all_capture_words() {
    layouts(0).await;
}

#[tokio::test]
async fn word_handlers_receive_literals_in_grammar_order() {
    layouts(1).await;
}

#[tokio::test]
async fn shape_handlers_receive_base_and_typed_verbs() {
    layouts(2).await;
}

#[tokio::test]
async fn generic_handlers_receive_exact_rule_text_and_the_complete_tail() {
    layouts(3).await;
}

#[tokio::test]
async fn word_placeholders_and_wrd_captures_have_distinct_names() {
    let item = format!("{ITEM} int direct_verb_rule() {{ return 1; }}");
    check(&[("/item.c",&item),("/verb.c",r#"
        int ok;
        void create() { parse_init(); parse_add_rule("mark","with WRD OBJ STR"); parse_add_synonym("paint","mark"); }
        void do_verb_word_wrd_obj_str(mixed a...) {
            ok=sizeof(a)==9 && a[0]=="mark" && a[1]=="with" && a[2]=="red" && a[3]==find_object("/item")
                && a[4]=="note" && a[5]=="paint" && a[6]=="red" && a[7]=="book" && a[8]=="note";
        }
        void do_verb_word_word_obj_str() { throw("WRD must not become word"); }
        int check() { return ok; }
    "#)],"",r#"
        return ({parse_sentence("paint with red book note",0,({find_object("/item")})),"/verb"->check()});
    "#,vec![1.into(),1.into()]).await;
}

#[tokio::test]
async fn empty_rules_use_shape_and_rule_handlers() {
    for (name, signature, condition) in [
        (
            "verb",
            "string base, string typed",
            "base==\"wave\" && typed==\"salute\"",
        ),
        (
            "verb_rule",
            "string base, string rule, string typed",
            "base==\"wave\" && rule==\"\" && typed==\"salute\"",
        ),
    ] {
        let verb = format!(
            r#"
            int calls;
            void create() {{ parse_init(); parse_add_rule("wave",""); parse_add_synonym("salute","wave"); }}
            int can_{name}({signature}) {{ if(!({condition})) throw("can layout"); calls++; return 1; }}
            int do_{name}({signature}) {{ if(!({condition})) throw("do layout"); calls++; return 0; }}
            int count() {{ return calls; }}
        "#
        );
        check(
            &[("/verb.c", &verb)],
            "",
            r#"
            return ({parse_sentence("salute"),"/verb"->count()});
        "#,
            vec![1.into(), 2.into()],
        )
        .await;
    }
}

#[tokio::test]
async fn actor_generic_permission_precedes_owner_specific_permission() {
    check(&[("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("wave",""); }
        int can_wave() { throw("owner can should not run"); return 0; }
        void do_wave() {}
    "#)],r#"
        int calls;
        int can_verb_rule(string base,string rule,string typed) {
            calls++; return base=="wave" && rule=="" && typed=="wave" && this_player()==this_object()
                && previous_object()==this_object() && origin()=="driver";
        }
    "#,r#"return ({parse_sentence("wave"),calls});"#,vec![1.into(),1.into()]).await;
}

#[tokio::test]
async fn actor_refusal_is_reported_without_trying_owner() {
    check(
        &[(
            "/verb.c",
            r#"
        void create() { parse_init(); parse_add_rule("wave",""); }
        int can_wave() { throw("owner can should not run"); return 1; }
        void do_wave() { throw("action should not run"); }
    "#,
        )],
        r##"string can_wave() { return "#not now"; }"##,
        r#"return ({parse_sentence("wave"),"/secure/master"->count()});"#,
        vec![s("/main:not now"), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn owner_permissions_receive_original_noun_objects_and_scalar_pairs() {
    let verb = r#"
        int direct; int indirect; int acted;
        void create() { parse_init(); parse_add_rule("put","OBS in OBJ"); }
        int direct_put_word_obj_word_obj() { throw("wrong shape"); return 0; }
        int direct_verb_obj_word_obj(string base, object item,string prep,object box,string typed,string a,string b) {
            if(base!="put" || item!=find_object("/item") || prep!="in" || box!=find_object("/box")
                || typed!="put" || a!="book" || b!="box") throw("direct fallback arguments");
            direct++; return 1;
        }
        int indirect_put_obj_word_obj(object item,string prep,object box,string a,string b) {
            if(item!=find_object("/item") || prep!="in" || box!=find_object("/box") || a!="book" || b!="box")
                throw("indirect fallback arguments");
            indirect++; return 1;
        }
        void do_verb_obs_word_obj(string base,mixed *items,string prep,object box,string typed,string a,string b) {
            acted=sizeof(items)==1 && items[0]==find_object("/item") && base=="put" && prep=="in"
                && box==find_object("/box") && typed=="put" && a=="book" && b=="box";
        }
        mixed *counts() { return ({direct,indirect,acted}); }
    "#;
    check(&[("/item.c",ITEM),("/box.c",BOX),("/verb.c",verb)],"",r#"
        return ({parse_sentence("put book in box",0,({find_object("/item"),find_object("/box")}))})+"/verb"->counts();
    "#,[1,2,2,1].map(LpcRef::from).to_vec()).await;
}

#[tokio::test]
async fn candidate_generic_permission_precedes_owner_specific_permission() {
    for reply in ["1", "0", "\"plain\"", "\"#soft\""] {
        let item = format!("{ITEM} mixed direct_verb_rule() {{ return {reply}; }}");
        let expected = match reply {
            "1" => 1.into(),
            "0" => s("kind:7"),
            "\"plain\"" => s("/item:plain"),
            _ => s("/item:soft"),
        };
        check(
            &[
                ("/item.c", &item),
                (
                    "/verb.c",
                    r#"
            void create() { parse_init(); parse_add_rule("touch","OBJ"); }
            int direct_touch_obj() { throw("owner must not override candidate"); return 1; }
            void do_touch_obj() {}
        "#,
                ),
            ],
            "",
            r#"return ({parse_sentence("touch book",0,({find_object("/item")}))});"#,
            vec![expected],
        )
        .await;
    }
}

#[tokio::test]
async fn owner_reason_attribution_survives_filtering_and_rechecks() {
    for (rule, family, noun, refusal_call) in [
        ("OBJ", "direct", "book", 1),
        ("OBJ", "direct", "book", 2),
        ("OBJ in OBJ", "direct", "book in box", 1),
        ("OBJ in OBJ", "indirect", "book in box", 2),
    ] {
        let verb = format!(
            r#"
            int checks;
            void create() {{ parse_init(); parse_add_rule("touch","{rule}"); }}
            mixed {family}_verb_rule() {{ checks++; if(checks=={refusal_call}) return "owner refused"; return 1; }}
            void do_verb_rule() {{ throw("must not act"); }}
            {other}
        "#,
            other = if family == "indirect" {
                "int direct_verb_rule() { return 1; }"
            } else {
                "int indirect_verb_rule() { return 1; }"
            }
        );
        check(&[("/item.c",ITEM),("/box.c",BOX),("/verb.c",&verb)],"",&format!(r#"
            return ({{parse_sentence("touch {noun}",0,({{find_object("/item"),find_object("/box")}}))}});
        "#),vec![s("/verb:owner refused")]).await;
    }
}

#[tokio::test]
async fn exceptions_stop_name_target_and_rule_fallback() {
    let item = format!(
        r#"{ITEM}
        int direct_touch_obj() {{ throw("permission exploded"); return 1; }}
        int direct_verb_rule() {{ throw("wrong generic"); return 1; }}
    "#
    );
    check(&[("/item.c",&item),("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("touch","OBJ"); parse_add_rule("touch","STR"); }
        int direct_touch_obj() { throw("wrong owner"); return 1; }
        void do_touch_obj() { throw("wrong action"); }
        void do_touch_str() { throw("wrong rule"); }
    "#)],"",r#"
        mixed err=catch(parse_sentence("touch book",0,({find_object("/item")})));
        return ({stringp(err) && strindex(err,"permission exploded")>=0,"/secure/master"->count()});
    "#,vec![1.into(),0.into()]).await;
}

#[tokio::test]
async fn generated_name_collisions_use_the_earliest_argument_layout() {
    check(
        &[(
            "/verb.c",
            r#"
        int ok;
        void create() { parse_init(); parse_add_rule("verb","word STR"); }
        void do_verb_word_str(mixed args...) {
            ok=sizeof(args)==2 && args[0]=="hello" && args[1]=="hello";
        }
        int check() { return ok; }
    "#,
        )],
        "",
        r#"return ({parse_sentence("verb word hello"),"/verb"->check()});"#,
        vec![1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn actor_can_own_the_rule_and_answer_once() {
    check(
        &[],
        r#"
        int checks; int actions;
        int can_verb_rule(string base,string rule,string typed) { checks++; return 1; }
        void do_verb(string base,string typed) { actions++; }
    "#,
        r#"
        parse_init(); parse_add_rule("wave","");
        return ({parse_sentence("wave"),checks,actions});
    "#,
        vec![1.into(), 1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn a_destructed_noun_is_not_rescued_by_owner_fallback() {
    let item = format!(
        r#"{ITEM} int direct_touch_obj_word_obj(object item,string prep,object box) {{ destruct(box); return 1; }}"#
    );
    check(&[("/item.c",&item),("/box.c",BOX),("/verb.c",r#"
        int indirect;
        void create() { parse_init(); parse_add_rule("touch","OBJ in OBJ"); }
        int indirect_verb_rule() { indirect++; return 1; }
        void do_verb_rule() { throw("must not act"); }
        int count() { return indirect; }
    "#)],"",r#"
        return ({parse_sentence("touch book in box",0,({find_object("/item"),find_object("/box")})),"/verb"->count()});
    "#,vec![(-2).into(),0.into()]).await;
}

#[tokio::test]
async fn nested_parses_restore_the_typed_verb_for_generic_actions() {
    check(&[("/verb.c",r#"
        string log="";
        void create() { parse_init(); parse_add_rule("wave",""); parse_add_synonym("salute","wave"); parse_add_rule("nod",""); }
        int can_verb(string base,string typed) {
            log+=typed+":";
            if(base=="wave") { parse_sentence("nod"); log+=query_verb()+":"; }
            return 1;
        }
        void do_verb_rule(string base,string rule,string typed) { log+=typed+"!"; }
        string seen() { return log; }
    "#)],"",r#"return ({command("salute"),"/verb"->seen()});"#,
    vec![1.into(),s("salute:nod:nod!salute:salute!")]).await;
}

#[tokio::test]
async fn literal_read_prepositions_reach_verb_and_object_handlers() {
    let item = format!(
        r#"{ITEM}
        int direct_read_word_str_word_obj(string p1,string topic,string p2,object book) {{
            return p1=="about" && topic=="history" && p2=="in" && book==this_object();
        }}
    "#
    );
    check(&[("/item.c",&item),("/verb.c",r#"
        int seen;
        void create() { parse_init(); parse_add_rule("read","about STR in OBJ"); }
        int can_read_word_str_word_obj(string p1,string topic,string p2,object book) {
            return p1=="about" && topic=="history" && p2=="in" && book==nil;
        }
        void do_read_word_str_word_obj(string p1,string topic,string p2,object book) {
            seen=p1=="about" && topic=="history" && p2=="in" && book==find_object("/item");
        }
        int check() { return seen; }
    "#)],"",r#"return ({parse_sentence("read about history in book",0,({find_object("/item")})),"/verb"->check()});"#,
    vec![1.into(),1.into()]).await;
}

#[tokio::test]
async fn generic_shape_actions_support_fixed_arity_forwarders() {
    let item = format!(
        r#"{ITEM}
        int actions;
        int direct_verb_rule() {{ return 1; }}
        void do_touch() {{ actions++; }}
        int count() {{ return actions; }}
    "#
    );
    check(
        &[
            ("/item.c", &item),
            (
                "/verb.c",
                r#"
        void create() { parse_init(); parse_add_rule("touch","OBJ"); }
        void do_verb_obj(string verb,object target) { call_other(target,"do_"+verb); }
    "#,
            ),
        ],
        "",
        r#"return ({parse_sentence("touch book",0,({find_object("/item")})),"/item"->count()});"#,
        vec![1.into(), 1.into()],
    )
    .await;
}

#[tokio::test]
async fn generic_soul_tail_counts_all_captures_and_shape_guard_runs() {
    check(&[("/bob.c",r#"
        void create() { enable_commands(); }
        string *parse_command_id_list() { return ({"bob"}); }
        int direct_verb_rule() { return 1; }
    "#),("/verb.c",r#"
        int captures; int acted;
        void create() { parse_init(); parse_add_rule("smile","WRD LIV"); parse_add_rule("smile","WRD"); }
        int can_verb_wrd(string verb,string adverb) { return adverb=="happily"; }
        int can_verb_rule() { return 1; }
        void do_verb_rule(string verb,string rule,mixed args...) {
            captures=(sizeof(args)-1)/2;
            acted++;
            if(rule=="WRD LIV" && (args[0]!="happily" || args[1]!=find_object("/bob")
                || args[2]!="smile" || args[3]!="happily" || args[4]!="bob")) throw("soul tail");
        }
        mixed *counts() { return ({captures,acted}); }
    "#)],"",r#"
        mixed full=parse_sentence("smile happily bob",0,({find_object("/bob")}));
        mixed *counts="/verb"->counts();
        mixed rejected=parse_sentence("smile nonsense");
        mixed short_form=parse_sentence("smile happily");
        return ({full})+counts+({rejected,short_form})+"/verb"->counts();
    "#,[1,2,1,-2,1,1,2].map(LpcRef::from).to_vec()).await;
}

#[tokio::test]
async fn owner_refusals_keep_plural_members_separate_and_diagnostics_deferred() {
    check(&[("/item.c",ITEM),("/other.c",ITEM),("/box.c",BOX),("/verb.c",r#"
        mixed *chosen;
        void create() { parse_init(); parse_add_rule("put","OBS in OBJ"); }
        mixed direct_verb_rule(string verb,string rule,object item,object box) {
            if(item==find_object("/other")) return "this book stays";
            return 1;
        }
        int indirect_verb_rule() { return 1; }
        void do_verb_rule(string verb,string rule,mixed *items,object box) { chosen=items; }
        int check() { return sizeof(chosen)==2 && chosen[0]==find_object("/item") && chosen[1]=="this book stays"; }
    "#)],"",r#"
        return ({parse_sentence("put all books in box",0,({find_object("/item"),find_object("/other"),find_object("/box")})),
            "/verb"->check(),"/secure/master"->count()});
    "#,[1,1,0].map(LpcRef::from).to_vec()).await;
}

#[tokio::test]
async fn inherited_word_handlers_participate_in_lookup() {
    let root = tempfile::tempdir().unwrap();
    std::fs::write(
        root.path().join("base.c"),
        r#"
        string result;
        void do_read_word_str(string prep,string topic) { result=prep+":"+topic; }
        string seen() { return result; }
    "#,
    )
    .unwrap();
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .lib_dir(root.path().to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap();
        assert_eq!(
            run_with(
                config,
                MASTER,
                &[(
                    "/verb.c",
                    r#"
            inherit "/base";
            void create() { parse_init(); parse_add_rule("read","about STR"); }
        "#
                )],
                r#"
            mixed *create() { enable_commands(); set_this_player(this_object());
                return ({parse_sentence("read about history"),"/verb"->seen()}); }
        "#
            )
            .await,
            vec![1.into(), s("about:history")],
            "{mode:?}"
        );
    }
}
