//! Multiword parser keys, selection and callback context under both null modes.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::run_with;
use crate::interpreter::lpc_ref::LpcRef;

const MASTER: &str = r#"
    mixed *reports=({}); int skip;
    int valid_destruct() { return 1; }
    void skip(int n) { skip=n; }
    mixed *reports() { return reports; }
    mixed parser_error_message(int kind, object ob, mixed arg, int flag) {
        reports+=({({query_verb(),query_verb(1),query_command(),kind})});
        if(sizeof(reports)<=skip) return 0;
        return "error:"+kind;
    }
"#;
const GEM: &str = r#"
    string *parse_command_id_list() { return ({"gem"}); }
    string *parse_command_plural_id_list() { return ({"gems"}); }
    int direct_verb_rule() { return 1; }
    int indirect_verb_rule() { return 1; }
    void move(object dest) { move_object(dest); }
"#;
const RECORD: &str = r#"
    mixed *seen=({}); int actions;
    void do_verb_rule(string base,string rule,mixed args...) {
        actions++; seen=({base,rule,args,query_verb(),query_verb(1),query_command()});
    }
    mixed *seen() { return seen; }
    int actions() { return actions; }
"#;

async fn check(objects: &[(&str, &str)], extra: &str, body: &str) {
    let mut sources = vec![("/gem.c", GEM)];
    sources.extend_from_slice(objects);
    let main = format!(
        r#"
        {extra}
        mixed *create() {{
            enable_commands(); set_this_player(this_object());
            object gem=find_object("/gem"); gem->move(this_object());
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
            run_with(config, MASTER, &sources, &main).await,
            vec![LpcRef::from(1)],
            "{mode:?}"
        );
    }
}

#[tokio::test]
async fn multiword_synonyms_keep_specific_base_handlers_in_both_entry_paths() {
    for (base, alias) in [
        ("get", "pick up"),
        ("drop", "put down"),
        ("remove", "take off"),
        ("wear", "put on"),
    ] {
        let verb = format!(
            r#"
            int correct;
            void create() {{ parse_init(); parse_add_rule("{base}","OBJ"); parse_add_synonym("{alias}","{base}"); }}
            void do_{base}_obj(object ob,string raw) {{
                correct=ob==find_object("/gem") && raw=="gem" && query_verb()=="{alias}" && query_verb(1)=="{alias}";
            }}
            int correct() {{ return correct; }}
        "#
        );
        for entry in ["parse_sentence", "command"] {
            check(
                &[("/verb.c", &verb)],
                "",
                &format!(
                    r#"
                return ({{{entry}("{alias} gem")==1 && "/verb"->correct()}});
            "#
                ),
            )
            .await;
        }
    }
}

#[tokio::test]
async fn normalization_preserves_raw_utf8_verb_and_remainder_text() {
    let verb = format!(
        "void create() {{ parse_init(); parse_add_rule(\"get\",\"STR\"); parse_add_synonym(\"  préndre\u{2003} ça \",\" get \"); }} {RECORD}"
    );
    for entry in ["parse_sentence", "command"] {
        check(
            &[("/verb.c", &verb)],
            "",
            &format!(
                r#"
            string line="  préndre{}\tça  MiXeD   text  ";
            if({entry}(line)!=1) return ({{0}});
            mixed *s="/verb"->seen();
            return ({{s[0]=="get" && s[2][0]=="MiXeD   text" && s[2][2]=="MiXeD   text"
                && s[2][1]=="préndre{}\tça" && s[3]=="préndre ça" && s[4]==s[2][1] && s[5]==line}});
        "#,
                "\u{2003}", "\u{2003}"
            ),
        )
        .await;
    }
}

#[tokio::test]
async fn verb_words_keep_exact_case_punctuation_and_boundaries() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("pick up","WRD"); parse_add_rule("Pick up!","WRD"); }} {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)],"",r#"
        foreach(string line in ({"pick upper gem","Pick up gem","pick up! gem","pick, up gem","pi'ck up gem"}))
            if(parse_sentence(line)!=0) return ({0});
        return ({parse_sentence("Pick up! gem")==1 && "/verb"->seen()[0]=="Pick up!"
            && parse_sentence("pick up gem")==1 && "/verb"->seen()[0]=="pick up"});
    "#).await;
}

#[tokio::test]
async fn multiword_bases_use_generic_handlers_without_underscore_aliasing() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule(" pick  up ","WRD"); }}
        void do_pick_up_wrd() {{ throw("invented handler name"); }}
        {RECORD}
    "#
    );
    check(
        &[("/verb.c", &verb)],
        "",
        r#"
        return ({parse_sentence("pick up gem")==1 && "/verb"->seen()[0]=="pick up"
            && "/verb"->seen()[2][1]=="pick up"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn longest_phrase_wins_regardless_of_registration_order() {
    for reverse in [false, true] {
        let mut keys = vec!["pick", "pick up", "pick up now"];
        if reverse {
            keys.reverse();
        }
        let registrations = keys
            .into_iter()
            .map(|key| format!("parse_add_rule(\"{key}\",\"STR\");"))
            .collect::<String>();
        let verb = format!("void create() {{ parse_init(); {registrations} }} {RECORD}");
        for entry in ["parse_sentence", "command"] {
            check(&[("/verb.c",&verb)],"",&format!(r#"
                return ({{{entry}("pick up now gem")==1 && "/verb"->seen()[0]=="pick up now" && "/verb"->seen()[2][0]=="gem"}});
            "#)).await;
        }
    }
}

#[tokio::test]
async fn shorter_phrases_follow_no_parse_refusal_and_missing_action() {
    for (rule, handlers) in [
        ("from WRD", RECORD.to_owned()),
        (
            "WRD",
            format!("int can_verb_rule() {{ return 0; }} {RECORD}"),
        ),
        ("WRD", String::new()),
    ] {
        let long = format!(
            "void create() {{ parse_init(); parse_add_rule(\"pick up\",\"{rule}\"); }} {handlers}"
        );
        let short =
            format!("void create() {{ parse_init(); parse_add_rule(\"pick\",\"STR\"); }} {RECORD}");
        for entry in ["parse_sentence", "command"] {
            check(
                &[("/long.c", &long), ("/short.c", &short)],
                "",
                &format!(
                    r#"
                return ({{{entry}("pick up gem")==1 && "/short"->seen()[2][0]=="up gem"
                    && sizeof("/secure/master"->reports())==0}});
            "#
                ),
            )
            .await;
        }
    }
}

#[tokio::test]
async fn equal_phrases_keep_registration_order_across_owners() {
    let verb = format!(
        r#"
        int accept=1;
        void create() {{ parse_init(); parse_add_rule("pick up","WRD"); }}
        int can_verb_rule() {{ return accept; }} void refuse() {{ accept=0; }} {RECORD}
    "#
    );
    check(&[("/first.c",&verb),("/second.c",&verb)],"",r#"
        if(parse_sentence("pick up gem")!=1 || "/first"->actions()!=1 || "/second"->actions()!=0) return ({0});
        "/first"->refuse();
        return ({parse_sentence("pick up gem")==1 && "/first"->actions()==1 && "/second"->actions()==1});
    "#).await;
}

#[tokio::test]
async fn a_shorter_success_suppresses_longer_diagnostics() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("pick up","OBJ"); parse_add_rule("pick","STR"); }} {RECORD}
    "#
    );
    check(
        &[("/verb.c", &verb)],
        "",
        r#"
        return ({parse_sentence("pick up missing")==1 && "/verb"->seen()[0]=="pick"
            && sizeof("/secure/master"->reports())==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn deferred_diagnostics_keep_each_phrases_query_context() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("pick","OBJ"); parse_add_rule("pick up","OBJ"); }} {RECORD}
    "#
    );
    for skipped in [0, 1] {
        check(&[("/verb.c",&verb)],"",&format!(r#"
            "/secure/master"->skip({skipped}); string line="  pick\t up missing";
            if(parse_sentence(line)!="error:7") return ({{0}});
            mixed *reports="/secure/master"->reports();
            if(sizeof(reports)!={skipped}+1 || reports[0][0]!="pick up" || reports[0][1]!="pick\t up" || reports[0][2]!=line) return ({{0}});
            return ({{{skipped}==0 || (reports[1][0]=="pick" && reports[1][1]=="pick" && reports[1][2]==line)}});
        "#)).await;
    }
}

#[tokio::test]
async fn silent_rejection_codes_combine_across_phrases() {
    let long = r#"void create() { parse_init(); parse_add_rule("pick up","OBJ"); } void do_verb_rule() {}"#;
    let short = r#"void create() { parse_init(); parse_add_rule("pick","STR"); } int can_verb_rule() { return 0; } void do_verb_rule() {}"#;
    check(
        &[("/long.c", long), ("/short.c", short)],
        "",
        r#"
        "/secure/master"->skip(100);
        return ({parse_sentence("pick up missing")==-3});
    "#,
    )
    .await;
}

#[tokio::test]
async fn missing_verbs_empty_rules_and_no_parse_remain_distinct() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("pick up","WRD"); parse_add_rule("stand up",""); }} {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)],"",r#"
        return ({parse_sentence("")==0 && parse_sentence("pick")==0 && parse_sentence("pick up")==-1
            && parse_sentence("stand up")==1 && "/verb"->seen()[0]=="stand up" && "/verb"->seen()[2][0]=="stand up"});
    "#).await;
}

#[tokio::test]
async fn exceptions_stop_shorter_fallback_and_restore_the_outer_frame() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("pick","STR"); parse_add_rule("pick up","WRD"); }}
        int can_verb_rule() {{ if(query_verb()=="pick up") throw("long failure"); return 1; }} {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)],r#"
        int outer(string rest) {
            mixed err=catch(parse_sentence("pick\t up gem"));
            return stringp(err) && strindex(err,"long failure")>=0 && query_verb()=="outer" && query_verb(1)=="outer";
        }
    "#,r#"
        add_action(outer,"outer");
        return ({command("outer x")==1 && "/verb"->actions()==0 && sizeof("/secure/master"->reports())==0});
    "#).await;
}

#[tokio::test]
async fn refused_trials_keep_effects_when_a_shorter_phrase_succeeds() {
    let verb = format!(
        r#"
        int checks;
        void create() {{ parse_init(); parse_add_rule("pick","STR"); parse_add_rule("pick up","WRD"); }}
        int can_verb_rule() {{ checks++; return query_verb()=="pick"; }}
        int checks() {{ return checks; }} {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)],"",r#"
        return ({parse_sentence("pick up gem")==1 && "/verb"->checks()==2 && "/verb"->seen()[0]=="pick"});
    "#).await;
}

#[tokio::test]
async fn nested_multiword_commands_restore_the_enclosing_phrase() {
    let verb = r#"
        int correct;
        void create() { parse_init(); parse_add_rule("outer action",""); parse_add_rule("inner action",""); }
        void do_verb_rule(string base,string rule,string typed) {
            if(base=="inner action") {
                if(typed!="inner\t action" || query_verb()!="inner action") throw("inner context");
                return;
            }
            correct=parse_sentence("inner\t action")==1 && query_verb()=="outer action"
                && query_verb(1)=="outer\t action" && query_command()=="outer\t action";
        }
        int correct() { return correct; }
    "#;
    check(
        &[("/verb.c", verb)],
        "",
        r#"
        return ({command("outer\t action")==1 && "/verb"->correct()});
    "#,
    )
    .await;
}

#[tokio::test]
async fn scope_and_vocabulary_hooks_see_the_complete_parser_phrase() {
    let box_code = r#"
        void move(object dest) { move_object(dest); }
        int inventory_visible() {
            if(query_verb()!="pick up" || query_verb(1)!="pick\t up") throw("scope context"); return 1;
        }
    "#;
    let noun = r#"
        void move(object dest) { move_object(dest); }
        string *parse_command_id_list() {
            if(query_verb()!="pick up" || query_verb(1)!="pick\t up") throw("vocabulary context"); return ({"item"});
        }
        int direct_verb_rule() { return 1; }
    "#;
    let verb = format!(
        r#"void create() {{ parse_init(); parse_add_rule("get","OBJ"); parse_add_synonym("pick up","get"); }} {RECORD}"#
    );
    check(&[("/box.c",box_code),("/item.c",noun),("/verb.c",&verb)],"",r#"
        object box=find_object("/box"); object item=find_object("/item"); box->move(this_object()); item->move(box);
        return ({command("pick\t up item")==1 && parse_sentence("pick\t up item",0,({box,({item})}))==1});
    "#).await;
}

#[tokio::test]
async fn normalized_aliases_keep_exact_filters_snapshots_and_base_removal() {
    let verb = format!(
        r#"
        void create() {{
            parse_init(); parse_add_rule(" take\t away "," OBJ:v ");
            parse_add_synonym(" pick\t up ","take away"," OBJ:v ");
            parse_add_synonym("pick it up"," pick  up "," OBJ:v ");
            parse_add_rule("take away","WRD");
        }}
        string *rules() {{ return parse_my_rules(); }}
        void remove(string key) {{ parse_remove(key); }}
        int rejects_filter() {{ return stringp(catch(parse_add_synonym("bad","take away","OBJ:v"))); }}
        {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)],"",r#"
        string *rules="/verb"->rules();
        if(sizeof(rules)!=4 || member_array("pick up  OBJ:v ",rules)<0 || strindex(parse_dump(),"pick up  OBJ:v ")<0 || !"/verb"->rejects_filter()) return ({0});
        if(parse_sentence("pick it up gem")!=1 || "/verb"->seen()[0]!="take away" || "/verb"->seen()[1]!=" OBJ:v ") return ({0});
        if(parse_sentence("pick it up missing")!="error:7") return ({0});
        "/verb"->remove(" pick\t up "); if(sizeof("/verb"->rules())!=4) return ({0});
        "/verb"->remove(" take   away ");
        return ({sizeof("/verb"->rules())==0 && parse_sentence("pick it up gem")==0});
    "#).await;
}

#[tokio::test]
async fn empty_names_fail_without_changing_the_registry() {
    check(&[],r#"
        void do_verb_rule() {}
    "#,r#"
        parse_init(); parse_add_rule("get","OBJ");
        foreach(string name in ({""," \t\n","　"})) {
            foreach(mixed error in ({catch(parse_add_rule(name,"")),catch(parse_add_synonym(name,"get")),
                    catch(parse_add_synonym("grab",name)),catch(parse_remove(name))})) {
                if(!stringp(error) || strindex(error,"verb must contain at least one word")<0) return ({0});
            }
        }
        return ({sizeof(parse_my_rules())==1 && parse_sentence("")==0});
    "#).await;
}

#[tokio::test]
async fn removal_and_destruction_preserve_other_owners_aliases() {
    let verb = format!(
        r#"
        void create() {{ parse_init(); parse_add_rule("get","OBJ"); parse_add_synonym("pick up","get"); }}
        void remove() {{ parse_remove(" get "); }} {RECORD}
    "#
    );
    check(&[("/first.c",&verb),("/second.c",&verb)],"",r#"
        "/first"->remove();
        if(parse_sentence("pick up gem")!=1 || "/first"->actions()!=0 || "/second"->actions()!=1) return ({0});
        destruct(find_object("/second"));
        return ({parse_sentence("pick up gem")==0 && strindex(parse_dump(),"pick up")<0});
    "#).await;
}

#[tokio::test]
async fn registry_mutations_take_effect_on_the_next_parse() {
    let verb = format!(
        r#"
        int changed;
        void create() {{ parse_init(); parse_add_rule("pick","STR"); parse_add_synonym("pick up","pick"); }}
        int can_pick_str() {{
            if(!changed && query_verb()=="pick up") {{
                changed=1; parse_remove("pick"); parse_add_rule("pick up",""); return 0;
            }}
            return 1;
        }}
        {RECORD}
    "#
    );
    check(&[("/verb.c",&verb)],"",r#"
        if(parse_sentence("pick up gem")!=1 || "/verb"->seen()[0]!="pick" || "/verb"->seen()[2][0]!="up gem") return ({0});
        return ({parse_sentence("pick up gem")==-1 && "/verb"->actions()==1});
    "#).await;
}

#[tokio::test]
async fn destructed_owners_are_skipped_in_an_existing_phrase_snapshot() {
    let long = r#"
        void create() { parse_init(); parse_add_rule("pick up","WRD"); }
        int can_verb_rule() { destruct(find_object("/short")); return 0; }
        void do_verb_rule() { throw("long action"); }
    "#;
    let short = r#"
        void create() { parse_init(); parse_add_rule("pick","STR"); }
        void do_verb_rule() { throw("dead owner action"); }
    "#;
    check(
        &[("/long.c", long), ("/short.c", short)],
        "",
        r#"
        return ({parse_sentence("pick up gem")==-2 && !find_object("/short")});
    "#,
    )
    .await;
}

#[tokio::test]
async fn actor_rules_keep_precedence_and_first_word_context() {
    let verb =
        format!(r#"void create() {{ parse_init(); parse_add_rule("pick up","WRD"); }} {RECORD}"#);
    for native in [false, true] {
        let register = if native {
            r#"add_rule("'pick' %s", action);"#
        } else {
            r#"add_action(action,"pick");"#
        };
        check(&[("/verb.c",&verb)],r#"
            int accept=1; int actions;
            int action(string rest) {
                if(query_verb()!="pick" || query_verb(1)!="pick" || rest!="up gem") throw("actor context");
                actions++; return accept;
            }
        "#,&format!(r#"
            {register}
            if(command("pick up gem")!=1 || actions!=1 || "/verb"->actions()!=0) return ({{0}});
            accept=0;
            return ({{command("pick up gem")==1 && actions==2 && "/verb"->actions()==1}});
        "#)).await;
    }
}

#[tokio::test]
async fn command_fallback_restores_first_word_context() {
    let verb = r#"
        int correct;
        void create() { parse_init(); parse_add_rule("pick up","WRD"); }
        mixed failure() { correct=query_verb()=="pick" && query_verb(1)=="pick" && query_command()=="pick\t up gem"; return 0; }
        int correct() { return correct; }
        int can_verb_rule() { notify_fail(failure); return 0; }
        void do_verb_rule() { throw("refused action"); }
    "#;
    check(
        &[("/verb.c", verb)],
        "",
        r#"
        return ({command("pick\t up gem")==0 && "/verb"->correct()});
    "#,
    )
    .await;
}

#[tokio::test]
async fn command_preprocessing_runs_before_phrase_matching() {
    let verb =
        format!(r#"void create() {{ parse_init(); parse_add_rule("pick up","WRD"); }} {RECORD}"#);
    check(
        &[("/verb.c", &verb)],
        r#"
        string process_input(string line) { return line=="grab gem" ? "pick\t up gem" : line; }
    "#,
        r#"
        return ({parse_sentence("grab gem")==0 && command("grab gem")==1
            && "/verb"->seen()[4]=="pick\t up" && "/verb"->seen()[5]=="pick\t up gem"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn all_handler_tiers_receive_the_full_typed_phrase() {
    for (method, args, condition) in [
        (
            "do_get_obj_with_wrd",
            "object ob,string word,string raw_ob,string raw_word",
            "word==\"blue\"",
        ),
        (
            "do_get_obj_word_wrd",
            "object ob,string prep,string word,string raw_ob,string raw_word",
            "prep==\"with\" && word==\"blue\"",
        ),
        (
            "do_verb_obj_word_wrd",
            "string base,object ob,string prep,string word,string typed,string raw_ob,string raw_word",
            "base==\"get\" && prep==\"with\" && word==\"blue\" && typed==\"pick\\t up\"",
        ),
        (
            "do_verb_rule",
            "string base,string rule,object ob,string word,string typed,string raw_ob,string raw_word",
            "base==\"get\" && rule==\"OBJ with WRD\" && word==\"blue\" && typed==\"pick\\t up\"",
        ),
    ] {
        let verb = format!(
            r#"
            int correct;
            void create() {{ parse_init(); parse_add_rule("get","OBJ with WRD"); parse_add_synonym("pick up","get"); }}
            void {method}({args}) {{ correct={condition} && ob==find_object("/gem") && raw_ob=="gem" && raw_word=="blue" && query_verb()=="pick up" && query_verb(1)=="pick\t up"; }}
            int correct() {{ return correct; }}
        "#
        );
        check(
            &[("/verb.c", &verb)],
            "",
            r#"
            return ({parse_sentence("pick\t up gem with blue")==1 && "/verb"->correct()});
        "#,
        )
        .await;
    }
}

#[tokio::test]
async fn plural_relation_rules_keep_their_layout_after_a_multiword_verb() {
    let container = r#"
        string *parse_command_id_list() { return ({"box"}); }
        int indirect_verb_rule() { return 1; }
    "#;
    let verb = r#"
        int correct;
        void create() { parse_init(); parse_add_rule("get","OBS:v from OBJ"); parse_add_synonym("pick up","get"); }
        void do_get_obs_from_obj(mixed *items,object box,string raw_items,string raw_box) {
            correct=sizeof(items)==1 && items[0]==find_object("/gem") && box==find_object("/box")
                && raw_items=="gems" && raw_box=="box" && query_verb()=="pick up";
        }
        int correct() { return correct; }
    "#;
    check(&[("/box.c",container),("/verb.c",verb)],"",r#"
        return ({parse_sentence("pick up gems from box",0,({gem,find_object("/box")}))==1 && "/verb"->correct()});
    "#).await;
}
