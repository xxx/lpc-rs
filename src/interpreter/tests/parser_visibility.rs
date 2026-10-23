//! Visible-only slots and structured scopes, under both null modes.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::run_with;
use crate::interpreter::lpc_ref::LpcRef;

const MASTER: &str = r#"
    int reports;
    int valid_destruct() { return 1; }
    string parse_command_all_word() { return "all"; }
    int parse_command_numeral(string word) { return word=="second" ? -2 : 0; }
    string parser_error_message(int kind, object ob, mixed arg, int flag) {
        reports++; return "error:"+kind;
    }
    int count() { return reports; }
"#;
const ROOM: &str = "";
const BOX: &str = r#"
    int visible=1; int accessible; int v_calls; int a_calls;
    void context(string how, object player, object caller) {
        if(how!="driver" || player!=find_object("/main") || caller!=player) throw("scope context");
    }
    int inventory_visible() {
        context(origin(),this_player(),previous_object()); v_calls++; return visible;
    }
    int inventory_accessible() {
        context(origin(),this_player(),previous_object()); a_calls++; return accessible;
    }
    void set(int v,int a) { visible=v; accessible=a; }
    int *counts() { return ({v_calls,a_calls}); }
    void move(object dest) { move_object(dest); }
"#;
const GEM: &str = r#"
    int calls;
    string *parse_command_id_list() { return ({"gem"}); }
    string *parse_command_plural_id_list() { return ({"gems"}); }
    int direct_verb_rule() { calls++; return 1; }
    int indirect_verb_rule() { calls++; return 1; }
    int count() { return calls; }
    void move(object dest) { move_object(dest); }
"#;

async fn check(objects: &[(&str, &str)], globals: &str, body: &str) {
    let mut sources: Vec<_> = [("/room.c", ROOM), ("/box.c", BOX), ("/gem.c", GEM)]
        .into_iter()
        .filter(|(path, _)| !objects.iter().any(|(p, _)| p == path))
        .collect();
    sources.extend_from_slice(objects);
    let main = format!(
        r#"
        {globals}
        mixed *create() {{
            enable_commands(); set_this_player(this_object());
            object room=find_object("/room"); object box=find_object("/box"); object gem=find_object("/gem");
            move_object(room); box->move(room); gem->move(box);
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
async fn all_object_tokens_can_select_visible_inaccessible_objects() {
    for token in ["OBJ", "OBS", "LIV", "LVS"] {
        let item = format!("{GEM} void create() {{ enable_commands(); }}");
        let verb = format!(
            r#"
            int correct;
            void create() {{ parse_init(); parse_add_rule("see","{token}:v"); parse_add_rule("touch","{token}"); }}
            void do_verb_rule(string base,string rule,mixed ob,string typed,string raw) {{
                correct=base=="see" && rule=="{token}:v" && typed=="see" && raw=="gem"
                    && ({});
            }}
            int correct() {{ return correct; }}
        "#,
            if token == "OBS" || token == "LVS" {
                "arrayp(ob) && sizeof(ob)==1 && ob[0]==find_object(\"/gem\")"
            } else {
                "ob==find_object(\"/gem\")"
            }
        );
        check(&[("/gem.c",&item),("/verb.c",&verb)],"",r#"
            return ({parse_sentence("touch gem")=="error:3" && parse_sentence("see gem")==1 && "/verb"->correct()});
        "#).await;
    }
}

#[tokio::test]
async fn visibility_does_not_bypass_living_checks() {
    check(
        &[(
            "/verb.c",
            r#"
        void create() { parse_init(); parse_add_rule("greet","LIV:v"); }
        void do_greet_liv() { throw("nonliving action"); }
    "#,
        )],
        "",
        r#"return ({parse_sentence("greet gem")=="error:2" && gem->count()==0});"#,
    )
    .await;
}

#[tokio::test]
async fn opaque_contents_stay_hidden_in_both_scope_forms() {
    check(
        &[(
            "/verb.c",
            r#"
        void create() { parse_init(); parse_add_rule("see","OBJ:v"); }
        void do_see_obj() { throw("hidden action"); }
    "#,
        )],
        "",
        r#"
        box->set(0,1);
        return ({parse_sentence("see gem")=="error:7"
            && parse_sentence("see gem",0,({box,({gem})}))=="error:7" && gem->count()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn a_visible_slot_does_not_relax_its_partner() {
    for (rule, line) in [
        ("WRD OBJ:v with OBJ", "blue gem with target"),
        ("WRD OBJ with OBJ:v", "blue target with gem"),
    ] {
        let target = GEM.replace("gem", "target");
        let verb = format!(
            r#"
            int actions;
            void create() {{ parse_init(); parse_add_rule("pair","{rule}"); }}
            void do_verb_rule() {{ actions++; }} int count() {{ return actions; }}
        "#
        );
        check(
            &[("/target.c", &target), ("/verb.c", &verb)],
            "",
            &format!(
                r#"
            object target=find_object("/target"); target->move(box);
            if(parse_sentence("pair {line}")!="error:3") return ({{0}});
            target->move(room);
            return ({{parse_sentence("pair {line}")==1 && "/verb"->count()==1}});
        "#
            ),
        )
        .await;
    }
}

#[tokio::test]
async fn visible_plural_pairs_keep_scalar_permissions_and_array_actions() {
    for (rule, line) in [
        ("OBS:v with OBJ", "gems with target"),
        ("OBJ with OBS:v", "target with gems"),
    ] {
        let target = GEM.replace("gem", "target");
        let item = GEM
            .replace(
                "int direct_verb_rule()",
                "int direct_verb_rule(string base,string rule,object a,object b)",
            )
            .replace(
                "int indirect_verb_rule()",
                "int indirect_verb_rule(string base,string rule,object a,object b)",
            );
        let verb = format!(
            r#"
            int correct;
            void create() {{ parse_init(); parse_add_rule("pair","{rule}"); }}
            void do_verb_rule(string base,string rule,mixed a,mixed b) {{
                mixed group={}; correct=arrayp(group) && sizeof(group)==2;
            }}
            int correct() {{ return correct; }}
        "#,
            if rule.starts_with("OBS") { "a" } else { "b" }
        );
        check(
            &[
                ("/gem.c", &item),
                ("/second.c", &item),
                ("/target.c", &target),
                ("/verb.c", &verb),
            ],
            "",
            &format!(
                r#"
            "/target"->move(room); "/second"->move(box);
            return ({{parse_sentence("pair {line}")==1 && "/verb"->correct() && gem->count()==2}});
        "#
            ),
        )
        .await;
    }
}

#[tokio::test]
async fn two_plural_slots_keep_independent_access_requirements() {
    let target = GEM.replace("gem", "target");
    check(&[("/target.c",&target),("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("pair","OBS:v with OBS"); }
        void do_verb_rule(string base,string rule,mixed a,mixed b) {
            if(!arrayp(a) || !arrayp(b) || sizeof(a)!=1 || sizeof(b)!=1) throw("aggregate arguments");
        }
    "#)],"",r#"
        object target=find_object("/target"); target->move(box);
        if(parse_sentence("pair gems with targets")!="error:3") return ({0});
        target->move(room); return ({parse_sentence("pair gems with targets")==1});
    "#).await;
}

#[tokio::test]
async fn ordinals_and_counts_follow_slot_access_policy() {
    check(&[("/second.c",GEM),("/verb.c",r#"
        mixed selected;
        void create() { parse_init(); parse_add_rule("see","OBS:v"); parse_add_rule("touch","OBS"); }
        void do_verb_rule(string b,string r,mixed ob) { selected=ob; }
        mixed selected() { return selected; }
    "#)],"",r#"
        object second=find_object("/second"); second->move(room);
        mixed scope=({box,({gem}),second});
        if(parse_sentence("see second gem",0,scope)!=1 || "/verb"->selected()[0]!=second) return ({0});
        if(parse_sentence("touch second gem",0,scope)!="error:5") return ({0});
        if(parse_sentence("see 2 gems",0,scope)!=1 || sizeof("/verb"->selected())!=2) return ({0});
        return ({parse_sentence("touch 2 gems",0,scope)==1 && sizeof("/verb"->selected())==1});
    "#).await;
}

#[tokio::test]
async fn bare_all_still_applies_access_requirements() {
    check(&[("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("see","OBS:v"); parse_add_rule("touch","OBS"); }
        void do_verb_rule(string b,string r,mixed group) {
            if(sizeof(group)!=1 || group[0]!=find_object("/gem")) throw("all selection");
        }
    "#)],"",r#"
        mixed scope=({box,({gem})});
        return ({parse_sentence("touch all",0,scope)=="error:3" && parse_sentence("see all",0,scope)==1});
    "#).await;
}

#[tokio::test]
async fn modified_rules_keep_all_handler_tiers_and_synonym_arguments() {
    for (name, signature, condition) in [
        (
            "read_about_str_in_obj",
            "string topic,object ob,string source,string noun",
            "topic==\"history\" && ob==find_object(\"/gem\") && source==\"history\" && noun==\"gem\"",
        ),
        (
            "read_word_str_word_obj",
            "string a,string topic,string b,object ob,string source,string noun",
            "a==\"about\" && b==\"in\" && topic==\"history\" && ob==find_object(\"/gem\") && source==\"history\" && noun==\"gem\"",
        ),
        (
            "verb_word_str_word_obj",
            "string base,string a,string topic,string b,object ob,string typed,string source,string noun",
            "base==\"read\" && typed==\"consult\" && a==\"about\" && b==\"in\" && topic==\"history\" && ob==find_object(\"/gem\") && source==\"history\" && noun==\"gem\"",
        ),
        (
            "verb_rule",
            "string base,string rule,string topic,object ob,string typed,string source,string noun",
            "base==\"read\" && rule==\"about STR  in OBJ:v\" && typed==\"consult\" && topic==\"history\" && ob==find_object(\"/gem\") && source==\"history\" && noun==\"gem\"",
        ),
    ] {
        let verb = format!(
            r#"
            void create() {{ parse_init(); parse_add_rule("read","about STR  in OBJ:v"); parse_add_synonym("consult","read"); }}
            void do_{name}({signature}) {{ if(!({condition})) throw("modified handler arguments"); }}
        "#
        );
        check(
            &[("/verb.c", &verb)],
            "",
            r#"return ({parse_sentence("consult about history in gem")==1});"#,
        )
        .await;
    }
}

#[tokio::test]
async fn visible_objects_still_need_permission_and_errors_stay_deferred() {
    let item = GEM.replace("return 1;", "return 0;");
    check(&[("/gem.c",&item),("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("see","OBJ:v"); parse_add_rule("see","STR"); }
        void do_see_obj() { throw("refused object"); }
        void do_see_str() {}
    "#)],"",r#"return ({parse_sentence("see gem")==1 && "/secure/master"->count()==0 && gem->count()==1});"#).await;
}

#[tokio::test]
async fn flat_scope_remains_an_explicit_override() {
    check(&[("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("touch","OBJ"); }
        void do_touch_obj() {}
    "#)],"",r#"
        box->set(0,0);
        return ({parse_sentence("touch gem",0,({gem}))==1 && box->counts()[0]==0 && box->counts()[1]==0});
    "#).await;
}

#[tokio::test]
async fn nested_scopes_follow_supplied_ancestry_instead_of_physical_inventory() {
    check(&[("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("see","OBJ:v"); parse_add_rule("touch","OBJ"); }
        void do_verb_rule() {}
    "#)],"",r#"
        gem->move(room);
        mixed scope=({box,({gem})});
        return ({parse_sentence("touch gem",0,scope)=="error:3" && parse_sentence("see gem",0,scope)==1});
    "#).await;
}

#[tokio::test]
async fn inaccessible_ancestry_is_not_restored_by_an_open_child() {
    check(&[("/inner.c",BOX),("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("see","OBJ:v"); parse_add_rule("touch","OBJ"); }
        void do_verb_rule() {}
    "#)],"",r#"
        object inner=find_object("/inner"); inner->set(1,1);
        mixed scope=({box,({inner,({gem})})});
        return ({parse_sentence("touch gem",0,scope)=="error:3" && parse_sentence("see gem",0,scope)==1
            && inner->counts()[1]==0});
    "#).await;
}

#[tokio::test]
async fn each_child_array_belongs_only_to_its_immediate_parent() {
    check(&[("/second.c",GEM),("/open.c",BOX),("/verb.c",r#"
        int correct;
        void create() { parse_init(); parse_add_rule("see","OBS:v"); }
        void do_see_obs(mixed *group) { correct=sizeof(group)==1 && group[0]==find_object("/second"); }
        int correct() { return correct; }
    "#)],"",r#"
        box->set(0,0); object open=find_object("/open"); open->set(1,1);
        return ({parse_sentence("see gems",0,({box,({gem}),open,({find_object("/second")})}))==1 && "/verb"->correct()});
    "#).await;
}

#[tokio::test]
async fn malformed_nested_scopes_are_rejected_even_under_hidden_parents() {
    check(&[],"",r#"
        box->set(0,0);
        object dead=clone_object("/gem"); destruct(dead);
        mixed *scopes=({({({gem})}),({box,"junk",({gem})}),({dead,({gem})}),({box,({}),({gem})}),({box,({({gem})})})});
        foreach(mixed scope in scopes) {
            mixed err=catch(parse_sentence("missing",0,scope));
            if(!stringp(err) || strindex(err,"must immediately follow a live object")<0) return ({0});
        }
        return ({box->counts()[0]==0});
    "#).await;
}

#[tokio::test]
async fn duplicate_paths_merge_reachability_without_changing_order() {
    check(
        &[
            ("/second.c", GEM),
            (
                "/verb.c",
                r#"
        mixed group;
        void create() { parse_init(); parse_add_rule("touch","OBS"); }
        void do_touch_obs(mixed *obs) { group=obs; }
        mixed group() { return group; }
    "#,
            ),
        ],
        "",
        r#"
        object second=find_object("/second");
        foreach(mixed scope in ({({box,({gem}),second,gem}),({gem,second,box,({gem})})})) {
            if(parse_sentence("touch gems",0,scope)!=1) return ({0});
            mixed group="/verb"->group();
            if(sizeof(group)!=2 || group[0]!=gem || group[1]!=second) return ({0});
        }
        return ({1});
    "#,
    )
    .await;
}

#[tokio::test]
async fn duplicate_parents_share_hooks_but_keep_each_paths_access() {
    check(&[("/inner.c",BOX),("/second.c",GEM),("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("touch","OBJ"); }
        void do_touch_obj(object ob) { if(ob!=find_object("/second")) throw("wrong path"); }
    "#)],"",r#"
        object inner=find_object("/inner"); inner->set(1,1);
        mixed scope=({box,({inner,({gem})}),inner,({find_object("/second")})});
        return ({parse_sentence("touch gem",0,scope)==1 && inner->counts()[0]==1 && inner->counts()[1]==1});
    "#).await;
}

#[tokio::test]
async fn structure_is_snapshotted_before_hooks_mutate_child_arrays() {
    let box_code = r#"
        mixed *children;
        void set_children(mixed *array) { children=array; }
        int inventory_visible() { children[0]=find_object("/second"); return 1; }
        int inventory_accessible() { return 1; }
        void move(object dest) { move_object(dest); }
    "#;
    check(&[("/box.c",box_code),("/second.c",GEM),("/verb.c",r#"
        object selected;
        void create() { parse_init(); parse_add_rule("touch","OBJ"); }
        void do_touch_obj(object ob) { selected=ob; }
        object selected() { return selected; }
    "#)],"",r#"
        mixed *children=({gem}); box->set_children(children);
        mixed scope=({box,children});
        if(parse_sentence("touch gem",0,scope)!=1 || "/verb"->selected()!=gem) return ({0});
        return ({parse_sentence("touch gem",0,scope)==1 && "/verb"->selected()==find_object("/second")});
    "#).await;
}

#[tokio::test]
async fn policy_is_fresh_for_each_rule_attempt() {
    check(&[("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("touch","OBJ"); parse_add_rule("touch","OBJ"); }
        int can_touch_obj() { "/box"->set(1,1); return 1; }
        void do_touch_obj() {}
    "#)],"",r#"
        return ({parse_sentence("touch gem",0,({box,({gem})}))==1 && box->counts()[0]==2
            && box->counts()[1]==2 && "/secure/master"->count()==0});
    "#).await;
}

#[tokio::test]
async fn destructed_parents_cannot_expose_supplied_children() {
    for hook in ["inventory_visible", "inventory_accessible"] {
        let other = if hook == "inventory_visible" {
            "inventory_accessible"
        } else {
            "inventory_visible"
        };
        let box_code = format!(
            r#"
            int {hook}() {{ destruct(this_object()); return 1; }}
            int {other}() {{ return 1; }}
            void move(object dest) {{ move_object(dest); }}
        "#
        );
        check(
            &[
                ("/box.c", &box_code),
                (
                    "/verb.c",
                    r#"
            void create() { parse_init(); parse_add_rule("see","OBJ:v"); }
            void do_see_obj() { throw("dead parent action"); }
        "#,
                ),
            ],
            "",
            r#"return ({parse_sentence("see gem",0,({box,({gem})}))=="error:7"});"#,
        )
        .await;
    }
}

#[tokio::test]
async fn hook_exceptions_propagate_before_handlers() {
    check(&[("/box.c",r#"
        int inventory_visible() { throw("scope failure"); return 1; }
        void move(object dest) { move_object(dest); }
    "#),("/verb.c",r#"
        void create() { parse_init(); parse_add_rule("see","OBJ:v"); }
        void do_see_obj() { throw("action"); }
    "#)],"",r#"
        mixed err=catch(parse_sentence("see gem",0,({box,({gem})})));
        return ({stringp(err) && strindex(err,"scope failure")>=0 && gem->count()==0 && "/secure/master"->count()==0});
    "#).await;
}

#[tokio::test]
async fn valid_parent_cycles_and_excessive_depth_are_bounded() {
    check(
        &[],
        "",
        r#"
        mixed *cycle=({box,nil}); cycle[1]=cycle;
        mixed err=catch(parse_sentence("missing",0,cycle));
        if(!stringp(err) || strindex(err,"nests deeper than 20")<0) return ({0});
        mixed scope=({gem});
        for(int i=0;i<20;i++) scope=({box,scope});
        if(catch(parse_sentence("missing",0,scope))) return ({0});
        scope=({box,scope}); err=catch(parse_sentence("missing",0,scope));
        return ({stringp(err) && strindex(err,"nests deeper than 20")>=0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn ordinary_command_dispatch_obeys_visible_modifiers() {
    check(
        &[(
            "/verb.c",
            r#"
        int actions;
        void create() { parse_init(); parse_add_rule("see","OBJ:v"); }
        void do_see_obj() { actions++; }
        int count() { return actions; }
    "#,
        )],
        "",
        r#"return ({command("see gem")==1 && "/verb"->count()==1});"#,
    )
    .await;
}
