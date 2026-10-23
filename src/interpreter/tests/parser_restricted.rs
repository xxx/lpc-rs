//! Caller-owned parsing with explicit actors, argument results and optional actions.

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::run_with;
use crate::interpreter::lpc_ref::LpcRef;

const MASTER: &str = r#"
    int reports, quiet; mixed *last; object *users=({});
    int valid_destruct() { return 1; }
    string parse_command_all_word() { return "all"; }
    int parse_command_numeral(string word) { return word=="second" ? -2 : 0; }
    object *parse_command_users() { return users; }
    void users(object *value) { users=value; }
    void quiet() { quiet=1; }
    mixed parser_error_message(int kind, object ob, mixed arg, int flag) {
        reports++; last=({kind,ob,arg,flag,this_player(),previous_object(),origin(),query_verb(),query_command()});
        return quiet ? 0 : "error:"+kind;
    }
    int reports() { return reports; }
    mixed *last() { return last; }
"#;
const ACTOR: &str = r#"
    void create() { enable_commands(); }
    void move(object dest) { move_object(dest); }
    string *parse_command_id_list() { return ({"actor"}); }
"#;
const ITEM: &str = r#"
    void move(object dest) { move_object(dest); }
    string *parse_command_id_list() { return ({"gem"}); }
    string *parse_command_plural_id_list() { return ({"gems"}); }
    int direct_verb_rule() { return 1; }
    int indirect_verb_rule() { return 1; }
"#;
const ENTRY: &str = r#"
    mixed inspect(object actor, string line) { return parse_my_rules(actor,line); }
    mixed submit(object actor, string line, int execute) { return parse_my_rules(actor,line,execute); }
    string *rules() { return query_parser_rules(); }
"#;

async fn check(owner: &str, objects: &[(&str, &str)], extra: &str, body: &str) {
    let owner = format!("{ENTRY}\n{owner}");
    let mut sources = vec![("/owner.c", owner.as_str())];
    if !objects.iter().any(|(path, _)| *path == "/actor.c") {
        sources.push(("/actor.c", ACTOR));
    }
    sources.extend_from_slice(objects);
    let main = format!(
        r#"
        {extra}
        mixed *create() {{
            enable_commands(); set_this_player(this_object());
            object actor=find_object("/actor"), owner=find_object("/owner");
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
async fn omitted_and_zero_flags_return_arguments_without_an_action_handler() {
    check(
        r#"void create(){parse_init();parse_add_rule("wave","at STR");}"#,
        &[],
        "",
        r#"
        mixed *a=owner->inspect(actor,"wave at warmly");
        mixed *b=owner->submit(actor,"wave at warmly",0);
        return ({sizeof(a)==5 && a[0]=="wave" && a[1]=="at STR" && a[2]=="warmly"
            && a[3]=="wave" && a[4]=="warmly" && sizeof(b)==5 && b[2]==a[2]
            && this_player()==this_object() && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn returning_arguments_never_looks_up_an_action_tier() {
    check(
        r#"
        void create(){parse_init();parse_add_rule("wave","at STR");}
        void do_wave_at_str(){throw("specific action");}
        void do_wave_word_str(){throw("word action");}
        void do_verb_word_str(){throw("shape action");}
        void do_verb_rule(){throw("generic action");}
    "#,
        &[],
        "",
        r#"return ({arrayp(owner->inspect(actor,"wave at warmly"))});"#,
    )
    .await;
}

#[tokio::test]
async fn returned_text_keeps_unicode_spacing_and_generic_layout() {
    check(
        r#"void create(){parse_init();parse_add_rule("wave","WRD  STR at STR");}"#,
        &[],
        "",
        r#"
        mixed *a=owner->inspect(actor,"wave Éclair! very\t warmly at 遠い塔");
        return ({sizeof(a)==9 && a[0]=="wave" && a[1]=="WRD  STR at STR"
            && a[2]=="Éclair!" && a[3]=="very\t warmly" && a[4]=="遠い塔"
            && a[5]=="wave" && a[6]==a[2] && a[7]==a[3] && a[8]==a[4]});
    "#,
    )
    .await;
}

#[tokio::test]
async fn empty_rules_return_base_rule_and_typed_verb() {
    check(r#"void create(){parse_init();parse_add_rule("smile","");parse_add_synonym("beam at","smile");}"#, &[], "", r#"
        mixed *a=owner->inspect(actor,"beam  at");
        return ({sizeof(a)==3 && a[0]=="smile" && a[1]=="" && a[2]=="beam  at"});
    "#).await;
}

#[tokio::test]
async fn longer_and_same_name_rules_on_other_owners_are_not_tried() {
    let other = r#"
        void create(){parse_init();parse_add_rule("wave at","STR");parse_add_rule("wave","STR");}
        int can_verb_rule(){throw("unrelated permission");return 0;}
        void do_verb_rule(){throw("unrelated action");}
    "#;
    check(
        r#"
        void create(){parse_init();}
        void register(){parse_add_rule("wave","STR");}
        void do_verb_rule(){ }
    "#,
        &[("/other.c", other)],
        "",
        r#"
        owner->register();
        mixed *a=owner->inspect(actor,"wave at warmly");
        return ({a[1]=="STR" && a[2]=="at warmly" && owner->submit(actor,"wave at warmly",1)==1});
    "#,
    )
    .await;
}

#[tokio::test]
async fn another_owners_verb_counts_as_no_verb_and_runs_no_scope_callbacks() {
    let other = r#"void create(){parse_init();parse_add_rule("wave","OBJ");}"#;
    let box_code =
        format!(r#"{ITEM} int inventory_visible(){{throw("unexpected scope");return 0;}}"#);
    check(
        "",
        &[("/other.c", other), ("/box.c", &box_code)],
        "",
        r#"
        "/box"->move(actor);
        return ({owner->inspect(actor,"wave gem")==0 && owner->submit(actor,"wave gem",1)==0
            && sizeof(owner->rules())==0 && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn other_owners_cannot_handle_or_supply_a_failure_after_own_refusal() {
    let other = r#"
        void create(){parse_init();parse_add_rule("wave","STR");}
        string can_verb_rule(){throw("other owner checked");return "other";}
    "#;
    check(
        r#"
        void create(){parse_init();parse_add_rule("wave","STR");}
        int can_verb_rule(){return 0;}
    "#,
        &[("/other.c", other)],
        "",
        r#"
        return ({owner->inspect(actor,"wave warmly")==-2 && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}

async fn execution_tier(tier: usize) {
    let names = ["wave_at_str", "wave_word_str", "verb_word_str", "verb_rule"];
    let expected = [
        r#"sizeof(args)==2 && args[0]=="warmly" && args[1]=="warmly""#,
        r#"sizeof(args)==3 && args[0]=="at" && args[1]=="warmly" && args[2]=="warmly""#,
        r#"sizeof(args)==5 && args[0]=="wave" && args[1]=="at" && args[2]=="warmly" && args[3]=="wave" && args[4]=="warmly""#,
        r#"sizeof(args)==5 && args[0]=="wave" && args[1]=="at STR" && args[2]=="warmly" && args[3]=="wave" && args[4]=="warmly""#,
    ];
    let mut source = format!(
        r#"
        int calls;
        void create(){{parse_init();parse_add_rule("wave","at STR");}}
        int calls(){{return calls;}}
        mixed *do_{}(mixed args...){{
            if(!({}))throw("action arguments");
            if(this_player()!=find_object("/actor") || previous_object()!=this_player() || origin()!="driver")throw("action context");
            calls++;return ({{"ignored action result"}});
        }}
    "#,
        names[tier], expected[tier]
    );
    for name in &names[tier + 1..] {
        source.push_str(&format!("void do_{name}(){{throw(\"wrong tier\");}}"));
    }
    check(&source,&[],"",r#"
        return ({owner->submit(actor,"wave at warmly",1)==1 && owner->submit(actor,"wave at warmly",-7)==1
            && owner->calls()==2 && this_player()==this_object()});
    "#).await;
}

#[tokio::test]
async fn execution_uses_the_specific_action_layout() {
    execution_tier(0).await;
}
#[tokio::test]
async fn execution_uses_the_literal_word_action_layout() {
    execution_tier(1).await;
}
#[tokio::test]
async fn execution_uses_the_generic_shape_action_layout() {
    execution_tier(2).await;
}
#[tokio::test]
async fn execution_uses_the_generic_rule_action_layout() {
    execution_tier(3).await;
}

#[tokio::test]
async fn missing_actions_refuse_only_when_execution_is_requested() {
    check(r#"void create(){parse_init();parse_add_rule("wave","STR");}"#, &[], "", r#"
        return ({arrayp(owner->inspect(actor,"wave warmly")) && owner->submit(actor,"wave warmly",1)==-2
            && "/secure/master"->reports()==0});
    "#).await;
}

#[tokio::test]
async fn missing_actions_allow_later_rules_in_execution_mode() {
    check(r#"
        int actions;void create(){parse_init();parse_add_rule("wave","STR");parse_add_rule("wave","at STR");}
        void do_wave_at_str(){actions++;}int actions(){return actions;}
    "#, &[], "", r#"
        mixed *a=owner->inspect(actor,"wave at warmly");
        return ({a[1]=="STR" && a[2]=="at warmly" && owner->actions()==0
            && owner->submit(actor,"wave at warmly",1)==1 && owner->actions()==1});
    "#).await;
}

#[tokio::test]
async fn denied_splits_retry_with_effects_before_a_later_rule() {
    check(r#"
        string *seen=({});void create(){parse_init();parse_add_rule("wave","STR at STR");parse_add_rule("wave","STR");}
        mixed can_wave_str_at_str(string left){seen+=({left});return left=="warmly" ? 1 : "retry";}
        int can_wave_str(){throw("later rule tried");return 0;}
        string trace(){return implode(seen,"|");}
    "#, &[], "", r#"
        mixed *a=owner->inspect(actor,"wave warmly at dusk at tower");
        return ({a[2]=="warmly" && a[3]=="dusk at tower" && owner->trace()=="warmly at dusk|warmly"
            && "/secure/master"->reports()==0});
    "#).await;
}

#[tokio::test]
async fn longer_phrases_refuse_before_shorter_owned_rules_succeed() {
    check(r#"
        int calls;void create(){parse_init();parse_add_rule("wave","STR");parse_add_rule("wave at","STR");}
        int can_verb_rule(string verb){calls++;return verb=="wave";}
        int calls(){return calls;}
    "#, &[], "", r#"
        mixed *a=owner->inspect(actor,"wave at warmly");
        return ({a[0]=="wave" && a[2]=="at warmly" && owner->calls()==2});
    "#).await;
}

#[tokio::test]
async fn repeated_calls_have_fresh_silent_failure_codes() {
    check(r#"
        void create(){parse_init();parse_add_rule("wave","at OBJ");parse_add_rule("refuse","STR");}
        int can_refuse_str(){return 0;}
    "#, &[], "", r#"
        "/secure/master"->quiet();
        return ({owner->inspect(actor,"unknown")==0 && owner->inspect(actor,"wave")==-1
            && owner->inspect(actor,"wave at missing")==-3 && owner->inspect(actor,"refuse warmly")==-2
            && owner->inspect(actor,"unknown")==0});
    "#).await;
}

#[tokio::test]
async fn deferred_errors_use_the_explicit_actor_and_do_not_deliver_messages() {
    check(
        r#"
        void create(){parse_init();parse_add_rule("wave","STR");parse_add_rule("wave","at STR");}
        string can_wave_str(){return "first";}
        string can_wave_at_str(){return "second";}
    "#,
        &[],
        r#"void catch_tell(string text){throw("delivered to caller");}"#,
        r#"
        mixed result=owner->inspect(actor,"wave at warmly");
        mixed *a="/secure/master"->last();
        return ({result=="error:6" && "/secure/master"->reports()==1 && a[2]=="first"
            && a[4]==actor && a[5]==actor && a[6]=="driver" && a[7]=="wave"
            && a[8]=="wave at warmly" && this_player()==this_object() && !query_command()});
    "#,
    )
    .await;
}

#[tokio::test]
async fn permission_exceptions_stop_retries_but_keep_writes() {
    check(r#"
        int calls;void create(){parse_init();parse_add_rule("wave","STR");parse_add_rule("wave","at STR");}
        int can_wave_str(){calls++;throw("stop here");return 0;}
        int can_wave_at_str(){throw("later rule");return 0;}
        int calls(){return calls;}
    "#, &[], "", r#"
        mixed error=catch(owner->inspect(actor,"wave at warmly"));
        return ({stringp(error) && strindex(error,"stop here")>=0 && owner->calls()==1
            && "/secure/master"->reports()==0 && this_player()==this_object() && !query_verb()});
    "#).await;
}

#[tokio::test]
async fn action_exceptions_only_run_in_execution_mode() {
    check(
        r#"
        int calls;void create(){parse_init();parse_add_rule("wave","STR");}
        void do_wave_str(){calls++;throw("action error");}int calls(){return calls;}
    "#,
        &[],
        "",
        r#"
        if(!arrayp(owner->inspect(actor,"wave warmly")) || owner->calls())return ({0});
        mixed error=catch(owner->submit(actor,"wave warmly",1));
        return ({stringp(error) && strindex(error,"action error")>=0 && owner->calls()==1
            && this_player()==this_object() && !query_command()});
    "#,
    )
    .await;
}

#[tokio::test]
async fn explicit_actor_controls_scope_vocabulary_and_permissions() {
    let actor_code = format!(
        r#"{ACTOR}
        int checks;
        int can_verb_rule(){{
            if(this_player()!=this_object() || previous_object()!=this_object() || origin()!="driver")throw("actor context");
            checks++;return 1;
        }}
        int checks(){{return checks;}}
    "#
    );
    let item = r#"
        int ids, checks;
        void move(object dest){move_object(dest);}
        void context(string how,object player,object caller){
            if(how!="driver" || player!=find_object("/actor") || caller!=player
                || query_verb()!="wave" || query_command()!="wave at gem")throw("item context");
        }
        string *parse_command_id_list(){context(origin(),this_player(),previous_object());ids++;return ({"gem"});}
        int direct_wave_at_obj(){context(origin(),this_player(),previous_object());checks++;return 1;}
        int correct(){return ids>0 && checks==2;}
    "#;
    check(
        r#"
        void create(){parse_init();parse_add_rule("wave","at OBJ");}
        int can_verb_rule(){throw("owner should not replace actor permission");return 0;}
    "#,
        &[("/actor.c", &actor_code), ("/gem.c", item)],
        "",
        r#"
        "/gem"->move(actor);
        mixed *a=owner->inspect(actor,"wave at gem");
        return ({a[2]==find_object("/gem") && "/gem"->correct() && actor->checks()==1
            && this_player()==this_object() && previous_object()==nil});
    "#,
    )
    .await;
}

#[tokio::test]
async fn nested_visibility_uses_the_actor_and_preserves_access_policy() {
    let box_code = r#"
        void move(object dest){move_object(dest);}
        int inventory_visible(){
            if(this_player()!=find_object("/actor") || previous_object()!=this_player() || origin()!="driver")throw("scope context");return 1;
        }
        int inventory_accessible(){return 0;}
    "#;
    check(r#"void create(){parse_init();parse_add_rule("wave","at OBJ");parse_add_rule("see","at OBJ:v");}"#,
        &[("/box.c",box_code),("/gem.c",ITEM)], "", r#"
        "/box"->move(actor);"/gem"->move(find_object("/box"));
        if(owner->inspect(actor,"wave at gem")!="error:3")return ({0});
        mixed *a=owner->inspect(actor,"see at gem");
        return ({a[1]=="at OBJ:v" && a[2]==find_object("/gem")});
    "#).await;
}

#[tokio::test]
async fn remote_livings_are_available_to_the_explicit_actor() {
    let remote = format!(r#"{ACTOR} int direct_verb_rule(){{return 1;}}"#);
    check(
        r#"void create(){parse_init();parse_add_rule("wave","LIV");}"#,
        &[("/remote.c", &remote)],
        "",
        r#"
        "/secure/master"->users(({find_object("/remote")}));
        mixed *a=owner->inspect(actor,"wave actor");
        return ({a[2]==find_object("/remote") && a[4]=="actor"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn plural_results_include_permitted_objects_and_plain_refusals() {
    let refused = r#"
        void move(object dest){move_object(dest);}
        string *parse_command_id_list(){return ({"gem"});}
        string *parse_command_plural_id_list(){return ({"gems"});}
        string direct_verb_rule(){return "too heavy";}
    "#;
    check(r#"void create(){parse_init();parse_add_rule("wave","OBS");}"#,
        &[("/yes.c",ITEM),("/no.c",refused)], "", r#"
        "/yes"->move(actor);"/no"->move(actor);
        mixed *a=owner->inspect(actor,"wave gems");
        return ({sizeof(a)==5 && arrayp(a[2]) && sizeof(a[2])==2 && a[2][0]==find_object("/yes")
            && a[2][1]=="too heavy" && a[3]=="wave" && a[4]=="gems" && "/secure/master"->reports()==0});
    "#).await;
}

#[tokio::test]
async fn relation_results_keep_both_objects_and_source_phrases() {
    let actor_code = format!(r#"{ACTOR} int indirect_verb_rule(){{return 1;}}"#);
    check(
        r#"void create(){parse_init();parse_add_rule("give","OBJ to LIV");}"#,
        &[("/gem.c", ITEM), ("/actor.c", &actor_code)],
        "",
        r#"
        "/gem"->move(actor);
        mixed *a=owner->inspect(actor,"give gem to actor");
        return ({sizeof(a)==7 && a[0]=="give" && a[1]=="OBJ to LIV" && a[2]==find_object("/gem")
            && a[3]==actor && a[4]=="give" && a[5]=="gem" && a[6]=="actor"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn two_plural_slots_keep_distinct_arrays_even_with_the_same_members() {
    check(
        r#"void create(){parse_init();parse_add_rule("pair","OBS with OBS");}"#,
        &[("/gem.c", ITEM)],
        "",
        r#"
        "/gem"->move(actor);mixed *a=owner->inspect(actor,"pair gems with gems");
        a[2][0]=nil;
        return ({sizeof(a)==7 && arrayp(a[2]) && a[2][0]==nil && a[3][0]==find_object("/gem")
            && a[5]=="gems" && a[6]=="gems"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn nested_owned_parses_restore_the_outer_actor_and_command() {
    let other = format!(
        r#"{ENTRY}
        void create(){{parse_init();parse_add_rule("inner","STR");}}
        int can_verb_rule(){{
            if(this_player()!=find_object("/main") || previous_object()!=this_player()
                || query_command()!="inner text" || query_verb()!="inner" || origin()!="driver")throw("inner context");return 1;
        }}
    "#
    );
    check(r#"
        void create(){parse_init();parse_add_rule("outer","STR");}
        int can_verb_rule(){
            object actor=this_player();
            mixed *a="/other"->inspect(find_object("/main"),"inner text");
            if(a[0]!="inner" || this_player()!=actor || previous_object()!=actor || origin()!="driver"
                || query_verb()!="outer" || query_command()!="outer words")throw("outer context");return 1;
        }
    "#, &[("/other.c",&other)], "", r#"
        mixed *a=owner->inspect(actor,"outer words");
        return ({a[0]=="outer" && a[2]=="words" && this_player()==this_object() && !query_verb()});
    "#).await;
}

#[tokio::test]
async fn restricted_parsing_inside_ordinary_commands_restores_the_command_frame() {
    for entry in ["parse_sentence", "command"] {
        let outer = r#"
            int correct;
            void create(){parse_init();parse_add_rule("outer","STR");}
            void do_outer_str(){
                object actor=this_player();
                mixed *a="/owner"->inspect(find_object("/actor"),"inner words");
                correct=a[0]=="inner" && this_player()==actor && previous_object()==actor
                    && query_verb()=="outer" && query_command()=="outer text" && origin()=="driver";
            }
            int correct(){return correct;}
        "#;
        check(r#"void create(){parse_init();parse_add_rule("inner","STR");}"#,
            &[("/outer.c",outer)], "", &format!(r#"
            return ({{{entry}("outer text")==1 && "/outer"->correct() && this_player()==this_object() && !query_command()}});
        "#)).await;
    }
}

#[tokio::test]
async fn recursion_uses_the_command_depth_limit_and_unwinds_cleanly() {
    check(r#"
        void create(){parse_init();parse_add_rule("loop","");parse_add_rule("ok","");}
        int can_loop(){parse_my_rules(this_player(),"loop");return 1;}
    "#, &[], "", r#"
        mixed error=catch(owner->inspect(actor,"loop"));
        return ({stringp(error) && strindex(error,"parse_my_rules: nesting deeper than 16")>=0
            && arrayp(owner->inspect(actor,"ok")) && this_player()==this_object() && !query_verb()});
    "#).await;
}

#[tokio::test]
async fn owner_destruction_during_permission_cannot_return_a_success() {
    for execute in [0, 1] {
        check(r#"
            void create(){parse_init();parse_add_rule("wave","STR");}
            int can_verb_rule(){destruct(this_object());return 1;}
            void do_verb_rule(){throw("dead owner action");}
        "#, &[], "", &format!(r#"
            return ({{owner->submit(actor,"wave warmly",{execute})==-2 && !owner && "/secure/master"->reports()==0}});
        "#)).await;
    }
}

#[tokio::test]
async fn destruction_during_recheck_uses_normal_dead_object_values() {
    let gem = r#"
        int checks;void move(object dest){move_object(dest);}
        string *parse_command_id_list(){return ({"gem"});}
        int direct_verb_rule(){if(++checks==2)destruct(this_object());return 1;}
    "#;
    check(
        r#"void create(){parse_init();parse_add_rule("wave","OBJ");}"#,
        &[("/gem.c", gem)],
        "",
        r#"
        "/gem"->move(actor);mixed *a=owner->inspect(actor,"wave gem");
        return ({sizeof(a)==5 && a[2]==nil && a[4]=="gem"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn removal_during_permission_affects_later_calls_not_the_active_snapshot() {
    check(
        r#"
        void create(){parse_init();parse_add_rule("wave","STR");parse_add_synonym("greet","wave");}
        int can_verb_rule(){parse_remove("wave");return 1;}
    "#,
        &[],
        "",
        r#"
        mixed *a=owner->inspect(actor,"greet warmly");
        return ({a[0]=="wave" && a[3]=="greet" && sizeof(owner->rules())==0
            && owner->inspect(actor,"greet warmly")==0 && owner->inspect(actor,"wave warmly")==0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn returned_arrays_do_not_change_registration_or_later_results() {
    check(
        r#"void create(){parse_init();parse_add_rule("wave","STR");}"#,
        &[],
        "",
        r#"
        mixed *a=owner->inspect(actor,"wave warmly");a[0]="different";a[2]="changed";
        mixed *b=owner->inspect(actor,"wave softly");
        return ({b[0]=="wave" && b[2]=="softly" && owner->rules()[0]=="wave STR"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn invalid_actors_are_rejected_before_any_parser_callbacks() {
    check(r#"
        void create(){parse_init();parse_add_rule("wave","STR");}
        int can_verb_rule(){throw("unexpected permission");return 0;}
    "#, &[], "", r#"
        object dead=clone_object("/actor");destruct(dead);
        foreach(mixed invalid:({nil,owner,dead})){
            mixed error=catch(owner->inspect(invalid,"wave warmly"));
            if(!stringp(error) || strindex(error,"parse_my_rules: the actor is not a living")<0)return ({0});
        }
        return ({"/secure/master"->reports()==0});
    "#).await;
}

#[tokio::test]
async fn dynamic_line_and_execution_flag_types_are_checked() {
    check(
        r#"
        mixed bad_line(object actor){mixed value=17;return parse_my_rules(actor,value);}
        mixed bad_flag(object actor){mixed value="yes";return parse_my_rules(actor,"wave",value);}
    "#,
        &[],
        "",
        r#"
        mixed line=catch(owner->bad_line(actor)), flag=catch(owner->bad_flag(actor));
        return ({stringp(line) && strindex(line,"the line must be a string")>=0
            && stringp(flag) && strindex(flag,"execute must be an int")>=0});
    "#,
    )
    .await;
}

#[tokio::test]
async fn efun_pointers_parse_the_pointer_owners_rules() {
    check(
        r#"
        void create(){parse_init();parse_add_rule("wave","STR");}
        function pointer(){return parse_my_rules;}
    "#,
        &[],
        "",
        r#"
        function f=owner->pointer();mixed *a=f(actor,"wave warmly");
        return ({a[0]=="wave" && a[2]=="warmly"});
    "#,
    )
    .await;
}

#[tokio::test]
async fn argument_mode_obeys_the_parse_count_cap() {
    let input = (0..41)
        .map(|i| format!("w{i}"))
        .collect::<Vec<_>>()
        .join(" at ");
    check(
        r#"
        int calls;
        void create(){parse_init();parse_add_rule("cast","STR at STR");}
        mixed can_cast_str_at_str(string left){calls++;return left=="w0" ? 1 : "not this split";}
        int calls(){return calls;}
    "#,
        &[],
        "",
        &format!(
            r#"
        mixed result=owner->inspect(actor,"cast {input}");
        return ({{stringp(result) && owner->calls()==32 && "/secure/master"->reports()==1}});
    "#
        ),
    )
    .await;
}

#[tokio::test]
async fn an_explicit_actor_works_without_an_ambient_player() {
    check(r#"
        void create(){parse_init();parse_add_rule("wave","STR");}
        int can_verb_rule(){return this_player()==find_object("/actor") && previous_object()==this_player();}
    "#, &[], "", r#"
        set_this_player(nil);
        return ({arrayp(owner->inspect(actor,"wave warmly")) && !this_player() && !query_command()});
    "#).await;
}

#[tokio::test]
async fn consumers_can_slice_or_spread_the_returned_argument_array() {
    check(r#"
        mixed *seen; int sent;
        void create(){parse_init();parse_add_rule("wave","WRD LIV");}
        int direct_verb_rule(){return 1;}
        mixed *build(string verb,string rule,mixed *args){seen=({verb,rule,args});return seen;}
        mixed *spread(string verb,string rule,mixed args...){return build(verb,rule,args);}
        mixed local_soul(object actor,string line){
            mixed result=parse_my_rules(actor,line,0);
            if(!result || intp(result) || stringp(result))return nil;
            return build(result[0],result[1],result[2..]);
        }
        mixed remote_soul(object actor,string line){
            mixed result=parse_my_rules(actor,line,0);
            if(!result || intp(result) || stringp(result))return nil;
            return spread(result...);
        }
        void do_verb_rule(){sent++;}
        int sent(){return sent;}
    "#, &[], "", r#"
        mixed *a=owner->local_soul(actor,"wave warmly actor");
        mixed *b=owner->remote_soul(actor,"wave warmly actor");
        foreach(mixed *result:({a,b})){
            mixed *args=result[2];
            if(result[0]!="wave" || result[1]!="WRD LIV" || sizeof(args)!=5
                || args[0]!="warmly" || args[1]!=actor || args[2]!="wave"
                || args[3]!="warmly" || args[4]!="actor")return ({0});
        }
        return ({!owner->local_soul(actor,"wave") && !owner->remote_soul(actor,"unknown") && owner->sent()==0});
    "#).await;
}

#[tokio::test]
async fn an_inherited_wrapper_parses_the_inheriting_objects_rules() {
    let root = tempfile::TempDir::new().unwrap();
    std::fs::write(root.path().join("base.c"), ENTRY).unwrap();
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .lib_dir(root.path().to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap();
        let objects = [
            ("/actor.c", ACTOR),
            (
                "/owner.c",
                r#"
                inherit "/base";
                void create(){parse_init();parse_add_rule("wave","STR");}
            "#,
            ),
            (
                "/other.c",
                r#"
                inherit "/base";
                void create(){parse_init();parse_add_rule("wave","");}
            "#,
            ),
        ];
        let result = run_with(
            config,
            MASTER,
            &objects,
            r#"
            mixed *create(){
                object actor=find_object("/actor");
                mixed *a="/owner"->inspect(actor,"wave warmly");
                mixed *b="/other"->inspect(actor,"wave");
                return ({a[1]=="STR" && a[2]=="warmly" && b[1]=="" && sizeof(b)==3});
            }
        "#,
        )
        .await;
        assert_eq!(result, vec![LpcRef::from(1)], "{mode:?}");
    }
}

#[tokio::test]
async fn the_ulib_helper_returns_the_accepted_text_split_without_printing() {
    let owner = include_str!("../../../ulib/examples/verb.c").replace("#if 0\n", "#if 1\n");
    check(
        &owner,
        &[],
        r#"
        string heard="";
        void catch_tell(string message){heard+=message;}
    "#,
        r#"
        mixed *a=owner->match_text(this_object(),"cast spark at dusk at tower");
        return ({sizeof(a)==7 && a[0]=="cast" && a[1]=="STR at STR"
            && a[2]=="spark" && a[3]=="dusk at tower" && a[4]=="cast"
            && a[5]==a[2] && a[6]==a[3] && heard=="" && "/secure/master"->reports()==0});
    "#,
    )
    .await;
}
