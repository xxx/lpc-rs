use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run, run_with, s};
use crate::{
    compiler::Compiler,
    interpreter::lpc_ref::LpcRef,
    test_support::{lib_holding, temp_lib_config, test_config},
};

#[tokio::test]
async fn scalar_replacement_rebinds_the_owner_and_preserves_aliases() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="aé😀z", old=text;
            int result=(text[1]='中');
            text[-2]='x';
            text[<1]=128512;
            return ({old,text,result,sizeof(text)});
        }
    "#
        )
        .await,
        vec![s("aé😀z"), s("a中x😀"), 20013.into(), 4.into()]
    );
}

#[tokio::test]
async fn combining_marks_joiners_and_nuls_are_individual_scalars() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="é👩‍👧";
            text[1]='!'; text[3]=0; text[<1]='中';
            return ({sizeof(text),text[0..2],text[3],text[4],strindex(text,"中")});
        }
    "#
        )
        .await,
        vec![LpcRef::from(5), s("e!👩"), 0.into(), 20013.into(), 4.into()]
    );
}

#[tokio::test]
async fn indexed_and_member_owners_update_shared_slots_only() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        class Box { string text; }
        mixed *create() {
            class Box box=new(class Box,text:"abc");
            string *rows=({"abc"}), *alias=rows;
            mapping record=(["text":"abc"]);
            string old=rows[0];
            rows[<1][<2]='X'; box.text[-1]='Y'; record["text"][0]='Z';
            return ({old,alias[0],box.text,record["text"]});
        }
    "#
        )
        .await,
        vec![s("abc"), s("aXc"), s("abY"), s("Zbc")]
    );
}

#[tokio::test]
async fn closures_and_reference_parameters_rebind_captured_strings() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        void local_only(string text){text[0]='L';}
        void by_ref(string ref text){text[0]='R';}
        mixed *create(){
            string text="abc";
            function change=(: text[1]='X' :);
            change(); local_only(text);
            string old=text; by_ref(ref text);
            return ({old,text});
        }
    "#
        )
        .await,
        vec![s("aXc"), s("RXc")]
    );
}

#[tokio::test]
async fn operands_run_once_in_rhs_owner_index_order_across_yields() {
    assert_eq!(run("", &[], r#"
        int order; string *rows=({"abc"});
        int rhs(){order=order*10+1;return 'X';}
        string *receiver(){order=order*10+2;return rows;}
        int key(){order=order*10+3;return 0;}
        int index(){int sum;for(int i=0;i<3000;i++)sum+=i;order=order*10+4;return sum>0;}
        mixed *create(){int result=(receiver()[key()][index()]=rhs());return ({order,result,rows[0]});}
    "#).await, vec![LpcRef::from(1234),88.into(),s("aXc")]);
}

#[tokio::test]
async fn later_operands_cannot_redirect_source_rhs_receiver_or_key() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        string *rows=({"abc","def"}); int key=-1, rhs='X'; string text="abc";
        int change(){rows=({"new"});key=0;rhs='Y';text="other";return 1;}
        mixed *create(){
            string *old=rows;
            rows[key][change()]=rhs;
            text="abc";text[change()]='Z';
            return ({old[1],rows[0],text,rhs});
        }
    "#
        )
        .await,
        vec![s("dXf"), s("new"), s("aZc"), 89.into()]
    );
}

#[tokio::test]
async fn member_capture_keeps_the_original_instance() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        class Box {string text;}
        class Box box=new(class Box,text:"abc");
        int change(){box=new(class Box,text:"new");return 1;}
        mixed *create(){class Box old=box;box.text[change()]='X';return ({old.text,box.text});}
    "#
        )
        .await,
        vec![s("aXc"), s("new")]
    );
}

#[tokio::test]
async fn mixed_dispatch_preserves_each_collection_and_assignment_result() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        assert_eq!(
            run_with(
                config,
                "",
                &[],
                r#"
            mixed *create(){
                mixed text="abc", a=({1,2}), m=([]);
                int x=(text[0]=a[1]=m[-1]='X');
                m["name"]=text;
                return ({text,a[0],a[1],m[-1],m["name"],x});
            }
        "#
            )
            .await,
            vec![
                s("Xbc"),
                1.into(),
                88.into(),
                88.into(),
                s("Xbc"),
                88.into()
            ]
        );
    }
}

#[tokio::test]
async fn one_mixed_store_handles_changing_reference_target_types() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed store(mixed ref target, mixed key, mixed value) {
                return target[key] = value;
            }
            mixed *create() {
                mixed target = "abc";
                mixed scalar = store(ref target, 1, 'é');
                string text = target;
                target = ({1, 2});
                mixed array = target;
                mixed item = store(ref target, -1, "item");
                target = ([]);
                mixed record = target;
                mixed entry = store(ref target, "key", array);
                target = "xyz";
                store(ref target, 0, 'Z');
                return ({text, scalar, array[1], item,
                         record["key"] == array, entry == array, target});
            }
        "#
        )
        .await,
        vec![
            s("aéc"),
            233.into(),
            s("item"),
            s("item"),
            1.into(),
            1.into(),
            s("Zyz")
        ]
    );
}

#[tokio::test]
async fn mixed_mutable_targets_do_not_rebind_a_reassigned_parent_slot() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed target=({1});
        mapping owner=(["slot":({2})]);
        class Box {mixed slot;} class Box box=new(class Box,slot:(["x":3]));
        int change(){target="changed";return 0;}
        int change_slot(){owner["slot"]="changed";return 0;}
        string change_member(){box.slot="changed";return "x";}
        mixed *create(){
            mixed a=target,b=owner["slot"],c=box.slot;
            target[change()]=7; owner["slot"][change_slot()]=8; box.slot[change_member()]=9;
            return ({target,owner["slot"],box.slot,a[0],b[0],c["x"]});
        }
    "#
        )
        .await,
        vec![
            s("changed"),
            s("changed"),
            s("changed"),
            7.into(),
            8.into(),
            9.into()
        ]
    );
}

#[tokio::test]
async fn temporary_mixed_arrays_and_mappings_still_accept_stores() {
    assert_eq!(run("", &[], r#"
        mixed a=({1}),m=([]);
        mixed array(){return a;} mixed map(){return m;} mixed text(){return "abc";}
        mixed *create(){array()[0]=7;map()["key"]=8;return ({a[0],m["key"],stringp(catch(text()[0]='X'))});}
    "#).await,vec![LpcRef::from(7),8.into(),1.into()]);
}

#[tokio::test]
async fn invalid_indices_never_append_clip_or_wrap() {
    for index in [
        "3",
        "-4",
        "<0",
        "<4",
        "<-1",
        "9223372036854775807",
        "-9223372036854775808",
        "<-9223372036854775808",
    ] {
        assert_eq!(run("",&[],&format!(r#"
            mixed *create(){{string text="aé😀";mixed err=catch(text[{index}]='X');return ({{stringp(err),text}});}}
        "#)).await,vec![LpcRef::from(1),s("aé😀")],"{index}");
    }
    assert_eq!(run("",&[],r#"
        mixed *create(){string text="";return ({stringp(catch(text[0]='X')),stringp(catch(text[<1]='X')),text});}
    "#).await,vec![LpcRef::from(1),1.into(),s("")]);
}

#[tokio::test]
async fn invalid_scalars_and_dynamic_types_leave_strings_unchanged() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        assert_eq!(run_with(config,"",&[],r#"
            mixed *create(){
                string text="abc"; int errors;
                foreach(mixed value in ({-1,55296,57343,1114112,9223372036854775807,4294967296,"X",1.0,({}),([]),to_bytes("x","UTF-8")})){
                    if(stringp(catch(text[1]=value)))errors++;
                }
                mixed bad="index";
                if(stringp(catch(text[bad]=88)))errors++;
                if(stringp(catch(text[<bad]=88)))errors++;
                return ({errors,text});
            }
        "#).await,vec![LpcRef::from(13),s("abc")]);
    }
}

#[tokio::test]
async fn nil_obeys_null_mode_but_integer_zero_always_writes_nul() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        let failed = i64::from(mode == NullMode::Distinct);
        assert_eq!(run_with(config,"",&[],r#"
            mixed *create(){string text="abc";mixed value=nil;int failed=stringp(catch(text[0]=value));text[1]=0;return ({failed,text[0],text[1],text[2],sizeof(text)});}
        "#).await,vec![LpcRef::from(failed),if failed==1 {97.into()}else{0.into()},0.into(),99.into(),3.into()]);
    }
}

#[tokio::test]
async fn scalar_edges_are_valid_except_surrogates() {
    assert_eq!(run("",&[],r#"
        mixed *create(){string text="abcd";text[0]=55295;text[1]=57344;text[2]=1114111;text[3]=0;return ({text[0],text[1],text[2],text[3],sizeof(text)});}
    "#).await,vec![LpcRef::from(55295),57344.into(),1114111.into(),0.into(),4.into()]);
}

#[tokio::test]
async fn final_byte_limit_applies_to_growth_shrinkage_and_noops() {
    assert_eq!(run("",&[],r#"
        mixed *create(){
            string text="a"*8192;string old=text;
            int failed=stringp(catch(text[0]='é'));
            int intact=text==old;
            text[0]=0;
            string large=sprintf("%8192s","é");
            int noop=stringp(catch(large[0]=' '));
            large[<1]='a';
            string unicode="😀"*2048;unicode[0]='a';
            return ({failed,intact,sizeof(text),text[0],noop,sizeof(to_bytes(large,"UTF-8")),sizeof(to_bytes(unicode,"UTF-8"))});
        }
    "#).await,vec![LpcRef::from(1),1.into(),8192.into(),0.into(),1.into(),8192.into(),8189.into()]);
}

#[tokio::test]
async fn known_temporary_strings_and_compounds_are_rejected() {
    for owner in [r#""abc""#, "make()", "text[0..1]", "(text+text)"] {
        let err=Compiler::new(test_config()).compile_string("/bad.c",format!(r#"string make(){{return "abc";}}void create(){{string text="abc";{owner}[0]='X';}}"#)).await.unwrap_err();
        assert!(
            err.to_string().contains("requires an assignable target"),
            "{owner}: {err}"
        );
    }
    for op in [
        "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&&=", "||=",
    ] {
        let err = Compiler::new(test_config())
            .compile_string(
                "/bad.c",
                format!(r#"void create(){{string text="abc";text[0]{op}1;}}"#),
            )
            .await
            .unwrap_err();
        assert!(
            err.to_string()
                .contains("Compound indexed string assignment"),
            "{op}: {err}"
        );
    }
}

#[tokio::test]
async fn known_noninteger_replacements_are_rejected() {
    for value in [
        r#""X""#,
        "1.0",
        "({88})",
        "([])",
        r#"to_bytes("X","UTF-8")"#,
    ] {
        let error = Compiler::new(test_config())
            .compile_string(
                "/bad.c",
                format!(r#"void create(){{string text="abc";text[0]={value};}}"#),
            )
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("Mismatched types"),
            "{value}: {error}"
        );
    }
}

#[tokio::test]
async fn mixed_noncollections_and_bytes_remain_unwritable() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        assert_eq!(
            run_with(
                config,
                "",
                &[],
                r#"
            mixed *create(){
                int errors;
                foreach(mixed value in ({0,nil,7,1.0,to_bytes("x","UTF-8")})){
                    if(stringp(catch(value[0]=88)))errors++;
                }
                return ({errors});
            }
        "#
            )
            .await,
            vec![LpcRef::from(5)]
        );
    }
}

#[tokio::test]
async fn compound_provenance_is_preserved_for_dynamic_receivers() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create(){
            mixed text="abc",a=({1}),m=(["x":2]);
            int errors;
            if(stringp(catch(text[0]+=1)))errors++;
            if(stringp(catch(text[0]&&=1)))errors++;
            if(stringp(catch(text[0]||=1)))errors++;
            if(stringp(catch(text[0]++)))errors++;
            if(stringp(catch(--text[0])))errors++;
            if(stringp(catch(sscanf("120","%d",text[0]))))errors++;
            text[0]=text[0]+1;
            a[0]+=2;m["x"]*=3;a[0]++;sscanf("7","%d",m["x"]);
            return ({errors,text,a[0],m["x"]});
        }
    "#
        )
        .await,
        vec![LpcRef::from(6), s("bbc"), 4.into(), 7.into()]
    );
}

#[tokio::test]
async fn typed_arrays_mappings_and_bytes_keep_existing_write_behavior() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create(){
            int *a=({1,2});mapping m=([]);bytes b=to_bytes("abc","UTF-8");
            a[<1]=7;m["x"]=8;a[0]+=2;m["x"]++;
            return ({a[0],a[1],m["x"],stringp(catch(b[0]=120))});
        }
    "#
        )
        .await,
        vec![LpcRef::from(3), 7.into(), 9.into(), 1.into()]
    );
}

#[tokio::test]
async fn inherited_index_assignment_relocates_registers_and_branch_targets() {
    let dir = lib_holding(
        "string-index-inheritance",
        &[
            ("padding.c", "int padding=9;"),
            (
                "base.c",
                r#"mixed text="abc";void use_array(){text=({1,2,3});} void replace(){for(int i=0;i<3;i++)text[i]='X';} mixed read(){return text;}"#,
            ),
        ],
    );
    assert_eq!(
        run_with(
            temp_lib_config(&dir),
            "",
            &[],
            r#"
        inherit "/padding";inherit "/base";
        mixed *create(){replace();string first=read();use_array();replace();mixed array=read();return ({first,array[0],array[1],array[2]});}
    "#
        )
        .await,
        vec![s("XXX"),88.into(),88.into(),88.into()]
    );
}

#[tokio::test]
async fn index_rebinding_retries_changes_to_the_source_or_replacement_owner() {
    use crate::{
        interpreter::{
            lpc_mapping::LpcMapping,
            stm::{CommitProtocol, Committer, Transaction},
            task::{SeedArg, SeedEntry, TaskSeed},
        },
        test_support::run_prog,
    };
    for change_replacement in [false, true] {
        let mut task =
            run_prog(r#"string replace(mapping m){m["source"][1]=m["rhs"];return m["source"];}"#)
                .await;
        let process = task.context.process().clone();
        let entry = SeedEntry::Function(
            process
                .initial_program()
                .lookup_function("replace")
                .unwrap()
                .clone(),
        );
        let mut committer = Committer::new();
        let mut setup = Transaction::new(committer.snapshot_clone());
        let mut values = LpcMapping::default();
        values.insert(s("source"), s("abc"));
        values.insert(s("rhs"), 88.into());
        let owner = setup.mint_mapping(values);
        committer.commit(setup.take_changeset()).unwrap();
        let changed = owner.id;
        let seed = TaskSeed {
            process,
            entry,
            args: vec![SeedArg::Value(LpcRef::Mapping(owner))],
            initializes: false,
        };
        let (tx, rx) = flume::unbounded();
        let committer_tx = tx.clone();
        let runner = tokio::spawn(async move {
            let mut injected = false;
            while let Ok(message) = rx.recv_async().await {
                if !injected && matches!(message, CommitProtocol::Commit { .. }) {
                    injected = true;
                    let mut writer = Transaction::new(committer.snapshot_clone());
                    writer
                        .with_mapping_cow(changed, |mapping| {
                            assert_eq!(mapping.get(&s("source")), Some(&s("abc")));
                            if change_replacement {
                                mapping.insert(s("rhs"), 89.into());
                            } else {
                                mapping.insert(s("source"), s("Qbc"));
                            }
                            Ok(())
                        })
                        .unwrap();
                    committer.commit(writer.take_changeset()).unwrap();
                }
                if !committer.process(message, &committer_tx) {
                    break;
                }
            }
            assert!(injected);
            committer.snapshot_clone()
        });
        let (result, stats) = task.eval_with_committer(&tx, &seed).await;
        tx.send(CommitProtocol::Close).unwrap();
        let snapshot = runner.await.unwrap();
        result.unwrap();
        assert_eq!((stats.attempts, stats.conflicts), (2, 1));
        let expected = if change_replacement {
            s("aYc")
        } else {
            s("QXc")
        };
        assert_eq!(task.result(), Some(expected.clone()));
        let committed = snapshot.read(changed).unwrap().into_mapping().unwrap();
        assert_eq!(committed.get(&s("source")), Some(&expected));
    }
}

#[tokio::test]
async fn an_uncaught_error_discards_string_owner_rebinding() {
    use crate::interpreter::{
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    };
    let vm = Vm::new(test_config());
    let initialized = vm
        .initialize_process_from_code(
            "/string-index.c",
            r#"
        string text="abc";
        void replace(){text[1]=88;throw("abort");}
        string read(){return text;}
    "#,
        )
        .await
        .unwrap();
    let process = initialized.context.process().clone();
    let template = TaskTemplate::from(vm.global_state.clone());
    let error = apply_function_by_name("replace", &[], process.clone(), template.clone(), None)
        .await
        .unwrap()
        .unwrap_err();
    assert!(error.to_string().contains("abort"));
    let result = apply_function_by_name("read", &[], process, template, None)
        .await
        .unwrap()
        .unwrap();
    assert_eq!(result, s("abc"));
}
