use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run, run_with, s};
use crate::{
    compiler::Compiler,
    interpreter::lpc_ref::LpcRef,
    test_support::{lib_holding, temp_lib_config, test_config},
};

#[tokio::test]
async fn replacement_rebinds_strings_without_changing_aliases() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="abc";
            string old=text;
            text[1..1]="X";
            string equal=text;
            text[1..1]="YZ";
            string grown=text;
            text[1..2]="";
            return ({ old, equal, grown, text });
        }
    "#
        )
        .await,
        vec![s("abc"), s("aXc"), s("aYZc"), s("ac")]
    );
}

#[tokio::test]
async fn unicode_ranges_count_scalars_including_combining_marks() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="aé😀z";
            text[1..2]="中";
            string combining="é!";
            combining[1..1]="x";
            string joined="👩‍👧";
            joined[1..1]="!";
            return ({ text, combining, joined, sizeof(text) });
        }
    "#
        )
        .await,
        vec![s("a中z"), s("ex!"), s("👩!👧"), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn negative_reverse_and_open_bounds_preserve_string_indexing() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="aé😀z";
            text[-3..<2]="中";
            text[..-2]="Q";
            text[<1..]="!";
            string whole="abc";
            whole[0..-1]="W";
            return ({ text, whole });
        }
    "#
        )
        .await,
        vec![s("Q!"), s("W")]
    );
}

#[tokio::test]
async fn adjacent_bounds_insert_at_utf8_boundaries_and_into_empty_strings() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="é😀";
            text[1..0]="中";
            text[0..<sizeof(text)+1]="[";
            text[sizeof(text)..]="]";
            string empty="";
            empty[0..]="";
            empty[<0..]="é";
            empty[0..0]="";
            return ({ text, empty });
        }
    "#
        )
        .await,
        vec![s("[é中😀]"), s("")]
    );
}

#[tokio::test]
async fn nul_in_sources_and_replacements_preserves_the_suffix() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="a"+sprintf("%c",0)+"éz";
            text[2..2]="😀"+sprintf("%c",0);
            return ({ sizeof(text), text[1], text[2], text[3], text[4], strindex(text,"z") });
        }
    "#
        )
        .await,
        vec![
            LpcRef::from(5),
            0.into(),
            128512.into(),
            0.into(),
            122.into(),
            4.into()
        ]
    );
}

#[tokio::test]
async fn self_and_overlapping_replacements_use_captured_values() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string text="aé😀";
            text[1..1]=text;
            string self=text;
            text[0..2]=text[2..];
            return ({ self, text });
        }
    "#
        )
        .await,
        vec![s("aaé😀😀"), s("é😀😀😀😀")]
    );
}

#[tokio::test]
async fn assignment_expressions_return_the_replacement_string() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create() {
            string a="abc", b="def";
            string result=(a[1..1]=b[1..1]="XY");
            return ({ a,b,result });
        }
    "#
        )
        .await,
        vec![s("aXYc"), s("dXYf"), s("XY")]
    );
}

#[tokio::test]
async fn member_and_indexed_owners_receive_the_new_string() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        class Box { string text; }
        mixed *create() {
            class Box box=new(class Box,text:"abc");
            string *rows=({"aéz"});
            string *alias=rows;
            string old=rows[0];
            mapping record=(["text":"abc"]);
            box.text[1..1]="X";
            rows[<1][1..1]="😀";
            record["text"][1..1]="Y";
            return ({ box.text,alias[0],old,record["text"] });
        }
    "#
        )
        .await,
        vec![s("aXc"), s("a😀z"), s("aéz"), s("aYc")]
    );
}

#[tokio::test]
async fn closures_and_reference_parameters_rebind_their_owner() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        void local_only(string text) { text[0..0]="L"; }
        void by_ref(string ref text) { text[0..0]="R"; }
        mixed *create() {
            string text="abc";
            function change=(: text[1..1]="X" :);
            change();
            local_only(text);
            string before=text;
            by_ref(ref text);
            return ({before,text});
        }
    "#
        )
        .await,
        vec![s("aXc"), s("RXc")]
    );
}

#[tokio::test]
async fn operands_run_once_in_order_across_cooperative_yields() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        int order;
        string *rows=({"abc"});
        string rhs(){order=order*10+1;return "X";}
        string *receiver(){order=order*10+2;return rows;}
        int key(){order=order*10+3;return 0;}
        int first(){order=order*10+4;return 1;}
        int last(){
            int sum;
            for(int i=0;i<3000;i++)sum+=i;
            order=order*10+5;
            return sum>0 ? 1 : 0;
        }
        mixed *create(){
            receiver()[key()][first()..last()]=rhs();
            return ({order,rows[0]});
        }
    "#
        )
        .await,
        vec![12345.into(), s("aXc")]
    );
}

#[tokio::test]
async fn side_effects_cannot_redirect_saved_receivers_keys_or_rhs() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        string *rows=({"abc","def"});
        int key=-1;
        string rhs="X";
        int change(){rows=({"new"});key=0;rhs="Y";return 1;}
        mixed *create(){
            string *old=rows;
            rows[key][1..change()]=rhs;
            return ({old[0],old[1],rows[0],rhs});
        }
    "#
        )
        .await,
        vec![s("abc"), s("dXf"), s("new"), s("Y")]
    );
}

#[tokio::test]
async fn side_effects_preserve_captured_source_start_and_member_instance() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        class Box {string text;}
        class Box box=new(class Box,text:"abc");
        string text="abc";
        int first=1;
        int change(){text="other";first=0;box=new(class Box,text:"new");return 1;}
        mixed *create(){
            class Box old=box;
            box.text[first..change()]="X";
            text="abc";first=1;
            text[first..change()]="Y";
            return ({old.text,box.text,text});
        }
    "#
        )
        .await,
        vec![s("aXc"), s("new"), s("aYc")]
    );
}

#[tokio::test]
async fn invalid_bounds_do_not_clip_or_change_the_owner() {
    for bounds in [
        "-4..0",
        "0..3",
        "4..",
        "2..0",
        "<5..0",
        "0..<5",
        "<0..0",
        "0..<0",
        "0..<-1",
        "9223372036854775807..",
        "-9223372036854775808..",
        "<-9223372036854775808..",
        "0..<9223372036854775807",
    ] {
        assert_eq!(
            run(
                "",
                &[],
                &format!(
                    r#"
            mixed *create(){{
                string text="aé😀";
                mixed err=catch(text[{bounds}]="X");
                return ({{stringp(err),text}});
            }}
        "#
                )
            )
            .await,
            vec![1.into(), s("aé😀")],
            "{bounds}"
        );
    }
}

#[tokio::test]
async fn empty_strings_still_validate_endpoint_values_and_types() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create(){
            string text="";
            mixed bad="bad", fraction=1.0;
            return ({stringp(catch(text[0..0]="x")),stringp(catch(text[bad..]="x")),
                stringp(catch(text[0..<bad]="x")),stringp(catch(text[0..fraction]="x")),text});
        }
    "#
        )
        .await,
        vec![1.into(), 1.into(), 1.into(), 1.into(), s("")]
    );
}

#[tokio::test]
async fn final_byte_length_controls_growth_shrinkage_and_empty_edits() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create(){
            string text="a"*8192;
            string old=text;
            mixed error=catch(text[0..0]="é");
            int intact=text==old;
            text[0..1]="é";
            int full=sizeof(to_bytes(text,"UTF-8"));
            string large=sprintf("%8193s","x");
            mixed noop=catch(large[0..<sizeof(large)+1]="");
            large[0..0]="";
            string removed="é"*4096;
            removed[0..]="";
            return ({stringp(error),intact,full,stringp(noop),sizeof(large),removed});
        }
    "#
        )
        .await,
        vec![
            1.into(),
            1.into(),
            8192.into(),
            1.into(),
            8192.into(),
            s("")
        ]
    );
}

#[tokio::test]
async fn supplementary_character_growth_is_limited_by_bytes() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create(){
            string text="😀"*2048;
            mixed error=catch(text[<0..]="x");
            int size=sizeof(text);
            text[<1..]="abc";
            return ({stringp(error),size,sizeof(to_bytes(text,"UTF-8")),text[<3..]});
        }
    "#
        )
        .await,
        vec![1.into(), 2048.into(), 8191.into(), s("abc")]
    );
}

#[tokio::test]
async fn mixed_operands_require_matching_collection_kinds_in_both_null_modes() {
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
                foreach(mixed bad in ({0,nil,7,1.0,({}),([]),to_bytes("x","UTF-8")})){
                    mixed source=bad;
                    if(stringp(catch(source[0..]="x")))errors++;
                    source="abc";
                    if(stringp(catch(source[0..0]=bad)))errors++;
                }
                mixed source="abc", rhs="X";
                source[1..1]=rhs;
                mixed a=({1,2}), r=({9});
                a[0..0]=r;
                return ({errors,source,a[0],a[1]});
            }
        "#
            )
            .await,
            vec![14.into(), s("aXc"), 9.into(), 2.into()]
        );
    }
}

#[tokio::test]
async fn known_invalid_operands_are_rejected_without_string_coercion() {
    for code in [
        r#"string text="abc";text[0..0]=0;"#,
        r#"string text="abc";text[0..0]=nil;"#,
        r#"string text="abc";text[0..0]=1.0;"#,
        r#"string text="abc";text[0..0]=({1});"#,
        r#"int *text=({1});text[0..0]="x";"#,
        r#"bytes text=to_bytes("abc","UTF-8");text[0..0]="x";"#,
        r#"string text="abc";text[0..0]=to_bytes("x","UTF-8");"#,
    ] {
        let error = Compiler::new(test_config())
            .compile_string("/bad.c", format!("void create(){{{code}}}"))
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("requires arrays or strings")
                || error.to_string().contains("Mismatched types"),
            "{code}: {error}"
        );
    }
}

#[tokio::test]
async fn temporary_strings_and_compound_slices_remain_unassignable() {
    for owner in [r#""abc""#, "make()", "text[0..1]", "(text+text)"] {
        let error=Compiler::new(test_config()).compile_string("/bad.c",format!(
            r#"string make(){{return "abc";}}void create(){{string text="abc";{owner}[0..0]="x";}}"#)).await.unwrap_err();
        assert!(
            error
                .to_string()
                .contains("assignable array or string target"),
            "{owner}: {error}"
        );
    }
    for op in [
        "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&&=", "||=",
    ] {
        let error = Compiler::new(test_config())
            .compile_string(
                "/bad.c",
                format!(r#"void create(){{string text="abc";text[0..0]{op}"x";}}"#),
            )
            .await
            .unwrap_err();
        assert!(
            error.to_string().contains("Compound slice assignment"),
            "{op}: {error}"
        );
    }
}

#[tokio::test]
async fn indexed_string_stepping_and_output_arguments_remain_unsupported() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
        mixed *create(){
            string text="abc";
            return ({stringp(catch(text[0]++)),stringp(catch(sscanf("120","%d",text[0]))),text});
        }
    "#
        )
        .await,
        vec![1.into(), 1.into(), s("abc")]
    );
}

#[tokio::test]
async fn inherited_string_slices_relocate_registers() {
    let dir = lib_holding(
        "string-slice-inheritance",
        &[
            ("padding.c", "int padding=9;"),
            (
                "base.c",
                r#"string text="aé😀";void replace(){text[<2..]="X";}string read(){return text;}"#,
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
        mixed *create(){replace();return ({read()});}
    "#
        )
        .await,
        vec![s("aX")]
    );
}

#[tokio::test]
async fn slice_rebinding_retries_changes_to_the_source_or_replacement_owner() {
    use crate::{
        interpreter::{
            lpc_mapping::LpcMapping,
            stm::{CommitProtocol, Committer, Transaction},
            task::{SeedArg, SeedEntry, TaskSeed},
        },
        test_support::run_prog,
    };
    for change_replacement in [false, true] {
        let mut task = run_prog(
            r#"string replace(mapping m){m["source"][1..1]=m["rhs"];return m["source"];}"#,
        )
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
        values.insert(s("rhs"), s("X"));
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
                                mapping.insert(s("rhs"), s("Y"));
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
            "/string.c",
            r#"
        string text="abc";
        void replace(){text[1..1]="XY";throw("abort");}
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
