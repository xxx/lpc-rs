use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use super::{run, run_with};
use crate::{
    compiler::Compiler,
    interpreter::lpc_ref::LpcRef,
    test_support::{lib_holding, temp_lib_config, test_config},
};

#[tokio::test]
async fn equal_length_replacement_rebinds_without_mutating_aliases() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                int *alias = a;
                a[1..1] = ({ 9 });
                return ({ a == alias, a[1], alias[1] });
            }
        "#
        )
        .await,
        vec![LpcRef::from(0), LpcRef::from(9), LpcRef::from(2)]
    );
}

#[tokio::test]
async fn growing_and_shrinking_leave_previous_arrays_intact() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                int *original = a;
                a[1..1] = ({ 8, 9 });
                int *grown = a;
                a[1..2] = ({});
                return ({ save_value(a), save_value(grown), save_value(original) });
            }
        "#
        )
        .await,
        run(
            "",
            &[],
            r#"mixed *create() {
            return ({ save_value(({1,3})), save_value(({1,8,9,3})), save_value(({1,2,3})) });
        }"#
        )
        .await
    );
}

#[tokio::test]
async fn array_replacements_charge_for_their_result_capacity() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *data = ({});
            mixed *create() {
                data = allocate(1024, 1);
                data[0..1023] = ({ 7 });
                int shrunk = object_memory_info(this_object())["data_bytes"];
                data = ({ 7 });
                int compact = object_memory_info(this_object())["data_bytes"];
                data[0..0] = allocate(64, 7);
                int grown = object_memory_info(this_object())["data_bytes"];
                int count = sizeof(data), first = data[0], last = data[63];
                data = allocate(64, 7);
                int direct = object_memory_info(this_object())["data_bytes"];
                return ({ shrunk > 0, shrunk == compact, count, first, last,
                    grown > compact, grown == direct });
            }
        "#
        )
        .await,
        [1, 1, 64, 7, 7, 1, 1]
            .into_iter()
            .map(LpcRef::from)
            .collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn deleting_first_last_and_all_elements_uses_inclusive_endpoints() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3, 4 });
                a[0..0] = ({});
                a[<1..] = ({});
                int first = a[0];
                int last = a[<1];
                a[0..] = ({});
                return ({ first, last, sizeof(a) });
            }
        "#
        )
        .await,
        vec![LpcRef::from(2), LpcRef::from(3), LpcRef::from(0)]
    );
}

#[tokio::test]
async fn adjacent_bounds_insert_in_the_middle_and_at_both_ends() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 2, 4 });
                a[1..0] = ({ 3 });
                a[0..<sizeof(a) + 1] = ({ 1 });
                a[sizeof(a)..] = ({ 5 });
                return a;
            }
        "#
        )
        .await,
        (1..=5).map(LpcRef::from).collect::<Vec<_>>()
    );
}

#[tokio::test]
async fn empty_arrays_accept_an_empty_interval_and_still_get_a_new_identity() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({});
                int *old = a;
                a[0..] = ({});
                int same = a == old;
                a[<0..] = ({ 7 });
                return ({ same, a[0], sizeof(old) });
            }
        "#
        )
        .await,
        vec![LpcRef::from(0), LpcRef::from(7), LpcRef::from(0)]
    );
}

#[tokio::test]
async fn negative_reverse_and_omitted_bounds_select_the_same_elements() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3, 4 });
                a[-3..<2] = ({ 8 });
                a[..-2] = ({ 5 });
                a[<1..] = ({ 9 });
                return a;
            }
        "#
        )
        .await,
        vec![LpcRef::from(5), LpcRef::from(9)]
    );
}

#[tokio::test]
async fn self_replacement_and_overlapping_slices_read_the_old_array() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                a[1..1] = a;
                a[0..2] = a[2..];
                return a;
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(2),
            LpcRef::from(3),
            LpcRef::from(3),
            LpcRef::from(3),
            LpcRef::from(3)
        ]
    );
}

#[tokio::test]
async fn replacement_is_shallow_for_retained_and_inserted_members() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                mixed *child = ({ 1 });
                mapping inserted = ([ "n": 2 ]);
                mixed *a = ({ child, 0 });
                mixed *old = a;
                a[1..1] = ({ inserted });
                a[0][0] = 7;
                a[1]["n"] = 8;
                return ({ old[0][0], inserted["n"], old[1], a == old });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(7),
            LpcRef::from(8),
            LpcRef::from(0),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn assignment_expression_returns_the_rhs_array() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                int *rhs = ({ 9 });
                int *result = (a[1..1] = rhs);
                result[0] = 8;
                return ({ result == rhs, result == a, rhs[0], a[1] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(0),
            LpcRef::from(8),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn class_members_and_indexed_slots_receive_the_new_array() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            class Box { int *values; }
            mixed *create() {
                class Box box = new(class Box, values: ({ 1, 2 }));
                class Box alias = box;
                int *old = box.values;
                box.values[0..0] = ({ 7 });
                mapping m = ([ "values": ({ 3, 4 }) ]);
                m["values"][1..1] = ({ 8 });
                mixed *rows = ({ ({ 5, 6 }) });
                rows[0][0..0] = ({ 9 });
                return ({ alias.values[0], old[0], m["values"][1], rows[0][0] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(7),
            LpcRef::from(1),
            LpcRef::from(8),
            LpcRef::from(9)
        ]
    );
}

#[tokio::test]
async fn closure_captures_and_explicit_reference_parameters_rebind_their_owner() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            void replace(int ref *values) { values[0..0] = ({ 8 }); }
            mixed *create() {
                int *a = ({ 1, 2 });
                int *old = a;
                function change = (: a[1..1] = ({ 7 }) :);
                change();
                replace(ref a);
                return ({ a[0], a[1], old[0], old[1] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(8),
            LpcRef::from(7),
            LpcRef::from(1),
            LpcRef::from(2)
        ]
    );
}

#[tokio::test]
async fn ordinary_parameters_do_not_rebind_the_callers_array() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            int *remove(int *a) { a[0..0] = ({}); return a; }
            mixed *create() {
                int *a = ({ 1, 2 });
                int *b = remove(a);
                return ({ sizeof(a), sizeof(b), a[0], b[0] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(2),
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(2)
        ]
    );
}

#[tokio::test]
async fn operands_run_once_in_rhs_receiver_index_start_end_order() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            int order;
            mixed *rows = ({ ({ 1, 2 }) });
            int *rhs() { order = order * 10 + 1; return ({ 9 }); }
            mixed *receiver() { order = order * 10 + 2; return rows; }
            int index() { order = order * 10 + 3; return 0; }
            int first() { order = order * 10 + 4; return 1; }
            int last() { order = order * 10 + 5; return 1; }
            mixed *create() {
                receiver()[index()][first()..last()] = rhs();
                return ({ order, rows[0][1] });
            }
        "#
        )
        .await,
        vec![LpcRef::from(12345), LpcRef::from(9)]
    );
}

#[tokio::test]
async fn bounds_cannot_redirect_captured_receivers_keys_or_rhs_references() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *rows = ({ ({ 1 }), ({ 2 }) });
            int index = -1;
            int *rhs = ({ 9 });
            int change() {
                rows = ({ ({ 3 }), ({ 4 }), ({ 5 }) });
                index = 0;
                rhs = ({ 8 });
                return 0;
            }
            mixed *create() {
                mixed *old = rows;
                rows[index][0..change()] = rhs;
                return ({ old[0][0], old[1][0], rows[2][0], rhs[0] });
            }
        "#
        )
        .await,
        vec![
            LpcRef::from(1),
            LpcRef::from(9),
            LpcRef::from(5),
            LpcRef::from(8)
        ]
    );
}

#[tokio::test]
async fn the_start_value_is_saved_before_evaluating_the_end() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            int first = 1;
            int last() { first = 0; return 1; }
            mixed *create() {
                int *a = ({ 1, 2, 3 });
                a[first..last()] = ({ 9 });
                return a;
            }
        "#
        )
        .await,
        vec![LpcRef::from(1), LpcRef::from(9), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn member_receivers_are_saved_before_bound_side_effects() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            class Box { int *values; }
            class Box box = new(class Box, values: ({ 1, 2 }));
            int change() { box = new(class Box, values: ({ 3 })); return 0; }
            mixed *create() {
                class Box old = box;
                box.values[change()..0] = ({ 9 });
                return ({ old.values[0], box.values[0] });
            }
        "#
        )
        .await,
        vec![LpcRef::from(9), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn array_contents_are_read_after_operand_side_effects() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            int *a = ({ 1, 2, 3 });
            int *rhs = ({ 7 });
            int change() { a[0] = 5; rhs[0] = 9; return 1; }
            mixed *create() { a[change()..1] = rhs; return a; }
        "#
        )
        .await,
        vec![LpcRef::from(5), LpcRef::from(9), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn a_rebound_variable_receives_the_result_of_its_captured_array() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            int *a = ({ 1, 2, 3 });
            int change() { a = ({ 4 }); return 1; }
            mixed *create() { a[change()..1] = ({ 9 }); return a; }
        "#
        )
        .await,
        vec![LpcRef::from(1), LpcRef::from(9), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn invalid_ranges_raise_errors_without_clipping_or_rebinding() {
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
                mixed *create() {{
                    int *a = ({{ 1, 2, 3 }});
                    int *old = a;
                    a[0] = 7;
                    mixed error = catch(a[{bounds}] = ({{ 9 }}));
                    return ({{ stringp(error), a == old, a[0], a[1], sizeof(a) }});
                }}
            "#
                )
            )
            .await,
            vec![
                LpcRef::from(1),
                LpcRef::from(1),
                LpcRef::from(7),
                LpcRef::from(2),
                LpcRef::from(3)
            ],
            "{bounds}"
        );
    }
}

#[tokio::test]
async fn empty_receivers_still_validate_endpoints() {
    assert_eq!(
        run(
            "",
            &[],
            r#"
            mixed *create() {
                int *a = ({});
                int *old = a;
                mixed index = "bad";
                return ({ stringp(catch(a[0..0] = ({1}))),
                    stringp(catch(a[index..] = ({1}))),
                    stringp(catch(a[0..<index] = ({1}))), a == old });
            }
        "#
        )
        .await,
        vec![LpcRef::from(1); 4]
    );
}

#[tokio::test]
async fn mixed_operands_require_arrays_in_both_null_modes() {
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
                mixed *create() {
                    int errors;
                    foreach (mixed bad in ({ 0, nil, 7, "x", ([]), to_bytes("x", "UTF-8") })) {
                        mixed a = bad;
                        if (stringp(catch(a[0..] = ({})))) errors++;
                        a = ({1});
                        if (stringp(catch(a[0..0] = bad))) errors++;
                    }
                    mixed a = ({1,2});
                    mixed rhs = ({9});
                    a[0..0] = rhs;
                    return ({ errors, a[0], a[1] });
                }
            "#
            )
            .await,
            vec![LpcRef::from(12), LpcRef::from(9), LpcRef::from(2)]
        );
    }
}

#[tokio::test]
async fn unsupported_slice_operands_and_incompatible_array_types_are_rejected() {
    for (code, diagnostic) in [
        (
            r#"bytes a=to_bytes("abc","UTF-8"); a[0..0]=({1});"#,
            "requires arrays",
        ),
        ("mapping a=([]); a[0..0]=({1});", "requires arrays"),
        ("int *a=({1}); a[0..0]=0;", "requires arrays"),
        (r#"int *a=({1}); a[0..0]=({"wrong"});"#, "Mismatched types"),
        (
            "class A *a=({}); a[0..]=({new(class B)});",
            "Mismatched types",
        ),
    ] {
        let error = Compiler::new(test_config())
            .compile_string(
                "/slice.c",
                format!("class A {{ int n; }} class B {{ int n; }} void create() {{ {code} }}"),
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains(diagnostic), "{code}: {error}");
    }
}

#[tokio::test]
async fn replacement_requires_an_assignable_owner() {
    for owner in ["({1,2})", "make()", "a[0..1]", "(a + a)"] {
        let error = Compiler::new(test_config()).compile_string("/slice.c", format!(
            "int *make() {{ return ({{1,2}}); }} void create() {{ int *a=({{1,2}}); {owner}[0..0]=({{9}}); }}"
        )).await.unwrap_err();
        assert!(
            error
                .to_string()
                .contains("assignable array or string target"),
            "{owner}: {error}"
        );
    }
}

#[tokio::test]
async fn compound_slice_assignments_remain_rejected() {
    for op in [
        "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "&&=", "||=",
    ] {
        let error = Compiler::new(test_config())
            .compile_string(
                "/slice.c",
                format!("void create() {{ int *a=({{1,2}}); a[0..0] {op} ({{9}}); }}"),
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
async fn inherited_slice_assignments_relocate_global_and_temporary_registers() {
    let dir = lib_holding(
        "slice-inheritance",
        &[
            (
                "base.c",
                r#"
            int *values = ({ 1, 2, 3 });
            void replace() { values[<2..<1] = ({ 9 }); }
            int *read() { return values; }
        "#,
            ),
            ("padding.c", "int padding = 99;"),
        ],
    );
    assert_eq!(
        run_with(
            temp_lib_config(&dir),
            "",
            &[],
            r#"
            inherit "/padding";
            inherit "/base";
            mixed *create() { replace(); return read(); }
        "#
        )
        .await,
        vec![LpcRef::from(1), LpcRef::from(9)]
    );
}

#[tokio::test]
async fn slice_assignment_retries_changes_to_either_array_and_discards_aborted_results() {
    use crate::{
        interpreter::{
            lpc_array::LpcArray,
            stm::{CommitProtocol, Committer, Transaction},
            task::{SeedArg, SeedEntry, TaskSeed},
        },
        test_support::run_prog,
    };

    for change_replacement in [false, true] {
        let mut task =
            run_prog("int *replace(int *a, int *rhs) { a[1..1] = rhs; return a; }").await;
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
        let source = setup.mint_array(LpcArray::new(vec![1.into(), 2.into(), 3.into()]));
        let rhs = setup.mint_array(LpcArray::new(vec![9.into()]));
        committer.commit(setup.take_changeset()).unwrap();
        let changed = if change_replacement {
            rhs.id
        } else {
            source.id
        };
        let seed = TaskSeed {
            process,
            entry,
            args: vec![
                SeedArg::Value(LpcRef::Array(source)),
                SeedArg::Value(LpcRef::Array(rhs)),
            ],
            initializes: false,
        };
        let (tx, rx) = flume::unbounded();
        let committer_tx = tx.clone();
        let runner = tokio::spawn(async move {
            let mut aborted = None;
            while let Ok(message) = rx.recv_async().await {
                if aborted.is_none()
                    && let CommitProtocol::Commit { changeset, .. } = &message
                {
                    let writes = changeset.touched_vars();
                    assert!(!writes.is_empty());
                    for id in &writes {
                        assert!(committer.snapshot_clone().read(*id).is_none());
                    }
                    aborted = Some(writes);
                    let mut writer = Transaction::new(committer.snapshot_clone());
                    writer
                        .with_array_cow(changed, |array| {
                            array[0] = if change_replacement {
                                8.into()
                            } else {
                                7.into()
                            };
                            Ok(())
                        })
                        .unwrap();
                    committer.commit(writer.take_changeset()).unwrap();
                }
                if !committer.process(message, &committer_tx) {
                    break;
                }
            }
            (committer.snapshot_clone(), aborted.unwrap())
        });
        let (result, stats) = task.eval_with_committer(&tx, &seed).await;
        tx.send(CommitProtocol::Close).unwrap();
        let (snapshot, aborted) = runner.await.unwrap();
        result.unwrap();
        assert_eq!((stats.attempts, stats.conflicts), (2, 1));
        for id in aborted {
            assert!(snapshot.read(id).is_none());
        }
        let Some(LpcRef::Array(result)) = task.result() else {
            panic!("array result");
        };
        let result = snapshot.read(result.id).unwrap().into_array().unwrap();
        let expected: Vec<LpcRef> = if change_replacement {
            vec![1.into(), 8.into(), 3.into()]
        } else {
            vec![7.into(), 9.into(), 3.into()]
        };
        assert_eq!(result.to_vec(), expected);
    }
}

#[tokio::test]
async fn an_uncaught_error_discards_a_global_slice_rebinding() {
    use crate::interpreter::{
        CommittedReader,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    };

    let vm = Vm::new(test_config());
    let initialized = vm
        .initialize_process_from_code(
            "/slice.c",
            r#"
        int *values = ({ 1, 2, 3 });
        void replace() { values[1..1] = ({ 9, 8 }); throw("abort"); }
        int *read() { return values; }
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
    let LpcRef::Array(result) = result else {
        panic!("array result");
    };
    assert_eq!(
        vm.global_state.committed_array(result.id).unwrap().to_vec(),
        vec![LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)]
    );
}
