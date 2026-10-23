use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext, lpc_ref::LpcRef, save_format::write_value,
};

/// Serialize one value using the native save format, without a name or newline.
pub fn save_value<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let mut text = String::new();
    write_value(&mut text, context.arg(0), context.txn(), 0)
        .map_err(|e| context.runtime_error(format!("save_value: {e}")))?;
    context.return_efun_result(LpcRef::from(text));
    Ok(())
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::{TempLib, run_prog},
    };

    #[tokio::test]
    async fn scalars_use_the_save_grammar_without_a_name_or_newline() {
        let task = run_prog(
            r##"
            int create() {
                string text = "quote\" slash\\ newline\n tab\t nul\0 café 🐈";
                bytes binary = to_bytes(({ 97, 0, 255, 34, 92 }));
                return save_value(42) == "42" && save_value(-17) == "-17"
                    && save_value(3.25) == "#0x1.ap+1#"
                    && save_value("") == "\"\""
                    && restore_value(save_value(text)) == text
                    && restore_value(save_value(binary)) == binary
                    && restore_value(save_value(3.25)) == 3.25
                    && floatp(restore_value(save_value(0.0)));
            }
        "##,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn saving_observes_pending_changes_and_freezes_their_values() {
        let task = run_prog(
            r#"
            class record { int n; }
            int create() {
                mixed *a = ({ 1 });
                mapping m = ([ "with spaces": a, 7: new(class record, n: 2) ]);
                class record r = m[7];
                a[0] = 3;
                r.n = 4;
                m["added"] = 5;
                string text = save_value(m);
                a[0] = 6;
                r.n = 7;
                m_delete(m, "added");
                mapping restored = restore_value(text);
                class record copy = restored[7];
                return restored["with spaces"][0] == 3 && copy.n == 4
                    && restored["added"] == 5 && sizeof(restored) == 3;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn repeated_containers_restore_independent_instances_including_mapping_keys() {
        let task = run_prog(
            r#"
            class record { int n; }
            int create() {
                mixed *a = ({ 1 });
                mapping m = ([ "n": 2 ]);
                class record r = new(class record, n: 3);
                mixed *copy = restore_value(save_value(({ a, a, m, m, r, r,
                    ([ a: a, r: r ]) })));
                class record left = copy[4], right = copy[5];
                copy[0][0] = 10;
                copy[2]["n"] = 20;
                left.n = 30;
                mixed *keys = keys(copy[6]);
                return copy[1][0] == 1 && copy[3]["n"] == 2 && right.n == 3
                    && a[0] == 1 && m["n"] == 2 && r.n == 3
                    && keys[0] != copy[6][keys[0]] && keys[1] != copy[6][keys[1]];
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn nil_and_function_values_serialize_using_the_selected_null() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = TempLib::new("save-value-null");
            let vm = Vm::new(
                ConfigBuilder::default()
                    .lib_dir(root.to_str().unwrap())
                    .null_mode(mode)
                    .build()
                    .unwrap(),
            );
            let task = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                int create() {
                    string expected = nil == 0 ? "0" : "nil";
                    function f = (: 42 :);
                    mixed *restored = restore_value(save_value(({ f, nil, 0 })));
                    return save_value(nil) == expected && save_value(f) == expected
                        && save_value(0) == "0" && restored[0] == nil
                        && restored[1] == nil && intp(restored[2]);
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn classes_retain_identity_and_mutable_fields_after_commit_and_gc() {
        let root = TempLib::new("save-value-class");
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(NullMode::Distinct)
                .build()
                .unwrap(),
        );
        let task = vm
            .initialize_process_from_code(
                "/test.c",
                r#"
            class record { int n; string label; mixed *items; }
            class record saved;
            int check() {
                saved.items[0]++;
                return saved.n == 7 && saved.label == nil && saved.items[0] == 10;
            }
            void create() {
                saved = restore_value(save_value(new(class record, n: 7, items: ({ 9 }))));
            }
        "#,
            )
            .await
            .unwrap();
        vm.global_state.gc().await.unwrap().unwrap();
        let result = apply_function_by_name(
            "check",
            &[],
            task.context.process,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from(1));
    }

    #[tokio::test]
    async fn cycles_and_excessive_nesting_raise_catchable_errors() {
        let task = run_prog(
            r#"
            class record { mixed next; }
            int create() {
                mixed value = 0;
                for (int i = 0; i < 128; i++) value = ({ value });
                if (!stringp(save_value(value))) return 0;
                mixed *a = ({ nil });
                a[0] = a;
                mapping m = ([]);
                m["self"] = m;
                class record r = new(class record);
                r.next = r;
                foreach (mixed cyclic : ({ ({ value }), a, m, r })) {
                    mixed err = catch(save_value(cyclic));
                    if (!stringp(err) || strindex(err, "save_value:") < 0
                        || strindex(err, "nesting deeper than 128 levels") < 0) return 0;
                }
                return 1;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn non_finite_floats_cannot_be_saved() {
        let task = run_prog(
            r#"
            int create() {
                float infinity = 1.0e200 * 1.0e200;
                foreach (mixed value : ({ infinity, ({ infinity }) })) {
                    mixed err = catch(save_value(value));
                    if (!stringp(err) || strindex(err, "save_value:") < 0
                        || strindex(err, "a float must be finite") < 0) return 0;
                }
                return 1;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
