use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Test mapping membership using the same key rules as indexing.
pub fn has_key<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let mapping = context.arg(0);
    if !matches!(mapping, LpcRef::Mapping(_)) {
        return Err(
            context.runtime_error(format!("has_key: {} is not a mapping", mapping.type_name()))
        );
    }
    let txn = context.txn();
    let key = context.arg(1).mapping_key(txn);
    let found = mapping.with_mapping(txn, |m| m.contains_key(&key))?;
    context.return_efun_result(LpcRef::from(found));
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

    fn vm(mode: NullMode) -> (TempLib, Vm) {
        let root = TempLib::new("has-key");
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .build()
                .unwrap(),
        );
        (root, vm)
    }

    #[tokio::test]
    async fn membership_does_not_depend_on_the_stored_value() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                int create() {
                    mapping m = ([ "zero": 0, "nil": nil, "text": "" ]);
                    return has_key(m, "zero") == 1 && has_key(m, "nil") == 1
                        && has_key(m, "text") == 1 && has_key(m, "missing") == 0
                        && has_key(([]), "missing") == 0;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn distinct_nil_and_zero_are_independent_keys() {
        let (_root, vm) = vm(NullMode::Distinct);
        let task = vm
            .initialize_process_from_code(
                "/test.c",
                r#"
            int create() {
                mapping m = ([ nil: 0 ]);
                if (!has_key(m, nil) || has_key(m, 0)) return 0;
                m[0] = nil;
                m_delete(m, nil);
                return has_key(m, 0) == 1 && has_key(m, nil) == 0;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn zero_mode_nil_and_zero_name_the_same_key() {
        let task = run_prog(
            r#"
            int create() {
                mapping m = ([ nil: 0 ]);
                if (!has_key(m, nil) || !has_key(m, 0)) return 0;
                m_delete(m, 0);
                return !has_key(m, nil) && !has_key(m, 0);
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn keys_use_mapping_value_and_identity_rules() {
        let task = run_prog(
            r#"
            class record { int n; }
            int create() {
                mixed *array = ({ 1 });
                mapping inner = ([]);
                function callback = (: 1 :);
                class record record = new(class record);
                mapping m = ([ "hello": 0, 1: 0, array: 0, inner: 0,
                    callback: 0, record: 0, this_object(): 0 ]);
                return has_key(m, "he" + "llo") && has_key(m, 1) && !has_key(m, 1.0)
                    && has_key(m, array) && !has_key(m, ({ 1 }))
                    && has_key(m, inner) && !has_key(m, ([]))
                    && has_key(m, callback) && !has_key(m, (: 1 :))
                    && has_key(m, record) && !has_key(m, new(class record))
                    && has_key(m, this_object());
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn membership_observes_pending_insertions_and_deletions() {
        let task = run_prog(
            r#"
            int create() {
                mapping m = ([ "first": 0 ]), alias = m;
                if (has_key(alias, "last")) return 0;
                m["last"] = 0;
                if (!has_key(alias, "last")) return 0;
                m_delete(m, "first");
                return !has_key(alias, "first") && has_key(alias, "last")
                    && sizeof(m) == 1 && keys(m)[0] == "last";
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn has_key_is_callable_as_a_function_value() {
        let task = run_prog(
            r#"
            int create() {
                function contains = has_key;
                mapping m = ([ "present": 0 ]);
                return contains(m, "present") == 1 && contains(m, "absent") == 0
                    && evaluate(has_key, m, "present") == 1;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn non_mappings_raise_runtime_errors_in_both_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                int create() {
                    foreach (mixed value : ({ 0, nil, "text", ({}) })) {
                        mixed error = catch(has_key(value, "key"));
                        if (!stringp(error) || strindex(error, "has_key:") < 0
                            || strindex(error, "is not a mapping") < 0) return 0;
                    }
                    return 1;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn the_compiler_checks_mapping_type_and_argument_count() {
        let (_root, vm) = vm(NullMode::Zero);
        for (call, expected) in [
            ("has_key(([]))", "incorrect argument count"),
            ("has_key(([]), 1, 2)", "incorrect argument count"),
            ("has_key(1, 2)", "unexpected argument type"),
        ] {
            let error = vm
                .initialize_process_from_code(
                    "/test.c",
                    &format!("int create() {{ return {call}; }}"),
                )
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains(expected), "{call}: {error}");
        }
    }

    #[tokio::test]
    async fn destructed_object_keys_use_the_observing_transactions_null_mode() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm) = vm(mode);
            vm.initialize_process_from_code(
                "/secure/master.c",
                r#"
                int valid_destruct(object o, object c, string p) { return 1; }
            "#,
            )
            .await
            .unwrap();
            vm.initialize_process_from_code("/victim.c", "void create() {}")
                .await
                .unwrap();
            let task = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                object victim = find_object("/victim");
                mapping old_key = ([ victim: 1 ]);
                int check() {
                    mapping null_key = ([ nil: 0 ]);
                    mapping zero_key = ([ 0: nil ]);
                    return !has_key(old_key, victim) && !has_key(old_key, nil)
                        && has_key(null_key, victim)
                        && has_key(zero_key, victim) == (nil == 0);
                }
                int create() {
                    if (!has_key(old_key, victim)) return 0;
                    destruct(victim);
                    return check();
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(
                task.result(),
                Some(LpcRef::from(1)),
                "{mode}: before commit"
            );
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
            assert_eq!(result, LpcRef::from(1), "{mode}: after commit and GC");
        }
    }
}
