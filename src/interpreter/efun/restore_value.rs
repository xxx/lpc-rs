use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{efun_context::EfunContext, restore_object::resolve_object},
    save_format::read_value_counted,
    stm::Effect,
};

/// Restore one saved value, resolving object references without loading objects.
pub fn restore_value<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(text) = context.arg(0).as_str() else {
        return Err(context.runtime_error("restore_value: text must be a string"));
    };
    let (value, collapsed) = {
        let resolve = resolve_object(context);
        read_value_counted(text, context.txn(), &resolve)
            .map_err(|e| context.runtime_error(format!("restore_value: {e}")))?
    };
    if collapsed != 0 {
        context.record_effect(Effect::Warning(format!(
            "restore_value: converted {collapsed} nil value(s) to 0 (NULL_MODE=zero)"
        )));
    }
    context.return_efun_result(value);
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::{config::ConfigBuilder, debug_log::DebugLog};
    use tokio::io::AsyncReadExt;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::{TempLib, run_prog},
    };

    fn logged_vm(mode: NullMode) -> (TempLib, Vm, tokio::io::DuplexStream) {
        let root = TempLib::new("restore-value");
        let (writer, reader) = tokio::io::duplex(4096);
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .debug_log(DebugLog::new(writer))
                .build()
                .unwrap(),
        );
        (root, vm, reader)
    }

    #[tokio::test]
    async fn native_text_accepts_decimal_floats_clamped_integers_and_trailing_spaces() {
        let task = run_prog(
            r#"
            int create() {
                return restore_value("3.25") == 3.25
                    && restore_value("42   ") == 42
                    && restore_value("99999999999999999999999") == 9223372036854775807
                    && restore_value("-99999999999999999999999") == -9223372036854775807 - 1
                    && restore_value("\"a\\qb\"") == "aqb"
                    && sizeof(restore_value("({})")) == 0
                    && sizeof(restore_value("([])")) == 0;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn classes_restore_without_visible_definitions_or_source_files() {
        let (_root, vm, _reader) = logged_vm(NullMode::Distinct);
        let task = vm
            .initialize_process_from_code(
                "/test.c",
                r#"
            int create() {
                string text = "c(1,\"/missing.h\",\"record\",({({\"n\",({\"int\",0,}),7,}),}),({({0,}),}))";
                mixed record = restore_value(text);
                return classp(record) && save_value(record) == text;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(vm.global_state.object_space.lookup("/missing.h").is_none());
    }

    #[tokio::test]
    async fn malformed_values_and_extra_text_raise_catchable_errors() {
        let task = run_prog(
            r#"
            int create() {
                foreach (string text : ({ "", " ", " 1", "1\n", "1\t", "1 2",
                    "({1})", "({1, 2,})", "([1:2])", "\"unfinished", "b\"\\xzz\"",
                    "c(2,", "c(1,\"/x.h\",\"r\",({({\"s\",({\"string\",0,}),7,}),}),({({0,}),}))" })) {
                    mixed err = catch(restore_value(text));
                    if (!stringp(err) || strindex(err, "restore_value:") < 0) return 0;
                }
                return restore_value("7") == 7;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn restoring_enforces_the_value_nesting_limit() {
        let task = run_prog(
            r#"
            int create() {
                string text = "0";
                for (int i = 0; i < 128; i++) text = "({" + text + ",})";
                if (!arrayp(restore_value(text))) return 0;
                mixed err = catch(restore_value("({" + text + ",})"));
                return stringp(err) && strindex(err, "restore_value:") >= 0
                    && strindex(err, "nesting deeper than 128 levels") >= 0;
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn zero_stays_numeric_and_nil_conversion_warns_once_with_a_count() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm, mut reader) = logged_vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                int create() {
                    mixed *values = restore_value("({nil,0,([nil:nil,0:7,]),})");
                    mapping m = values[2];
                    return values[0] == nil && intp(values[1]) && values[1] == 0
                        && intp(restore_value("0")) && m[0] == 7
                        && (nil == 0 ? sizeof(m) == 1 : sizeof(m) == 2 && m[nil] == nil);
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
            let mut buf = [0; 4096];
            let read =
                tokio::time::timeout(Duration::from_millis(100), reader.read(&mut buf)).await;
            if mode == NullMode::Zero {
                let n = read.unwrap().unwrap();
                let log = String::from_utf8_lossy(&buf[..n]);
                assert_eq!(
                    log.matches("restore_value: converted 3 nil value(s) to 0 (NULL_MODE=zero)")
                        .count(),
                    1,
                    "{log}"
                );
            } else {
                assert!(read.is_err(), "distinct mode emitted a warning");
            }
        }
    }

    #[tokio::test]
    async fn failed_restores_do_not_warn_for_discarded_nil_conversions() {
        let (_root, vm, mut reader) = logged_vm(NullMode::Zero);
        let task = vm
            .initialize_process_from_code(
                "/test.c",
                r#"
            int create() {
                mixed value = "unchanged";
                mixed error = catch(value = restore_value("({nil,broken,})"));
                return stringp(error) && value == "unchanged";
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        let mut buf = [0; 4096];
        assert!(
            tokio::time::timeout(Duration::from_millis(100), reader.read(&mut buf))
                .await
                .is_err()
        );
    }

    #[tokio::test]
    async fn aborted_tasks_do_not_deliver_restore_warnings() {
        let (_root, vm, mut reader) = logged_vm(NullMode::Zero);
        let error = vm
            .initialize_process_from_code(
                "/test.c",
                r#"void create() { restore_value("nil"); throw("abort"); }"#,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("abort"), "{error}");
        let mut buf = [0; 4096];
        assert!(
            tokio::time::timeout(Duration::from_millis(100), reader.read(&mut buf))
                .await
                .is_err()
        );
    }

    #[tokio::test]
    async fn object_references_use_transactional_liveness_without_loading() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (root, vm, _reader) = logged_vm(mode);
            std::fs::write(root.join("unloaded.c"), "void create() {}").unwrap();
            vm.initialize_process_from_code(
                "/secure/master.c",
                "int valid_destruct(object o, object c, string p) { return 1; }",
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
                string text;
                int check() {
                    return restore_value(text) == nil && save_value(victim) == save_value(nil)
                        && restore_value("$0@/victim$") == nil
                        && restore_value("$0@/unloaded$") == nil;
                }
                int create() {
                    text = save_value(victim);
                    if (restore_value(text) != victim
                        || restore_value(save_value(this_object())) != this_object()) return 0;
                    if (restore_value("$0@/victim$") != nil) return 0;
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
            assert!(vm.global_state.object_space.lookup("/unloaded").is_none());
        }
    }

    #[tokio::test]
    async fn non_string_arguments_raise_runtime_errors_in_both_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let (_root, vm, _reader) = logged_vm(mode);
            let task = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                int create() {
                    foreach (mixed value : ({ 0, nil, 1, ({}), ([]), to_bytes(({ 65 })) })) {
                        mixed err = catch(restore_value(value));
                        if (!stringp(err) || strindex(err, "restore_value: text must be a string") < 0)
                            return 0;
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
    async fn the_compiler_checks_argument_counts_and_the_restore_text_type() {
        let (_root, vm, _reader) = logged_vm(NullMode::Zero);
        for (call, expected) in [
            ("save_value()", "incorrect argument count"),
            ("save_value(1, 2)", "incorrect argument count"),
            ("restore_value()", "incorrect argument count"),
            ("restore_value(\"1\", \"2\")", "incorrect argument count"),
            ("restore_value(1)", "unexpected argument type"),
        ] {
            let error = vm
                .initialize_process_from_code(
                    "/test.c",
                    &format!("mixed create() {{ return {call}; }}"),
                )
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains(expected), "{call}: {error}");
        }
    }

    #[tokio::test]
    async fn both_efuns_are_callable_as_function_values() {
        let task = run_prog(
            r#"
            int create() {
                function save = save_value, restore = restore_value;
                return restore(save(42)) == 42
                    && evaluate(restore_value, evaluate(save_value, "text")) == "text";
            }
        "#,
        )
        .await;
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
