use std::sync::Arc;

use lpc_rs_core::{lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize_save},
    lpc_ref::LpcRef,
    process::Process,
    save_format::{read_value_counted, split_line},
    task_context::ObjectLookup,
};

/// The object resolver for `$created@name$` references: the live object
/// the context finds under `name`, never loaded.
pub(crate) fn resolve_object<'a, const N: usize>(
    context: &'a EfunContext<'_, N>,
) -> impl Fn(&str) -> Option<Arc<Process>> + 'a {
    move |name| {
        let path = LpcPath::new_in_game(name, "/", &*context.config().lib_dir);
        match context.find_object(&path) {
            ObjectLookup::Found(process) => Some(process),
            ObjectLookup::Removed | ObjectLookup::NotCreated => None,
        }
    }
}

/// `restore_object(file)`: the calling object's globals from `<file>.o`,
/// root-relative, once the master's `valid_read` allows the unsuffixed
/// path. Unknown and `static`/`nosave` names are ignored, variables absent from the
/// file keep their values. 1 on success, 0 when the file cannot be read,
/// an error on a corrupt line (earlier lines stay applied).
pub async fn restore_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let mut collapsed = 0;
    let access = authorize_save(context, "restore_object", VALID_READ, 0).await?;
    let Some(text) = access.read_save(context).await else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    let process = context.process().clone();
    let image = process.image(context.txn());
    let restored = (|| -> Result<()> {
        let resolve = resolve_object(context);
        for (index, line) in text.lines().enumerate() {
            let err = |e| access.line_error(context, index + 1, e);
            let (name, value_text) = split_line(line).map_err(err)?;
            let Some(symbol) = image.program.global_variables.get(name) else {
                continue;
            };
            if symbol.flags.is_static() {
                continue;
            }
            let Some(RegisterVariant::Global(reg)) = symbol.location else {
                continue;
            };
            let (value, count) =
                read_value_counted(value_text, context.txn(), &resolve).map_err(err)?;
            collapsed += count;
            context
                .txn()
                .with(|t| t.write(image.var_id(reg.index()), value));
        }
        Ok(())
    })();
    if collapsed != 0 {
        context.record_effect(crate::interpreter::stm::Effect::Warning(format!(
            "restore_object: {access}: converted {collapsed} nil value(s) to 0 (NULL_MODE=zero)"
        )));
    }
    restored?;
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, lib_holding, string_global, temp_lib_config},
    };

    const ALLOWING: &str = indoc! { r#"
        int valid_read(string p, string e, object c, string g) { return 1; }
        int valid_write(string p, string e, object c, string g) { return 1; }
    "# };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        vm
    }

    /// `/r.c`: `a`, `s`, `m`, `hidden` and `left` are the restore targets;
    /// `got` is what `restore_object("/r")` returned.
    const R: &str = indoc! { r#"
        int a;
        string s;
        mapping m;
        static int hidden = 5;
        int left = 77;
        int got;
        void create() { got = restore_object("/r"); }
    "# };

    #[tokio::test]
    async fn a_caught_invalid_class_record_preserves_that_global_and_applies_earlier_lines() {
        let root = TempLib::new("restore-class-corrupt");
        std::fs::write(root.join("r.o"), concat!(
            "before 9\n",
            "value c(1,\"/r.c\",\"record\",({({\"n\",({\"int\",0,}),\"wrong\",}),}),({({0,}),}))\n",
            "after 10\n"
        )).unwrap();
        let vm = allowing_vm(&root).await;
        let result = vm.initialize_process_from_code("/r.c", r#"
            class record { int n; }
            int before = 1;
            class record value = new(class record, n: 7);
            int after = 2;
            nosave string error;
            int create() {
                class record original = value;
                error = catch(restore_object("/r"));
                return stringp(error) && before == 9 && after == 2 && value == original && value.n == 7;
            }
        "#).await.unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
        let error = string_global(&vm, &result.context.process, "error");
        assert!(error.contains("restore_object: /r.o line 2"), "{error}");
        assert!(error.contains("invalid value for"), "{error}");
    }

    #[tokio::test]
    async fn restored_class_globals_still_enforce_nominal_identity_when_consumed() {
        let root = TempLib::new("restore-class-identity");
        std::fs::write(
            root.join("r.o"),
            "value c(1,\"/other.c\",\"record\",({}),({({}),}))\n",
        )
        .unwrap();
        let vm = allowing_vm(&root).await;
        let result = vm
            .initialize_process_from_code(
                "/r.c",
                r#"
            class record {}
            class record value;
            int create() {
                restore_object("/r");
                mixed error = catch(value);
                return stringp(error);
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn an_aborted_class_restore_leaves_committed_globals_unchanged() {
        use crate::interpreter::task::{
            apply_function::apply_function_by_name, task_template::TaskTemplate,
        };
        let root = TempLib::new("restore-class-abort");
        std::fs::write(
            root.join("r.o"),
            "value c(1,\"/r.c\",\"record\",({({\"n\",({\"int\",0,}),99,}),}),({({0,}),}))\n",
        )
        .unwrap();
        let vm = allowing_vm(&root).await;
        let process = vm
            .initialize_process_from_code(
                "/r.c",
                r#"
            class record { int n; }
            class record value = new(class record, n: 7);
            int read() { return value.n; }
            void restore_and_fail() { restore_object("/r"); throw("abort class restore"); }
        "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let error = apply_function_by_name(
            "restore_and_fail",
            &[],
            process.clone(),
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap_err();
        assert!(error.to_string().contains("abort class restore"));
        let result = apply_function_by_name(
            "read",
            &[],
            process,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from(7));
    }

    #[tokio::test]
    async fn nosave_values_and_their_aliases_survive_restore() {
        let root = TempLib::new("restore-nosave-values");
        std::fs::write(
            root.join("r.o"),
            "transient malformed\ncache ({99})\nlegacy malformed\nsaved 9\n",
        )
        .unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"
            nosave int transient = 4;
            nosave int *cache = ({ 5 });
            nosave int absent = 6;
            private int saved = 1;
            static int legacy = 7;
            string create() {
                int *alias = cache;
                cache[0] = 8;
                transient = 10;
                restore_object("/r");
                return transient + " " + cache[0] + " " + alias[0] + " "
                    + saved + " " + legacy + " " + absent;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from("10 8 8 9 7 6")));
    }

    #[tokio::test]
    async fn inherited_nosave_globals_are_not_restore_targets() {
        let root = lib_holding(
            "restore-nosave-inheritance",
            &[(
                "base.c",
                r#"
            public nosave int public_cache = 1;
            protected nosave int protected_cache = 2;
            private nosave int private_cache = 3;
            int saved = 4;
            int private_value() { return private_cache; }
        "#,
            )],
        );
        std::fs::write(
            root.join("data.o"),
            "public_cache 100\nprotected_cache 200\nprivate_cache 300\nown_cache 500\nsaved 9\n",
        )
        .unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                r#"
            inherit "/base";
            nosave int own_cache = 5;
            int create() {
                restore_object("/data");
                return public_cache + protected_cache + private_value() + own_cache + saved;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(20)));
    }

    #[tokio::test]
    async fn restores_the_named_globals_and_leaves_the_rest() {
        let root = TempLib::new("restore-basic");
        std::fs::write(
            root.join("r.o"),
            "a 42\ns \"hi\\nthere\"\nm ([\"k\":({1,2,}),])\nhidden 9\nunknown 1\n",
        )
        .unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(42));
        assert_eq!(string_global(&vm, &r, "s"), "hi\nthere");
        let m = vm.global_state.committed_global(&r, 2u16);
        assert!(matches!(m, LpcRef::Mapping(_)), "{m:?}");
        assert_eq!(
            vm.global_state.committed_global(&r, 3u16),
            LpcRef::from(5),
            "static untouched"
        );
        assert_eq!(
            vm.global_state.committed_global(&r, 4u16),
            LpcRef::from(77),
            "absent untouched"
        );
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_hyphenated_name_is_ignored_whether_or_not_the_object_declares_the_prefix() {
        let root = TempLib::new("restore-hyphenated-name");
        std::fs::write(root.join("r.o"), "a-b 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int a; int got; void create() { got = restore_object("/r"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(0));
        assert_eq!(vm.global_state.committed_global(&r, 1u16), LpcRef::from(1));
        let z = vm
            .initialize_process_from_code(
                "/z.c",
                r#"int z; int got; void create() { got = restore_object("/r"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&z, 0u16), LpcRef::from(0));
        assert_eq!(vm.global_state.committed_global(&z, 1u16), LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_file_missing_its_final_newline_still_restores_the_last_line() {
        let root = TempLib::new("restore-no-final-newline");
        std::fs::write(root.join("r.o"), "a 5\nleft 1").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(5));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_empty_file_succeeds_and_changes_nothing() {
        let root = TempLib::new("restore-empty-file");
        std::fs::write(root.join("r.o"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(1));
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(0));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(77));
    }

    #[tokio::test]
    async fn a_crlf_file_restores_both_values() {
        let root = TempLib::new("restore-crlf");
        std::fs::write(root.join("r.o"), "a 5\r\nleft 1\r\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(5));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_string_restored_into_an_int_variable_lands_as_the_string() {
        let root = TempLib::new("restore-string-into-int");
        std::fs::write(root.join("r.o"), "a \"str\"\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int a; void create() { restore_object("/r"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "a"), "str");
    }

    #[tokio::test]
    async fn an_object_reference_to_a_clone_resolves_by_its_numbered_name() {
        let root = lib_holding(
            "restore-clone-ref",
            &[
                ("secure/master.c", ALLOWING),
                (
                    "w.c",
                    indoc! { r#"
                        object me;
                        void become() { me = this_object(); save_object("/data"); }
                    "# },
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_read(string p, string e, object c, string g) { return 1; }
                int valid_write(string p, string e, object c, string g) { return 1; }
                int valid_load(string p, string f, object c, string g) { return 1; }
            "# },
        )
        .await
        .unwrap();
        let clone_name = vm
            .initialize_process_from_code(
                "/main.c",
                r#"string got; void create() { object c = clone_object("/w"); c->become(); got = file_name(c); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let expected = string_global(&vm, &clone_name, "got");
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    object me;
                    string got;
                    void create() { restore_object("/data"); got = me ? file_name(me) : "none"; }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "got"), expected);
        assert!(expected.contains('#'), "{expected}");
    }

    #[tokio::test]
    async fn a_missing_file_is_zero_without_an_error() {
        let root = TempLib::new("restore-missing");
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(0));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(77));
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn an_unreadable_file_is_zero_without_an_error() {
        use std::os::unix::fs::PermissionsExt;
        let root = TempLib::new("restore-unreadable");
        let file = root.join("r.o");
        std::fs::write(&file, "a 1\n").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o000)).unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(0));
    }

    #[tokio::test]
    async fn a_corrupt_line_is_an_error_naming_the_file_and_line() {
        let root = TempLib::new("restore-corrupt");
        std::fs::write(root.join("r.o"), "a 5\ns ({1,2})\nleft 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("restore_object: /r.o line 2"), "{err}");
        assert!(err.contains("expected `,`"), "{err}");
    }

    #[tokio::test]
    async fn under_catch_the_earlier_lines_stay_applied() {
        let root = TempLib::new("restore-partial");
        std::fs::write(root.join("r.o"), "a 5\ns ({1,2})\nleft 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int a;
                    string s;
                    mapping m;
                    static int hidden = 5;
                    int left = 77;
                    string err;
                    void create() { err = catch(restore_object("/r")); }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(5));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(77));
        assert!(string_global(&vm, &r, "err").contains("restore_object: /r.o line 2"));
    }

    #[tokio::test]
    async fn the_master_sees_the_path_before_the_suffix_and_a_refusal_is_an_error() {
        let root = TempLib::new("restore-gate");
        std::fs::write(root.join("r.o"), "a 1\n").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string seen;
                    int valid_read(string p, string e, object c, string g) {
                        seen = p + " " + e;
                        return 0;
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int a; string err; void create() { err = catch(restore_object("r")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &master, 0), "/r restore_object");
        assert!(string_global(&vm, &r, "err").contains("restore_object: permission denied"));
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(0));
    }

    #[tokio::test]
    async fn a_saved_object_restores_into_a_fresh_object() {
        let root = lib_holding("restore-round-trip", &[("secure/master.c", ALLOWING)]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                int i = 42;
                float f = -2.5e-7;
                string s = "q\"b\\n\nt\tuéé";
                mixed *a = ({ 1, "two", ({ 3, ({}) }), ([ "k": 4 ]) });
                mapping m = ([ "b": 1, "c": 3, "a": 2 ]);
                void create() { save_object("/data"); }
            "# },
        )
        .await
        .unwrap();
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int i;
                    float f;
                    string s;
                    mixed *a;
                    mapping m;
                    string check;
                    void create() {
                        restore_object("/data");
                        check = i + " " + to_string(f) + " " + s + " " + sizeof(a) + " " + a[2][0] + " " + a[3]["k"] + " " + m["c"] + " " + implode(keys(m), ",");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            string_global(&vm, &r, "check"),
            "42 -0.00000025 q\"b\\n\nt\tuéé 4 3 4 3 b,c,a"
        );
    }

    #[tokio::test]
    async fn a_bytes_global_round_trips_through_save_and_restore() {
        let root = lib_holding("restore-bytes-round-trip", &[("secure/master.c", ALLOWING)]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                bytes b = to_bytes(({ 34, 92, 0, 255, 97 }));
                void create() { save_object("/data"); }
            "# },
        )
        .await
        .unwrap();
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    bytes b;
                    void create() { restore_object("/data"); }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&r, 0u16),
            LpcRef::from(vec![34u8, 92, 0, 255, 97])
        );
    }

    #[tokio::test]
    async fn an_object_reference_is_re_found_while_the_object_lives() {
        let root = lib_holding("restore-object-ref", &[("secure/master.c", ALLOWING)]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"object me; void create() { me = this_object(); save_object("/data"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    object me;
                    string got;
                    void create() { restore_object("/data"); got = me ? file_name(me) : "none"; }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "got"), "/w");
        drop(w);
        std::fs::write(root.join("data.o"), "me $1@/w$\n").unwrap();
        let r2 = vm
            .initialize_process_from_code(
                "/r2.c",
                indoc! { r#"
                    object me;
                    string got;
                    void create() { restore_object("/data"); got = me ? file_name(me) : "none"; }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            string_global(&vm, &r2, "got"),
            "none",
            "creation time mismatch"
        );
    }

    #[tokio::test]
    async fn a_latin1_file_reads_as_text() {
        let root = TempLib::new("restore-latin1");
        std::fs::write(root.join("r.o"), b"s \"caf\xe9\"\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string s; void create() { restore_object("/r"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "s"), "café");
    }
}
