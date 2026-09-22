use lpc_rs_core::{RegisterSize, register::RegisterVariant};
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize_save},
    lpc_ref::LpcRef,
    program::Program,
    save_format::write_line,
};

/// The globals `save_object` writes: those without `static`/`nosave`, in slot
/// order (parents first). A name declared twice keeps its last declaration.
pub(crate) fn saved_globals(program: &Program) -> Vec<(&str, RegisterSize)> {
    let mut slots: Vec<(&str, RegisterSize)> = program
        .global_variables
        .iter()
        .filter(|(_, sym)| !sym.flags.is_static())
        .filter_map(|(name, sym)| match sym.location {
            Some(RegisterVariant::Global(reg)) => Some((name.as_str(), reg.index())),
            _ => None,
        })
        .collect();
    slots.sort_by_key(|(_, reg)| *reg);
    slots
}

/// `save_object(file)`: the calling object's globals without `static`/`nosave` as
/// `name value` lines in `<file>.o`, root-relative, once the master's
/// `valid_write` allows the unsuffixed path. Built now, written at commit.
/// Returns the in-game path written.
pub async fn save_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize_save(context, "save_object", VALID_WRITE, 0).await?;
    let in_game = access.to_string();
    let image = context.process().image(context.txn());
    let mut contents = String::new();
    for (name, reg) in saved_globals(&image.program) {
        let value = context
            .txn()
            .with(|t| t.read(image.var_id(reg)))
            .unwrap_or_else(|| LpcRef::from(0));
        write_line(&mut contents, name, &value, context.txn())
            .map_err(|e| e.with_span(context.call_site_span()))?;
    }
    access.record_save(context, contents).await?;
    context.return_efun_result(LpcRef::from(in_game));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, lib_holding, string_global, temp_lib_config},
    };

    /// A master allowing every read and write.
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

    #[tokio::test]
    async fn class_globals_round_trip_pending_saves_with_nested_values_and_nosave_exclusions() {
        let root = TempLib::new("save-class-globals");
        let vm = allowing_vm(&root).await;
        let result = vm.initialize_process_from_code("/w.c", r#"
            class record {
                int n;
                string label;
                class record child;
                int *numbers;
                mapping metadata;
                function callback;
                object owner;
                bytes raw;
            }
            class record value, alias;
            nosave class record cache;
            int create() {
                value = new(class record, n: 7, label: "hello\nworld",
                    child: new(class record, n: 12), numbers: ({ 3, 4 }),
                    metadata: ([ "answer": 42 ]), callback: (: 1 :),
                    owner: this_object(), raw: to_bytes("hé", "utf8"));
                alias = value;
                cache = new(class record, n: 8);
                cache.child = cache;
                save_object("/w");
                value.n = 99;
                int restored = restore_object("/w");
                if (!restored || !classp(value) || value == alias || value.child == alias.child) return 0;
                if (value.n != 7 || alias.n != 7 || cache.n != 8 || cache.child != cache) return 0;
                if (value.callback != 0 || value.owner != this_object() || value.raw != to_bytes("hé", "utf8")) return 0;
                if (value.numbers[1] != 4 || value.metadata["answer"] != 42 || value.label != "hello\nworld") return 0;
                value.child.n = 50;
                return alias.child.n == 12;
            }
        "#).await.unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
        let text = std::fs::read_to_string(root.join("w.o")).unwrap();
        assert!(text.contains("value c(1,\"/w.c\",\"record\","));
        assert!(!text.contains("cache "));
    }

    #[tokio::test]
    async fn an_aborted_class_save_preserves_the_existing_file() {
        let root = TempLib::new("save-class-abort");
        std::fs::write(root.join("w.o"), "previous 42\n").unwrap();
        let vm = allowing_vm(&root).await;
        let error = vm
            .initialize_process_from_code(
                "/w.c",
                r#"
            class record { int n; }
            class record value = new(class record, n: 7);
            void create() { save_object("/w"); throw("abort class save"); }
        "#,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("abort class save"));
        assert_eq!(
            std::fs::read_to_string(root.join("w.o")).unwrap(),
            "previous 42\n"
        );
        assert!(!root.join("w.o.tmp").exists());
    }

    #[tokio::test]
    async fn caught_class_cycles_do_not_replace_an_earlier_pending_save() {
        let root = TempLib::new("save-class-cycle");
        let vm = allowing_vm(&root).await;
        let result = vm
            .initialize_process_from_code(
                "/w.c",
                r#"
            class record { mixed next; }
            class record value = new(class record);
            int create() {
                save_object("/w");
                value.next = ([ "cycle": ({ value }) ]);
                mixed error = catch(save_object("/w"));
                if (!stringp(error)) return 0;
                restore_object("/w");
                return classp(value) && value.next == 0;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    /// The file lands at commit; a restore in the same task already reads it.
    #[tokio::test]
    async fn a_same_task_restore_reads_the_pending_save() {
        let root = TempLib::new("save-then-restore");
        let vm = allowing_vm(&root).await;
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    int n = 1;
                    int found;
                    void create() {
                        save_object("/w");
                        n = 2;
                        found = restore_object("/w");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&w, 0u16), LpcRef::from(1));
        assert_eq!(vm.global_state.committed_global(&w, 1u16), LpcRef::from(1));
        assert!(root.join("w.o").exists());
    }

    #[tokio::test]
    async fn writes_every_non_static_global_in_slot_order() {
        let root = TempLib::new("save-basic");
        std::fs::create_dir_all(root.join("data")).unwrap();
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                int i = 42;
                static int hidden = 9;
                private string p = "priv";
                float f = 3.25;
                mixed *a = ({ 1, "two" });
                mapping m = ([ "k": 4 ]);
                int z;
                string got;
                void create() { got = save_object("/data/w"); }
            "# },
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("data/w.o")).unwrap(),
            "i 42\np \"priv\"\nf #0x1.ap+1#\na ({1,\"two\",})\nm ([\"k\":4,])\nz 0\ngot 0\n"
        );
    }

    #[tokio::test]
    async fn nosave_excludes_globals_independently_of_visibility() {
        let root = TempLib::new("save-nosave-flags");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"
            nosave int temporary = 1;
            public nosave int public_cache = 2;
            private nosave int private_cache = 3;
            nosave protected nomask int protected_cache = 4;
            nosave static int both = 5;
            static int legacy = 6;
            nosave mixed *cycle;
            private int saved_private = 7;
            int saved_public = 8;
            void create() {
                cycle = ({ 0 });
                cycle[0] = cycle;
                save_object("/w");
            }
        "#,
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("w.o")).unwrap(),
            "saved_private 7\nsaved_public 8\n",
        );
    }

    #[tokio::test]
    async fn inherited_nosave_globals_stay_excluded() {
        let root = lib_holding(
            "save-nosave-inheritance",
            &[
                (
                    "base.c",
                    r#"
                public nosave int public_cache = 1;
                private nosave int private_cache = 2;
                protected nosave int protected_cache = 3;
                int base_saved = 4;
                int private_value() { return private_cache; }
            "#,
                ),
                (
                    "middle.c",
                    r#"
                private inherit "/base";
                int total_cache() { return public_cache + protected_cache + private_value(); }
            "#,
                ),
            ],
        );
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/child.c",
            r#"
            inherit "/middle";
            nosave int own_cache = 5;
            int kept = 6;
            int total;
            void create() { total = total_cache() + own_cache; save_object("/child"); }
        "#,
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("child.o")).unwrap(),
            "base_saved 4\nkept 6\ntotal 11\n",
        );
    }

    #[tokio::test]
    async fn nosave_redeclarations_use_the_effective_variables_storage_flag() {
        let root = lib_holding(
            "save-nosave-redeclaration",
            &[("base.c", "int omitted = 1; nosave int kept = 2;")],
        );
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/child.c",
            r#"
            inherit "/base";
            nosave int omitted = 3;
            int kept = 4;
            void create() { save_object("/child"); }
        "#,
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("child.o")).unwrap(),
            "kept 4\n",
        );
    }

    #[tokio::test]
    async fn nosave_values_survive_a_same_task_save_and_restore() {
        let root = TempLib::new("save-nosave-round-trip");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/w.c",
                r#"
            int saved = 1;
            nosave int cache = 10;
            int create() {
                save_object("/w");
                saved = 2;
                cache = 20;
                restore_object("/w");
                return saved * 100 + cache;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(120)));
    }

    #[tokio::test]
    async fn nosave_excludes_the_declaration_without_hiding_shared_values() {
        let root = TempLib::new("save-nosave-alias");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"
            nosave int *cache = ({ 7 });
            int *saved = cache;
            void create() { cache[0] = 9; save_object("/w"); }
        "#,
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("w.o")).unwrap(),
            "saved ({9,})\n",
        );
    }

    #[tokio::test]
    async fn returns_the_in_game_path_with_the_suffix() {
        let root = TempLib::new("save-return");
        let vm = allowing_vm(&root).await;
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"string got; void create() { got = save_object("/w"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &w, "got"), "/w.o");
    }

    #[tokio::test]
    async fn a_relative_name_is_root_relative_and_o_is_always_appended() {
        let root = TempLib::new("save-paths");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/d/w.c",
            indoc! { r#"
                void create() {
                    save_object("x");
                    save_object("/y.o");
                    save_object("/z.c");
                }
            "# },
        )
        .await
        .unwrap();
        assert!(root.join("x.o").exists(), "root-relative, not /d/x.o");
        assert!(!root.join("d/x.o").exists());
        assert!(root.join("y.o.o").exists());
        assert!(root.join("z.c.o").exists());
    }

    #[tokio::test]
    async fn the_master_sees_the_path_before_the_suffix_and_the_efun_name() {
        let root = TempLib::new("save-gate");
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string seen;
                    int valid_write(string p, string e, object c, string g) {
                        seen = p + " " + e;
                        return 0;
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"string err; void create() { err = catch(save_object("/data/w")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &master, 0), "/data/w save_object");
        assert!(committed_string(&vm, &w, 0).contains("save_object: permission denied"));
        assert!(!root.join("data/w.o").exists());
    }

    #[tokio::test]
    async fn inherited_globals_come_first() {
        let root = lib_holding(
            "save-inherit",
            &[
                ("secure/master.c", ALLOWING),
                ("pa.c", "int pa_v = 1; int dup = 11;"),
                (
                    "pb.c",
                    "int pb_v = 2; int dup = 22; private int pb_pv = 222;",
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/ch.c",
            indoc! { r#"
                inherit "/pa";
                inherit "/pb";
                int ch_v = 3;
                void create() { save_object("/ch"); }
            "# },
        )
        .await
        .unwrap();
        let file = std::fs::read_to_string(root.join("ch.o")).unwrap();
        assert_eq!(file, "pa_v 1\npb_v 2\ndup 22\npb_pv 222\nch_v 3\n");
    }

    #[tokio::test]
    async fn an_aborted_task_writes_nothing() {
        let root = TempLib::new("save-abort");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"int i = 1; void create() { save_object("/w"); throw("boom"); }"#,
        )
        .await
        .unwrap_err();
        assert!(!root.join("w.o").exists());
        assert!(!root.join("w.o.tmp").exists());
    }

    #[tokio::test]
    async fn a_missing_parent_directory_is_a_runtime_error() {
        let root = TempLib::new("save-no-parent");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/w.c", r#"void create() { save_object("/nodir/w"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("save_object: /nodir/w.o: parent directory does not exist"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn a_non_string_argument_is_a_runtime_error() {
        let root = TempLib::new("save-non-string");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/w.c",
                r#"void create() { mixed f = 5; save_object(f); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("save_object: path must be a string"), "{err}");
    }

    #[tokio::test]
    async fn a_function_pointer_global_is_skipped() {
        let root = TempLib::new("save-fp");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                int a = 1;
                function f;
                int b = 2;
                void create() { f = &create(); save_object("/w"); }
            "# },
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("w.o")).unwrap(),
            "a 1\nb 2\n"
        );
    }

    #[tokio::test]
    async fn an_object_global_writes_as_a_reference() {
        let root = TempLib::new("save-object-ref");
        let vm = allowing_vm(&root).await;
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"object me; void create() { me = this_object(); save_object("/w"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            std::fs::read_to_string(root.join("w.o")).unwrap(),
            format!("me ${}@/w$\n", w.created)
        );
    }
}
