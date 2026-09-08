use lpc_rs_core::{RegisterSize, register::RegisterVariant};
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{
        efun_context::EfunContext,
        file_access::{authorize_save, record_save},
    },
    lpc_ref::LpcRef,
    process::Process,
    save_format::write_line,
};

/// The globals `save_object` writes: every non-static one by name, in slot
/// order (parents first). A name declared twice keeps its last declaration.
pub(crate) fn saved_globals(process: &Process) -> Vec<(&str, RegisterSize)> {
    let mut slots: Vec<(&str, RegisterSize)> = process
        .program
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

/// `save_object(file)`: the calling object's non-static globals as
/// `name value` lines in `<file>.o`, root-relative, once the master's
/// `valid_write` allows the unsuffixed path. Built now, written at commit.
/// Returns the in-game path written.
pub async fn save_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize_save(context, "save_object", VALID_WRITE, 0).await?;
    let in_game = access.name().to_string();
    let process = context.process().clone();
    let mut contents = String::new();
    for (name, reg) in saved_globals(&process) {
        let value = context
            .txn()
            .with(|t| t.read(process.var_id(reg)))
            .unwrap_or_else(|| LpcRef::from(0));
        write_line(&mut contents, name, &value, context.txn())
            .map_err(|e| e.with_span(context.call_site_span()))?;
    }
    record_save(context, "save_object", access, contents).await?;
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
