use indexmap::IndexMap;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{
        efun_context::EfunContext, file_access::authorize_save, restore_object::resolve_object,
    },
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    save_format::{read_value_counted, split_line},
};

/// `restore_map(file)`: the `name value` lines of `<file>.o` as a mapping,
/// root-relative, once the master's `valid_read` allows the unsuffixed
/// path. A file that cannot be read is an empty mapping; a corrupt line is
/// an error.
pub async fn restore_map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let mut collapsed = 0;
    let access = authorize_save(context, "restore_map", VALID_READ, 0).await?;
    let mut entries: IndexMap<LpcRef, LpcRef> = IndexMap::new();
    if let Some(text) = access.read_save(context).await {
        let resolve = resolve_object(context);
        for (index, line) in text.lines().enumerate() {
            let err = |e| access.line_error(context, index + 1, e);
            let (name, value_text) = split_line(line).map_err(err)?;
            let (value, count) =
                read_value_counted(value_text, context.txn(), &resolve).map_err(err)?;
            collapsed += count;
            entries.insert(LpcRef::from(name), value);
        }
    }
    if collapsed != 0 {
        context.record_effect(crate::interpreter::stm::Effect::Warning(format!(
            "restore_map: {access}: converted {collapsed} nil value(s) to 0 (NULL_MODE=zero)"
        )));
    }
    context.return_mapping(LpcMapping::new(entries));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::vm::Vm,
        test_support::{TempLib, string_global, temp_lib_config},
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

    #[tokio::test]
    async fn generic_helpers_restore_multiple_class_views_after_a_driver_restart() {
        use crate::{interpreter::lpc_ref::LpcRef, test_support::lib_holding};
        let header = r#"
            #ifdef EVOLVED
            class record { string changed; int added, n; }
            #else
            class record { int n, changed; int *removed; }
            #endif
        "#;
        let root = lib_holding(
            "restore-class-restart",
            &[
                ("record.h", header),
                (
                    "generic.c",
                    r#"void save(mixed r) { save_map(([ "record": r ]), "/m"); }"#,
                ),
                (
                    "evolved.c",
                    r#"
                #define EVOLVED
                #include "record.h"
                void change(class record r) { r.changed = "new"; r.added = 10; r.n = 70; }
            "#,
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            &format!(
                "{ALLOWING}\nint valid_load(string p, string f, object c, string g) {{ return 1; }}"
            ),
        )
        .await
        .unwrap();
        vm.initialize_process_from_code(
            "/owner.c",
            r#"
            #include "record.h"
            void create() {
                class record r = new(class record, n: 7, changed: 8, removed: ({ 9 }));
                "/evolved"->change(r);
                "/generic"->save(r);
            }
        "#,
        )
        .await
        .unwrap();
        let text = std::fs::read_to_string(root.join("m.o")).unwrap();
        drop(vm);
        std::fs::remove_file(root.join("record.h")).unwrap();
        let vm = allowing_vm(&root).await;
        let result = vm
            .initialize_process_from_code(
                "/generic.c",
                r#"
            mapping data;
            mixed record() { return data["record"]; }
            int create() {
                data = restore_map("/m");
                if (!classp(data["record"])) return 0;
                data["record"] = copy_class(data["record"]);
                save_map(data, "/again");
                return 1;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read_to_string(root.join("again.o")).unwrap(), text);
        assert!(vm.global_state.object_space.lookup("/record.h").is_none());
        vm.global_state.gc().await.unwrap().unwrap();
        std::fs::write(root.join("record.h"), header).unwrap();
        let result = vm
            .initialize_process_from_code(
                "/new_reader.c",
                r#"
            #define EVOLVED
            #include "record.h"
            int create() {
                class record r = "/generic"->record();
                if (r.changed != "new" || r.added != 10 || r.n != 70) return 0;
                r.n++;
                return 1;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
        let result = vm
            .initialize_process_from_code(
                "/old_reader.c",
                r#"
            #include "record.h"
            int create() {
                class record r = "/generic"->record();
                return r.n == 71 && r.changed == 8 && r.removed[0] == 9;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }

    /// `check` is the mapping's keys and one value, or "empty".
    const R: &str = indoc! { r#"
        string check;
        void create() {
            mapping m = restore_map("/m");
            check = sizeof(m) ? implode(keys(m), ",") + " " + m["x"] : "empty";
        }
    "# };

    #[tokio::test]
    async fn reads_lines_as_entries_in_file_order() {
        let root = TempLib::new("restore-map-basic");
        std::fs::write(root.join("m.o"), "x 1\nn 0\nz ({1,2,})\ny \"s\"\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "check"), "x,n,z,y 1");
    }

    #[tokio::test]
    async fn a_hyphenated_line_restores_with_its_full_name_as_the_key() {
        let root = TempLib::new("restore-map-hyphen");
        std::fs::write(root.join("m.o"), "a-b 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string check; void create() { mapping m = restore_map("/m"); check = implode(keys(m), ","); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "check"), "a-b");
    }

    #[tokio::test]
    async fn a_leading_space_line_restores_with_the_empty_string_key() {
        let root = TempLib::new("restore-map-empty-key");
        std::fs::write(root.join("m.o"), " 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string check; void create() { mapping m = restore_map("/m"); check = implode(keys(m), ","); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "check"), "");
    }

    #[tokio::test]
    async fn a_missing_file_is_an_empty_mapping() {
        let root = TempLib::new("restore-map-missing");
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "check"), "empty");
    }

    #[tokio::test]
    async fn a_save_object_file_restores_as_a_mapping() {
        let root = TempLib::new("restore-map-of-object");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"int x = 7; string s = "t"; void create() { save_object("/m"); }"#,
        )
        .await
        .unwrap();
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "check"), "x,s 7");
    }

    #[tokio::test]
    async fn a_corrupt_line_is_an_error_the_lib_can_catch() {
        let root = TempLib::new("restore-map-corrupt");
        std::fs::write(root.join("m.o"), "x 1\nbad ({\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; void create() { err = catch(restore_map("/m")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(string_global(&vm, &r, "err").contains("restore_map: /m.o line 2"));
    }
}
