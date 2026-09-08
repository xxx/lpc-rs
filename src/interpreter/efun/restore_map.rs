use indexmap::IndexMap;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{
        efun_context::EfunContext,
        file_access::{authorize_save, line_error},
        restore_object::{read_save_file, resolve_object},
    },
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    save_format::{read_value, split_line},
};

/// `restore_map(file)`: the `name value` lines of `<file>.o` as a mapping,
/// root-relative, once the master's `valid_read` allows the unsuffixed
/// path. A file that cannot be read is an empty mapping; a corrupt line is
/// an error.
pub async fn restore_map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize_save(context, "restore_map", VALID_READ, 0).await?;
    let mut entries: IndexMap<LpcRef, LpcRef> = IndexMap::new();
    if let Some(text) = read_save_file(context, &access.server).await {
        let resolve = resolve_object(context);
        for (index, line) in text.lines().enumerate() {
            let err = |e| line_error(context, "restore_map", &access.in_game, index + 1, e);
            let (name, value_text) = split_line(line).map_err(err)?;
            let value = read_value(value_text, context.txn(), &resolve).map_err(err)?;
            entries.insert(LpcRef::from(name), value);
        }
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
