use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{
        efun_context::EfunContext,
        file_access::{authorize_save, record_save},
    },
    lpc_ref::LpcRef,
    save_format::write_line,
};

/// `save_map(m, file)`: the string-keyed mapping `m` as `key value` lines
/// in `<file>.o`, root-relative, once the master's `valid_write` allows the
/// unsuffixed path. A non-string key, or a string key containing whitespace,
/// is an error before anything is checked or written. Built now, written at
/// commit.
pub async fn save_map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    if !matches!(context.arg(0), LpcRef::Mapping(_)) {
        return Err(context.runtime_error("save_map: the first argument must be a mapping"));
    }
    let entries = context.arg(0).with_mapping(context.txn(), |mapping| {
        mapping
            .iter()
            .map(|(key, value)| match key {
                LpcRef::String(s) => {
                    let key = s.to_str().to_owned();
                    if key.contains([' ', '\t', '\n', '\r']) {
                        return Err(context.runtime_error(format!(
                            "save_map: a key cannot contain whitespace: \"{key}\""
                        )));
                    }
                    Ok((key, value.clone()))
                }
                other => Err(context.runtime_error(format!(
                    "save_map: a key must be a string, not {}",
                    other.type_name()
                ))),
            })
            .collect::<Result<Vec<_>>>()
    })??;
    let access = authorize_save(context, "save_map", VALID_WRITE, 1).await?;
    let mut contents = String::new();
    for (key, value) in &entries {
        write_line(&mut contents, key, value, context.txn())
            .map_err(|e| e.with_span(context.call_site_span()))?;
    }
    record_save(context, "save_map", access, contents).await?;
    context.return_efun_result(LpcRef::from(0));
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
    async fn writes_each_entry_as_a_line_in_insertion_order() {
        let root = TempLib::new("save-map-basic");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                void create() {
                    save_map(([ "x": 1, "n": 0, "z": ({ 1, 2 }), "y": "s" ]), "/m");
                }
            "# },
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("m.o")).unwrap(),
            "x 1\nn 0\nz ({1,2,})\ny \"s\"\n"
        );
    }

    #[tokio::test]
    async fn a_non_string_key_is_an_error_before_anything_is_written() {
        let root = TempLib::new("save-map-key");
        let vm = allowing_vm(&root).await;
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"string err; void create() { err = catch(save_map(([ 1: 2 ]), "/m")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(string_global(&vm, &w, "err").contains("save_map: a key must be a string"));
        assert!(!root.join("m.o").exists());
    }

    #[tokio::test]
    async fn a_key_that_is_not_an_identifier_saves_and_restores() {
        let root = TempLib::new("save-map-hyphen-key");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"void create() { save_map(([ "a-b": 1 ]), "/m"); }"#,
        )
        .await
        .unwrap();
        assert_eq!(
            std::fs::read_to_string(root.join("m.o")).unwrap(),
            "a-b 1\n"
        );
    }

    #[tokio::test]
    async fn an_empty_key_saves_as_a_leading_space_and_restores_as_the_empty_string() {
        let root = TempLib::new("save-map-empty-key");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"void create() { save_map(([ "": 1 ]), "/m"); }"#,
        )
        .await
        .unwrap();
        assert_eq!(std::fs::read_to_string(root.join("m.o")).unwrap(), " 1\n");
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
    async fn a_key_containing_whitespace_is_an_error_before_anything_is_written() {
        let root = TempLib::new("save-map-whitespace-key");
        let vm = allowing_vm(&root).await;
        for (i, (code, key)) in [
            (
                r#"string err; void create() { err = catch(save_map(([ "A B": 1 ]), "/m")); }"#,
                "A B",
            ),
            (
                r#"string err; void create() { err = catch(save_map(([ "a\tb": 1 ]), "/m")); }"#,
                "a\tb",
            ),
            (
                r#"string err; void create() { err = catch(save_map(([ "a\nb": 1 ]), "/m")); }"#,
                "a\nb",
            ),
            (
                r#"string err; void create() { err = catch(save_map(([ "a\rb": 1 ]), "/m")); }"#,
                "a\rb",
            ),
        ]
        .into_iter()
        .enumerate()
        {
            let w = vm
                .initialize_process_from_code(format!("/w{i}.c"), code)
                .await
                .unwrap()
                .context
                .process;
            assert!(
                string_global(&vm, &w, "err").contains(&format!(
                    "save_map: a key cannot contain whitespace: \"{key}\""
                )),
                "{}",
                string_global(&vm, &w, "err")
            );
        }
        assert!(!root.join("m.o").exists());
    }

    #[tokio::test]
    async fn a_non_mapping_is_an_error() {
        let root = TempLib::new("save-map-type");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/w.c",
                r#"void create() { mixed m = 3; save_map(m, "/m"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("save_map: the first argument must be a mapping"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn the_master_is_asked_with_the_efun_name() {
        let root = TempLib::new("save-map-gate");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                string seen;
                int valid_write(string p, string e, object c, string g) { seen = e; return 0; }
                string seen() { return seen; }
            "# },
        )
        .await
        .unwrap();
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"string err; void create() { err = catch(save_map(([ "a": 1 ]), "/m")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(string_global(&vm, &w, "err").contains("save_map: permission denied"));
        assert!(!root.join("m.o").exists());
    }
}
