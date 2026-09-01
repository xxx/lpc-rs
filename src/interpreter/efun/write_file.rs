use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `write_file(path, contents)`: append `contents`, creating the file, once
/// the master's `valid_write` allows it. Checked now, written at commit: a
/// read of the file later in this task sees it as it was.
pub async fn write_file<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(contents) = context.resolve_local_register(2 as RegisterSize).as_str() else {
        return Err(context.runtime_error("write_file: contents must be a string"));
    };
    let contents = contents.to_owned();
    let access = authorize(context, "write_file", VALID_WRITE, 1 as RegisterSize).await?;
    let io_error =
        |e: std::io::Error| context.runtime_error(format!("write_file: {}: {e}", access.in_game));
    match tokio::fs::metadata(&access.server).await {
        Ok(m) if m.is_dir() => {
            return Err(
                context.runtime_error(format!("write_file: {} is a directory", access.in_game))
            );
        }
        // Missing is fine: the target may be created.
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(io_error(e)),
        _ => {}
    }
    let parent_is_dir = match access.server.parent() {
        Some(parent) => match tokio::fs::metadata(parent).await {
            Ok(m) => m.is_dir(),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => false,
            Err(e) => return Err(io_error(e)),
        },
        None => false,
    };
    if !parent_is_dir {
        return Err(context.runtime_error(format!(
            "write_file: {}: parent directory does not exist",
            access.in_game
        )));
    }
    context.record_effect(Effect::AppendFile {
        in_game: access.in_game,
        server: access.server,
        contents,
    });
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_read(string p, string e, object c, string g) { return 1; }
                int valid_write(string p, string e, object c, string g) { return 1; }
            "# },
        )
        .await
        .unwrap();
        vm
    }

    #[tokio::test]
    async fn appends_to_an_existing_file() {
        let root = TempLib::new("write-append");
        std::fs::write(root.join("o.txt"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                r#"int r; void create() { r = write_file("/o.txt", "new"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "oldnew"
        );
        assert_eq!(
            vm.global_state.committed_global(&writer, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn creates_a_missing_file() {
        let root = TempLib::new("write-create");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code("/w.c", r#"void create() { write_file("/o.txt", "x"); }"#)
            .await
            .unwrap();
        assert_eq!(std::fs::read_to_string(root.join("o.txt")).unwrap(), "x");
    }

    #[tokio::test]
    async fn two_writes_in_one_task_land_in_order() {
        let root = TempLib::new("write-order");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"void create() { write_file("/o.txt", "a"); write_file("/o.txt", "b"); }"#,
        )
        .await
        .unwrap();
        assert_eq!(std::fs::read_to_string(root.join("o.txt")).unwrap(), "ab");
    }

    /// The append lands at commit; a read in the same task sees the file as
    /// it was.
    #[tokio::test]
    async fn a_same_task_read_does_not_see_the_write() {
        let root = TempLib::new("write-then-read");
        std::fs::write(root.join("o.txt"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    string got;
                    void create() {
                        write_file("/o.txt", "new");
                        got = read_file("/o.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &writer, 0), "old");
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "oldnew"
        );
    }

    #[tokio::test]
    async fn an_aborted_task_writes_nothing() {
        let root = TempLib::new("write-abort");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            r#"void create() { write_file("/o.txt", "x"); throw("boom"); }"#,
        )
        .await
        .unwrap_err();
        assert!(!root.join("o.txt").exists());
    }

    #[tokio::test]
    async fn a_missing_parent_directory_is_a_runtime_error() {
        let root = TempLib::new("write-no-parent");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/w.c",
                r#"void create() { write_file("/nodir/o.txt", "x"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("write_file: /nodir/o.txt: parent directory does not exist"),
            "{err}"
        );
        assert!(!root.join("nodir").exists());
    }

    /// A parent path component that is a regular file is `ENOTDIR`, not
    /// `NotFound`.
    #[tokio::test]
    async fn a_non_directory_path_component_is_a_runtime_error() {
        let root = TempLib::new("write-enotdir");
        std::fs::write(root.join("f.txt"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/w.c",
                r#"void create() { write_file("/f.txt/o.txt", "x"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("write_file: /f.txt/o.txt:"), "{err}");
        assert!(!err.contains(root.to_str().unwrap()), "{err}");
    }

    #[tokio::test]
    async fn a_directory_target_is_a_runtime_error() {
        let root = TempLib::new("write-dir");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/w.c", r#"void create() { write_file("/d", "x"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("write_file: /d is a directory"), "{err}");
    }

    #[tokio::test]
    async fn a_refusing_master_denies_and_nothing_lands() {
        let root = TempLib::new("write-denied");
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string seen_efun;
                    int valid_write(string p, string e, object c, string g) {
                        seen_efun = e;
                        return 0;
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                r#"string err; void create() { err = catch(write_file("/o.txt", "x")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &writer, 0).contains("write_file: permission denied"));
        assert_eq!(committed_string(&vm, &master, 0), "write_file");
        assert!(!root.join("o.txt").exists());
    }

    #[tokio::test]
    async fn non_string_contents_are_a_runtime_error() {
        let root = TempLib::new("write-non-string");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/w.c",
                r#"void create() { mixed c = 5; write_file("/o.txt", c); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("write_file: contents must be a string"),
            "{err}"
        );
    }
}
