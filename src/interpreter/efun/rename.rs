use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{
        efun_context::EfunContext,
        file_access::{authorize, parent_is_dir},
    },
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `rename(from, to)`: move `from` to `to`, once the master's `valid_write`
/// allows both; a `to` that is a directory takes `from` under its own
/// name. Checked now, moved at commit; 0 on success, the C convention.
pub async fn rename<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let from = authorize(context, "rename", VALID_WRITE, 0).await?;
    let to = authorize(context, "rename", VALID_WRITE, 1).await?;
    tokio::fs::symlink_metadata(from.path().server())
        .await
        .map_err(|e| from.error(context, e))?;
    let Some(directory_target) = from.path().in_directory(to.path()) else {
        return Err(context.runtime_error(format!("rename: {} cannot be moved", from)));
    };
    let target = match tokio::fs::metadata(to.path().server()).await {
        Ok(m) if m.is_dir() => directory_target,
        Ok(_) => to.path().clone(),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => to.path().clone(),
        Err(e) => return Err(to.error(context, e)),
    };
    if !parent_is_dir(context, target.server())
        .await
        .map_err(|e| to.error(context, e))?
    {
        return Err(to.error(context, "parent directory does not exist"));
    }
    context.record_effect(Effect::Rename {
        path: from.into_path(),
        to: target,
    });
    context.return_efun_result(LpcRef::from(0));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, string_global, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_write(string p, string e, object c, string g) { return 1; }",
        )
        .await
        .unwrap();
        vm
    }

    async fn error_of(vm: &Vm, code: &str) -> String {
        vm.initialize_process_from_code("/r.c", code)
            .await
            .unwrap_err()
            .to_string()
    }

    #[tokio::test]
    async fn rename_moves_the_file_when_the_task_commits_and_returns_zero() {
        let root = TempLib::new("rename-file");
        std::fs::write(root.join("a.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let mover = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int r = 7; void create() { r = rename("/a.txt", "/b.txt"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(!root.join("a.txt").exists());
        assert_eq!(std::fs::read_to_string(root.join("b.txt")).unwrap(), "x");
        assert_eq!(
            vm.global_state.committed_global(&mover, 0u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn a_directory_target_keeps_the_files_name() {
        let root = TempLib::new("rename-into-dir");
        std::fs::write(root.join("a.txt"), "x").unwrap();
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code("/r.c", r#"void create() { rename("/a.txt", "/d"); }"#)
            .await
            .unwrap();
        assert!(root.join("d/a.txt").is_file());
        assert!(!root.join("a.txt").exists());
    }

    #[tokio::test]
    async fn a_directory_rename_authorizes_the_requested_paths_and_exposes_the_pending_target() {
        let root = TempLib::new("rename-directory-view");
        std::fs::create_dir_all(root.join("room/archive")).unwrap();
        std::fs::write(root.join("room/a.txt"), "contents").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string paths = "";
                    int valid_write(string p, string e, object c, string g) {
                        paths += p + "\n";
                        return 1;
                    }
                    int valid_read(string p, string e, object c, string g) { return 1; }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let mover = vm
            .initialize_process_from_code(
                "/room/mover.c",
                indoc! { r#"
                    string got;
                    int old_size;
                    void create() {
                        rename("a.txt", "archive");
                        got = read_file("archive/a.txt");
                        old_size = file_size("a.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;

        assert_eq!(
            string_global(&vm, &master, "paths"),
            "/room/a.txt\n/room/archive\n"
        );
        assert_eq!(string_global(&vm, &mover, "got"), "contents");
        assert_eq!(
            vm.global_state.committed_global(&mover, 1u16),
            LpcRef::from(-1)
        );
        assert!(!root.join("room/a.txt").exists());
        assert_eq!(
            std::fs::read_to_string(root.join("room/archive/a.txt")).unwrap(),
            "contents"
        );
    }

    #[tokio::test]
    async fn a_pending_rename_reads_its_source_from_disk_before_earlier_writes_commit() {
        let root = TempLib::new("rename-pending-source");
        std::fs::write(root.join("a.txt"), "disk").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_write(string p, string e, object c, string g) { return 1; }
                int valid_read(string p, string e, object c, string g) { return 1; }
            "# },
        )
        .await
        .unwrap();
        let mover = vm
            .initialize_process_from_code(
                "/mover.c",
                indoc! { r#"
                    string got;
                    void create() {
                        write_file("/a.txt", "+pending");
                        rename("/a.txt", "/b.txt");
                        got = read_file("/b.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;

        assert_eq!(string_global(&vm, &mover, "got"), "disk");
        assert_eq!(
            std::fs::read_to_string(root.join("b.txt")).unwrap(),
            "disk+pending"
        );
    }

    #[tokio::test]
    async fn a_missing_source_is_an_error() {
        let root = TempLib::new("rename-missing");
        let vm = allowing_vm(&root).await;
        let err = error_of(&vm, r#"void create() { rename("/a.txt", "/b.txt"); }"#).await;
        assert!(err.contains("rename: /a.txt:"), "{err}");
    }

    #[tokio::test]
    async fn a_target_in_a_missing_directory_is_an_error() {
        let root = TempLib::new("rename-no-parent");
        std::fs::write(root.join("a.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let err = error_of(&vm, r#"void create() { rename("/a.txt", "/no/b.txt"); }"#).await;
        assert!(
            err.contains("rename: /no/b.txt: parent directory does not exist"),
            "{err}"
        );
        assert!(root.join("a.txt").exists());
    }

    #[tokio::test]
    async fn an_aborted_task_moves_nothing() {
        let root = TempLib::new("rename-abort");
        std::fs::write(root.join("a.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        error_of(
            &vm,
            r#"void create() { rename("/a.txt", "/b.txt"); throw("boom"); }"#,
        )
        .await;
        assert!(root.join("a.txt").exists());
        assert!(!root.join("b.txt").exists());
    }

    /// Both ends are put to `valid_write`; a refusal of the target denies.
    #[tokio::test]
    async fn the_target_needs_write_permission_too() {
        let root = TempLib::new("rename-denied-target");
        std::fs::write(root.join("a.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_write(string p, string e, object c, string g) { return p != "/b.txt"; }"#,
        )
        .await
        .unwrap();
        let mover = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; void create() { err = catch(rename("/a.txt", "/b.txt")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &mover, 0).contains("rename: permission denied"));
        assert!(root.join("a.txt").exists());
    }
}
