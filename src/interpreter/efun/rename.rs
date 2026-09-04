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
    let io_error =
        |path: &str, e: std::io::Error| context.runtime_error(format!("rename: {path}: {e}"));
    tokio::fs::symlink_metadata(&from.server)
        .await
        .map_err(|e| io_error(&from.in_game, e))?;
    let Some(name) = from.server.file_name() else {
        return Err(context.runtime_error(format!("rename: {} cannot be moved", from.in_game)));
    };
    let target = match tokio::fs::metadata(&to.server).await {
        Ok(m) if m.is_dir() => to.server.join(name),
        Ok(_) => to.server,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => to.server,
        Err(e) => return Err(io_error(&to.in_game, e)),
    };
    if !parent_is_dir(&target)
        .await
        .map_err(|e| io_error(&to.in_game, e))?
    {
        return Err(context.runtime_error(format!(
            "rename: {}: parent directory does not exist",
            to.in_game
        )));
    }
    context.record_effect(Effect::Rename {
        in_game: from.in_game,
        from: from.server,
        to: target,
    });
    context.return_efun_result(LpcRef::from(0));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, temp_lib_config},
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
