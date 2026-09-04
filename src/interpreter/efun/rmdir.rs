use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `rmdir(path)`: remove the empty directory, once the master's
/// `valid_write` allows it. Checked now (a directory with no entries),
/// removed at commit; 1 on success.
pub async fn rmdir<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "rmdir", VALID_WRITE, 0).await?;
    let io_error =
        |e: std::io::Error| context.runtime_error(format!("rmdir: {}: {e}", access.in_game));
    let metadata = tokio::fs::symlink_metadata(&access.server)
        .await
        .map_err(io_error)?;
    if !metadata.is_dir() {
        return Err(context.runtime_error(format!("rmdir: {} is not a directory", access.in_game)));
    }
    let mut entries = tokio::fs::read_dir(&access.server)
        .await
        .map_err(io_error)?;
    if entries.next_entry().await.map_err(io_error)?.is_some() {
        return Err(context.runtime_error(format!("rmdir: {} is not empty", access.in_game)));
    }
    context.record_effect(Effect::RemoveDir {
        in_game: access.in_game,
        server: access.server,
    });
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, temp_lib_config},
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
    async fn rmdir_removes_an_empty_directory_when_the_task_commits() {
        let root = TempLib::new("rmdir-empty");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let remover = vm
            .initialize_process_from_code("/r.c", r#"int r; void create() { r = rmdir("/d"); }"#)
            .await
            .unwrap()
            .context
            .process;
        assert!(!root.join("d").exists());
        assert_eq!(
            vm.global_state.committed_global(&remover, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn a_directory_with_entries_is_an_error() {
        let root = TempLib::new("rmdir-full");
        std::fs::create_dir_all(root.join("d")).unwrap();
        std::fs::write(root.join("d/f"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let err = error_of(&vm, r#"void create() { rmdir("/d"); }"#).await;
        assert!(err.contains("rmdir: /d is not empty"), "{err}");
        assert!(root.join("d").is_dir());
    }

    #[tokio::test]
    async fn a_file_is_not_a_directory() {
        let root = TempLib::new("rmdir-file");
        std::fs::write(root.join("f"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let err = error_of(&vm, r#"void create() { rmdir("/f"); }"#).await;
        assert!(err.contains("rmdir: /f is not a directory"), "{err}");
        assert!(root.join("f").exists());
    }

    #[tokio::test]
    async fn a_missing_directory_is_an_error() {
        let root = TempLib::new("rmdir-missing");
        let vm = allowing_vm(&root).await;
        let err = error_of(&vm, r#"void create() { rmdir("/d"); }"#).await;
        assert!(err.contains("rmdir: /d:"), "{err}");
    }

    #[tokio::test]
    async fn an_aborted_task_removes_nothing() {
        let root = TempLib::new("rmdir-abort");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        error_of(&vm, r#"void create() { rmdir("/d"); throw("boom"); }"#).await;
        assert!(root.join("d").is_dir());
    }
}
