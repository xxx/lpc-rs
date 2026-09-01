use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `rm(path)`: unlink the file, once the master's `valid_write` allows it.
/// Checked now — it exists and is a file or a symlink — and removed at
/// commit. A symlink is unlinked as a link, never followed.
pub async fn rm<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "rm", VALID_WRITE, 1 as RegisterSize).await?;
    let metadata = tokio::fs::symlink_metadata(&access.server)
        .await
        .map_err(|e| context.runtime_error(format!("rm: {}: {e}", access.in_game)))?;
    if !(metadata.is_file() || metadata.is_symlink()) {
        return Err(context.runtime_error(format!("rm: {} is not a file", access.in_game)));
    }
    context.record_effect(Effect::RemoveFile {
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

    #[tokio::test]
    async fn removes_the_file_when_the_task_commits() {
        let root = TempLib::new("rm-file");
        std::fs::write(root.join("o.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let remover = vm
            .initialize_process_from_code("/r.c", r#"int r; void create() { r = rm("/o.txt"); }"#)
            .await
            .unwrap()
            .context
            .process;
        assert!(!root.join("o.txt").exists());
        assert_eq!(
            vm.global_state.committed_global(&remover, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn a_missing_file_is_a_runtime_error() {
        let root = TempLib::new("rm-missing");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/r.c", r#"void create() { rm("/o.txt"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("rm: /o.txt:"), "{err}");
        assert!(
            !err.contains(vm.global_state.config.lib_dir.as_str()),
            "server path leaked: {err}"
        );
    }

    /// A symlink to a directory is unlinked as a link; the directory it
    /// pointed at stays.
    #[tokio::test]
    async fn a_symlink_to_a_directory_is_removed_without_following_it() {
        let root = TempLib::new("rm-symlink-dir");
        std::fs::create_dir_all(root.join("d")).unwrap();
        std::os::unix::fs::symlink(root.join("d"), root.join("link")).unwrap();
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code("/r.c", r#"void create() { rm("/link"); }"#)
            .await
            .unwrap();
        assert!(std::fs::symlink_metadata(root.join("link")).is_err());
        assert!(root.join("d").is_dir());
    }

    #[tokio::test]
    async fn a_directory_is_not_a_file() {
        let root = TempLib::new("rm-dir");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/r.c", r#"void create() { rm("/d"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("rm: /d is not a file"), "{err}");
        assert!(root.join("d").is_dir());
    }

    #[tokio::test]
    async fn an_aborted_task_removes_nothing() {
        let root = TempLib::new("rm-abort");
        std::fs::write(root.join("o.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/r.c",
            r#"void create() { rm("/o.txt"); throw("boom"); }"#,
        )
        .await
        .unwrap_err();
        assert!(root.join("o.txt").exists());
    }

    #[tokio::test]
    async fn a_refusing_master_denies_and_the_file_stays() {
        let root = TempLib::new("rm-denied");
        std::fs::write(root.join("o.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                r#"string seen_efun; int valid_write(string p, string e, object c, string g) { seen_efun = e; return 0; }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let remover = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; void create() { err = catch(rm("/o.txt")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &remover, 0).contains("rm: permission denied"));
        assert_eq!(committed_string(&vm, &master, 0), "rm");
        assert!(root.join("o.txt").exists());
    }
}
