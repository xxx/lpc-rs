use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `rm(path)`: unlink the file, once the master's `valid_write` allows it.
/// Return 0 for a missing path, or 1 when removal is scheduled at commit.
/// A symlink is unlinked as a link, never followed.
pub async fn rm<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "rm", VALID_WRITE, 0).await?;
    let metadata = match tokio::fs::symlink_metadata(access.path().server()).await {
        Ok(metadata) => metadata,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            context.return_efun_result(LpcRef::from(0));
            return Ok(());
        }
        Err(e) => return Err(access.error(context, e)),
    };
    if !(metadata.is_file() || metadata.is_symlink()) {
        return Err(context.runtime_error(format!("rm: {} is not a file", access)));
    }
    context.record_effect(Effect::RemoveFile {
        path: access.into_path(),
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
    async fn a_missing_file_returns_zero() {
        let root = TempLib::new("rm-missing");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code("/r.c", r#"int create() { return rm("/o.txt"); }"#)
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn a_missing_parent_directory_returns_zero() {
        let root = TempLib::new("rm-missing-parent");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { return rm("/missing/o.txt"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn a_non_directory_path_component_is_still_an_error() {
        let root = TempLib::new("rm-nondirectory-parent");
        std::fs::write(root.join("f.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/r.c", r#"void create() { rm("/f.txt/o.txt"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("rm: /f.txt/o.txt:"), "{err}");
        assert!(!err.contains(root.to_str().unwrap()), "{err}");
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn a_dangling_symlink_is_removed() {
        let root = TempLib::new("rm-dangling-symlink");
        std::os::unix::fs::symlink(root.join("missing.txt"), root.join("link")).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code("/r.c", r#"int create() { return rm("/link"); }"#)
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::symlink_metadata(root.join("link"))
                .unwrap_err()
                .kind(),
            std::io::ErrorKind::NotFound
        );
    }

    #[tokio::test]
    async fn a_missing_file_still_requires_authorization() {
        let root = TempLib::new("rm-missing-denied");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_write(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let err = vm
            .initialize_process_from_code("/r.c", r#"void create() { rm("/missing.txt"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("rm: permission denied"), "{err}");
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
