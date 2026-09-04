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

/// `mkdir(path)`: make the directory, once the master's `valid_write`
/// allows it. Checked now (nothing at the path, its parent a directory),
/// created at commit; 1 on success.
pub async fn mkdir<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "mkdir", VALID_WRITE, 0).await?;
    let io_error =
        |e: std::io::Error| context.runtime_error(format!("mkdir: {}: {e}", access.in_game));
    match tokio::fs::symlink_metadata(&access.server).await {
        Ok(_) => {
            return Err(context.runtime_error(format!("mkdir: {} exists", access.in_game)));
        }
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(io_error(e)),
        Err(_) => {}
    }
    if !parent_is_dir(&access.server).await.map_err(io_error)? {
        return Err(context.runtime_error(format!(
            "mkdir: {}: parent directory does not exist",
            access.in_game
        )));
    }
    context.record_effect(Effect::CreateDir {
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
    async fn mkdir_creates_the_directory_when_the_task_commits() {
        let root = TempLib::new("mkdir-new");
        let vm = allowing_vm(&root).await;
        let maker = vm
            .initialize_process_from_code("/m.c", r#"int r; void create() { r = mkdir("/d"); }"#)
            .await
            .unwrap()
            .context
            .process;
        assert!(root.join("d").is_dir());
        assert_eq!(
            vm.global_state.committed_global(&maker, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn an_existing_path_is_an_error() {
        let root = TempLib::new("mkdir-exists");
        std::fs::write(root.join("d"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/m.c", r#"void create() { mkdir("/d"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("mkdir: /d exists"), "{err}");
    }

    #[tokio::test]
    async fn a_missing_parent_is_an_error() {
        let root = TempLib::new("mkdir-parent");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/m.c", r#"void create() { mkdir("/a/b"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("mkdir: /a/b: parent directory does not exist"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn an_aborted_task_creates_nothing() {
        let root = TempLib::new("mkdir-abort");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code("/m.c", r#"void create() { mkdir("/d"); throw("boom"); }"#)
            .await
            .unwrap_err();
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn a_refusing_master_denies() {
        let root = TempLib::new("mkdir-denied");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_write(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let maker = vm
            .initialize_process_from_code(
                "/m.c",
                r#"string err; void create() { err = catch(mkdir("/d")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &maker, 0).contains("mkdir: permission denied"));
        assert!(!root.join("d").exists());
    }
}
