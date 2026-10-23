use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    file_view::EntryKind,
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `mkdir(path)`: make the directory, once the master's `valid_write`
/// allows it. Checked now (nothing at the path, its parent a directory),
/// created at commit; 1 on success. This task's later file efuns see the
/// directory as existing.
pub async fn mkdir<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "mkdir", VALID_WRITE, 0).await?;
    if access.entry_kind(context).await? != EntryKind::Missing {
        return Err(context.runtime_error(format!("mkdir: {} exists", access)));
    }
    access.require_parent(context).await?;
    context.record_effect(Effect::CreateDir {
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
            "int valid_read(string p, string e, object c, string g) { return 1; }\n\
             int valid_write(string p, string e, object c, string g) { return 1; }",
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
    async fn a_directory_made_this_task_takes_a_save_and_a_nested_directory() {
        let root = TempLib::new("mkdir-pending");
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/m.c",
            r#"int x = 7; void create() { mkdir("/p"); mkdir("/p/q"); write_file("/p/f", "hi"); save_object("/p/q/m"); }"#,
        )
        .await
        .unwrap();
        assert!(root.join("p/q").is_dir());
        assert_eq!(std::fs::read_to_string(root.join("p/f")).unwrap(), "hi");
        assert!(root.join("p/q/m.o").is_file());
    }

    #[tokio::test]
    async fn a_directory_made_this_task_exists_to_a_second_mkdir() {
        let root = TempLib::new("mkdir-twice");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/m.c", r#"void create() { mkdir("/d"); mkdir("/d"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("mkdir: /d exists"), "{err}");
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn removed_directories_can_be_recreated_with_new_contents_before_commit() {
        let root = TempLib::new("mkdir-recreate");
        std::fs::create_dir_all(root.join("d/child")).unwrap();
        std::fs::write(root.join("d/child/old"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/m.c",
                r#"int create() {
                    rm("/d/child/old");
                    rmdir("/d/child");
                    rmdir("/d");
                    mkdir("/d");
                    mkdir("/d/child");
                    write_file("/d/child/new", "new");
                    return file_size("/d") == -2 &&
                        file_size("/d/child/old") == -1 &&
                        read_file("/d/child/new") == "new";
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("d/child/old").exists());
        assert_eq!(
            std::fs::read_to_string(root.join("d/child/new")).unwrap(),
            "new"
        );
    }

    #[tokio::test]
    async fn recreating_a_directory_is_rolled_back_with_the_task() {
        let root = TempLib::new("mkdir-recreate-abort");
        std::fs::create_dir(root.join("d")).unwrap();
        std::fs::write(root.join("d/old"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/m.c",
                r#"void create() {
                    rm("/d/old");
                    rmdir("/d");
                    mkdir("/d");
                    write_file("/d/new", "new");
                    throw("boom");
                }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("boom"), "{err}");
        assert_eq!(std::fs::read_to_string(root.join("d/old")).unwrap(), "old");
        assert!(!root.join("d/new").exists());
    }

    #[tokio::test]
    async fn a_pending_file_write_occupies_the_path() {
        let root = TempLib::new("mkdir-pending-file");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/m.c",
                r#"void create() { write_file("/d", "file"); mkdir("/d"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("mkdir: /d exists"), "{err}");
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn a_removed_file_path_can_become_a_directory() {
        let root = TempLib::new("mkdir-removed-file");
        std::fs::write(root.join("d"), "file").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/m.c",
                r#"int create() { rm("/d"); return mkdir("/d"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(root.join("d").is_dir());
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
