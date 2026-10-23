use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    file_view,
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `rmdir(path)`: remove the empty directory, once the master's
/// `valid_write` allows it. Checked against this task's pending entry changes,
/// removed at commit; 1 on success.
pub async fn rmdir<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "rmdir", VALID_WRITE, 0).await?;
    if !file_view::directory_is_empty(context.txn(), access.path().server())
        .await
        .map_err(|e| {
            if e.kind() == std::io::ErrorKind::NotADirectory {
                context.runtime_error(format!("rmdir: {} is not a directory", access))
            } else {
                access.error(context, e)
            }
        })?
    {
        return Err(context.runtime_error(format!("rmdir: {} is not empty", access)));
    }
    context.record_effect(Effect::RemoveDir {
        path: access.into_path(),
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

    #[tokio::test]
    async fn pending_child_removal_makes_its_parent_empty() {
        let root = TempLib::new("rmdir-child-parent");
        std::fs::create_dir_all(root.join("d/child/grandchild")).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { return rmdir("/d/child/grandchild") && rmdir("/d/child") && rmdir("/d"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn a_second_removal_is_a_missing_path_error() {
        let root = TempLib::new("rmdir-duplicate");
        std::fs::create_dir(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let caller = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; int result; void create() { result = rmdir("/d"); err = catch(rmdir("/d")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&caller, 1u16),
            LpcRef::from(1)
        );
        assert!(committed_string(&vm, &caller, 0).contains("rmdir: /d: No such file or directory"));
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn an_uncaught_second_removal_aborts_the_first() {
        let root = TempLib::new("rmdir-duplicate-abort");
        std::fs::create_dir(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let err = error_of(&vm, r#"void create() { rmdir("/d"); rmdir("/d"); }"#).await;
        assert!(
            err.contains("rmdir: /d: No such file or directory"),
            "{err}"
        );
        assert!(root.join("d").is_dir());
    }

    #[tokio::test]
    async fn newly_created_directories_can_be_removed_before_commit() {
        let root = TempLib::new("rmdir-created");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { mkdir("/d"); mkdir("/d/child"); return rmdir("/d/child") && rmdir("/d"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn pending_writes_saves_and_child_directories_prevent_removal() {
        for (effect, child) in [
            (r#"write_file("/d/file", "");"#, "file"),
            (r#"save_object("/d/object");"#, "object.o"),
            (r#"save_map(([ "x": 1 ]), "/d/map");"#, "map.o"),
            (r#"mkdir("/d/child");"#, "child"),
        ] {
            let root = TempLib::new("rmdir-pending-child");
            std::fs::create_dir(root.join("d")).unwrap();
            let vm = allowing_vm(&root).await;
            let caller = vm
                .initialize_process_from_code(
                    "/r.c",
                    format!("string err; void create() {{ {effect} err = catch(rmdir(\"/d\")); }}"),
                )
                .await
                .unwrap()
                .context
                .process;
            assert!(
                committed_string(&vm, &caller, 0).contains("rmdir: /d is not empty"),
                "{effect}"
            );
            assert!(root.join("d").is_dir());
            assert!(root.join("d").join(child).exists());
        }
    }

    #[tokio::test]
    async fn pending_file_removal_accounts_for_disk_and_new_binary_or_empty_files() {
        let root = TempLib::new("rmdir-removed-files");
        std::fs::create_dir(root.join("d")).unwrap();
        std::fs::write(root.join("d/binary"), [0xff, 0xfe]).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { write_file("/d/empty", ""); rm("/d/empty"); rm("/d/binary"); return rmdir("/d"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("d").exists());
    }

    #[tokio::test]
    async fn moving_the_last_file_out_makes_its_parent_empty() {
        let root = TempLib::new("rmdir-moved-child-out");
        std::fs::create_dir(root.join("d")).unwrap();
        std::fs::write(root.join("d/file"), "keep").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { rename("/d/file", "/file"); return rmdir("/d"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("d").exists());
        assert_eq!(std::fs::read_to_string(root.join("file")).unwrap(), "keep");
    }

    #[tokio::test]
    async fn moving_a_file_into_a_directory_prevents_removal() {
        let root = TempLib::new("rmdir-moved-child-in");
        std::fs::create_dir(root.join("d")).unwrap();
        std::fs::write(root.join("file"), "keep").unwrap();
        let vm = allowing_vm(&root).await;
        let caller = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; void create() { rename("/file", "/d"); err = catch(rmdir("/d")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &caller, 0).contains("rmdir: /d is not empty"));
        assert_eq!(
            std::fs::read_to_string(root.join("d/file")).unwrap(),
            "keep"
        );
    }

    #[tokio::test]
    async fn a_moved_directory_retains_its_contents_and_earlier_child_removals() {
        for remove_child in [false, true] {
            let root = TempLib::new("rmdir-moved-directory");
            std::fs::create_dir(root.join("from")).unwrap();
            std::fs::write(root.join("from/file"), "keep").unwrap();
            let vm = allowing_vm(&root).await;
            let before = if remove_child {
                r#"rm("/from/file");"#
            } else {
                ""
            };
            let task = vm
                .initialize_process_from_code(
                    "/r.c",
                    format!(r#"int create() {{ {before} rename("/from", "/to"); return !catch(rmdir("/to")); }}"#),
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(i64::from(remove_child))));
            assert!(!root.join("from").exists());
            if remove_child {
                assert!(!root.join("to").exists());
            } else {
                assert_eq!(
                    std::fs::read_to_string(root.join("to/file")).unwrap(),
                    "keep"
                );
            }
        }
    }

    #[tokio::test]
    async fn a_failed_move_over_a_nonempty_directory_keeps_the_source_parent_nonempty() {
        let root = TempLib::new("rmdir-failed-directory-move");
        std::fs::create_dir_all(root.join("from/child")).unwrap();
        std::fs::create_dir_all(root.join("to/child")).unwrap();
        std::fs::write(root.join("to/child/file"), "keep").unwrap();
        let vm = allowing_vm(&root).await;
        let caller = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; void create() { rename("/from/child", "/to"); err = catch(rmdir("/from")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &caller, 0).contains("rmdir: /from is not empty"));
        assert!(root.join("from/child").is_dir());
        assert_eq!(
            std::fs::read_to_string(root.join("to/child/file")).unwrap(),
            "keep"
        );
    }

    #[tokio::test]
    async fn authorization_changes_are_visible_to_the_emptiness_check() {
        let root = TempLib::new("rmdir-authorize-child");
        std::fs::create_dir(root.join("d")).unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_write(string p, string e, object c, string g) {
                    if (e == "rmdir") write_file(p + "/child", "callback");
                    return 1;
                }
            "# },
        )
        .await
        .unwrap();
        let caller = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string err; void create() { err = catch(rmdir("/d")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &caller, 0).contains("rmdir: /d is not empty"));
        assert_eq!(
            std::fs::read_to_string(root.join("d/child")).unwrap(),
            "callback"
        );
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn removing_a_child_link_never_removes_its_target_directory() {
        let root = TempLib::new("rmdir-symlink-child");
        std::fs::create_dir(root.join("d")).unwrap();
        std::fs::create_dir(root.join("target")).unwrap();
        std::os::unix::fs::symlink("../target", root.join("d/link")).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { rm("/d/link"); return rmdir("/d"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("d").exists());
        assert!(root.join("target").is_dir());
    }
}
