use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    file_view::EntryKind,
    lpc_ref::LpcRef,
    stm::Effect,
};

/// `rm(path)`: unlink the file, once the master's `valid_write` allows it.
/// Return 0 for a missing path, or 1 when removal is scheduled at commit.
/// A symlink is unlinked as a link, never followed.
pub async fn rm<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "rm", VALID_WRITE, 0).await?;
    match access.entry_kind(context).await? {
        EntryKind::Missing => {
            context.return_efun_result(LpcRef::from(0));
            return Ok(());
        }
        EntryKind::File | EntryKind::Symlink => {}
        EntryKind::Directory | EntryKind::Other => {
            return Err(context.runtime_error(format!("rm: {} is not a file", access)));
        }
    }
    context.record_effect(Effect::RemoveFile {
        path: access.into_path(),
    });
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

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
    async fn removes_new_files_in_both_null_modes_including_empty_files() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for contents in ["", "pending"] {
                let root = TempLib::new("rm-pending-write");
                let config = ConfigBuilder::default()
                    .lib_dir(root.to_str().unwrap())
                    .null_mode(mode)
                    .build()
                    .unwrap();
                let vm = Vm::new(config);
                vm.initialize_process_from_code(
                    "/secure/master.c",
                    indoc! { r#"
                        int valid_write(string p, string e, object c, string g) { return 1; }
                        int valid_read(string p, string e, object c, string g) { return 1; }
                    "# },
                )
                .await
                .unwrap();
                let task = vm
                    .initialize_process_from_code(
                        "/r.c",
                        format!(
                            r#"
                    int create() {{
                        write_file("/new", "{contents}");
                        int removed = rm("/new");
                        return removed == 1 && file_size("/new") == -1;
                    }}
                "#
                        ),
                    )
                    .await
                    .unwrap();
                assert_eq!(
                    task.result(),
                    Some(LpcRef::from(1)),
                    "{mode:?} {contents:?}"
                );
                assert!(!root.join("new").exists());
            }
        }
    }

    #[tokio::test]
    async fn removes_new_mapping_and_object_save_files() {
        let root = TempLib::new("rm-pending-save");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int create() {
                        save_map(([ "x": 1 ]), "/map");
                        save_object("/object");
                        int first = rm("/map.o");
                        int second = rm("/object.o");
                        return first == 1 && second == 1;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("map.o").exists());
        assert!(!root.join("object.o").exists());
    }

    #[tokio::test]
    async fn repeated_removal_returns_zero_until_the_file_is_recreated() {
        let root = TempLib::new("rm-recreate");
        std::fs::write(root.join("file"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int create() {
                        int first = rm("/file");
                        int second = rm("/file");
                        write_file("/file", "replacement");
                        int third = rm("/file");
                        int fourth = rm("/file");
                        return first == 1 && second == 0 && third == 1 && fourth == 0;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("file").exists());
    }

    #[tokio::test]
    async fn overwriting_a_pending_new_file_keeps_only_the_replacement() {
        let root = TempLib::new("rm-overwrite");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int create() {
                        write_file("/file", "first");
                        int removed = rm("/file");
                        write_file("/file", "second");
                        return removed;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read_to_string(root.join("file")).unwrap(),
            "second"
        );
    }

    #[tokio::test]
    async fn rejects_a_pending_directory_even_after_an_incompatible_write() {
        for write in ["", "write_file(\"/dir\", \"data\");"] {
            let root = TempLib::new("rm-pending-directory");
            let vm = allowing_vm(&root).await;
            let error = vm
                .initialize_process_from_code(
                    "/r.c",
                    format!(
                        r#"
                void create() {{ mkdir("/dir"); {write} rm("/dir"); }}
            "#
                    ),
                )
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains("rm: /dir is not a file"), "{error}");
            assert!(!root.join("dir").exists());
        }
    }

    #[tokio::test]
    async fn an_already_removed_directory_is_missing() {
        let root = TempLib::new("rm-removed-directory");
        std::fs::create_dir(root.join("dir")).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { rmdir("/dir"); return rm("/dir"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
        assert!(!root.join("dir").exists());
    }

    #[tokio::test]
    async fn rename_moves_the_removable_entry_to_the_destination() {
        for destination_exists in [false, true] {
            let root = TempLib::new("rm-rename");
            std::fs::write(root.join("from"), "old").unwrap();
            if destination_exists {
                std::fs::write(root.join("to"), "overwritten").unwrap();
            }
            let vm = allowing_vm(&root).await;
            let task = vm
                .initialize_process_from_code(
                    "/r.c",
                    indoc! { r#"
                        int create() {
                            rename("/from", "/to");
                            int source = rm("/from");
                            int destination = rm("/to");
                            int again = rm("/to");
                            return source == 0 && destination == 1 && again == 0;
                        }
                    "# },
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)));
            assert!(!root.join("from").exists());
            assert!(!root.join("to").exists());
        }
    }

    #[tokio::test]
    async fn a_self_rename_preserves_the_entry() {
        let root = TempLib::new("rm-self-rename");
        std::fs::write(root.join("file"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { rename("/file", "/file"); return rm("/file"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("file").exists());
    }

    #[tokio::test]
    async fn a_rename_with_an_already_removed_source_preserves_the_destination() {
        let root = TempLib::new("rm-failed-rename");
        std::fs::write(root.join("from"), "old").unwrap();
        std::fs::write(root.join("to"), "keep until removed").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int create() {
                        rm("/from");
                        rename("/from", "/to");
                        return rm("/to");
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("to").exists());
    }

    #[tokio::test]
    async fn a_renamed_directory_is_not_a_file() {
        let root = TempLib::new("rm-renamed-directory");
        std::fs::create_dir(root.join("from")).unwrap();
        let vm = allowing_vm(&root).await;
        let error = vm
            .initialize_process_from_code(
                "/r.c",
                r#"void create() { rename("/from", "/to"); rm("/to"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(error.contains("rm: /to is not a file"), "{error}");
        assert!(root.join("from").is_dir());
        assert!(!root.join("to").exists());
    }

    #[tokio::test]
    async fn observes_file_creation_by_the_authorization_callback() {
        let root = TempLib::new("rm-authorize-create");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_write(string p, string e, object c, string g) {
                    if (e == "rm") write_file(p, "callback");
                    return 1;
                }
            "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code("/r.c", r#"int create() { return rm("/new"); }"#)
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("new").exists());
    }

    #[tokio::test]
    async fn observes_file_removal_by_the_authorization_callback() {
        let root = TempLib::new("rm-authorize-remove");
        std::fs::write(root.join("file"), "old").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int active;
                int valid_write(string p, string e, object c, string g) {
                    if (e == "rm" && !active) { active = 1; rm(p); active = 0; }
                    return 1;
                }
            "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code("/r.c", r#"int create() { return rm("/file"); }"#)
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
        assert!(!root.join("file").exists());
    }

    #[tokio::test]
    async fn denying_removal_preserves_an_earlier_pending_write() {
        let root = TempLib::new("rm-denied-pending");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_write(string p, string e, object c, string g) { return e != "rm"; }"#,
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int create() {
                        write_file("/new", "pending");
                        string err = catch(rm("/new"));
                        return strindex(err, "rm: permission denied") >= 0;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read_to_string(root.join("new")).unwrap(),
            "pending"
        );
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn renamed_symlinks_are_unlinked_without_following_their_targets() {
        for target in ["missing", "dir", "file"] {
            let root = TempLib::new("rm-renamed-symlink");
            std::fs::create_dir(root.join("dir")).unwrap();
            std::fs::write(root.join("file"), "untouched").unwrap();
            std::os::unix::fs::symlink(target, root.join("link")).unwrap();
            let vm = allowing_vm(&root).await;
            let task = vm
                .initialize_process_from_code(
                    "/r.c",
                    r#"int create() { rename("/link", "/moved"); return rm("/moved"); }"#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)));
            assert!(std::fs::symlink_metadata(root.join("moved")).is_err());
            assert!(root.join("dir").is_dir());
            assert_eq!(
                std::fs::read_to_string(root.join("file")).unwrap(),
                "untouched"
            );
        }
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn removing_an_unreadable_rename_destination_does_not_require_a_read() {
        use std::os::unix::fs::PermissionsExt;

        let root = TempLib::new("rm-unreadable-rename");
        std::fs::write(root.join("from"), "unreadable").unwrap();
        std::fs::set_permissions(root.join("from"), std::fs::Permissions::from_mode(0o000))
            .unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int create() { rename("/from", "/to"); return rm("/to"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("from").exists());
        assert!(!root.join("to").exists());
    }

    #[tokio::test]
    async fn abort_discards_pending_creation_and_removal() {
        let root = TempLib::new("rm-pending-abort");
        let vm = allowing_vm(&root).await;
        let error = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    void create() {
                        write_file("/new", "pending");
                        if (rm("/new") != 1) throw("removal missed creation");
                        throw("abort after removal");
                    }
                "# },
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(error.contains("abort after removal"), "{error}");
        assert!(!root.join("new").exists());
    }

    #[tokio::test]
    async fn retries_rebuild_the_pending_entry_view_without_delivering_rejected_effects() {
        use std::sync::Arc;

        use crate::interpreter::vm::global_state::GlobalState;

        let root = TempLib::new("rm-pending-retry");
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(
            Arc::new(temp_lib_config(&root)),
            tx,
            3,
        ));
        state
            .object_space
            .create_process_from_code(
                "/secure/master.c",
                "int valid_write(string p, string e, object c, string g) { return 1; }",
            )
            .await
            .unwrap();
        let task = state
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
            int create() {
                write_file("/attempts", "once");
                write_file("/file", "pending");
                return rm("/file");
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(state.attempt_telemetry().conflicts, 3);
        assert_eq!(
            std::fs::read_to_string(root.join("attempts")).unwrap(),
            "once"
        );
        assert!(!root.join("file").exists());
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
