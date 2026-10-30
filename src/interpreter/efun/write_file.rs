use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
    stm::Effect,
};

/// Stage an authorized append (mode 0/default) or whole-file replacement (mode 1).
pub async fn write_file<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(contents) = context.arg(1).as_str() else {
        return Err(context.runtime_error("write_file: contents must be a string"));
    };
    let LpcRef::Int(value) = context.arg(2) else {
        return Err(context.runtime_error("write_file: mode must be an int"));
    };
    let mode = value.0;
    if !(0..=1).contains(&mode) {
        return Err(context.runtime_error("write_file: mode must be 0 (append) or 1 (overwrite)"));
    }
    let contents = contents.to_owned();
    let access = authorize(context, "write_file", VALID_WRITE, 0).await?;
    let io_error = |e: std::io::Error| access.error(context, e);
    match tokio::fs::metadata(access.path().server()).await {
        Ok(m) if m.is_dir() => {
            return Err(context.runtime_error(format!("write_file: {} is a directory", access)));
        }
        // Missing is fine: the target may be created.
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => return Err(io_error(e)),
        _ => {}
    }
    access.require_parent(context).await?;
    let path = access.into_path();
    context.record_effect(if mode == 0 {
        Effect::AppendFile { path, contents }
    } else {
        Effect::WriteFile { path, contents }
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
        compiler::CompilerBuilder,
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        allowing_vm_with_mode(root, NullMode::Zero).await
    }

    async fn allowing_vm_with_mode(root: &TempLib, mode: NullMode) -> Vm {
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .call_out_preserve_player(true)
                .max_execution_time(4000_u64)
                .build()
                .unwrap(),
        );
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
    async fn overwrites_the_whole_file_and_reads_pending_utf8_bytes() {
        let root = TempLib::new("write-overwrite");
        std::fs::write(root.join("o.txt"), "old contents that are longer").unwrap();
        let vm = allowing_vm(&root).await;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    int result;
                    string text;
                    string chars;
                    int size;
                    void create() {
                        result = write_file("/o.txt", "猫" + sprintf("%c", 0) + "\n", 1);
                        text = read_file("/o.txt");
                        chars = read_chars("/o.txt", 0, 5);
                        size = file_size("/o.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&writer, 0u16),
            LpcRef::from(1)
        );
        assert_eq!(committed_string(&vm, &writer, 1), "猫\0\n");
        assert_eq!(committed_string(&vm, &writer, 2), "猫\0\n");
        assert_eq!(
            vm.global_state.committed_global(&writer, 3u16),
            LpcRef::from(5)
        );
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "猫\0\n"
        );
    }

    #[tokio::test]
    async fn overwrite_creates_missing_files_and_can_write_empty_contents() {
        let root = TempLib::new("write-overwrite-empty");
        std::fs::write(root.join("old.txt"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                void create() {
                    write_file("/new.txt", "new", 1);
                    write_file("/empty.txt", "", 1);
                    write_file("/old.txt", "", 1);
                }
            "# },
        )
        .await
        .unwrap();
        for (path, expected) in [("new.txt", "new"), ("empty.txt", ""), ("old.txt", "")] {
            assert_eq!(std::fs::read_to_string(root.join(path)).unwrap(), expected);
        }
    }

    #[tokio::test]
    async fn omitted_and_explicit_zero_modes_append_in_both_native_null_modes() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = TempLib::new("write-default-mode");
            std::fs::write(root.join("o.txt"), "old").unwrap();
            let vm = allowing_vm_with_mode(&root, mode).await;
            vm.initialize_process_from_code(
                "/w.c",
                r#"void create() { write_file("/o.txt", "a"); write_file("/o.txt", "b", 0); }"#,
            )
            .await
            .unwrap();
            assert_eq!(
                std::fs::read_to_string(root.join("o.txt")).unwrap(),
                "oldab"
            );
        }
    }

    #[tokio::test]
    async fn appends_and_overwrites_land_in_order_and_reads_follow_each_change() {
        let root = TempLib::new("write-overwrite-order");
        std::fs::write(root.join("o.txt"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    string first;
                    string second;
                    string third;
                    string fourth;
                    void create() {
                        write_file("/o.txt", "a");
                        first = read_file("/o.txt");
                        write_file("/o.txt", "b", 1);
                        second = read_file("/o.txt");
                        write_file("/o.txt", "c", 0);
                        third = read_file("/o.txt");
                        write_file("/o.txt", "d", 1);
                        fourth = read_file("/o.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        for (index, expected) in ["olda", "b", "bc", "d"].into_iter().enumerate() {
            assert_eq!(committed_string(&vm, &writer, index as u16), expected);
        }
        assert_eq!(std::fs::read_to_string(root.join("o.txt")).unwrap(), "d");
    }

    #[tokio::test]
    async fn removal_and_overwrite_follow_the_pending_file_view() {
        let root = TempLib::new("write-overwrite-removal");
        std::fs::write(root.join("o.txt"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    string text;
                    int missing;
                    void create() {
                        rm("/o.txt");
                        write_file("/o.txt", "new", 1);
                        text = read_file("/o.txt");
                        write_file("/gone.txt", "new", 1);
                        rm("/gone.txt");
                        missing = file_size("/gone.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &writer, 0), "new");
        assert_eq!(
            vm.global_state.committed_global(&writer, 1u16),
            LpcRef::from(-1)
        );
        assert_eq!(std::fs::read_to_string(root.join("o.txt")).unwrap(), "new");
        assert!(!root.join("gone.txt").exists());
    }

    #[tokio::test]
    async fn caught_overwrite_refusal_preserves_the_original_file() {
        let root = TempLib::new("write-overwrite-denied");
        std::fs::write(root.join("o.txt"), "original").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string operations = "";
                    int valid_write(string p, string e, object c, string g) {
                        operations += e + ";";
                        return e == "rm";
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
                r#"string err; void create() { err = catch(write_file("/o.txt", "new", 1)); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert!(committed_string(&vm, &writer, 0).contains("write_file: permission denied"));
        assert_eq!(committed_string(&vm, &master, 0), "write_file;");
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "original"
        );
    }

    #[tokio::test]
    async fn inherited_overwrite_authorizes_the_owner_and_defining_program() {
        let root = TempLib::new("write-overwrite-inherited");
        std::fs::write(root.join("o.txt"), "original").unwrap();
        std::fs::write(
            root.join("module.c"),
            r#"protected int save(string path, string text) { return write_file(path, text, 1); }"#,
        )
        .unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .call_out_preserve_player(true)
            .max_execution_time(4000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string caller;
                    string program;
                    string operation;
                    int valid_write(string p, string e, object c, string g) {
                        caller = file_name(c);
                        program = g;
                        operation = e;
                        return caller == "/w" && g == "/module.c" && e == "write_file";
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
                r#"inherit "/module"; int result; void create() { result = save("/o.txt", "new"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&writer, 0u16),
            LpcRef::from(1)
        );
        assert_eq!(committed_string(&vm, &master, 0), "/w");
        assert_eq!(committed_string(&vm, &master, 1), "/module.c");
        assert_eq!(committed_string(&vm, &master, 2), "write_file");
        assert_eq!(std::fs::read_to_string(root.join("o.txt")).unwrap(), "new");
    }

    #[tokio::test]
    async fn an_aborted_overwrite_task_preserves_existing_and_missing_files() {
        let root = TempLib::new("write-overwrite-abort");
        std::fs::write(root.join("o.txt"), "original").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/w.c",
                r#"void create() { write_file("/o.txt", "new", 1); write_file("/new.txt", "new", 1); throw("boom"); }"#,
            )
            .await
            .unwrap_err();
        assert!(err.to_string().contains("boom"));
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "original"
        );
        assert!(!root.join("new.txt").exists());
    }

    #[tokio::test]
    async fn invalid_modes_are_caught_before_authorization_or_file_changes() {
        let root = TempLib::new("write-invalid-mode");
        std::fs::write(root.join("o.txt"), "original").unwrap();
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .call_out_preserve_player(true)
            .max_execution_time(4000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                r#"int calls; int valid_write(string p, string e, object c, string g) { calls++; return 1; }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        for (index, (value, expected)) in [
            ("-1", "mode must be 0 (append) or 1 (overwrite)"),
            ("2", "mode must be 0 (append) or 1 (overwrite)"),
            ("3", "mode must be 0 (append) or 1 (overwrite)"),
            ("1.0", "mode must be an int"),
            (r#""1""#, "mode must be an int"),
            ("nil", "mode must be an int"),
        ]
        .into_iter()
        .enumerate()
        {
            let writer = vm
                .initialize_process_from_code(
                    &format!("/w{index}.c"),
                    &format!(
                        r#"string err; void create() {{ mixed m = {value}; err = catch(write_file("/o.txt", "new", m)); }}"#
                    ),
                )
                .await
                .unwrap()
                .context
                .process;
            assert!(
                committed_string(&vm, &writer, 0).contains(expected),
                "{value}"
            );
        }
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(0)
        );
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "original"
        );
    }

    #[tokio::test]
    async fn overwrite_path_and_contents_errors_do_not_modify_the_file() {
        let root = TempLib::new("write-overwrite-invalid");
        std::fs::write(root.join("o.txt"), "original").unwrap();
        std::fs::create_dir(root.join("dir")).unwrap();
        let vm = allowing_vm(&root).await;
        for (index, (expression, expected)) in [
            (r#"write_file("/dir", "new", 1)"#, "/dir is a directory"),
            (
                r#"write_file("/missing/new", "new", 1)"#,
                "parent directory does not exist",
            ),
            (
                r#"write_file("/o.txt", bad, 1)"#,
                "contents must be a string",
            ),
            (r#"write_file(bad, "new", 1)"#, "path must be a string"),
        ]
        .into_iter()
        .enumerate()
        {
            let writer = vm
                .initialize_process_from_code(
                    &format!("/w{index}.c"),
                    &format!(r#"string err; void create() {{ mixed bad = 5; err = catch({expression}); }}"#),
                )
                .await
                .unwrap()
                .context
                .process;
            assert!(
                committed_string(&vm, &writer, 0).contains(expected),
                "{expression}"
            );
        }
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "original"
        );
        assert!(root.join("dir").is_dir());
        assert!(!root.join("missing").exists());
    }

    #[tokio::test]
    async fn direct_calls_check_the_optional_modes_type_and_argument_count() {
        let root = TempLib::new("write-signature");
        let compiler = CompilerBuilder::default()
            .config(temp_lib_config(&root))
            .build()
            .unwrap();
        for (expression, expected) in [
            (r#"write_file("/o.txt")"#, "incorrect argument count"),
            (
                r#"write_file("/o.txt", "", 0, 1)"#,
                "incorrect argument count",
            ),
            (
                r#"write_file("/o.txt", "", "1")"#,
                "unexpected argument type",
            ),
            (
                r#"write_file("/o.txt", "", 1.0)"#,
                "unexpected argument type",
            ),
        ] {
            let error = compiler
                .compile_string("/w.c", &format!("void create() {{ {expression}; }}"))
                .await
                .unwrap_err()
                .to_string();
            assert!(error.contains(expected), "{expression}: {error}");
        }
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

    /// The append lands at commit; a read in the same task already sees it.
    #[tokio::test]
    async fn a_same_task_read_sees_the_pending_append() {
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
        assert_eq!(committed_string(&vm, &writer, 0), "oldnew");
        assert_eq!(
            std::fs::read_to_string(root.join("o.txt")).unwrap(),
            "oldnew"
        );
    }

    /// A removed then re-created file reads as only the new contents, and a
    /// removed file reads as missing.
    #[tokio::test]
    async fn a_same_task_read_follows_a_removal() {
        let root = TempLib::new("rm-then-read");
        std::fs::write(root.join("o.txt"), "old").unwrap();
        std::fs::write(root.join("gone.txt"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let writer = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    string got;
                    string missing;
                    void create() {
                        rm("/o.txt");
                        write_file("/o.txt", "new");
                        got = read_file("/o.txt");
                        rm("/gone.txt");
                        missing = catch(read_file("/gone.txt"));
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &writer, 0), "new");
        assert!(committed_string(&vm, &writer, 1).contains("No such file"));
        assert_eq!(std::fs::read_to_string(root.join("o.txt")).unwrap(), "new");
        assert!(!root.join("gone.txt").exists());
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
        #[cfg(unix)]
        let cause = std::fs::metadata(root.join("f.txt/o.txt")).unwrap_err();
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
        #[cfg(unix)]
        assert!(err.contains(&cause.to_string()), "{err}");
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
