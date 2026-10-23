use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize_or_deny},
    file_view::FileMetadata,
    lpc_ref::LpcRef,
};

/// `file_size(path)`: the file's size in bytes; -1 for a missing file or
/// one the master's `valid_read` refuses, -2 for a directory. A write
/// earlier in this task is seen.
pub async fn file_size<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(access) = authorize_or_deny(context, "file_size", VALID_READ, 0).await? else {
        context.return_efun_result(LpcRef::from(-1));
        return Ok(());
    };
    let size = match access.metadata(context).await? {
        FileMetadata::Missing => -1,
        FileMetadata::Directory => -2,
        FileMetadata::File(size) | FileMetadata::Other(size) => {
            LpcIntInner::try_from(size).unwrap_or(LpcIntInner::MAX)
        }
    };
    context.return_efun_result(LpcRef::from(size));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
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

    async fn size_of(vm: &Vm, path: &str) -> LpcRef {
        let code = format!(r#"int create() {{ return file_size("{path}"); }}"#);
        vm.initialize_process_from_code("/sizer.c", &code)
            .await
            .unwrap()
            .result()
            .expect("a result")
    }

    /// A write or removal earlier in the task is measured, not the disk.
    #[tokio::test]
    async fn a_pending_write_or_removal_is_measured() {
        let root = TempLib::new("size-pending");
        std::fs::write(root.join("old.txt"), "xyz").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_read(string p, string e, object c, string g) { return 1; }
                int valid_write(string p, string e, object c, string g) { return 1; }
            "# },
        )
        .await
        .unwrap();
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                indoc! { r#"
                    int made;
                    int gone;
                    void create() {
                        write_file("/new.txt", "hello");
                        made = file_size("/new.txt");
                        rm("/old.txt");
                        gone = file_size("/old.txt");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&w, 0u16), LpcRef::from(5));
        assert_eq!(vm.global_state.committed_global(&w, 1u16), LpcRef::from(-1));
    }

    #[tokio::test]
    async fn file_size_is_the_byte_count() {
        let root = TempLib::new("size-file");
        std::fs::write(root.join("f.txt"), "héllo").unwrap();
        let vm = allowing_vm(&root).await;
        assert_eq!(size_of(&vm, "/f.txt").await, LpcRef::from(6));
    }

    #[tokio::test]
    async fn empty_and_binary_files_are_measured_relative_to_the_caller() {
        let root = TempLib::new("size-relative");
        std::fs::create_dir(root.join("cmd")).unwrap();
        std::fs::write(root.join("cmd/empty"), "").unwrap();
        std::fs::write(root.join("cmd/binary"), b"\xff\0\n").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/cmd/sizer.c",
                r#"int create() { return file_size("empty") == 0 && file_size("binary") == 3; }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn metadata_is_enough_even_with_unrelated_pending_writes() {
        use std::os::unix::fs::PermissionsExt;

        let root = TempLib::new("size-metadata");
        let path = root.join("unreadable");
        std::fs::write(&path, "contents").unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o000)).unwrap();
        let vm = allowing_vm(&root).await;
        let result = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        int before = file_size("/unreadable");
                        write_file("/other", "unrelated");
                        return before == 8 && file_size("/unreadable") == 8;
                    }
                "# },
            )
            .await;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        assert_eq!(result.unwrap().result(), Some(LpcRef::from(1)));
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn pending_appends_do_not_require_reading_the_file() {
        use std::os::unix::fs::PermissionsExt;

        let root = TempLib::new("size-pending-write-only");
        let path = root.join("write-only");
        std::fs::write(&path, "contents").unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o200)).unwrap();
        let vm = allowing_vm(&root).await;
        let result = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        write_file("/write-only", "append");
                        return file_size("/write-only");
                    }
                "# },
            )
            .await;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        assert_eq!(result.unwrap().result(), Some(LpcRef::from(14)));
        assert_eq!(std::fs::read(&path).unwrap(), b"contentsappend");
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn pending_renames_retain_content_read_errors() {
        use std::os::unix::fs::PermissionsExt;

        let root = TempLib::new("size-pending-rename-unreadable");
        let path = root.join("unreadable");
        std::fs::write(&path, "contents").unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o000)).unwrap();
        let vm = allowing_vm(&root).await;
        let result = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        rename("/unreadable", "/renamed");
                        return file_size("/renamed");
                    }
                "# },
            )
            .await;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        let error = result.unwrap_err().to_string();
        assert!(error.contains("file_size: /renamed:"), "{error}");
        assert!(!error.contains(root.to_str().unwrap()), "{error}");
        assert_eq!(std::fs::read(&path).unwrap(), b"contents");
        assert!(!root.join("renamed").exists());
    }

    #[tokio::test]
    async fn metadata_errors_keep_the_efun_and_in_game_path() {
        let root = TempLib::new("size-metadata-error");
        std::fs::write(root.join("file"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let error = vm
            .initialize_process_from_code(
                "/sizer.c",
                r#"int create() { return file_size("/file/child"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(error.contains("file_size: /file/child:"), "{error}");
        assert!(!error.contains(root.to_str().unwrap()), "{error}");
    }

    #[tokio::test]
    async fn authorization_precedes_metadata_and_includes_its_own_writes() {
        let root = TempLib::new("size-authorization");
        std::fs::write(root.join("file"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            indoc! { r#"
                int valid_read(string p, string e, object c, string g) {
                    if (e != "file_size" || g != "/sizer.c" || file_name(c) != "/sizer")
                        throw("wrong read context");
                    if (p == "/file/child") return 0;
                    if (p == "/new") {
                        write_file(p, "abc");
                        return 1;
                    }
                    return 0;
                }
                int valid_write(string p, string e, object c, string g) { return 1; }
            "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                r#"int create() { return file_size("/file/child") == -1 && file_size("/new") == 3; }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn pending_appends_and_removals_are_applied_in_order() {
        let root = TempLib::new("size-append-replace");
        std::fs::write(root.join("file"), "old").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        write_file("/file", "é");
                        if (file_size("/file") != 5) return 0;
                        rm("/file");
                        if (file_size("/file") != -1) return 0;
                        write_file("/file", "z");
                        return file_size("/file") == 1;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read(root.join("file")).unwrap(), b"z");
    }

    #[tokio::test]
    async fn a_pending_save_measures_the_replacement_contents() {
        let root = TempLib::new("size-save");
        std::fs::write(root.join("save.o"), "old contents are longer").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                r#"int counter = 7; int create() { save_object("/save"); return file_size("/save.o"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(10)));
        assert_eq!(std::fs::read(root.join("save.o")).unwrap(), b"counter 7\n");
    }

    #[tokio::test]
    async fn pending_directory_creation_and_removal_are_measured() {
        let root = TempLib::new("size-directories");
        std::fs::create_dir(root.join("old")).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        mkdir("/new");
                        rmdir("/old");
                        return file_size("/new") == -2 && file_size("/old") == -1;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_pending_rename_measures_both_source_and_destination() {
        let root = TempLib::new("size-rename");
        std::fs::write(root.join("source"), "content").unwrap();
        std::fs::write(root.join("target"), "x").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        rename("/source", "/target");
                        return file_size("/source") == -1 && file_size("/target") == 7;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read(root.join("target")).unwrap(), b"content");
    }

    #[tokio::test]
    async fn pending_byte_writes_measure_extension_and_overwrite() {
        let root = TempLib::new("size-byte-write");
        std::fs::write(root.join("file"), "abc").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        write_bytes("/file", 1, to_bytes("WXYZ", "UTF-8"));
                        if (file_size("/file") != 5) return 0;
                        write_bytes("/file", 0, to_bytes("Q", "UTF-8"));
                        return file_size("/file") == 5;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read(root.join("file")).unwrap(), b"QWXYZ");
    }

    #[tokio::test]
    async fn pending_character_replacements_measure_utf8_byte_changes() {
        let root = TempLib::new("size-char-write");
        std::fs::write(root.join("file"), "abc").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/sizer.c",
                indoc! { r#"
                    int create() {
                        write_chars("/file", 1, "森");
                        if (file_size("/file") != 5) return 0;
                        write_chars("/file", 1, "é");
                        if (file_size("/file") != 4) return 0;
                        write_chars("/file", 3, "🌲");
                        return file_size("/file") == 8;
                    }
                "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read_to_string(root.join("file")).unwrap(), "aéc🌲");
    }

    #[tokio::test]
    async fn a_missing_file_is_minus_one() {
        let root = TempLib::new("size-missing");
        let vm = allowing_vm(&root).await;
        assert_eq!(size_of(&vm, "/nope.txt").await, LpcRef::from(-1));
    }

    #[tokio::test]
    async fn a_directory_is_minus_two() {
        let root = TempLib::new("size-dir");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        assert_eq!(size_of(&vm, "/d").await, LpcRef::from(-2));
    }

    #[tokio::test]
    async fn the_mudlib_root_is_a_directory_from_a_nested_caller() {
        let root = TempLib::new("size-root");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/cmd/sizer.c",
                r#"int create() { return file_size("/"); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result().unwrap(), LpcRef::from(-2));
    }

    #[tokio::test]
    async fn a_refused_read_is_minus_one_not_an_error() {
        let root = TempLib::new("size-denied");
        std::fs::write(root.join("f.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        assert_eq!(size_of(&vm, "/f.txt").await, LpcRef::from(-1));
        assert_eq!(size_of(&vm, "/").await, LpcRef::from(-1));
    }

    #[tokio::test]
    async fn a_non_string_path_is_an_error() {
        let root = TempLib::new("size-nonstring");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/sizer.c",
                "int create() { mixed p = 1; return file_size(p); }",
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("file_size: path must be a string"), "{err}");
    }
}
