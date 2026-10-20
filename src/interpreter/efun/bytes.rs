//! `read_bytes` and `write_bytes`: a byte range of a file, through the
//! master's `valid_read` / `valid_write`.

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ, VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    file_view::FileMetadata,
    lpc_ref::LpcRef,
    stm::Effect,
};

/// Argument `i` as an int.
pub(super) fn int_arg<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    i: usize,
) -> Result<i64> {
    match context.arg(i) {
        LpcRef::Int(n) => Ok(n.0),
        other => Err(context.runtime_error(format!("{name}: {} is not an int", other.type_name()))),
    }
}

/// `start` against a file of `size` bytes: a negative start counts back
/// from the end and stops at the beginning.
pub(super) fn offset(start: i64, size: u64) -> u64 {
    if start < 0 {
        size.saturating_sub(start.unsigned_abs())
    } else {
        start as u64
    }
}

/// `read_bytes(path [, start [, length]])`: `length` bytes (to the end when
/// absent) from byte `start` of the file, the read cut at the end; 0 when
/// `start` is at or past the end. The result is `bytes`, of any length.
pub async fn read_bytes<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let start = if context.arg_count() > 1 {
        int_arg(context, "read_bytes", 1)?
    } else {
        0
    };
    let length = if context.arg_count() > 2 {
        let length = int_arg(context, "read_bytes", 2)?;
        if length < 0 {
            return Err(context.runtime_error(format!("read_bytes: negative length {length}")));
        }
        Some(length as u64)
    } else {
        None
    };
    let access = authorize(context, "read_bytes", VALID_READ, 0).await?;
    if let Some(bytes) = access.read_disk_range(context, start, length).await? {
        let result = if bytes.is_empty() {
            context.txn().nil()
        } else {
            LpcRef::from(bytes)
        };
        context.return_efun_result(result);
        return Ok(());
    }
    let all = access.read_bytes(context).await?;
    let size = all.len() as u64;
    let from = offset(start, size);
    let result = if from >= size {
        context.txn().nil()
    } else {
        let to = length.map_or(size, |n| from.saturating_add(n).min(size));
        let bytes = all[from as usize..to as usize].to_vec();
        LpcRef::from(bytes)
    };
    context.return_efun_result(result);
    Ok(())
}

/// `write_bytes(path, start, bytes)`: overwrite the file from byte `start`
/// (a negative start counts back from the end; the end itself appends)
/// with `bytes`; 1 on success, 0 for a missing file or a start past the
/// end. A string is refused; convert it with `to_bytes()`. Checked now,
/// written at commit; later reads in the same task include the pending overwrite.
pub async fn write_bytes<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let start = int_arg(context, "write_bytes", 1)?;
    let Some(contents) = context.arg(2).as_bytes() else {
        return Err(context.runtime_error(format!(
            "write_bytes: {} is not bytes; convert with to_bytes()",
            context.arg(2).type_name()
        )));
    };
    let contents = contents.to_vec();
    let access = authorize(context, "write_bytes", VALID_WRITE, 0).await?;
    let size = match access.metadata(context).await? {
        FileMetadata::File(size) => size,
        FileMetadata::Directory | FileMetadata::Other(_) => {
            return Err(context.runtime_error(format!("write_bytes: {} is not a file", access)));
        }
        FileMetadata::Missing => {
            context.return_efun_result(LpcRef::from(0));
            return Ok(());
        }
    };
    let from = offset(start, size);
    if from > size {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    }
    context.record_effect(Effect::WriteBytes {
        path: access.into_path(),
        start: from,
        contents,
    });
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, process::Process, vm::Vm},
        test_support::{TempLib, committed_string, temp_lib_config},
    };

    /// A lib at `root` holding `/d.txt` and `/u.txt` (`é` plus a lone
    /// `0xff` byte), with a master that allows every read and write.
    async fn lib(name: &str) -> (TempLib, Vm) {
        let root = TempLib::new(name);
        std::fs::write(root.join("d.txt"), "hello world\n").unwrap();
        std::fs::write(root.join("u.txt"), [0xc3, 0xa9, 0xff]).unwrap();
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
        (root, vm)
    }

    /// Run `expr` in a fresh object, its value landing in a global declared
    /// `declared`: global 0 is the value, global 1 the error caught, if any.
    async fn run_as(vm: &Vm, declared: &str, expr: &str) -> Arc<Process> {
        let code =
            format!("{declared} got; string err; void create() {{ err = catch(got = {expr}); }}");
        vm.initialize_process_from_code("/runner.c", &code)
            .await
            .unwrap()
            .context
            .process
    }

    async fn run(vm: &Vm, expr: &str) -> Arc<Process> {
        run_as(vm, "mixed", expr).await
    }

    async fn value_from(vm: &Vm, declared: &str, expr: &str) -> LpcRef {
        let p = run_as(vm, declared, expr).await;
        let err = vm.global_state.committed_global(&p, 1u16);
        assert_eq!(err, LpcRef::from(0), "{expr}");
        vm.global_state.committed_global(&p, 0u16)
    }

    async fn value_of(vm: &Vm, expr: &str) -> LpcRef {
        value_from(vm, "mixed", expr).await
    }

    /// The value of `expr` assigned to a global declared `bytes`, which the
    /// checker accepts only because `read_bytes` returns that type.
    async fn bytes_value_of(vm: &Vm, expr: &str) -> LpcRef {
        value_from(vm, "bytes", expr).await
    }

    async fn error_of(vm: &Vm, expr: &str) -> String {
        let p = run(vm, expr).await;
        committed_string(vm, &p, 1)
    }

    #[tokio::test]
    async fn read_bytes_reads_a_range() {
        let (_root, vm) = lib("rb-range").await;
        let got = bytes_value_of(&vm, r#"read_bytes("/d.txt", 6, 5)"#).await;
        assert_eq!(got, LpcRef::from(b"world".to_vec()));
    }

    #[tokio::test]
    async fn read_bytes_without_a_length_reads_to_the_end() {
        let (_root, vm) = lib("rb-to-end").await;
        let got = bytes_value_of(&vm, r#"read_bytes("/d.txt", 6)"#).await;
        assert_eq!(got, LpcRef::from(b"world\n".to_vec()));
        let got = bytes_value_of(&vm, r#"read_bytes("/d.txt")"#).await;
        assert_eq!(got, LpcRef::from(b"hello world\n".to_vec()));
    }

    #[tokio::test]
    async fn a_negative_start_counts_from_the_end() {
        let (_root, vm) = lib("rb-negative").await;
        let got = bytes_value_of(&vm, r#"read_bytes("/d.txt", -6, 5)"#).await;
        assert_eq!(got, LpcRef::from(b"world".to_vec()));
        let got = bytes_value_of(&vm, r#"read_bytes("/d.txt", -100, 5)"#).await;
        assert_eq!(got, LpcRef::from(b"hello".to_vec()));
    }

    #[tokio::test]
    async fn a_read_past_the_end_is_cut_at_the_end() {
        let (_root, vm) = lib("rb-truncate").await;
        let got = bytes_value_of(&vm, r#"read_bytes("/d.txt", 6, 100)"#).await;
        assert_eq!(got, LpcRef::from(b"world\n".to_vec()));
    }

    #[tokio::test]
    async fn a_start_at_or_past_the_end_is_zero() {
        let (_root, vm) = lib("rb-past").await;
        assert_eq!(
            value_of(&vm, r#"read_bytes("/d.txt", 12, 1)"#).await,
            LpcRef::from(0)
        );
        assert_eq!(
            value_of(&vm, r#"read_bytes("/d.txt", 100, 1)"#).await,
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn a_zero_length_is_the_empty_string_and_a_negative_one_an_error() {
        let (_root, vm) = lib("rb-zero").await;
        assert_eq!(
            value_of(&vm, r#"read_bytes("/d.txt", 0, 0)"#).await,
            LpcRef::from(Vec::<u8>::new())
        );
        let err = error_of(&vm, r#"read_bytes("/d.txt", 0, -1)"#).await;
        assert!(err.contains("read_bytes: negative length -1"), "{err}");
    }

    #[tokio::test]
    async fn read_bytes_answers_bytes_not_a_string() {
        let (_root, vm) = lib("rb-bytes").await;
        let got = bytes_value_of(&vm, r#"read_bytes("/u.txt", 1, 1)"#).await;
        assert_eq!(got, LpcRef::from(vec![0xa9]));
        let got = bytes_value_of(&vm, r#"read_bytes("/u.txt")"#).await;
        assert_eq!(got, LpcRef::from(vec![0xc3, 0xa9, 0xff]));
    }

    #[tokio::test]
    async fn a_missing_file_is_an_error() {
        let (_root, vm) = lib("rb-missing").await;
        let err = error_of(&vm, r#"read_bytes("/nope.txt", 0, 1)"#).await;
        assert!(err.contains("read_bytes: /nope.txt:"), "{err}");
    }

    #[tokio::test]
    async fn a_refused_read_is_an_error() {
        let root = TempLib::new("rb-refused");
        std::fs::write(root.join("d.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let err = error_of(&vm, r#"read_bytes("/d.txt", 0, 1)"#).await;
        assert!(err.contains("read_bytes: permission denied"), "{err}");
    }

    #[tokio::test]
    async fn read_bytes_arguments_are_typed() {
        let (_root, vm) = lib("rb-typed").await;
        let p = vm
            .initialize_process_from_code(
                "/typed.c",
                r#"string err; void create() { mixed a = "a"; err = catch(read_bytes("/d.txt", a, 1)); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("read_bytes: string is not an int"), "{err}");
    }

    #[tokio::test]
    async fn read_bytes_large_lengths_stop_at_eof() {
        let (_root, vm) = lib("rb-large-length").await;
        assert_eq!(
            bytes_value_of(&vm, r#"read_bytes("/u.txt", 1, 9223372036854775807)"#).await,
            LpcRef::from(vec![0xa9, 0xff])
        );
    }

    #[tokio::test]
    async fn read_bytes_reads_a_distant_region_of_a_sparse_file() {
        use std::io::{Seek, SeekFrom, Write};

        let (root, vm) = lib("rb-sparse-region").await;
        let mut file = std::fs::File::create(root.join("large.bin")).unwrap();
        file.set_len(64 * 1024 * 1024).unwrap();
        file.seek(SeekFrom::End(-4)).unwrap();
        file.write_all(&[0, 255, 65, 10]).unwrap();
        assert_eq!(
            bytes_value_of(&vm, r#"read_bytes("/large.bin", 67108860, 512)"#).await,
            LpcRef::from(vec![0, 255, 65, 10])
        );
    }

    #[tokio::test]
    async fn read_bytes_distinguishes_empty_ranges_from_distinct_nil_at_eof() {
        let root = TempLib::new("rb-distinct-eof");
        std::fs::write(root.join("data"), "x").unwrap();
        std::fs::write(root.join("empty"), "").unwrap();
        let config = lpc_rs_utils::config::ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(lpc_rs_core::null_mode::NullMode::Distinct)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string f, object c, string g) { return 1; }",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/reader.c",
                r#"int create() {
                    mixed empty = read_bytes("/data", 0, 0);
                    return bytesp(empty) && sizeof(empty) == 0
                        && nilp(read_bytes("/data", 1, 1))
                        && nilp(read_bytes("/data", 1, 0))
                        && nilp(read_bytes("/empty", 0, 1));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn read_bytes_sees_creation_by_its_permission_apply() {
        let root = TempLib::new("rb-permission-create");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_write(string p, string f, object c, string g) { return 1; }
            int valid_read(string p, string f, object c, string g) {
                if (f == "read_bytes") write_file(p, "abcdef", 1);
                return 1;
            }"#,
        )
        .await
        .unwrap();
        assert_eq!(
            bytes_value_of(&vm, r#"read_bytes("/new", 1, 2)"#).await,
            LpcRef::from(b"bc".to_vec())
        );
    }

    #[tokio::test]
    async fn read_bytes_sees_a_pending_utf8_append() {
        let (_root, vm) = lib("rb-pending-append").await;
        let task = vm
            .initialize_process_from_code(
                "/reader.c",
                r#"bytes create() {
                    write_file("/d.txt", "猫");
                    return read_bytes("/d.txt", 10, 100);
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(
            task.result(),
            Some(LpcRef::from("d\n猫".as_bytes().to_vec()))
        );
    }

    #[tokio::test]
    async fn read_bytes_sees_a_pending_rename() {
        let (_root, vm) = lib("rb-pending-rename").await;
        let task = vm
            .initialize_process_from_code(
                "/reader.c",
                r#"bytes create() {
                    rename("/u.txt", "/shift.bin");
                    return read_bytes("/shift.bin", 1, 2);
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(vec![0xa9, 0xff])));
    }

    #[tokio::test]
    async fn read_bytes_treats_a_pending_removal_as_missing() {
        let (_root, vm) = lib("rb-pending-remove").await;
        let Err(error) = vm
            .initialize_process_from_code(
                "/reader.c",
                r#"bytes create() { rm("/d.txt"); return read_bytes("/d.txt", 0, 1); }"#,
            )
            .await
        else {
            panic!("pending removal must make the read fail");
        };
        let error = error.to_string();
        assert!(error.contains("read_bytes: /d.txt:"), "{error}");
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn read_bytes_checks_read_permission_even_past_eof() {
        use std::os::unix::fs::PermissionsExt;

        let (root, vm) = lib("rb-unreadable-eof").await;
        let path = root.join("d.txt");
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o200)).unwrap();
        let error = error_of(&vm, r#"read_bytes("/d.txt", 100, 1)"#).await;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        assert!(error.contains("read_bytes: /d.txt:"), "{error}");
        assert!(!error.contains(root.to_str().unwrap()), "{error}");
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn read_bytes_checks_read_permission_even_for_zero_length() {
        use std::os::unix::fs::PermissionsExt;

        let (root, vm) = lib("rb-unreadable-zero").await;
        let path = root.join("d.txt");
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o200)).unwrap();
        let error = error_of(&vm, r#"read_bytes("/d.txt", 0, 0)"#).await;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        assert!(error.contains("read_bytes: /d.txt:"), "{error}");
        assert!(!error.contains(root.to_str().unwrap()), "{error}");
    }

    #[tokio::test]
    async fn read_bytes_rejects_a_directory() {
        let (_root, vm) = lib("rb-directory").await;
        let error = error_of(&vm, r#"read_bytes("/", 0, 1)"#).await;
        assert!(
            error.contains("read_bytes: /:") && error.contains("directory"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn write_bytes_overwrites_in_place_at_commit() {
        let (root, vm) = lib("wb-overwrite").await;
        let got = value_of(
            &vm,
            r#"write_bytes("/d.txt", 6, to_bytes("there", "UTF-8"))"#,
        )
        .await;
        assert_eq!(got, LpcRef::from(1));
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello there\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_with_a_negative_start_counts_from_the_end() {
        let (root, vm) = lib("wb-negative").await;
        value_of(
            &vm,
            r#"write_bytes("/d.txt", -6, to_bytes("WORLD", "UTF-8"))"#,
        )
        .await;
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello WORLD\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_at_the_end_appends() {
        let (root, vm) = lib("wb-append").await;
        value_of(&vm, r#"write_bytes("/d.txt", 12, to_bytes("!", "UTF-8"))"#).await;
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello world\n!"
        );
    }

    #[tokio::test]
    async fn write_bytes_past_the_end_is_zero_and_writes_nothing() {
        let (root, vm) = lib("wb-past").await;
        let got = value_of(&vm, r#"write_bytes("/d.txt", 13, to_bytes("!", "UTF-8"))"#).await;
        assert_eq!(got, LpcRef::from(0));
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello world\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_to_a_missing_file_is_zero() {
        let (root, vm) = lib("wb-missing").await;
        let got = value_of(
            &vm,
            r#"write_bytes("/nope.txt", 0, to_bytes("x", "UTF-8"))"#,
        )
        .await;
        assert_eq!(got, LpcRef::from(0));
        assert!(!root.join("nope.txt").exists());
    }

    #[tokio::test]
    async fn write_bytes_sees_a_pending_creation() {
        let (root, vm) = lib("wb-pending-create").await;
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    write_file("/new.bin", "", 1);
                    return write_bytes("/new.bin", 0, to_bytes(({0, 255, 65})));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read(root.join("new.bin")).unwrap(), [0, 255, 65]);
    }

    #[tokio::test]
    async fn write_bytes_appends_repeatedly_at_the_pending_end() {
        let (root, vm) = lib("wb-pending-append").await;
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    return write_bytes("/d.txt", file_size("/d.txt"), to_bytes("A", "UTF-8"))
                        && write_bytes("/d.txt", file_size("/d.txt"), to_bytes("B", "UTF-8"))
                        && file_size("/d.txt") == 14;
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read(root.join("d.txt")).unwrap(),
            b"hello world\nAB"
        );
    }

    #[tokio::test]
    async fn write_bytes_negative_offsets_use_the_pending_end() {
        let (root, vm) = lib("wb-pending-negative").await;
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    return write_bytes("/d.txt", 12, to_bytes("A", "UTF-8"))
                        && write_bytes("/d.txt", -1, to_bytes("B", "UTF-8"));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read(root.join("d.txt")).unwrap(),
            b"hello world\nB"
        );
    }

    #[tokio::test]
    async fn write_bytes_rejects_offsets_past_a_pending_truncation() {
        let (root, vm) = lib("wb-pending-truncate").await;
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    write_file("/d.txt", "", 1);
                    return write_bytes("/d.txt", 1, to_bytes("x", "UTF-8"));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
        assert_eq!(std::fs::read(root.join("d.txt")).unwrap(), b"");
    }

    #[tokio::test]
    async fn write_bytes_treats_a_pending_removal_as_missing() {
        let (root, vm) = lib("wb-pending-remove").await;
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    rm("/d.txt");
                    return write_bytes("/d.txt", 0, to_bytes("x", "UTF-8"));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(0)));
        assert!(!root.join("d.txt").exists());
    }

    #[tokio::test]
    async fn write_bytes_sees_a_pending_rename_destination() {
        let (root, vm) = lib("wb-pending-rename").await;
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    rename("/u.txt", "/renamed.bin");
                    return write_bytes("/renamed.bin", 3, to_bytes(({0})));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read(root.join("renamed.bin")).unwrap(),
            [0xc3, 0xa9, 0xff, 0]
        );
    }

    #[tokio::test]
    async fn write_bytes_sees_byte_length_after_a_character_write() {
        let (root, vm) = lib("wb-after-characters").await;
        std::fs::write(root.join("d.txt"), "é").unwrap();
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    write_chars("/d.txt", 1, "猫");
                    return write_bytes("/d.txt", 5, to_bytes(({0, 255})));
                }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read(root.join("d.txt")).unwrap(),
            [0xc3, 0xa9, 0xe7, 0x8c, 0xab, 0, 255]
        );
    }

    #[tokio::test]
    async fn write_bytes_refuses_a_pending_directory() {
        let (root, vm) = lib("wb-pending-directory").await;
        let process = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"string problem;
                    void create() {
                        mkdir("/folder");
                        problem = catch(write_bytes("/folder", 0, to_bytes("x", "UTF-8")));
                    }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let problem = committed_string(&vm, &process, 0);
        assert!(
            problem.contains("write_bytes: /folder is not a file"),
            "{problem}"
        );
        assert!(root.join("folder").is_dir());
    }

    #[tokio::test]
    async fn write_bytes_sees_creation_by_its_permission_apply() {
        let root = TempLib::new("wb-permission-create");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_write(string path, string func, object caller, string program) {
                if (func == "write_bytes") write_file(path, "", 1);
                return 1;
            }
            int valid_read(string path, string func, object caller, string program) { return 0; }"#,
        )
        .await
        .unwrap();
        assert_eq!(
            value_of(&vm, r#"write_bytes("/new.bin", 0, to_bytes(({0, 255})))"#).await,
            LpcRef::from(1)
        );
        assert_eq!(std::fs::read(root.join("new.bin")).unwrap(), [0, 255]);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn write_bytes_and_file_size_do_not_read_pending_binary_payloads() {
        use std::os::unix::fs::PermissionsExt;

        let (root, vm) = lib("wb-write-only").await;
        let path = root.join("d.txt");
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o200)).unwrap();
        let task = vm
            .initialize_process_from_code(
                "/writer.c",
                r#"int create() {
                    return write_bytes("/d.txt", 12, to_bytes("A", "UTF-8"))
                        && write_bytes("/d.txt", 13, to_bytes("B", "UTF-8"))
                        && file_size("/d.txt") == 14;
                }"#,
            )
            .await
            .unwrap();
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o600)).unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(std::fs::read(&path).unwrap(), b"hello world\nAB");
    }

    #[tokio::test]
    async fn a_refused_write_is_an_error() {
        let root = TempLib::new("wb-refused");
        std::fs::write(root.join("d.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_write(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let err = error_of(&vm, r#"write_bytes("/d.txt", 0, to_bytes("y", "UTF-8"))"#).await;
        assert!(err.contains("write_bytes: permission denied"), "{err}");
        assert_eq!(std::fs::read_to_string(root.join("d.txt")).unwrap(), "x");
    }

    #[tokio::test]
    async fn write_bytes_refuses_a_string() {
        let (_root, vm) = lib("wb-string").await;
        let p = vm
            .initialize_process_from_code(
                "/refuse.c",
                r#"string err; void create() { mixed s = "x"; err = catch(write_bytes("/d.txt", 0, s)); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("to_bytes"), "{err}");
    }

    /// The write lands at commit; a read in the same task already sees it.
    #[tokio::test]
    async fn write_bytes_lands_bytes() {
        let (root, vm) = lib("wb-lands").await;
        let p = vm
            .initialize_process_from_code(
                "/wb.c",
                indoc! { r#"
                    mixed wrote;
                    bytes reread;
                    void create() {
                        bytes payload = to_bytes("HELLO", "UTF-8");
                        wrote = write_bytes("/d.txt", 0, payload);
                        reread = read_bytes("/d.txt", 0, 5);
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&p, 0u16), LpcRef::from(1));
        assert_eq!(
            vm.global_state.committed_global(&p, 1u16),
            LpcRef::from(b"HELLO".to_vec())
        );
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "HELLO world\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_arguments_are_typed() {
        let (_root, vm) = lib("wb-typed").await;
        let p = vm
            .initialize_process_from_code(
                "/typed.c",
                r#"
                string e1;
                string e2;
                void create() {
                    mixed n = 1;
                    mixed s = "0";
                    e1 = catch(write_bytes("/d.txt", 0, n));
                    e2 = catch(write_bytes("/d.txt", s, to_bytes("x", "UTF-8")));
                }
                "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("write_bytes: int is not bytes"), "{err}");
        let err = committed_string(&vm, &p, 1);
        assert!(err.contains("write_bytes: string is not an int"), "{err}");
    }
}
