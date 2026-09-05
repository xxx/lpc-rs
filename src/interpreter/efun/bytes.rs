//! `read_bytes` and `write_bytes`: a byte range of a file, through the
//! master's `valid_read` / `valid_write`.

use lpc_rs_errors::Result;
use tokio::io::{AsyncReadExt, AsyncSeekExt};

use crate::interpreter::{
    VALID_READ, VALID_WRITE,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
    stm::Effect,
};

/// Argument `i` as an int.
fn int_arg<const N: usize>(context: &EfunContext<'_, N>, name: &str, i: usize) -> Result<i64> {
    match context.arg(i) {
        LpcRef::Int(n) => Ok(n.0),
        other => Err(context.runtime_error(format!("{name}: {} is not an int", other.type_name()))),
    }
}

/// `start` against a file of `size` bytes: a negative start counts back
/// from the end and stops at the beginning.
fn offset(start: i64, size: u64) -> u64 {
    if start < 0 {
        size.saturating_sub(start.unsigned_abs())
    } else {
        start as u64
    }
}

/// `read_bytes(path [, start [, length]])`: `length` bytes (to the end when
/// absent) from byte `start` of the file, the read cut at the end; 0 when
/// `start` is at or past the end. Bytes that are not UTF-8 are an error.
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
    let read = async {
        let mut file = tokio::fs::File::open(&access.server).await?;
        let size = file.metadata().await?.len();
        let from = offset(start, size);
        if from >= size {
            return Ok(None);
        }
        let to = length.map_or(size, |n| from.saturating_add(n).min(size));
        file.seek(std::io::SeekFrom::Start(from)).await?;
        let mut bytes = vec![0; (to - from) as usize];
        file.read_exact(&mut bytes).await?;
        Ok::<_, std::io::Error>(Some((from, to, bytes)))
    };
    let result = match read.await {
        Err(e) => {
            return Err(context.runtime_error(format!("read_bytes: {}: {e}", access.in_game)));
        }
        Ok(None) => LpcRef::from(0),
        Ok(Some((from, to, bytes))) => match String::from_utf8(bytes) {
            Ok(s) => LpcRef::from(s),
            Err(_) => {
                return Err(context.runtime_error(format!(
                    "read_bytes: bytes {from}..{to} of {} are not UTF-8",
                    access.in_game
                )));
            }
        },
    };
    context.return_efun_result(result);
    Ok(())
}

/// `write_bytes(path, start, str)`: overwrite the file from byte `start` (a
/// negative start counts back from the end; the end itself appends) with
/// `str`; 1 on success, 0 for a missing file or a start past the end.
/// Checked now, written at commit: a read later in this task sees the
/// bytes as they were.
pub async fn write_bytes<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let start = int_arg(context, "write_bytes", 1)?;
    let Some(contents) = context.arg(2).as_str() else {
        return Err(context.runtime_error(format!(
            "write_bytes: {} is not a string",
            context.arg(2).type_name()
        )));
    };
    let contents = contents.to_owned();
    let access = authorize(context, "write_bytes", VALID_WRITE, 0).await?;
    let size = match tokio::fs::metadata(&access.server).await {
        Ok(m) if m.is_file() => m.len(),
        Ok(_) => {
            return Err(
                context.runtime_error(format!("write_bytes: {} is not a file", access.in_game))
            );
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            context.return_efun_result(LpcRef::from(0));
            return Ok(());
        }
        Err(e) => {
            return Err(context.runtime_error(format!("write_bytes: {}: {e}", access.in_game)));
        }
    };
    let from = offset(start, size);
    if from > size {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    }
    context.record_effect(Effect::WriteBytes {
        in_game: access.in_game,
        server: access.server,
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

    /// A lib at `root` holding `/d.txt` and `/u.txt`, with a master that
    /// allows every read and write.
    async fn lib(name: &str) -> (TempLib, Vm) {
        let root = TempLib::new(name);
        std::fs::write(root.join("d.txt"), "hello world\n").unwrap();
        std::fs::write(root.join("u.txt"), "é").unwrap();
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

    /// Run `expr` in a fresh object: global 0 is its value, global 1 the
    /// error caught, if any.
    async fn run(vm: &Vm, expr: &str) -> Arc<Process> {
        let code = format!("mixed got; string err; void create() {{ err = catch(got = {expr}); }}");
        vm.initialize_process_from_code("/runner.c", &code)
            .await
            .unwrap()
            .context
            .process
    }

    async fn value_of(vm: &Vm, expr: &str) -> LpcRef {
        let p = run(vm, expr).await;
        let err = vm.global_state.committed_global(&p, 1u16);
        assert_eq!(err, LpcRef::from(0), "{expr}");
        vm.global_state.committed_global(&p, 0u16)
    }

    async fn error_of(vm: &Vm, expr: &str) -> String {
        let p = run(vm, expr).await;
        committed_string(vm, &p, 1)
    }

    #[tokio::test]
    async fn read_bytes_reads_a_range() {
        let (_root, vm) = lib("rb-range").await;
        let got = value_of(&vm, r#"read_bytes("/d.txt", 6, 5)"#).await;
        assert_eq!(got, LpcRef::from("world"));
    }

    #[tokio::test]
    async fn read_bytes_without_a_length_reads_to_the_end() {
        let (_root, vm) = lib("rb-to-end").await;
        let got = value_of(&vm, r#"read_bytes("/d.txt", 6)"#).await;
        assert_eq!(got, LpcRef::from("world\n"));
        let got = value_of(&vm, r#"read_bytes("/d.txt")"#).await;
        assert_eq!(got, LpcRef::from("hello world\n"));
    }

    #[tokio::test]
    async fn a_negative_start_counts_from_the_end() {
        let (_root, vm) = lib("rb-negative").await;
        let got = value_of(&vm, r#"read_bytes("/d.txt", -6, 5)"#).await;
        assert_eq!(got, LpcRef::from("world"));
        let got = value_of(&vm, r#"read_bytes("/d.txt", -100, 5)"#).await;
        assert_eq!(got, LpcRef::from("hello"));
    }

    #[tokio::test]
    async fn a_read_past_the_end_is_cut_at_the_end() {
        let (_root, vm) = lib("rb-truncate").await;
        let got = value_of(&vm, r#"read_bytes("/d.txt", 6, 100)"#).await;
        assert_eq!(got, LpcRef::from("world\n"));
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
            LpcRef::from("")
        );
        let err = error_of(&vm, r#"read_bytes("/d.txt", 0, -1)"#).await;
        assert!(err.contains("read_bytes: negative length -1"), "{err}");
    }

    #[tokio::test]
    async fn a_range_that_is_not_utf8_is_an_error() {
        let (_root, vm) = lib("rb-utf8").await;
        let err = error_of(&vm, r#"read_bytes("/u.txt", 0, 1)"#).await;
        assert!(
            err.contains("read_bytes: bytes 0..1 of /u.txt are not UTF-8"),
            "{err}"
        );
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
    async fn write_bytes_overwrites_in_place_at_commit() {
        let (root, vm) = lib("wb-overwrite").await;
        let got = value_of(&vm, r#"write_bytes("/d.txt", 6, "there")"#).await;
        assert_eq!(got, LpcRef::from(1));
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello there\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_with_a_negative_start_counts_from_the_end() {
        let (root, vm) = lib("wb-negative").await;
        value_of(&vm, r#"write_bytes("/d.txt", -6, "WORLD")"#).await;
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello WORLD\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_at_the_end_appends() {
        let (root, vm) = lib("wb-append").await;
        value_of(&vm, r#"write_bytes("/d.txt", 12, "!")"#).await;
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello world\n!"
        );
    }

    #[tokio::test]
    async fn write_bytes_past_the_end_is_zero_and_writes_nothing() {
        let (root, vm) = lib("wb-past").await;
        let got = value_of(&vm, r#"write_bytes("/d.txt", 13, "!")"#).await;
        assert_eq!(got, LpcRef::from(0));
        assert_eq!(
            std::fs::read_to_string(root.join("d.txt")).unwrap(),
            "hello world\n"
        );
    }

    #[tokio::test]
    async fn write_bytes_to_a_missing_file_is_zero() {
        let (root, vm) = lib("wb-missing").await;
        let got = value_of(&vm, r#"write_bytes("/nope.txt", 0, "x")"#).await;
        assert_eq!(got, LpcRef::from(0));
        assert!(!root.join("nope.txt").exists());
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
        let err = error_of(&vm, r#"write_bytes("/d.txt", 0, "y")"#).await;
        assert!(err.contains("write_bytes: permission denied"), "{err}");
        assert_eq!(std::fs::read_to_string(root.join("d.txt")).unwrap(), "x");
    }

    #[tokio::test]
    async fn a_read_in_the_same_task_sees_the_bytes_as_they_were() {
        let (root, vm) = lib("wb-deferred").await;
        let got = value_of(
            &vm,
            r#"write_bytes("/d.txt", 0, "HELLO") + read_bytes("/d.txt", 0, 5)"#,
        )
        .await;
        assert_eq!(got, LpcRef::from("1hello"));
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
                    e2 = catch(write_bytes("/d.txt", s, "x"));
                }
                "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("write_bytes: int is not a string"), "{err}");
        let err = committed_string(&vm, &p, 1);
        assert!(err.contains("write_bytes: string is not an int"), "{err}");
    }
}
