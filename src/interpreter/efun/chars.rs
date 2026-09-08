//! `read_chars` and `write_chars`: a character range of a UTF-8 file,
//! through the master's `valid_read` / `valid_write`.

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ, VALID_WRITE,
    efun::{
        bytes::{int_arg, offset},
        efun_context::EfunContext,
        file_access::{FileAccess, authorize},
        file_view::read_through,
    },
    lpc_ref::LpcRef,
    stm::Effect,
};

/// The file at `access` decoded, a write earlier in this task included;
/// an unreadable or non-UTF-8 file is the efun's error.
async fn text_of<const N: usize>(
    context: &EfunContext<'_, N>,
    name: &str,
    access: &FileAccess,
) -> Result<String> {
    let read = async { read_through(context, access.server()).await?.into_bytes() };
    match read.await {
        Err(e) => Err(context.runtime_error(format!("{name}: {}: {e}", access.name()))),
        Ok(bytes) => String::from_utf8(bytes)
            .map_err(|_| context.runtime_error(format!("{name}: {} is not UTF-8", access.name()))),
    }
}

/// `read_chars(path [, start [, length]])`: `length` characters (to the end
/// when absent) from character `start` of the file, the read cut at the
/// end; 0 when `start` is at or past the end.
pub async fn read_chars<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let start = if context.arg_count() > 1 {
        int_arg(context, "read_chars", 1)?
    } else {
        0
    };
    let length = if context.arg_count() > 2 {
        let length = int_arg(context, "read_chars", 2)?;
        if length < 0 {
            return Err(context.runtime_error(format!("read_chars: negative length {length}")));
        }
        Some(length as u64)
    } else {
        None
    };
    let access = authorize(context, "read_chars", VALID_READ, 0).await?;
    let text = text_of(context, "read_chars", &access).await?;
    let count = text.chars().count();
    let from = offset(start, count as u64);
    let result = if from >= count as u64 {
        LpcRef::from(0)
    } else {
        let take = length.map_or(count, |n| n.min(count as u64) as usize);
        LpcRef::from(
            text.chars()
                .skip(from as usize)
                .take(take)
                .collect::<String>(),
        )
    };
    context.return_efun_result(result);
    Ok(())
}

/// `write_chars(path, start, str)`: replace `str`'s worth of characters at
/// character `start` (a negative start counts back from the end; the end
/// itself appends) with `str`; 1 on success, 0 for a missing file or a
/// start past the end. Checked now, written at commit: a read later in
/// this task sees the characters as they were.
pub async fn write_chars<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let start = int_arg(context, "write_chars", 1)?;
    let Some(contents) = context.arg(2).as_str() else {
        return Err(context.runtime_error(format!(
            "write_chars: {} is not a string",
            context.arg(2).type_name()
        )));
    };
    let contents = contents.to_owned();
    let access = authorize(context, "write_chars", VALID_WRITE, 0).await?;
    match tokio::fs::metadata(access.server()).await {
        Ok(m) if m.is_file() => {}
        Ok(_) => {
            return Err(
                context.runtime_error(format!("write_chars: {} is not a file", access.name()))
            );
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            context.return_efun_result(LpcRef::from(0));
            return Ok(());
        }
        Err(e) => {
            return Err(context.runtime_error(format!("write_chars: {}: {e}", access.name())));
        }
    }
    let count = text_of(context, "write_chars", &access)
        .await?
        .chars()
        .count();
    let from = offset(start, count as u64);
    if from > count as u64 {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    }
    context.record_effect(Effect::ReplaceChars {
        path: access,
        start: from as usize,
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

    /// A lib at `root` holding `/d.txt` (ASCII), `/m.txt` (two-byte
    /// characters at 1 and 7) and `/u.txt` (not UTF-8), with a master that
    /// allows every read and write.
    async fn lib(name: &str) -> (TempLib, Vm) {
        let root = TempLib::new(name);
        std::fs::write(root.join("d.txt"), "hello world\n").unwrap();
        std::fs::write(root.join("m.txt"), "héllo wörld\n").unwrap();
        std::fs::write(root.join("u.txt"), [0xff, 0xfe]).unwrap();
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

    fn contents(root: &TempLib, name: &str) -> String {
        std::fs::read_to_string(root.join(name)).unwrap()
    }

    #[tokio::test]
    async fn read_chars_reads_a_range_of_characters() {
        let (_root, vm) = lib("rc-range").await;
        let got = value_of(&vm, r#"read_chars("/m.txt", 6, 5)"#).await;
        assert_eq!(got, LpcRef::from("wörld"));
    }

    #[tokio::test]
    async fn read_chars_without_a_length_reads_to_the_end() {
        let (_root, vm) = lib("rc-to-end").await;
        let got = value_of(&vm, r#"read_chars("/m.txt", 6)"#).await;
        assert_eq!(got, LpcRef::from("wörld\n"));
        let got = value_of(&vm, r#"read_chars("/m.txt")"#).await;
        assert_eq!(got, LpcRef::from("héllo wörld\n"));
    }

    #[tokio::test]
    async fn a_negative_start_counts_characters_from_the_end() {
        let (_root, vm) = lib("rc-negative").await;
        let got = value_of(&vm, r#"read_chars("/m.txt", -6, 5)"#).await;
        assert_eq!(got, LpcRef::from("wörld"));
        let got = value_of(&vm, r#"read_chars("/m.txt", -100, 5)"#).await;
        assert_eq!(got, LpcRef::from("héllo"));
    }

    #[tokio::test]
    async fn a_read_past_the_end_is_cut_at_the_end() {
        let (_root, vm) = lib("rc-truncate").await;
        let got = value_of(&vm, r#"read_chars("/m.txt", 6, 100)"#).await;
        assert_eq!(got, LpcRef::from("wörld\n"));
    }

    #[tokio::test]
    async fn a_start_at_or_past_the_end_is_zero() {
        let (_root, vm) = lib("rc-past").await;
        assert_eq!(
            value_of(&vm, r#"read_chars("/m.txt", 12, 1)"#).await,
            LpcRef::from(0)
        );
        assert_eq!(
            value_of(&vm, r#"read_chars("/m.txt", 100, 1)"#).await,
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn a_zero_length_is_the_empty_string_and_a_negative_one_an_error() {
        let (_root, vm) = lib("rc-zero").await;
        assert_eq!(
            value_of(&vm, r#"read_chars("/m.txt", 0, 0)"#).await,
            LpcRef::from("")
        );
        let err = error_of(&vm, r#"read_chars("/m.txt", 0, -1)"#).await;
        assert!(err.contains("read_chars: negative length -1"), "{err}");
    }

    #[tokio::test]
    async fn a_file_that_is_not_utf8_is_an_error() {
        let (_root, vm) = lib("rc-utf8").await;
        let err = error_of(&vm, r#"read_chars("/u.txt", 0, 1)"#).await;
        assert!(err.contains("read_chars: /u.txt is not UTF-8"), "{err}");
        let err = error_of(&vm, r#"write_chars("/u.txt", 0, "x")"#).await;
        assert!(err.contains("write_chars: /u.txt is not UTF-8"), "{err}");
    }

    #[tokio::test]
    async fn a_missing_file_is_an_error() {
        let (_root, vm) = lib("rc-missing").await;
        let err = error_of(&vm, r#"read_chars("/nope.txt", 0, 1)"#).await;
        assert!(err.contains("read_chars: /nope.txt:"), "{err}");
    }

    #[tokio::test]
    async fn a_refused_read_is_an_error() {
        let root = TempLib::new("rc-refused");
        std::fs::write(root.join("d.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let err = error_of(&vm, r#"read_chars("/d.txt", 0, 1)"#).await;
        assert!(err.contains("read_chars: permission denied"), "{err}");
    }

    #[tokio::test]
    async fn read_chars_arguments_are_typed() {
        let (_root, vm) = lib("rc-typed").await;
        let p = vm
            .initialize_process_from_code(
                "/typed.c",
                r#"string err; void create() { mixed a = "a"; err = catch(read_chars("/d.txt", a, 1)); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("read_chars: string is not an int"), "{err}");
    }

    #[tokio::test]
    async fn write_chars_replaces_characters_at_commit() {
        let (root, vm) = lib("wc-replace").await;
        let got = value_of(&vm, r#"write_chars("/m.txt", 6, "WÖRLD")"#).await;
        assert_eq!(got, LpcRef::from(1));
        assert_eq!(contents(&root, "m.txt"), "héllo WÖRLD\n");
    }

    #[tokio::test]
    async fn a_replacement_may_change_the_byte_width() {
        let (root, vm) = lib("wc-width").await;
        value_of(&vm, r#"write_chars("/d.txt", 0, "日本")"#).await;
        assert_eq!(contents(&root, "d.txt"), "日本llo world\n");
        value_of(&vm, r#"write_chars("/m.txt", 1, "e")"#).await;
        assert_eq!(contents(&root, "m.txt"), "hello wörld\n");
    }

    #[tokio::test]
    async fn two_writes_in_one_task_land_in_order() {
        let (root, vm) = lib("wc-twice").await;
        vm.initialize_process_from_code(
            "/twice.c",
            r#"void create() { write_chars("/d.txt", 0, "日"); write_chars("/d.txt", 1, "É"); }"#,
        )
        .await
        .unwrap();
        assert_eq!(contents(&root, "d.txt"), "日Éllo world\n");
    }

    #[tokio::test]
    async fn write_chars_with_a_negative_start_counts_from_the_end() {
        let (root, vm) = lib("wc-negative").await;
        value_of(&vm, r#"write_chars("/m.txt", -6, "WORLD")"#).await;
        assert_eq!(contents(&root, "m.txt"), "héllo WORLD\n");
    }

    #[tokio::test]
    async fn write_chars_at_the_end_appends_and_past_it_extends() {
        let (root, vm) = lib("wc-append").await;
        value_of(&vm, r#"write_chars("/m.txt", 12, "!")"#).await;
        assert_eq!(contents(&root, "m.txt"), "héllo wörld\n!");
        value_of(&vm, r#"write_chars("/d.txt", 11, "?!")"#).await;
        assert_eq!(contents(&root, "d.txt"), "hello world?!");
    }

    #[tokio::test]
    async fn write_chars_past_the_end_is_zero_and_writes_nothing() {
        let (root, vm) = lib("wc-past").await;
        let got = value_of(&vm, r#"write_chars("/m.txt", 13, "!")"#).await;
        assert_eq!(got, LpcRef::from(0));
        assert_eq!(contents(&root, "m.txt"), "héllo wörld\n");
    }

    #[tokio::test]
    async fn write_chars_to_a_missing_file_is_zero() {
        let (root, vm) = lib("wc-missing").await;
        let got = value_of(&vm, r#"write_chars("/nope.txt", 0, "x")"#).await;
        assert_eq!(got, LpcRef::from(0));
        assert!(!root.join("nope.txt").exists());
    }

    #[tokio::test]
    async fn a_refused_write_is_an_error() {
        let root = TempLib::new("wc-refused");
        std::fs::write(root.join("d.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_write(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let err = error_of(&vm, r#"write_chars("/d.txt", 0, "y")"#).await;
        assert!(err.contains("write_chars: permission denied"), "{err}");
        assert_eq!(contents(&root, "d.txt"), "x");
    }

    /// The write lands at commit; a read in the same task already sees it.
    #[tokio::test]
    async fn a_read_in_the_same_task_sees_the_pending_characters() {
        let (root, vm) = lib("wc-deferred").await;
        let got = value_of(
            &vm,
            r#"write_chars("/m.txt", 0, "HÉLLO") + read_chars("/m.txt", 0, 5)"#,
        )
        .await;
        assert_eq!(got, LpcRef::from("1HÉLLO"));
        assert_eq!(contents(&root, "m.txt"), "HÉLLO wörld\n");
    }

    #[tokio::test]
    async fn write_chars_arguments_are_typed() {
        let (_root, vm) = lib("wc-typed").await;
        let p = vm
            .initialize_process_from_code(
                "/typed.c",
                r#"
                string e1;
                string e2;
                void create() {
                    mixed n = 1;
                    mixed s = "0";
                    e1 = catch(write_chars("/d.txt", 0, n));
                    e2 = catch(write_chars("/d.txt", s, "x"));
                }
                "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let err = committed_string(&vm, &p, 0);
        assert!(err.contains("write_chars: int is not a string"), "{err}");
        let err = committed_string(&vm, &p, 1);
        assert!(err.contains("write_chars: string is not an int"), "{err}");
    }
}
