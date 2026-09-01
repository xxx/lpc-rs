use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
};

/// `read_file(path)`: the whole file as a string, once the master's
/// `valid_read` allows it. Reads live: a `write_file` earlier in this task
/// has not landed yet.
pub async fn read_file<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "read_file", VALID_READ, 1 as RegisterSize).await?;
    let contents = tokio::fs::read_to_string(&access.server)
        .await
        .map_err(|e| context.runtime_error(format!("read_file: {}: {e}", access.in_game)))?;
    context.return_efun_result(LpcRef::from(contents));
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

    /// A lib at `root` holding `/data.txt`.
    fn lib_with_data(name: &str) -> TempLib {
        let root = TempLib::new(name);
        std::fs::write(root.join("data.txt"), "hello\n").unwrap();
        root
    }

    const READER: &str = indoc! { r#"
        string got;
        string err;
        void create() { err = catch(got = read_file("/data.txt")); }
    "# };

    async fn read_under(vm: &Vm) -> Arc<Process> {
        vm.initialize_process_from_code("/reader.c", READER)
            .await
            .unwrap()
            .context
            .process
    }

    #[tokio::test]
    async fn an_allowing_master_reads_the_file() {
        let root = lib_with_data("read-allow");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 1; }",
        )
        .await
        .unwrap();
        let reader = read_under(&vm).await;
        assert_eq!(committed_string(&vm, &reader, 0), "hello\n");
    }

    fn assert_denied(vm: &Vm, reader: &Arc<Process>) {
        assert_eq!(
            vm.global_state.committed_global(reader, 0u16),
            LpcRef::from(0)
        );
        let err = committed_string(vm, reader, 1);
        assert!(err.contains("read_file: permission denied"), "{err}");
    }

    #[tokio::test]
    async fn no_master_refuses() {
        let root = lib_with_data("read-no-master");
        let vm = Vm::new(temp_lib_config(&root));
        let reader = read_under(&vm).await;
        assert_denied(&vm, &reader);
    }

    #[tokio::test]
    async fn a_master_without_valid_read_refuses() {
        let root = lib_with_data("read-no-apply");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", "")
            .await
            .unwrap();
        let reader = read_under(&vm).await;
        assert_denied(&vm, &reader);
    }

    #[tokio::test]
    async fn a_refusing_master_denies() {
        let root = lib_with_data("read-deny");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let reader = read_under(&vm).await;
        assert_denied(&vm, &reader);
    }

    #[tokio::test]
    async fn an_error_in_valid_read_is_the_callers() {
        let root = lib_with_data("read-throws");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_read(string p, string e, object c, string g) { throw("not today"); }"#,
        )
        .await
        .unwrap();
        let reader = read_under(&vm).await;
        assert_eq!(
            vm.global_state.committed_global(&reader, 0u16),
            LpcRef::from(0)
        );
        assert!(committed_string(&vm, &reader, 1).contains("not today"));
    }

    /// The master hears the efun's name and the canonical path.
    #[tokio::test]
    async fn the_master_hears_the_path_and_the_efun() {
        let root = lib_with_data("read-args");
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string seen_path;
                    string seen_efun;
                    int valid_read(string path, string which, object caller, string program) {
                        seen_path = path;
                        seen_efun = which;
                        return 1;
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        read_under(&vm).await;
        assert_eq!(committed_string(&vm, &master, 0), "/data.txt");
        assert_eq!(committed_string(&vm, &master, 1), "read_file");
    }

    /// A missing file is an error naming the in-game path — never a 0.
    #[tokio::test]
    async fn a_missing_file_is_a_runtime_error_naming_the_in_game_path() {
        let root = TempLib::new("read-missing");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 1; }",
        )
        .await
        .unwrap();
        let err = vm
            .initialize_process_from_code(
                "/reader.c",
                r#"string got; void create() { got = read_file("/missing.txt"); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("read_file: /missing.txt:"), "{err}");
        assert!(
            !err.contains(vm.global_state.config.lib_dir.as_str()),
            "the server path leaked: {err}"
        );
    }

    #[tokio::test]
    async fn a_non_string_path_is_a_runtime_error() {
        let root = TempLib::new("read-non-string");
        let vm = Vm::new(temp_lib_config(&root));
        let err = vm
            .initialize_process_from_code(
                "/reader.c",
                r#"void create() { mixed p = 5; read_file(p); }"#,
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("read_file: path must be a string"), "{err}");
    }
}
