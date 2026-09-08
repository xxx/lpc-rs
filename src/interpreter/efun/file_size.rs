use lpc_rs_core::LpcIntInner;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{
        efun_context::EfunContext,
        file_access::authorize_or_deny,
        file_view::{Seen, read_through},
    },
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
    let size = match read_through(context, &access.server).await {
        Ok(Seen::Dir) => -2,
        Ok(Seen::File(bytes)) => LpcIntInner::try_from(bytes.len()).unwrap_or(LpcIntInner::MAX),
        Ok(Seen::Missing) => -1,
        Err(e) => {
            return Err(context.runtime_error(format!("file_size: {}: {e}", access.in_game)));
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
            "int valid_read(string p, string e, object c, string g) { return 1; }",
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
