use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
};

/// `get_dir(path)`: the directory's entries as sorted plain names, once the
/// master's `valid_read` allows it; a `path` that is not a directory is an
/// error.
pub async fn get_dir<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "get_dir", VALID_READ, 1 as RegisterSize).await?;
    let io_error =
        |e: std::io::Error| context.runtime_error(format!("get_dir: {}: {e}", access.in_game));
    let mut entries = tokio::fs::read_dir(&access.server)
        .await
        .map_err(io_error)?;
    let mut names = Vec::new();
    while let Some(entry) = entries.next_entry().await.map_err(io_error)? {
        names.push(entry.file_name().to_string_lossy().into_owned());
    }
    names.sort();
    context.return_array(names.into_iter().map(LpcRef::from));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::vm::Vm,
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

    /// `create()`'s array result, as strings.
    async fn names_of(vm: &Vm, code: &str) -> Vec<String> {
        let task = vm
            .initialize_process_from_code("/lister.c", code)
            .await
            .unwrap();
        let mut names = Vec::new();
        let _ = task
            .result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                names = arr.iter().map(|x| x.to_string()).collect();
            });
        names
    }

    #[tokio::test]
    async fn entries_come_back_sorted_as_plain_names() {
        let root = TempLib::new("dir-sorted");
        std::fs::create_dir_all(root.join("d/sub")).unwrap();
        std::fs::write(root.join("d/b.c"), "").unwrap();
        std::fs::write(root.join("d/a.c"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d"); }"#).await;
        assert_eq!(names, ["a.c", "b.c", "sub"]);
    }

    #[tokio::test]
    async fn an_empty_directory_is_an_empty_array() {
        let root = TempLib::new("dir-empty");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d"); }"#).await;
        assert!(names.is_empty());
    }

    #[tokio::test]
    async fn a_file_is_not_a_directory() {
        let root = TempLib::new("dir-file");
        std::fs::write(root.join("f.txt"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/lister.c", r#"void create() { get_dir("/f.txt"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("get_dir: /f.txt:"), "{err}");
    }

    #[tokio::test]
    async fn the_master_hears_get_dir() {
        let root = TempLib::new("dir-denied");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_read(string p, string e, object c, string g) { return e != "get_dir"; }"#,
        )
        .await
        .unwrap();
        let err = vm
            .initialize_process_from_code("/lister.c", r#"void create() { get_dir("/d"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("get_dir: permission denied"), "{err}");
    }
}
