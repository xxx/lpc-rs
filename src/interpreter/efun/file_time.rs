use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize_or_deny},
    lpc_ref::LpcRef,
};

/// `file_time(path)`: when the file was last modified, in seconds since
/// the epoch; -1 for a missing file or one the master's `valid_read`
/// refuses.
pub async fn file_time<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(access) = authorize_or_deny(context, "file_time", VALID_READ, 0).await? else {
        context.return_efun_result(LpcRef::from(-1));
        return Ok(());
    };
    let time = match tokio::fs::metadata(access.server())
        .await
        .and_then(|m| m.modified())
    {
        // Not chrono's `From<SystemTime>`: it unwraps on an mtime a
        // filesystem can hold.
        Ok(modified) => match modified.duration_since(std::time::UNIX_EPOCH) {
            Ok(d) => i64::try_from(d.as_secs()).unwrap_or(i64::MAX),
            Err(e) => -i64::try_from(e.duration().as_secs()).unwrap_or(i64::MAX),
        },
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => -1,
        Err(e) => {
            return Err(context.runtime_error(format!("file_time: {}: {e}", access.name())));
        }
    };
    context.return_efun_result(LpcRef::from(time));
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
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

    async fn time_of(vm: &Vm, path: &str) -> i64 {
        let code = format!(r#"int create() {{ return file_time("{path}"); }}"#);
        let result = vm
            .initialize_process_from_code("/timer.c", &code)
            .await
            .unwrap()
            .result();
        let Some(LpcRef::Int(t)) = result else {
            panic!("an int, actually {result:?}");
        };
        t.0
    }

    #[tokio::test]
    async fn file_time_is_the_modification_second() {
        let root = TempLib::new("time-file");
        let before = chrono::Utc::now().timestamp();
        std::fs::write(root.join("f.txt"), "x").unwrap();
        let after = chrono::Utc::now().timestamp();
        let vm = allowing_vm(&root).await;
        let t = time_of(&vm, "/f.txt").await;
        assert!(
            (before..=after).contains(&t),
            "{t} not in {before}..={after}"
        );
    }

    #[tokio::test]
    async fn a_directory_answers_its_time() {
        let root = TempLib::new("time-dir");
        let before = chrono::Utc::now().timestamp();
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        assert!(time_of(&vm, "/d").await >= before);
    }

    #[tokio::test]
    async fn a_missing_file_is_minus_one() {
        let root = TempLib::new("time-missing");
        let vm = allowing_vm(&root).await;
        assert_eq!(time_of(&vm, "/nope.txt").await, -1);
    }

    #[tokio::test]
    async fn a_refused_read_is_minus_one_not_an_error() {
        let root = TempLib::new("time-denied");
        std::fs::write(root.join("f.txt"), "x").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        assert_eq!(time_of(&vm, "/f.txt").await, -1);
    }

    #[tokio::test]
    async fn a_non_string_path_is_an_error() {
        let root = TempLib::new("time-nonstring");
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code(
                "/timer.c",
                "int create() { mixed p = 1; return file_time(p); }",
            )
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("file_time: path must be a string"), "{err}");
    }
}
