use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
};

/// Find or load an object by path, returning zero if loading fails.
pub async fn load_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.arg(0);
    let result = match lpc_ref.as_str() {
        Some(path) => match context.load_object(path).await {
            Ok(proc) => LpcRef::from(Arc::downgrade(&proc)),
            Err(_) => NULL,
        },
        None => NULL,
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{CommittedReader, vm::Vm},
        test_support::{TempLib, permissive_master, temp_lib_config},
    };

    #[tokio::test]
    async fn loads_and_initializes_once_then_reuses_the_object() {
        let root = TempLib::new("load-object-once");
        std::fs::create_dir_all(root.join("d")).unwrap();
        std::fs::write(
            root.join("d/target.c"),
            "int creates; void create() { creates++; } int count() { return creates; }",
        )
        .unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            "/d/loader.c",
            indoc! { r#"
                object get() { return load_object("target"); }
            "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! { r#"
                    int create() {
                        object target = "/d/loader"->get();
                        function loader = &load_object();
                        return objectp(target)
                            && target == load_object("/d/target.c")
                            && target == loader("/d/target")
                            && target == find_object("/d/target")
                            && target->count() == 1;
                    }
                "# },
            )
            .await
            .unwrap();

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        let target = vm.global_state.object_space.lookup("/d/target").unwrap();
        assert!(vm.global_state.is_initialized(&target));
        let task = vm
            .initialize_process_from_code(
                "/later.c",
                r#"int create() { return load_object("/d/target")->count(); }"#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(Arc::ptr_eq(
            &target,
            &vm.global_state.object_space.lookup("/d/target").unwrap()
        ));
    }

    #[tokio::test]
    async fn failed_loads_return_zero_without_leaving_an_object() {
        let root = TempLib::new("load-object-failure");
        std::fs::write(root.join("broken.c"), "int broken = ;").unwrap();
        std::fs::write(
            root.join("throws.c"),
            r#"void create() { throw("failed"); }"#,
        )
        .unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/loader.c",
                indoc! { r#"
                    int create() {
                        return load_object("/missing") == 0
                            && load_object("/broken") == 0
                            && load_object("/throws") == 0
                            && load_object("/../escape") == 0
                            && load_object("") == 0;
                    }
                "# },
            )
            .await
            .unwrap();

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        for path in ["/missing", "/broken", "/throws"] {
            assert!(
                vm.global_state.object_space.lookup(path).is_none(),
                "{path}"
            );
        }
    }
}
