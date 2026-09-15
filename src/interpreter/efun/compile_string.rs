use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::LpcRef,
    task_context::{Loader, ObjectLookup},
};

/// Compile supplied LPC source into a named object in the caller's transaction.
pub async fn compile_string<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(name) = context.arg(0).as_str() else {
        return Err(context.runtime_error("compile_string: name must be a string"));
    };
    let Some(code) = context.arg(1).as_str() else {
        return Err(context.runtime_error("compile_string: source must be a string"));
    };
    let path = context
        .task_context()
        .object_path(name, context.in_game_cwd(), "compile_string")?;
    if path.is_clone() {
        return Err(context.runtime_error("compile_string: name must not identify a clone"));
    }
    let vacant = || {
        if matches!(context.find_object(&path), ObjectLookup::Found(_)) {
            Err(context.runtime_error(format!("compile_string: `{path}` is already loaded")))
        } else {
            Ok(())
        }
    };
    vacant()?;
    let loader = Loader {
        func: "compile_string".into(),
        chain: context.chain(),
        program: context.calling_program(),
    };
    let process = context
        .task_context()
        .compile_source_process(&path, &loader, Some(code))
        .await
        .map_err(|e| e.with_label("compiled from here", context.call_site_span()))?;
    // Permission and warning hooks may have loaded the name during compilation.
    vacant()?;
    context
        .task_context()
        .insert_and_initialize(loader.callers(), &process)
        .await
        .map_err(|e| e.with_label("compiled from here", context.call_site_span()))?;
    context.return_efun_result(LpcRef::from(Arc::downgrade(&process)));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            vm::{Vm, global_state::GlobalState},
        },
        test_support::{PERMISSIVE_MASTER, TempLib, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.create_process_from_code(
            "/secure/master.c",
            format!("{PERMISSIVE_MASTER}\nint valid_write() {{ return 1; }}"),
        )
        .await
        .unwrap();
        vm
    }

    #[tokio::test]
    async fn compiles_initializes_and_executes_without_a_source_file() {
        let root = TempLib::new("compile-string-execute");
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/wizard/caller.c", indoc! {r##"
            int create() {
                set_this_player(this_object());
                object scratch = compile_string("scratch.c",
                    "object player; object caller; int count; void create() { count++; player = this_player(); caller = previous_object(); } int value() { return count == 1 && player == this_player() && caller == previous_object(); }");
                return scratch == find_object("/wizard/scratch")
                    && scratch == load_object("/wizard/scratch") && scratch->value();
            }
        "##}).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("wizard").exists());
        assert!(
            vm.global_state
                .object_space
                .lookup("/wizard/scratch")
                .is_some()
        );
    }

    #[tokio::test]
    async fn includes_pending_headers_and_inherits_pending_source() {
        let root = TempLib::new("compile-string-includes");
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", indoc! {r##"
            int create() {
                write_file("/personal.h", "#define VALUE 42\n");
                write_file("/parent.c", "int parent_value() { return 1; }");
                object scratch = compile_string("/scratch", "#include \"personal.h\"\ninherit \"parent\"; int value() { return VALUE + parent_value(); }");
                return scratch->value();
            }
        "##}).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(43)));
        assert!(!root.join("scratch.c").exists());
    }

    #[tokio::test]
    async fn collisions_fail_and_a_destructed_name_can_be_reused() {
        let root = TempLib::new("compile-string-collision");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                indoc! {r##"
            int create() {
                object first = compile_string("/scratch", "int value() { return 1; }");
                string err = catch(compile_string("/scratch.c", "int value() { return 2; }"));
                if (!err || first->value() != 1) return 0;
                destruct(first);
                function compile = &compile_string();
                object second = compile("/scratch", "int value() { return 3; }");
                return second->value();
            }
        "##},
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(3)));
    }

    #[tokio::test]
    async fn failed_compilation_or_initialization_leaves_no_object() {
        let root = TempLib::new("compile-string-failure");
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", indoc! {r##"
            int create() {
                string syntax = catch(compile_string("/broken", "int value = ;"));
                string init = catch(compile_string("/throws", "void create() { throw(\"init failed\"); }"));
                string clone = catch(compile_string("/scratch#1", ""));
                string escape = catch(compile_string("/../escape", ""));
                mixed bad = 123;
                string name = catch(compile_string(bad, ""));
                string source = catch(compile_string("/bad", bad));
                return !!syntax && !!init && !!clone && !!escape && !!name && !!source
                    && !find_object("/broken") && !find_object("/throws");
            }
        "##}).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(vm.global_state.object_space.lookup("/broken").is_none());
        assert!(vm.global_state.object_space.lookup("/throws").is_none());
    }

    #[tokio::test]
    async fn source_identity_requires_both_write_and_load_permission() {
        for (write, load) in [(0, 1), (1, 0)] {
            let root = TempLib::new("compile-string-permissions");
            let vm = Vm::new(temp_lib_config(&root));
            vm.create_process_from_code(
                "/secure/master.c",
                format!(
                    "int valid_write() {{ return {write}; }} int valid_load() {{ return {load}; }}"
                ),
            )
            .await
            .unwrap();
            let error = vm
                .initialize_process_from_code(
                    "/caller.c",
                    r##"
                void create() { compile_string("/scratch", ""); }
            "##,
                )
                .await
                .unwrap_err();
            assert!(
                error
                    .to_string()
                    .contains("compile_string: permission denied"),
                "{error}"
            );
            assert!(vm.global_state.object_space.lookup("/scratch").is_none());
        }
    }

    #[tokio::test]
    async fn permissions_receive_the_canonical_name_and_actual_caller() {
        let root = TempLib::new("compile-string-permission-args");
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/secure/master.c",
            indoc! {r##"
            int valid_write(string path, string func, object caller, string program) {
                return path == "/wizard/scratch.c" && func == "compile_string"
                    && file_name(caller) == "/wizard/caller" && program == "/wizard/caller.c";
            }
            int valid_load(string path, string func, object caller, string program) {
                return valid_write(path, func, caller, program);
            }
        "##},
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/wizard/caller.c",
                r##"
            int create() { return objectp(compile_string("./scratch.c", "")); }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn an_abort_discards_the_object_and_its_initializers_effects() {
        let root = TempLib::new("compile-string-abort");
        let vm = allowing_vm(&root).await;
        let error = vm.initialize_process_from_code("/caller.c", r##"
            void create() {
                compile_string("/scratch", "void create() { write_file(\"/effect.txt\", \"once\"); }");
                throw("abort after initialization");
            }
        "##).await.unwrap_err();
        assert!(
            error.to_string().contains("abort after initialization"),
            "{error}"
        );
        assert!(vm.global_state.object_space.lookup("/scratch").is_none());
        assert!(!root.join("effect.txt").exists());
    }

    #[tokio::test]
    async fn a_permission_hook_cannot_cause_an_existing_object_to_be_overwritten() {
        let root = TempLib::new("compile-string-hook-collision");
        std::fs::write(root.join("scratch.c"), "int value() { return 7; }").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/secure/master.c",
            r#"
            int valid_load() { return 1; }
            int valid_write(string path) { load_object(path); return 1; }
        "#,
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"
            int create() {
                string err = catch(compile_string("/scratch", "int value() { return 8; }"));
                return !!err && find_object("/scratch")->value() == 7;
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn includes_and_inherits_still_require_permission() {
        let root = TempLib::new("compile-string-dependency-permissions");
        std::fs::write(root.join("secret.h"), "").unwrap();
        std::fs::write(root.join("parent.c"), "").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/secure/master.c",
            r#"
            int valid_write() { return 1; }
            int valid_load() { return 1; }
        "#,
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r##"
            int create() {
                string header = catch(compile_string("/child", "#include \"secret.h\"\n"));
                string parent = catch(compile_string("/child", "inherit \"parent\";"));
                return !!header && !!parent && !find_object("/child");
            }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn concurrent_compiles_of_one_name_do_not_replace_each_other() {
        let root = TempLib::new("compile-string-concurrent");
        let vm = allowing_vm(&root).await;
        let (first, second) = tokio::join!(
            vm.initialize_process_from_code(
                "/first.c",
                r#"
                int create() { return compile_string("/scratch", "int value = 1;") != 0; }
            "#
            ),
            vm.initialize_process_from_code(
                "/second.c",
                r#"
                int create() { return compile_string("/scratch", "int value = 2;") != 0; }
            "#
            ),
        );
        let (winner, error) = match (first, second) {
            (Ok(_), Err(error)) => (1, error),
            (Err(error), Ok(_)) => (2, error),
            other => panic!("exactly one compile must succeed: {other:?}"),
        };
        assert!(error.to_string().contains("already loaded"), "{error}");
        let scratch = vm.global_state.object_space.lookup("/scratch").unwrap();
        assert_eq!(
            vm.global_state.committed_global(&scratch, 0u16),
            LpcRef::from(winner)
        );
    }

    #[tokio::test]
    async fn a_retry_recreates_the_object_and_commits_its_effects_once() {
        let root = TempLib::new("compile-string-retry");
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let state = Arc::new(GlobalState::new_rejecting(
            Arc::new(temp_lib_config(&root)),
            tx,
            3,
        ));
        state
            .object_space
            .create_process_from_code(
                "/secure/master.c",
                format!("{PERMISSIVE_MASTER}\nint valid_write() {{ return 1; }}"),
            )
            .await
            .unwrap();
        let task = state.initialize_process_from_code("/caller.c", r##"
            int create() {
                object scratch = compile_string("/scratch", "int count; void create() { count++; write_file(\"/effect.txt\", \"once\"); } int value() { return count; }");
                return scratch->value();
            }
        "##).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(state.attempt_telemetry().conflicts, 3);
        assert_eq!(
            std::fs::read_to_string(root.join("effect.txt")).unwrap(),
            "once"
        );
        let scratch = state.object_space.lookup("/scratch").unwrap();
        assert_eq!(state.committed_global(&scratch, 0u16), LpcRef::from(1));
    }
}
