use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::efun_context::EfunContext,
    lpc_ref::{LpcRef, NULL},
    task_context::ObjectLookup,
};

/// Find an existing object by path without loading or initializing it.
pub fn find_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let lpc_ref = context.arg(0);
    let path = lpc_ref.as_str().and_then(|path| {
        context
            .task_context()
            .object_path(path, context.in_game_cwd(), "find_object")
            .ok()
    });
    let result = match path {
        Some(path) => match context.find_object(&path) {
            ObjectLookup::Found(proc) => LpcRef::from(Arc::downgrade(&proc)),
            ObjectLookup::Removed | ObjectLookup::NotCreated => NULL,
        },
        None => NULL,
    };

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::{path::Path, sync::Arc};

    use indoc::indoc;
    use lpc_rs_core::{lpc_path::LpcPath, register::RegisterVariant};
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            object_space::ObjectSpace,
            process::Process,
            program::{Program, ProgramBuilder},
            task::Task,
            task_context::TaskContext,
            vm::{Vm, global_state::GlobalState, vm_op::VmOp},
        },
        test_support::{TempLib, compile_prog, permissive_master, temp_lib_config, test_config},
    };

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);

        let global_state = GlobalState::new(config, tx);

        TaskContext::new(Arc::new(global_state), process, None)
    }

    #[tokio::test]
    async fn test_find_object_success() {
        let code = indoc! { r#"
            object foo = find_object("/example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);
        let to_find = ProgramBuilder::default()
            .filename(LpcPath::in_game(Path::new("/example").to_path_buf()))
            .build()
            .unwrap();
        let proc = Process::new(to_find);
        ObjectSpace::insert_process_physical(context.object_space(), proc);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 500)
            .await
            .expect("task failed");

        let process = task.context.process();
        let sym = &process.program.global_variables["foo"];
        let Some(RegisterVariant::Global(reg)) = sym.location else {
            panic!("`foo` is not a global register");
        };
        let LpcRef::Object(obj) = task
            .context
            .global_state
            .committed_global(process, reg.index())
        else {
            panic!("expected object");
        };

        assert_eq!(
            obj.upgrade().unwrap().program.filename.to_str().unwrap(),
            "/example"
        );
    }

    #[tokio::test]
    async fn test_find_object_failure() {
        let code = indoc! { r#"
            object foo = find_object("/non-existent");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 500)
            .await
            .expect("task failed");

        assert_eq!(task.result().unwrap(), NULL);
    }

    #[tokio::test]
    async fn missing_objects_do_not_load_or_consult_the_master() {
        let root = TempLib::new("find-without-loading");
        std::fs::write(root.join("target.c"), "void create() {}\n").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    int loads; int virtuals;
                    int valid_load(string p, string f, object c, string g) {
                        loads++;
                        return 1;
                    }
                    string compile_object(string p, string f, object c, string g) {
                        virtuals++;
                        return "/target";
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let task = vm
            .initialize_process_from_code(
                "/finder.c",
                indoc! { r#"
                    int create() {
                        return find_object("/target") == 0
                            && find_object("/virtual") == 0
                            && find_object("/target#42") == 0
                            && find_object("/../target") == 0
                            && find_object("") == 0;
                    }
                "# },
            )
            .await
            .unwrap();

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(vm.global_state.committed_global(&master, 0u16), NULL);
        assert_eq!(vm.global_state.committed_global(&master, 1u16), NULL);
        assert!(vm.global_state.object_space.lookup("/target").is_none());
        assert!(vm.global_state.object_space.lookup("/virtual").is_none());
    }

    #[tokio::test]
    async fn existing_objects_resolve_relative_paths_without_initialization() {
        let vm = Vm::new(test_config());
        let target = vm
            .create_process_from_code("/d/target.c", r#"void create() { throw("initialized"); }"#)
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/d/finder.c",
            indoc! { r#"
                int check() {
                    object target = find_object("target");
                    function lookup = &find_object();
                    return objectp(target)
                        && target == lookup("target")
                        && target == find_object("target.c")
                        && target == find_object("/d/target")
                        && target == find_object("/d/target.c");
                }
            "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r#"int create() { return "/d/finder"->check(); }"#,
            )
            .await
            .unwrap();

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!vm.global_state.is_initialized(&target));
    }

    #[tokio::test]
    async fn finds_objects_loaded_this_transaction_until_destructed() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let task = vm
            .initialize_process_from_code(
                "/finder.c",
                indoc! { r#"
                    int create() {
                        object target = load_object("/empty");
                        int found = objectp(target) && find_object("/empty") == target;
                        destruct(target);
                        return found && find_object("/empty") == 0;
                    }
                "# },
            )
            .await
            .unwrap();

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(vm.global_state.object_space.lookup("/empty").is_none());
    }

    #[tokio::test]
    async fn finds_committed_objects_until_destructed() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code("/loader.c", r#"void create() { load_object("/empty"); }"#)
            .await
            .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/finder.c",
                indoc! { r#"
                    int create() {
                        object target = find_object("/empty");
                        int found = objectp(target);
                        destruct(target);
                        return found && find_object("/empty") == 0;
                    }
                "# },
            )
            .await
            .unwrap();

        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(vm.global_state.object_space.lookup("/empty").is_none());
    }

    // The clone's physical insert is deferred until commit.
    #[tokio::test]
    async fn test_finds_clone_created_this_transaction() {
        let code = indoc! { r#"
            object clone;

            object create() {
                clone = clone_object("/example");
                return find_object("/example#0");
            }
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(code).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);
        permissive_master(context.object_space()).await;

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 500)
            .await
            .expect("task failed");

        let LpcRef::Object(found) = task.result().unwrap() else {
            panic!("find_object should have found the just-created clone");
        };

        // `process.filename()` is the object path (the cell/physical-map key),
        // e.g. `/example#0` for a clone; `program.filename` is the source `/example.c`.
        assert_eq!(
            found.upgrade().unwrap().filename().as_ref(),
            "/example#0",
            "same-transaction find_object must see the clone created this txn"
        );
    }
}
