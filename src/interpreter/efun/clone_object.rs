use std::sync::Arc;

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    compile_time_config::MAX_TASK_CHAIN,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process},
};

async fn load_prototype<const N: usize>(
    context: &mut EfunContext<'_, N>,
    path: &str,
) -> Result<Arc<Process>> {
    let full_path = context.in_game_path(path);

    if full_path.is_clone() {
        return Err(context.runtime_error(format!("Cannot clone a clone: {}", full_path)));
    }

    context.load_object(path).await
}

/// `clone_object`, the efun for creating new object instances.
pub async fn clone_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.resolve_local_register(1 as RegisterSize);

    let path = arg.with_string(|s| s.to_string())?;

    let prototype = load_prototype(context, &path).await?;

    debug_assert!(!prototype.is_clone(), "prototype cannot be a clone");

    {
        if prototype.program.pragmas.no_clone() {
            return Err(context.runtime_error(format!(
                "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                prototype.program.filename
            )));
        }
    }

    // The message only; `TaskContext::nested` refuses at the same bound —
    // checked before creating anything, so a refusal here leaves nothing to
    // roll back.
    if context.chain_count() >= MAX_TASK_CHAIN {
        return Err(context.runtime_error("infinite clone recursion detected"));
    }

    let new_prog = prototype.program.clone();
    let clone_process = context.object_space().create_clone_process(new_prog);
    debug_assert!(clone_process.is_clone(), "new_clone must be a clone");
    context
        .task_context()
        .insert_and_initialize(Some(context.chain()), &clone_process)
        .await?;

    let result = LpcRef::from(Arc::downgrade(&clone_process));
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_core::{RegisterSize, register::RegisterVariant};
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        assert_regex,
        interpreter::{
            CommittedReader,
            lpc_ref::{LpcRef, NULL},
            program::Program,
            task::Task,
            task_context::TaskContext,
            vm::{Vm, global_state::GlobalState, vm_op::VmOp},
        },
        test_support::{compile_prog, permissive_master, test_config},
    };

    /// Committed global values by name, read through the committer.
    fn committed_globals_by_name(
        gs: &Arc<GlobalState>,
        proc: &Process,
    ) -> std::collections::HashMap<String, LpcRef> {
        proc.program
            .global_variables
            .iter()
            .filter_map(|(name, sym)| {
                let RegisterVariant::Global(reg) = sym.location? else {
                    return None;
                };
                Some((name.clone(), gs.committed_global(proc, reg.index())))
            })
            .collect()
    }

    async fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);
        let global_state = Arc::new(GlobalState::new(config, tx));
        permissive_master(&global_state.object_space).await;

        TaskContext::new(global_state, process, None)
    }

    #[tokio::test]
    async fn does_not_create_multiple_prototype_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx).await;

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 300)
            .await
            .expect("first task failed");

        let mut task = Task::<10>::new(context);
        task.timed_eval(func, &[], 300)
            .await
            .expect("second task failed");

        // procs are the master, /example, /example#0, /example#1
        assert_eq!(task.context.object_space().len(), 4);
    }

    #[tokio::test]
    async fn returns_error_if_no_clone() {
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let context = task_context_fixture(program, config, tx).await;
        let mut task = Task::<10>::new(context);

        let result = task.timed_eval(func, &[], 300).await;

        assert_regex!(
            result.as_ref().unwrap_err().message(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }

    #[tokio::test]
    async fn initializes_clone_of_an_uninitialized_prototype() {
        let cloned = indoc! { r#"
            int i = 123;
        "# };

        let cloner = indoc! { r#"
            object foo = clone_object("cloned");
        "# };

        let vm = Vm::new(test_config());
        let cloned_proc = vm
            .create_process_from_code("cloned.c", cloned)
            .await
            .unwrap();

        assert_eq!(
            committed_globals_by_name(&vm.global_state, &cloned_proc)
                .get("i")
                .unwrap(),
            &NULL
        );
        assert!(!vm.global_state.is_initialized(&cloned_proc));

        let cloner_proc = vm
            .initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap()
            .context
            .process;
        assert!(vm.global_state.is_initialized(&cloner_proc));

        assert_eq!(
            committed_globals_by_name(&vm.global_state, &cloned_proc)
                .get("i")
                .unwrap(),
            &NULL
        );
        let LpcRef::Object(foo) = committed_globals_by_name(&vm.global_state, &cloner_proc)
            .get("foo")
            .unwrap()
            .clone()
        else {
            panic!("foo is not an object");
        };

        let foo = foo.upgrade().unwrap();
        assert!(vm.global_state.is_initialized(&foo));

        let foo_i = committed_globals_by_name(&vm.global_state, &foo)
            .get("i")
            .unwrap()
            .clone();
        assert_eq!(foo_i, LpcRef::from(123));
    }

    #[tokio::test]
    async fn initializes_clone_of_an_initialized_prototype() {
        let cloned = indoc! { r#"
            int i = 123;
        "# };

        let cloner = indoc! { r#"
            object foo = clone_object("cloned");
        "# };

        let vm = Vm::new(test_config());
        let cloned_proc = vm
            .initialize_process_from_code("cloned.c", cloned)
            .await
            .unwrap()
            .context
            .process;
        assert!(vm.global_state.is_initialized(&cloned_proc));

        let cloner_proc = vm
            .initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap()
            .context
            .process;
        let LpcRef::Object(foo) = committed_globals_by_name(&vm.global_state, &cloner_proc)
            .get("foo")
            .unwrap()
            .clone()
        else {
            panic!("foo is not an object");
        };
        let foo = foo.upgrade().unwrap();

        assert!(vm.global_state.is_initialized(&foo));
        assert_eq!(
            committed_globals_by_name(&vm.global_state, &foo)
                .get("i")
                .unwrap(),
            &LpcRef::from(123)
        );
    }

    #[tokio::test]
    async fn initializes_a_missing_prototype_and_its_clone() {
        let cloner = indoc! { r#"
            object foo = clone_object("/clone_target");
        "# };

        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap();

        let object_space = &vm.global_state.object_space;
        for key in ["/clone_target", "/clone_target#0"] {
            let process = object_space.lookup(key).unwrap();
            assert!(vm.global_state.is_initialized(&process), "{key}");
            assert_eq!(
                committed_globals_by_name(&vm.global_state, &process)
                    .get("i")
                    .unwrap(),
                &LpcRef::from(123),
                "{key}"
            );
        }
    }

    #[tokio::test]
    async fn clones_its_own_prototype() {
        let caller = indoc! { r#"
            object copy = "/self_copy"->copy();
        "# };

        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let caller_proc = vm
            .initialize_process_from_code("caller.c", caller)
            .await
            .unwrap()
            .context
            .process;

        let LpcRef::Object(copy) = committed_globals_by_name(&vm.global_state, &caller_proc)
            .get("copy")
            .unwrap()
            .clone()
        else {
            panic!("copy is not an object");
        };
        let copy = copy.upgrade().unwrap();

        assert_eq!(copy.filename(), "/self_copy#0");
        assert!(vm.global_state.is_initialized(&copy));
        assert_eq!(
            committed_globals_by_name(&vm.global_state, &copy)
                .get("i")
                .unwrap(),
            &LpcRef::from(123)
        );
    }

    #[tokio::test]
    async fn handles_clone_self_recursion() {
        // Each clone's initializer clones the same path again.
        let prototype = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        let self_clone = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        let vm = Vm::new(test_config());
        let _self_clone_proc = vm
            .create_process_from_code("self_clone.c", self_clone)
            .await
            .unwrap();

        let prototype_proc = vm
            .initialize_process_from_code("prototype.c", prototype)
            .await;

        assert!(
            prototype_proc
                .unwrap_err()
                .to_string()
                .contains("infinite clone recursion detected")
        );
    }

    #[tokio::test]
    async fn empties_vars_before_initialization() {
        let prototype = indoc! { r#"
            void create() {
                "/clone"->set_name("proto foo");

                // The clone starts from its own initializer, not from a
                // copy of the prototype's data.
                object student = clone_object("/clone");
            }
        "# };

        let clone = indoc! { r#"
            string name;

            void set_name(string new_name) {
                name = new_name;
            }
        "# };

        let vm = Vm::new(test_config());
        let _clone_proc = vm
            .create_process_from_code("/clone.c", clone)
            .await
            .unwrap();

        let _prototype_proc = vm
            .initialize_process_from_code("/prototype.c", prototype)
            .await
            .unwrap();

        let student = vm.global_state.object_space.lookup("/clone#0").unwrap();

        let slots = vm.global_state.global_slot_count(&student);
        for i in 0..slots {
            assert!(
                vm.global_state
                    .committed_global(&student, i as RegisterSize)
                    .is_null()
            );
        }
    }
}
