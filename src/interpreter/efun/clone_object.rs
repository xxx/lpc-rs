use std::sync::Arc;

use lpc_rs_core::{RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::Result;

use crate::{
    compile_time_config::MAX_CLONE_CHAIN,
    interpreter::{
        efun::efun_context::EfunContext, object_flags::ObjectFlags, process::Process, task::Task,
    },
};

async fn load_prototype<const N: usize>(
    context: &mut EfunContext<'_, N>,
    path: &str,
) -> Result<Arc<Process>> {
    let full_path = LpcPath::new_in_game(path, context.in_game_cwd(), &*context.config().lib_dir);

    if full_path.is_clone() {
        return Err(context.runtime_error(format!("Cannot clone a clone: {}", full_path)));
    }

    let path_str: &str = full_path.as_ref();

    if context.frame().process.program.filename.to_str().unwrap() == path_str {
        return Err(context.runtime_error(format!("Cannot clone self: {}", path_str)));
    }

    context.create_object(&full_path).await
}

/// `clone_object`, the efun for creating new object instances.
pub async fn clone_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.resolve_local_register(1 as RegisterSize);

    let path = arg.with_string(|s| s.to_string())?;

    let prototype = load_prototype(context, &path).await?;

    debug_assert!(
        !prototype.flags.test(ObjectFlags::Clone),
        "prototype cannot be a clone"
    );

    {
        if prototype.program.pragmas.no_clone() {
            return Err(context.runtime_error(format!(
                "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                prototype.program.filename
            )));
        }
    }

    let new_prog = prototype.program.clone();
    let clone_process = context.insert_clone(new_prog);

    debug_assert!(
        clone_process.flags.test(ObjectFlags::Clone),
        "new_clone must be a clone"
    );

    // if the prototype is not initialized, we initialize the clone.
    let return_val = if !prototype.flags.test(ObjectFlags::Initialized) {
        if context.chain_count() >= MAX_CLONE_CHAIN {
            return Err(context.runtime_error("infinite clone recursion detected"));
        }

        let new_task_context = context
            .task_context_builder()
            .process(clone_process)
            .chain_count(context.chain_count() + 1)
            .build()
            .unwrap();

        Task::<N>::initialize_sub_process(context.task_id(), new_task_context)
            .await?
            .context
            .process
    } else {
        clone_process
    };

    let v = Arc::downgrade(&return_val);
    let result = v.into();

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
            task_context::{TaskContext, TaskContextBuilder},
            vm::{
                Vm,
                global_state::{GlobalState, GlobalStateBuilder},
                vm_op::VmOp,
            },
        },
        test_support::{compile_prog, test_config},
        util::process_builder::ProcessCreator,
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

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);
        let (committer_tx, committer_handle) = GlobalState::spawn_committer();
        let global_state = GlobalStateBuilder::default()
            .config(config)
            .tx(tx)
            .committer_tx(committer_tx)
            .committer_handle(Some(committer_handle))
            .build()
            .unwrap();

        TaskContextBuilder::default()
            .global_state(Arc::new(global_state))
            .process(process)
            .build()
            .unwrap()
    }

    #[tokio::test]
    async fn does_not_create_multiple_prototype_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx);

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 300)
            .await
            .expect("first task failed");

        let mut task = Task::<10>::new(context);
        task.timed_eval(func, &[], 300)
            .await
            .expect("second task failed");

        // procs are /example, /example#0, /example#1
        assert_eq!(task.context.object_space().len(), 3);
    }

    #[tokio::test]
    async fn returns_error_if_no_clone() {
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let context = task_context_fixture(program, config, tx);
        let mut task = Task::<10>::new(context);

        let result = task.timed_eval(func, &[], 300).await;

        assert_regex!(
            result.as_ref().unwrap_err().as_ref().as_ref(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }

    #[tokio::test]
    async fn initializes_clone_if_prototype_not_initialized() {
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
        assert!(!cloned_proc.flags.test(ObjectFlags::Initialized));

        let cloner_proc = vm
            .initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap()
            .context
            .process;
        assert!(cloner_proc.flags.test(ObjectFlags::Initialized));

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
        assert!(foo.flags.test(ObjectFlags::Initialized));

        let foo_i = committed_globals_by_name(&vm.global_state, &foo)
            .get("i")
            .unwrap()
            .clone();
        assert_eq!(foo_i, LpcRef::from(123));
    }

    #[tokio::test]
    async fn handles_clone_self_recursion() {
        // This tests the case where an uninitialized prototype is cloned, (which
        // initializes the clone), and that clone then clones itself. This is
        // an infinite loop because if the prototype is _not_ initialized, then
        // the clone _will_ be initialized, and then clone itself, and so on.

        // This object will be initialized as a prototype, and so clones of it will
        // _not_ be initialized. It's not self-cloning, as it has a different path
        // than "self_clone".
        let prototype = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        // This is the self-cloning object, which has a different path than the prototype,
        // (even though the code is the same, which is just a coincidence). It will
        // be cloned by the prototype above, _without_ its prototype being initialized,
        // meaning it _will_ be initialized, and start a clone chain.
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

                // Prototype has been called, and so is initialized.
                // Clone should not be initialized, and should not have a
                // copy of the prototype's data
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
