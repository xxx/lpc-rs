use std::sync::Arc;

use lpc_rs_core::{lpc_path::LpcPath, RegisterSize};
use lpc_rs_errors::Result;

use crate::{
    compile_time_config::MAX_CLONE_CHAIN,
    interpreter::{
        efun::efun_context::EfunContext, into_lpc_ref::IntoLpcRef, lpc_ref::LpcRef,
        object_flags::ObjectFlags, process::Process, task::Task,
    },
    util::process_builder::ProcessBuilder,
};

async fn load_master<const N: usize>(
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

    match context.lookup_process(path_str) {
        Some(proc) => Ok(proc),
        None => {
            context
                .process_create_from_path(&full_path)
                .await
                .map_err(|e| {
                    let debug_span = context.current_debug_span();

                    e.with_span(debug_span).into()
                })
            // let compiler = CompilerBuilder::default()
            //     .config(context.config().clone())
            //     .build()?;
            //
            // match compiler
            //     .compile_in_game_file(&full_path, context.current_debug_span())
            //     .await
            // {
            //     Ok(prog) => {
            //         // Masters are not initialized unless a call is made against them directly via call_other.
            //         let process: Arc<Process> = Process::new(prog).into();
            //         context.insert_process(process.clone());
            //
            //         Ok(process)
            //     }
            //     Err(e) => {
            //         let debug_span = context.current_debug_span();
            //
            //         Err(e.with_span(debug_span))
            //     }
            // }
        }
    }
}

/// `clone_object`, the efun for creating new object instances.
pub async fn clone_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.resolve_local_register(1 as RegisterSize);

    if let LpcRef::String(s) = arg {
        let path = s.read().to_string();

        let master = load_master(context, &path).await?;

        debug_assert!(
            !master.flags.test(ObjectFlags::CLONE),
            "master cannot be a clone"
        );

        {
            if master.program.pragmas.no_clone() {
                return Err(context.runtime_error(format!(
                    "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                    master.program.filename
                )));
            }
        }

        let new_prog = master.program.clone();
        let new_clone = context.insert_clone(new_prog);

        debug_assert!(
            new_clone.flags.test(ObjectFlags::CLONE),
            "new_clone must be a clone"
        );

        // if the master is not initialized, we initialize the clone.
        let return_val = if !master.flags.test(ObjectFlags::INITIALIZED) {
            if context.chain_count() >= MAX_CLONE_CHAIN {
                return Err(context.runtime_error("infinite clone recursion detected"));
            }

            let new_context = context
                .task_context_builder()
                .process(new_clone)
                .chain_count(context.chain_count() + 1)
                .build()
                .unwrap();

            Task::<N>::initialize_process(new_context)
                .await?
                .context
                .process
        } else {
            new_clone
        };

        let v = Arc::downgrade(&return_val);
        let result = v.into_lpc_ref(context.memory());

        context.return_efun_result(result);
    } else {
        return Err(context.runtime_error(format!(
            "invalid argument passed to `clone_object`: {}",
            arg
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_utils::config::Config;
    use parking_lot::RwLock;

    use super::*;
    use crate::{
        assert_regex,
        interpreter::{
            call_outs::CallOuts,
            heap::Heap,
            lpc_ref::NULL,
            program::Program,
            task_context::{TaskContext, TaskContextBuilder},
            vm::{vm_op::VmOp, Vm},
        },
        test_support::{compile_prog, test_config},
    };

    fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);

        TaskContextBuilder::default()
            .config(config)
            .process(process)
            .memory(Heap::new(10))
            .call_outs(RwLock::new(CallOuts::new(tx.clone())))
            .tx(tx)
            .build()
            .unwrap()
    }

    #[tokio::test]
    async fn does_not_create_multiple_master_objects() {
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
    async fn initializes_clone_if_master_not_initialized() {
        let cloned = indoc! { r#"
            int i = 123;
        "# };

        let cloner = indoc! { r#"
            object foo = clone_object("cloned");
        "# };

        let vm = Vm::new(test_config());
        let cloned_proc = vm
            .process_create_from_code("cloned.c", cloned)
            .await
            .unwrap();

        assert_eq!(
            cloned_proc.global_variable_values().get("i").unwrap(),
            &NULL
        );
        assert!(!cloned_proc.flags.test(ObjectFlags::INITIALIZED));

        let cloner_proc = vm
            .process_initialize_from_code("cloner.c", cloner)
            .await
            .unwrap()
            .context
            .process;
        assert!(cloner_proc.flags.test(ObjectFlags::INITIALIZED));

        assert_eq!(
            cloned_proc.global_variable_values().get("i").unwrap(),
            &NULL
        );
        let LpcRef::Object(foo) = cloner_proc.global_variable_values().get("foo").unwrap().clone() else {
            panic!("foo is not an object");
        };

        let foo = foo.upgrade().unwrap();
        assert!(foo.flags.test(ObjectFlags::INITIALIZED));

        let foo_i = foo.global_variable_values().get("i").unwrap().clone();
        assert_eq!(foo_i, LpcRef::from(123));
    }

    #[tokio::test]
    async fn handles_clone_self_recursion() {
        // This tests the case where an uninitialized master is cloned, (which
        // initializes the clone), and that clone then clones itself. This is
        // an infinite loop because if the master is _not_ initialized, then
        // the clone _will_ be initialized, and then clone itself, and so on.

        // This object will be initialized as a master, and so clones of it will
        // _not_ be initialized. It's not self-cloning, as it has a different path
        // than "self_clone".
        let master = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        // This is the self-cloning object, which has a different path than the master,
        // (even though the code is the same, which is just a coincidence). It will
        // be cloned by the master above, _without_ its master being initialized,
        // meaning it _will_ be initialized, and start a clone chain.
        let self_clone = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        let vm = Vm::new(test_config());
        let _self_clone_proc = vm
            .process_create_from_code("self_clone.c", self_clone)
            .await
            .unwrap();

        let master_proc = vm.process_initialize_from_code("master.c", master).await;

        assert!(master_proc
            .unwrap_err()
            .to_string()
            .contains("infinite clone recursion detected"));
    }
}
