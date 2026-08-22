use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};
use tokio::task::JoinHandle;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        lpc_ref::LpcRef,
        object_flags::ObjectFlags,
        task::{
            Task, apply_function::apply_runtime_error, into_task_context::IntoTaskContext,
            task_id::TaskId, task_template::TaskTemplate,
        },
        task_context::TaskContext,
        vm::{Vm, vm_op::VmOp},
    },
};

impl Vm {
    /// Handler for [`VmOp::PrioritizeCallOut`].
    ///
    /// # Arguments
    ///
    /// * `idx` - The index of the call out to run
    ///
    /// Errors are communicated directly to the [`Vm`] via it's channel.
    pub async fn prioritize_call_out(&self, idx: usize) -> JoinHandle<()> {
        let global_state = self.global_state.clone();

        tokio::spawn(async move {
            if global_state.with_call_outs(|co| co.get(idx).is_none()) {
                return;
            }

            let pair = {
                global_state.with_call_outs(|co| -> Result<(Arc<FunctionPtr>, bool)> {
                    let call_out = co.get(idx).unwrap();
                    if let LpcRef::Function(ref func) = call_out.func_ref {
                        let repeating = call_out.is_repeating();
                        Ok((func.clone(), repeating))
                    } else {
                        Err(lpc_error!("invalid function sent to `call_out`"))
                    }
                })
            };

            let Ok((ptr_arc, repeating)) = pair else {
                global_state.with_call_outs_mut(|co| co.remove(idx));
                let _ = global_state
                    .tx
                    .send(VmOp::TaskError(TaskId(0), pair.unwrap_err()))
                    .await;
                return;
            };

            // The transactional seam: a create-on-miss goes through the
            // committer, and a destruct in the committed-unflushed window is an
            // error instead of a resurrection.
            if let Err(e) = global_state.resolve_dynamic_string_receiver(&ptr_arc).await {
                global_state.with_call_outs_mut(|co| co.remove(idx));
                let _ = global_state.tx.send(VmOp::TaskError(TaskId(0), e)).await;
                return;
            }
            let triple =
                FunctionPtr::triple(&ptr_arc, &global_state.config, &global_state.object_space)
                    .await;
            let Ok((process, function, args)) = triple else {
                global_state.with_call_outs_mut(|co| co.remove(idx));
                let _ = global_state
                    .tx
                    .send(VmOp::TaskError(TaskId(0), triple.unwrap_err()))
                    .await;
                return;
            };

            if !process.flags.test(ObjectFlags::Initialized) {
                let template = TaskTemplate::from(global_state.clone());

                let ctx = template.into_task_context(process.clone());
                if let Err(e) = Task::<MAX_CALL_STACK_SIZE>::initialize_process(ctx).await {
                    let template = TaskTemplate::from(global_state.clone());

                    let Some(Ok(_)) = apply_runtime_error(&e, Some(process), template).await else {
                        global_state.config.debug_log(e.diagnostic_string()).await;
                        return;
                    };

                    return;
                }
            }

            global_state.with_call_outs_mut(|co| {
                if repeating {
                    co.get_mut(idx).unwrap().refresh();
                } else {
                    co.remove(idx);
                }
            });

            let max_execution_time = global_state.config.max_execution_time;
            let task_context = TaskContext::new(
                global_state.clone(),
                process,
                None,
                Some(&ptr_arc.upvalue_ptrs).cloned(),
            );

            let mut task = Task::<MAX_CALL_STACK_SIZE>::new(task_context);
            let id = task.id;

            if let Err(e) = task.timed_eval(function, &args, max_execution_time).await {
                let _ = global_state
                    .tx
                    .send(VmOp::TaskError(
                        id,
                        Box::new(e.with_stack_trace(task.stack.stack_trace())),
                    ))
                    .await;
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use thin_vec::thin_vec;
    use ustr::ustr;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            call_outs::CallOutBuilder,
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            object_flags::ObjectFlags,
            process::Process,
        },
        test_support::test_config,
        util::process_builder::ProcessCreator,
    };

    #[tokio::test]
    async fn test_prioritize_call_out() {
        let code = indoc! { r#"
            int i = 123;

            void foo() {
                i += 42;
            }
        "# };

        let vm = Vm::new(test_config());

        let r = vm.initialize_process_from_code("/foo/bar.c", code).await;
        let proc = r.unwrap().context.process;
        let func = proc.program.lookup_function("foo").unwrap().clone();
        let ptr = FunctionPtrBuilder::default()
            .address(FunctionAddress::Local(Arc::downgrade(&proc), func.clone()))
            .build()
            .unwrap();

        let call_out = CallOutBuilder::default()
            .id(0)
            .process(Arc::downgrade(&proc))
            .func_ref(ptr.into())
            ._handle(tokio::spawn(async {}))
            .build()
            .unwrap();

        let idx = vm.global_state.with_call_outs_mut(|co| co.push(call_out));

        let handle = vm.prioritize_call_out(idx).await;
        handle.await.unwrap();

        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(165)
        );
        vm.global_state
            .with_call_outs(|co| assert!(co.get(idx).is_none()));
    }

    mod test_string_receivers {
        use super::*;
        async fn check(vm: &Vm, bar_proc: &Arc<Process>) {
            let ptr = FunctionPtrBuilder::default()
                .address(FunctionAddress::Dynamic(ustr("foo")))
                .partial_args(thin_vec![Some("/bar".into())])
                .build()
                .unwrap();

            let call_out = CallOutBuilder::default()
                .id(0)
                .process(Arc::downgrade(bar_proc))
                .func_ref(ptr.into())
                ._handle(tokio::spawn(async {}))
                .build()
                .unwrap();

            let idx = vm.global_state.with_call_outs_mut(|co| co.push(call_out));

            let handle = vm.prioritize_call_out(idx).await;
            handle.await.unwrap();

            assert_eq!(
                vm.global_state.committed_global(bar_proc, 0u16),
                LpcRef::from(165)
            );
            assert!(bar_proc.flags.test(ObjectFlags::Initialized));
            vm.global_state.with_call_outs(|co| {
                assert!(co.get(idx).is_none());
            });
        }

        #[tokio::test]
        async fn works_with_string_preinitialized_receivers() {
            let bar = indoc! { r#"
            int i = 123;
            void foo(string s) {
                i += 42;
            }
        "# };

            let vm = Vm::new(test_config());

            let r = vm.initialize_process_from_code("/bar.c", bar).await;
            let bar_proc = r.unwrap().context.process;

            check(&vm, &bar_proc).await;
        }

        #[tokio::test]
        async fn works_with_string_noninitialized_receivers() {
            let bar = indoc! { r#"
            int i = 123;
            void foo(string s) {
                i += 42;
            }
        "# };

            let vm = Vm::new(test_config());

            let bar_proc = vm.create_process_from_code("/bar.c", bar).await.unwrap();

            check(&vm, &bar_proc).await;
        }
    }
}
