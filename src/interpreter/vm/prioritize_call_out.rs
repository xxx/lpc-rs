use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};
use tokio::task::JoinHandle;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        function_type::function_ptr::FunctionPtr, lpc_ref::LpcRef, task::Task,
        vm::global_state::GlobalState,
    },
};

impl GlobalState {
    /// Handler for [`VmOp::PrioritizeCallOut`](crate::interpreter::vm::vm_op::VmOp::PrioritizeCallOut):
    /// run the call out with `id`, emitting any error's diagnostics.
    pub async fn prioritize_call_out(self: &Arc<Self>, id: u64) -> JoinHandle<()> {
        let global_state = self.clone();

        tokio::spawn(async move {
            let pair =
                global_state.with_call_outs(|co| -> Option<Result<(Arc<FunctionPtr>, bool)>> {
                    let call_out = co.get_by_id(id)?;
                    Some(if let LpcRef::Function(ref func) = call_out.func_ref {
                        Ok((func.clone(), call_out.is_repeating()))
                    } else {
                        Err(lpc_error!("invalid function sent to `call_out`"))
                    })
                });
            let Some(pair) = pair else {
                return;
            };

            let (ptr_arc, repeating) = match pair {
                Ok(pair) => pair,
                Err(e) => {
                    global_state.with_call_outs_mut(|co| co.remove_by_id(id));
                    e.emit_diagnostics();
                    return;
                }
            };

            let prepared = match global_state.prepare_function_ptr(&ptr_arc, &[], None).await {
                Ok(Some(prepared)) => prepared,
                Ok(None) => return,
                Err(e) => {
                    global_state.with_call_outs_mut(|co| co.remove_by_id(id));
                    e.emit_diagnostics();
                    return;
                }
            };

            global_state.with_call_outs_mut(|co| {
                if repeating {
                    co.get_mut_by_id(id).unwrap().refresh();
                } else {
                    co.remove_by_id(id);
                }
            });

            let max_execution_time = global_state.config.max_execution_time;
            let mut task = Task::<MAX_CALL_STACK_SIZE>::new(prepared.context);

            if let Err(e) = task
                .timed_eval(prepared.function, &prepared.args, max_execution_time)
                .await
            {
                e.with_stack_trace(task.stack.stack_trace())
                    .emit_diagnostics();
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
            process::Process,
            vm::Vm,
        },
        test_support::test_config,
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

        vm.global_state.with_call_outs_mut(|co| co.push(call_out));

        let handle = vm.global_state.prioritize_call_out(0).await;
        handle.await.unwrap();

        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(165)
        );
        vm.global_state
            .with_call_outs(|co| assert!(co.get_by_id(0).is_none()));
    }

    mod test_string_receivers {
        use super::*;
        async fn check(vm: &Vm, bar_proc: &Arc<Process>) {
            let ptr = FunctionPtrBuilder::default()
                .owner(Arc::downgrade(bar_proc))
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

            vm.global_state.with_call_outs_mut(|co| co.push(call_out));

            let handle = vm.global_state.prioritize_call_out(0).await;
            handle.await.unwrap();

            assert_eq!(
                vm.global_state.committed_global(bar_proc, 0u16),
                LpcRef::from(165)
            );
            assert!(vm.global_state.is_initialized(bar_proc));
            vm.global_state.with_call_outs(|co| {
                assert!(co.get_by_id(0).is_none());
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

    mod upvalues {
        use super::*;
        use crate::test_support::run_prog_with_vm_rx;

        async fn fire_the_one_call_out(code: &str) -> LpcRef {
            let (task, _rx) = run_prog_with_vm_rx(code).await;
            let gs = task.context.global_state.clone();
            let proc = task.context.process.clone();
            let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
            gs.prioritize_call_out(id).await.await.unwrap();
            gs.committed_global(&proc, 0u16)
        }

        #[tokio::test]
        async fn a_closure_fires_with_its_captures() {
            let code = r##"
                int result;
                void create() { int j = 5; call_out((: result = j + 1 :), 100); }
            "##;
            assert_eq!(fire_the_one_call_out(code).await, LpcRef::from(6));
        }

        #[tokio::test]
        async fn a_bound_dynamic_receiver_fires() {
            let code = r##"
                int result;
                void create() { call_out(papplyv(&->tick(1), ({ this_object() })), 100); }
                void tick(int x) { result = x + 5; }
            "##;
            assert_eq!(fire_the_one_call_out(code).await, LpcRef::from(6));
        }

        #[tokio::test]
        async fn a_static_function_fires_without_the_creators_captures() {
            let code = r##"
                int result;
                function g;
                void foo() { int k = 7; function h = (: k :); result = g(); }
                void create() { int j = 5; g = (: j :); call_out(&foo(), 100); }
            "##;
            assert_eq!(fire_the_one_call_out(code).await, LpcRef::from(5));
        }
    }
}
