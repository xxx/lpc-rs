use if_chain::if_chain;
use lpc_rs_errors::LpcError;
use tokio::task::JoinHandle;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        function_type::function_ptr::FunctionPtr,
        lpc_ref::LpcRef,
        task::{task_id::TaskId, Task},
        task_context::TaskContext,
        vm::{vm_op::VmOp, Vm},
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
        let call_outs = self.call_outs.clone();
        let config = self.config.clone();
        let object_space = self.object_space.clone();
        let memory = self.memory.clone();
        let upvalues = self.upvalues.clone();
        let tx = self.tx.clone();

        tokio::spawn(async move {
            if call_outs.read().get(idx).is_none() {
                return;
            }

            let repeating: bool;
            let pair = {
                if_chain! {
                    let b = call_outs.read();
                    let call_out = b.get(idx).unwrap();
                    if let LpcRef::Function(ref func) = call_out.func_ref;
                    then {
                        repeating = call_out.is_repeating();
                        Ok((func.clone(), repeating))
                    } else {
                        Err(LpcError::new("invalid function sent to `call_out`"))
                    }
                }
            };

            let Ok((ptr_arc, repeating)) = pair else {
                call_outs.write().remove(idx);
                let _ = tx.send(VmOp::TaskError(TaskId(0), pair.unwrap_err())).await;
                return;
            };

            let triple = FunctionPtr::triple(&ptr_arc, &config, &object_space);
            let Ok((process, function, args)) = triple else {
                call_outs.write().remove(idx);
                let _ = tx.send(VmOp::TaskError(TaskId(0), triple.unwrap_err())).await;
                return;
            };

            {
                let mut call_outs = call_outs.write();
                if repeating {
                    call_outs.get_mut(idx).unwrap().refresh();
                } else {
                    call_outs.remove(idx);
                }
            }

            let task_context = TaskContext::new(
                config,
                process,
                object_space,
                memory,
                upvalues,
                call_outs,
                None,
                Some(&ptr_arc.read().upvalue_ptrs).cloned(),
                tx.clone(),
            );

            let mut task = Task::<MAX_CALL_STACK_SIZE>::new(task_context);
            let id = task.id;

            if let Err(e) = task.timed_eval(function, &args).await {
                let _ = tx
                    .send(VmOp::TaskError(
                        id,
                        e.with_stack_trace(task.stack.stack_trace()),
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

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOutBuilder,
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            into_lpc_ref::IntoLpcRef,
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

        let mut vm = Vm::new(test_config());

        let prog = vm
            .with_async_compiler(|compiler| async move {
                compiler.compile_string("/foo/bar.c", code).await
            })
            .await
            .unwrap();

        let func = prog.lookup_function("foo").unwrap().clone();
        let proc = vm.create_and_initialize_task(prog).await.unwrap().process;
        let ptr = FunctionPtrBuilder::default()
            .address(FunctionAddress::Local(Arc::downgrade(&proc), func.clone()))
            .build()
            .unwrap();

        let call_out = CallOutBuilder::default()
            .process(Arc::downgrade(&proc))
            .func_ref(ptr.into_lpc_ref(&vm.memory))
            ._handle(tokio::spawn(async {}))
            .build()
            .unwrap();

        let idx = vm.call_outs.write().push(call_out);

        let handle = vm.prioritize_call_out(idx).await;
        handle.await.unwrap();

        assert_eq!(proc.globals.read().get(0).unwrap(), &LpcRef::from(165));
        assert!(vm.call_outs.read().get(idx).is_none());
    }
}
