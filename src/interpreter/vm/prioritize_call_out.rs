use std::sync::Arc;

use if_chain::if_chain;
use lpc_rs_errors::LpcError;
use tokio::task::JoinHandle;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        efun::EFUN_FUNCTIONS,
        function_type::function_address::FunctionAddress,
        lpc_ref::{LpcRef, NULL},
        process::Process,
        task::{task_id::TaskId, Task},
        task_context::TaskContext,
        vm::{vm_op::VmOp, Vm},
    },
    util::get_simul_efuns,
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

            let Ok((ptr_lock, repeating)) = pair else {
                call_outs.write().remove(idx);
                let _ = tx.send(VmOp::TaskError(TaskId(0), pair.unwrap_err())).await;
                return;
            };

            let triple = {
                let ptr = ptr_lock.read();

                // call outs don't get any additional args passed to them, so just set up the partial args.
                // use int 0 for any that were not applied at the time the pointer was created
                // TODO: error instead of int 0?
                let args = ptr
                    .partial_args
                    .iter()
                    .map(|arg| match arg {
                        Some(lpc_ref) => lpc_ref.clone(),
                        None => NULL,
                    })
                    .collect::<Vec<_>>();

                match ptr.address {
                    FunctionAddress::Local(ref proc, ref function) => {
                        if let Some(proc) = proc.upgrade() {
                            Ok((proc, function.clone(), args))
                        } else {
                            Err(LpcError::new(
                                "attempted to prioritize a function pointer to a dead process",
                            ))
                        }
                    }
                    FunctionAddress::Dynamic(_) => Err(LpcError::new(
                        "attempted to prioritize a dynamic receiver passed to call_out",
                    )),
                    FunctionAddress::SimulEfun(name) => {
                        match get_simul_efuns(&config, &object_space) {
                            Some(simul_efuns) => match simul_efuns.program.lookup_function(name) {
                                Some(function) => Ok((simul_efuns.clone(), function.clone(), args)),
                                None => Err(LpcError::new(format!(
                                    "call to unknown simul_efun `{name}`"
                                ))),
                            },
                            None => Err(LpcError::new(
                                "function pointer to simul_efun passed, but no simul_efuns?",
                            )),
                        }
                    }
                    FunctionAddress::Efun(name) => {
                        let pf = EFUN_FUNCTIONS.get(name.as_str()).unwrap();

                        Ok((Arc::new(Process::default()), pf.clone(), args))
                    }
                }
            };

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
    use indoc::indoc;

    use super::*;
    use crate::{
        interpreter::{
            call_outs::CallOutBuilder, function_type::function_ptr::FunctionPtrBuilder,
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
            .with_compiler(|compiler| compiler.compile_string("/foo/bar.c", code))
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
