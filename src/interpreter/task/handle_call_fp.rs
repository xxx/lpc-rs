use std::borrow::Cow;

use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{RegisterSize, register::RegisterVariant};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::instrument;

use crate::interpreter::{
    call_frame::CallFrame,
    function_type::{function_address::FunctionAddress, function_ptr::ResolvedCall},
    lpc_ref::{LpcRef, NULL},
    process::Liveness,
    task::{Task, get_location, handle_call::check_arg_type},
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// A pointer to a resident, initialized object's own function is called
    /// with no future built: its frame is pushed with the bound arguments
    /// stored in place. `false` leaves every other pointer to `handle_call_fp`.
    pub(crate) fn call_fp_local(
        &mut self,
        location: RegisterVariant,
        list: ArgList,
    ) -> Result<bool> {
        let passed = self.args_of(list)?;
        if passed.iter().any(|arg| matches!(arg, Arg::Ref(_))) {
            return Ok(false);
        }
        let passed = passed.len();
        let (process, function, upvalue_ptrs, num_args) = {
            let ptr_ref = get_location(&self.stack, &self.context.txn, location)?;
            let LpcRef::Function(ptr) = &*ptr_ref else {
                return Ok(false);
            };
            let FunctionAddress::Local(receiver, function) = &ptr.address else {
                return Ok(false);
            };
            let Some(process) = receiver.upgrade() else {
                return Ok(false);
            };
            if !matches!(process.liveness(&self.context.txn), Liveness::Ready)
                || function.prototype.is_efun()
                || function.prototype.first_ref_param().is_some()
            {
                return Ok(false);
            }
            (
                process,
                function.clone(),
                ptr.upvalue_ptrs.clone(),
                ptr.bound_len(passed),
            )
        };
        let num_args = RegisterSize::try_from(num_args)?;
        self.stack.push_new(
            process,
            function.clone(),
            num_args,
            num_args,
            Some(upvalue_ptrs),
        )?;
        if let Err(e) = self.bind_pointer_args(location, list, &function) {
            let depth = self.stack.len() - 1;
            self.stack.truncate(depth);
            let caller_span = self
                .stack
                .current_frame()
                .ok()
                .and_then(CallFrame::current_debug_span);
            return Err(e.or_span(caller_span));
        }
        Ok(true)
    }

    /// Store the call's arguments in the callee frame on top: the pointer in
    /// the caller's `location` binds the values the caller's `list` names,
    /// each checked against `function`'s parameter type.
    fn bind_pointer_args(
        &mut self,
        location: RegisterVariant,
        list: ArgList,
        function: &ProgramFunction,
    ) -> Result<()> {
        let txn = &self.context.txn;
        let (callee, below) = self.stack.split_last_mut()?;
        let Some(caller) = below.last() else {
            return Err(
                callee.runtime_bug("a pointer call with no frame to read its arguments from")
            );
        };
        let ptr_ref = caller.get_location(txn, location)?;
        let LpcRef::Function(ptr) = &*ptr_ref else {
            return Err(callee.runtime_bug("the pointer left its register"));
        };
        let passed = caller.function.args(list).iter().map(|arg| match *arg {
            Arg::Value(loc) => caller.get_location(txn, loc).map(Cow::into_owned),
            Arg::Ref(_) => Err(LpcError::runtime_bug(
                "a by-reference argument reached a function pointer call",
            )),
        });
        let prototype = &function.prototype;
        ptr.each_bound(passed, |i, value| {
            check_arg_type(
                txn,
                &value,
                prototype.arg_types.get(i),
                prototype.arg_spans.get(i),
                &prototype.name,
            )?;
            callee.push_arg(txn, i, value)
        })?;
        callee.origin = ptr.origin.clone();
        callee.external = true;
        Ok(())
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) async fn handle_call_fp(
        &mut self,
        location: RegisterVariant,
        list: ArgList,
    ) -> Result<()> {
        let ptr = {
            let lpc_ref = &*get_location(&self.stack, &self.context.txn, location)?;
            let LpcRef::Function(ptr) = lpc_ref else {
                return Err(
                    self.runtime_error(format!("callfp instruction on non-function: {}", lpc_ref))
                );
            };
            ptr.clone()
        };

        let passed = self
            .args_of(list)?
            .iter()
            .map(|arg| match *arg {
                Arg::Value(loc) => {
                    get_location(&self.stack, &self.context.txn, loc).map(Cow::into_owned)
                }
                Arg::Ref(_) => {
                    Err(self.runtime_bug("a by-reference argument reached a function pointer call"))
                }
            })
            .collect::<Result<Vec<_>>>()?;

        let ResolvedCall {
            process,
            function,
            args,
        } = match ptr
            .prepare_call(&passed, &self.context, || self.chain().map(Some))
            .await
        {
            Ok(Some(prepared)) => prepared,
            Ok(None) => {
                self.stack.current_frame_mut()?.registers[0] = NULL;
                return Ok(());
            }
            Err(e) => return Err(e.or_span(self.stack.current_frame()?.current_debug_span())),
        };

        if !process.is_initialized(&self.context.txn) {
            let callers = Some(self.chain()?);
            Self::initialize_process(self.context.nested(callers, process.clone())?).await?;
        }

        let prototype = &function.prototype;
        for (i, arg) in args.iter().enumerate() {
            self.type_check_call_arg(
                arg,
                prototype.arg_types.get(i),
                prototype.arg_spans.get(i),
                &prototype.name,
            )?;
        }

        if function.prototype.is_efun() {
            let efun = self.efun_of(&function)?;
            return self
                .call_fired_efun(efun, args, process, ptr.origin.clone())
                .await;
        }

        let mut new_frame = CallFrame::new(
            process,
            function.clone(),
            RegisterSize::try_from(args.len())?,
            Some(ptr.upvalue_ptrs.clone()),
        );
        for (i, arg) in args.into_iter().enumerate() {
            new_frame.push_arg(&self.context.txn, i, arg)?;
        }
        new_frame.origin = ptr.origin.clone();
        new_frame.external = true;

        self.stack.push(new_frame)?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_asm::instruction::Instruction;
    use thin_vec::ThinVec;

    use super::super::eval_loop::{AsyncCall, Slice};
    use crate::{
        compile_time_config::MAX_CALL_STACK_SIZE,
        interpreter::{
            call_frame::CallFrame,
            lpc_ref::LpcRef,
            stm::{LiveSnapshot, Transaction, TxnHandle, VarId, start_txn},
            task::{Task, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::test_config,
    };

    /// `code`'s `create`, stepped to its first pointer call under a live
    /// snapshot (an empty handle reads every receiver as absent).
    async fn task_at_first_call_fp(code: &str) -> (Task<MAX_CALL_STACK_SIZE>, LiveSnapshot) {
        let vm = Vm::new(test_config());
        let process = vm.create_process_from_code("/main.c", code).await.unwrap();
        let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let mut context =
            TaskTemplate::from(vm.global_state.clone()).into_task_context(process.clone());
        context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        process.claim_init(&context.txn);
        let mut task = Task::new(context);
        let create = process.program.lookup_function("create").unwrap().clone();
        let frame = CallFrame::new(process, create, 0, None::<ThinVec<VarId>>);
        task.stack.push(frame).unwrap();
        for _ in 0..32 {
            let at = task.stack.current_frame().unwrap().instruction();
            if matches!(at, Some(Instruction::CallFp(..))) {
                return (task, live);
            }
            task.run_slice(&mut 1).unwrap();
        }
        panic!("no pointer call in {code}");
    }

    #[tokio::test]
    async fn a_pointer_to_a_resident_function_is_called_with_no_await() {
        let code = indoc! { r#"
            int one(int x) { return x; }
            int got;
            void create() { function f = &one(); got = f(7); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        let frame = task.stack.current_frame().unwrap();
        assert_eq!(frame.function.name(), "one");
        assert_eq!(frame.registers[1], LpcRef::from(7));
        assert!(frame.external);
    }

    #[tokio::test]
    async fn a_partial_application_binds_its_holes_in_the_frame() {
        let code = indoc! { r#"
            int add(int a, int b, int c) { return a + b + c; }
            int got;
            void create() { function f = &add(, 20, ); got = f(1, 300); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        let frame = task.stack.current_frame().unwrap();
        assert_eq!(frame.function.name(), "add");
        assert_eq!(frame.registers[1], LpcRef::from(1));
        assert_eq!(frame.registers[2], LpcRef::from(20));
        assert_eq!(frame.registers[3], LpcRef::from(300));
    }

    #[tokio::test]
    async fn a_closure_carries_its_captured_cells_with_no_await() {
        let code = indoc! { r#"
            int got;
            void create() { int k = 5; function f = (: $1 + k :); got = f(1); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        let frame = task.stack.current_frame().unwrap();
        assert_eq!(frame.upvalue_ptrs.len(), 1);
        assert_eq!(frame.registers[1], LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_efun_pointer_awaits() {
        let code = indoc! { r#"
            int got;
            void create() { function f = &intp(); got = f(1); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(
            slice,
            Slice::Await(AsyncCall::FunctionPointer(..))
        ));
        assert_eq!(task.stack.len(), 1);
    }

    #[tokio::test]
    async fn a_mismatched_argument_fails_at_the_call_site_with_no_frame_left() {
        let code = indoc! { r#"
            int one(int x) { return x; }
            int got;
            void create() { function f = &one(); mixed s = "s"; got = f(s); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let Err(err) = task.run_slice(&mut 1) else {
            panic!("the call is refused");
        };

        assert_eq!(
            err.to_string(),
            "runtime error: unexpected argument type to `one`: string. expected int."
        );
        assert_eq!(task.stack.len(), 1);
        assert_eq!(
            err.span(),
            task.stack.current_frame().unwrap().current_debug_span()
        );
    }
}
