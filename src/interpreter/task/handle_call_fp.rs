//! Calling a function pointer: the sync door resolves a pointer into a
//! resident object and pushes its frame inside `step`; the slow door loads
//! and initializes.

use std::{borrow::Cow, sync::Arc};

use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{RegisterSize, register::RegisterVariant};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::instrument;

use crate::interpreter::{
    call_frame::CallFrame,
    efun::Efun,
    function_type::{
        function_address::FunctionAddress,
        function_ptr::{FunctionPtr, ResolvedCall},
    },
    lpc_ref::{LpcRef, NULL},
    process::{Liveness, Process},
    task::{
        Task, advance::Advance, eval_loop::AsyncCall, get_location, handle_call::check_arg_type,
        handle_call_other::Standing,
    },
};

/// The values a pointer call passes, before the pointer's partial
/// application binds on top of them.
pub(crate) enum Passed<'a> {
    /// The calling instruction's list, read from the caller frame.
    List(ArgList),
    /// Values in hand.
    Values(&'a [LpcRef]),
}

/// How the pointer door left the call.
pub(crate) enum Called {
    /// The callee's frame is on top (an efun's is its entry frame); its
    /// `Ret` delivers the answer.
    Framed,
    /// The pointer names no function: the answer is 0.
    Unresolved,
    /// Loading, initializing, or an efun that suspends: the slow door's.
    Suspends,
    /// The top frame holds a suspended pending call: the efun ran in its
    /// entry frame and asked for a callback that needs the async arm.
    Pending,
}

/// The error a pointer whose receiver was destructed gives.
fn destructed_receiver(ptr: &FunctionPtr) -> LpcError {
    LpcError::runtime(format!(
        "attempted to call a pointer to a function in a destructed object: {ptr}"
    ))
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// The `CallFp` instruction with no future built, or the call it must
    /// await.
    pub(super) fn call_fp_local(
        &mut self,
        location: RegisterVariant,
        list: ArgList,
    ) -> Result<Option<AsyncCall>> {
        if self
            .args_of(list)?
            .iter()
            .any(|arg| matches!(arg, Arg::Ref(_)))
        {
            return Ok(Some(AsyncCall::FunctionPointer(location, list)));
        }
        let ptr = match &*get_location(&self.stack, &self.context.txn, location)? {
            LpcRef::Function(ptr) => ptr.clone(),
            _ => return Ok(Some(AsyncCall::FunctionPointer(location, list))),
        };
        match self.call_pointer_now(&ptr, Passed::List(list)) {
            Ok(Called::Framed) => Ok(None),
            Ok(Called::Unresolved) => {
                self.stack.current_frame_mut()?.registers[0] = NULL;
                Ok(None)
            }
            Ok(Called::Suspends) => Ok(Some(AsyncCall::FunctionPointer(location, list))),
            Ok(Called::Pending) => Ok(Some(AsyncCall::Pending)),
            Err(e) => Err(e.or_span(self.stack.current_frame()?.current_debug_span())),
        }
    }

    /// Call `ptr` with `passed` where no loading is needed: a frame pushed
    /// with the arguments bound in place, an efun run in its entry frame.
    pub(crate) fn call_pointer_now(
        &mut self,
        ptr: &Arc<FunctionPtr>,
        passed: Passed<'_>,
    ) -> Result<Called> {
        match &ptr.address {
            FunctionAddress::Local(receiver, function) => {
                let Some(process) = receiver.upgrade() else {
                    return Err(destructed_receiver(ptr));
                };
                match process.liveness(&self.context.txn) {
                    Liveness::Dead => Err(destructed_receiver(ptr)),
                    Liveness::Uninitialized => Ok(Called::Suspends),
                    Liveness::Ready => {
                        self.push_pointer_frame(process, function.clone(), ptr, passed)?;
                        Ok(Called::Framed)
                    }
                }
            }
            FunctionAddress::SimulEfun(name) => {
                let Some(simul_efuns) = self.context.simul_efuns().cloned() else {
                    return Err(LpcError::runtime(format!(
                        "call to simul efun `{name}`: no simul-efun object is loaded"
                    )));
                };
                let Some(function) = simul_efuns.program.lookup_function(name).cloned() else {
                    return Err(LpcError::runtime(format!(
                        "call to unknown simul efun `{name}`"
                    )));
                };
                self.push_pointer_frame(simul_efuns, function, ptr, passed)?;
                Ok(Called::Framed)
            }
            FunctionAddress::Efun(name) => {
                let Some(owner) = ptr
                    .owner
                    .upgrade()
                    .filter(|owner| owner.is_live(&self.context.txn))
                else {
                    return Err(LpcError::runtime(format!(
                        "attempted to call an efun pointer whose owner is destructed: {ptr}"
                    )));
                };
                let Some(efun) = Efun::from_name(name) else {
                    return Err(
                        self.runtime_bug(format!("`{name}` is typed efun but has no table row"))
                    );
                };
                if efun.suspends() {
                    return Ok(Called::Suspends);
                }
                let args = ptr.bound_args(&self.passed_values(passed)?);
                self.refuse_ref_params(efun)?;
                self.push_entry_frame(owner.clone(), ptr.origin.clone())?;
                match self.call_fired_efun_now(efun, args, owner, ptr.origin.clone())? {
                    Advance::Running => Ok(Called::Framed),
                    Advance::Suspends => Ok(Called::Pending),
                }
            }
            FunctionAddress::Dynamic(name) => {
                let mut args = ptr.bound_args(&self.passed_values(passed)?);
                let receiver = if args.is_empty() {
                    NULL
                } else {
                    args.remove(0)
                };
                let process = match &receiver {
                    LpcRef::Object(_) | LpcRef::String(_) => {
                        match Self::standing(&receiver, &self.context)? {
                            Standing::Ready(process) => process,
                            Standing::Dead => {
                                return Err(LpcError::runtime(format!(
                                    "attempted to call `{name}` on a destructed object"
                                )));
                            }
                            Standing::Removed(path) => {
                                return Err(LpcError::runtime(format!(
                                    "attempted to call `{name}` on a destructed object `{path}`"
                                )));
                            }
                            Standing::Uncreated(_) | Standing::Uninitialized(_) => {
                                return Ok(Called::Suspends);
                            }
                        }
                    }
                    _ => {
                        return Err(LpcError::runtime(format!(
                            "`&->{name}()` needs an object or path as its receiver, got `{receiver}`"
                        )));
                    }
                };
                let Some(function) = process.program.lookup_function(name).cloned() else {
                    return Ok(Called::Unresolved);
                };
                if let Some(i) = function.prototype.first_ref_param() {
                    return Err(LpcError::runtime(format!(
                        "`{}` takes argument {} by reference; call it directly",
                        function.name(),
                        i + 1
                    )));
                }
                let prototype = &function.prototype;
                for (i, arg) in args.iter().enumerate() {
                    check_arg_type(
                        &self.context.txn,
                        arg,
                        prototype.arg_types.get(i),
                        prototype.arg_spans.get(i),
                        &prototype.name,
                    )?;
                }
                self.push_external_frame(process, function, args.into_iter(), ptr.origin.clone())?;
                Ok(Called::Framed)
            }
        }
    }

    /// `passed` as owned values.
    fn passed_values(&self, passed: Passed<'_>) -> Result<Vec<LpcRef>> {
        match passed {
            Passed::Values(values) => Ok(values.to_vec()),
            Passed::List(list) => {
                self.args_of(list)?
                    .iter()
                    .map(|arg| match *arg {
                        Arg::Value(loc) => {
                            get_location(&self.stack, &self.context.txn, loc).map(Cow::into_owned)
                        }
                        Arg::Ref(_) => Err(self.runtime_bug(
                            "a by-reference argument reached a function pointer call",
                        )),
                    })
                    .collect()
            }
        }
    }

    /// Push `function`'s frame on `process` with `ptr`'s binding of `passed`
    /// stored in place; a refused argument pops it and reports at the caller.
    fn push_pointer_frame(
        &mut self,
        process: Arc<Process>,
        function: Arc<ProgramFunction>,
        ptr: &FunctionPtr,
        passed: Passed<'_>,
    ) -> Result<()> {
        if let Some(i) = function.prototype.first_ref_param() {
            return Err(LpcError::runtime(format!(
                "`{}` takes argument {} by reference; call it directly",
                function.name(),
                i + 1
            )));
        }
        let passed_len = match passed {
            Passed::List(list) => self.args_of(list)?.len(),
            Passed::Values(values) => values.len(),
        };
        let num_args = RegisterSize::try_from(ptr.bound_len(passed_len))?;
        self.stack.push_new(
            process,
            function.clone(),
            num_args,
            num_args,
            Some(ptr.upvalue_ptrs.clone()),
        )?;
        if let Err(e) = self.bind_pointer_args(ptr, passed, &function) {
            let depth = self.stack.len() - 1;
            self.stack.truncate(depth);
            let caller_span = self
                .stack
                .current_frame()
                .ok()
                .and_then(CallFrame::current_debug_span);
            return Err(e.or_span(caller_span));
        }
        Ok(())
    }

    /// Store the bound arguments in the callee frame on top, each checked
    /// against `function`'s parameter type.
    fn bind_pointer_args(
        &mut self,
        ptr: &FunctionPtr,
        passed: Passed<'_>,
        function: &ProgramFunction,
    ) -> Result<()> {
        let txn = &self.context.txn;
        let (callee, below) = self.stack.split_last_mut()?;
        let Some(caller) = below.last() else {
            return Err(
                callee.runtime_bug("a pointer call with no frame to read its arguments from")
            );
        };
        let prototype = &function.prototype;
        let mut store = |i: usize, value: LpcRef| {
            check_arg_type(
                txn,
                &value,
                prototype.arg_types.get(i),
                prototype.arg_spans.get(i),
                &prototype.name,
            )?;
            callee.push_arg(txn, i, value)
        };
        match passed {
            Passed::List(list) => ptr.each_bound(
                caller.function.args(list).iter().map(|arg| match *arg {
                    Arg::Value(loc) => caller.get_location(txn, loc).map(Cow::into_owned),
                    Arg::Ref(_) => Err(LpcError::runtime_bug(
                        "a by-reference argument reached a function pointer call",
                    )),
                }),
                &mut store,
            )?,
            Passed::Values(values) => ptr.each_bound(values.iter().cloned().map(Ok), &mut store)?,
        }
        callee.origin = ptr.origin.clone();
        callee.external = true;
        Ok(())
    }

    /// Call `ptr` with `passed`, loading or initializing the receiver and
    /// running a suspending efun; never `Suspends`.
    pub(crate) async fn call_pointer_slow(
        &mut self,
        ptr: &Arc<FunctionPtr>,
        passed: Vec<LpcRef>,
    ) -> Result<Called> {
        let ResolvedCall {
            process,
            function,
            args,
        } = match ptr
            .prepare_call(&passed, &self.context, || self.chain().map(Some))
            .await
        {
            Ok(Some(prepared)) => prepared,
            Ok(None) => return Ok(Called::Unresolved),
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
            self.refuse_ref_params(efun)?;
            self.push_entry_frame(process.clone(), ptr.origin.clone())?;
            self.call_fired_efun(efun, args, process, ptr.origin.clone())
                .await?;
            return Ok(Called::Framed);
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
        Ok(Called::Framed)
    }

    /// The `CallFp` instruction's async arm.
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
        let passed = self.passed_values(Passed::List(list))?;
        if let Called::Unresolved = self.call_pointer_slow(&ptr, passed).await? {
            self.stack.current_frame_mut()?.registers[0] = NULL;
        }
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

    const OTHER: &str = indoc! { r#"
        int two() { return 2; }
        int three() { return 3; }
    "# };

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
    async fn a_plain_efun_pointer_runs_in_an_entry_frame_with_no_await() {
        let code = indoc! { r#"
            int got;
            void create() { function f = &intp(); got = f(1); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        let top = task.stack.current_frame().unwrap();
        assert!(top.is_entry());
        assert_eq!(top.registers[0], LpcRef::from(1));
    }

    #[tokio::test]
    async fn an_efun_pointer_that_suspends_awaits() {
        let code = indoc! { r#"
            mixed got;
            void create() { function f = &find_object(); got = f("/nowhere"); }
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
    async fn a_dynamic_pointer_on_a_resident_object_is_called_with_no_await() {
        let code = indoc! { r#"
            int one(int x) { return x; }
            int got;
            void create() { object ob = this_object(); function f = &->one(); got = f(ob, 3); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        let frame = task.stack.current_frame().unwrap();
        assert_eq!(frame.function.name(), "one");
        assert!(frame.external);
        assert_eq!(frame.registers[1], LpcRef::from(3));
    }

    #[tokio::test]
    async fn a_dynamic_pointer_to_an_unloaded_path_awaits() {
        let code = indoc! { r#"
            int got;
            void create() { function f = &->two(); got = f("/other", 1); }
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

    #[tokio::test]
    async fn a_by_reference_efun_pointer_fails_at_the_call_site_with_no_frame_left() {
        let code = indoc! { r#"
            mixed got;
            void create() { function f = &sscanf(); got = f("1", "%d", 0); }
        "# };
        let (mut task, _live) = task_at_first_call_fp(code).await;

        let Err(err) = task.run_slice(&mut 1) else {
            panic!("the call is refused");
        };

        let msg = err.to_string();
        assert!(msg.contains("sscanf"), "{msg}");
        assert!(msg.contains("must be passed by reference"), "{msg}");
        assert_eq!(task.stack.len(), 1);
        assert_eq!(
            err.span(),
            task.stack.current_frame().unwrap().current_debug_span()
        );
    }

    #[tokio::test]
    async fn a_dynamic_pointer_to_a_destructed_string_receiver_names_the_path() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/other.c", OTHER)
            .await
            .unwrap();
        let code = indoc! { r#"
            mixed got;
            void create() {
                function f = &->two();
                destruct(find_object("/other"));
                got = f("/other");
            }
        "# };

        let err = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap_err();

        assert_eq!(
            err.to_string(),
            "runtime error: attempted to call `two` on a destructed object `/other`"
        );
    }
}
