use lpc_rs_core::null_mode::NullMode;
use std::sync::Arc;

use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{
    CREATE_FUNCTION, INIT_PROGRAM, RegisterSize, lpc_path::LpcPath, lpc_type::LpcType,
};
use lpc_rs_errors::{LpcError, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace};
use ustr::Ustr;

use crate::interpreter::program::Function;
use crate::interpreter::{
    call_frame::{CallFrame, FrameReceiver},
    call_origin::CallOrigin,
    efun::{Efun, call_efun, call_efun_sync, efun_context::EfunContext},
    lpc_ref::LpcRef,
    process::Process,
    stm::{TxnHandle, VarId},
    task::{Task, advance::Advance},
};

/// How a frame is entered: a direct or simul-efun call was compiled against
/// its callee's parameter list; a virtual call reached a redefinition of
/// the callee, and a door was compiled against nothing. An initializer
/// forwards constructor arguments whose types are known only at runtime.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum CallEntry {
    Direct,
    Virtual,
    SimulEfun,
    Door,
    Initializer,
}

impl CallEntry {
    /// Whether the frame counts as entered from outside its program.
    fn external(self) -> bool {
        !matches!(self, Self::Direct | Self::Virtual | Self::Initializer)
    }

    /// Whether the argument count is held to the callee's parameter list.
    /// A redefinition runs with the count its parent's call site was
    /// compiled for: missing arguments are 0, extra ones ignored (CD,
    /// FluffOS, DGD).
    fn counted(self) -> bool {
        matches!(self, Self::Direct | Self::SimulEfun)
    }
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// A plain call by name: the object's most-derived definition of the name.
    #[instrument(level = "debug", skip_all)]
    pub(crate) fn handle_call(&mut self, name: Ustr, list: ArgList) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        // Codegen emits `Call` only for a name of this program; a miss is a bug.
        let Some(target) = current_frame.image.program.target(name) else {
            return Err(self
                .stack
                .runtime_bug(format!("call to unknown local function `{name}`")));
        };
        let func = target.function.clone();
        let entry =
            if current_frame.function.name() == INIT_PROGRAM && func.name() == CREATE_FUNCTION {
                CallEntry::Initializer
            } else if target.overridden {
                CallEntry::Virtual
            } else {
                CallEntry::Direct
            };
        let receiver = current_frame.receiver.clone();

        self.push_receiver_frame(receiver, func, list, entry)
    }

    /// `::f()` or `name::f()`: the definition the mangled name itself names.
    #[instrument(level = "debug", skip_all)]
    pub(crate) fn handle_call_qualified(
        &mut self,
        name: Ustr,
        list: ArgList,
    ) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        let Some(func) = current_frame.image.program.function(name).cloned() else {
            return Err(self
                .stack
                .runtime_bug(format!("call to unknown local function `{name}`")));
        };
        let receiver = current_frame.receiver.clone();

        self.push_receiver_frame(receiver, func, list, CallEntry::Direct)
    }

    /// A resolved `call_inherited` with value arguments and local-call history.
    pub(super) fn push_inherited_frame(
        &mut self,
        receiver: Arc<FrameReceiver>,
        function: Function,
        args: impl ExactSizeIterator<Item = LpcRef>,
    ) -> lpc_rs_errors::Result<()> {
        if let Some(i) = function.prototype.first_ref_param() {
            return Err(self.runtime_error(format!(
                "call_inherited: `{}` takes argument {} by reference; call it directly",
                function.name(),
                i + 1
            )));
        }
        let num_args = self.checked_register_count(args.len(), &function)?;
        if !function.prototype.accepts_arg_count(usize::from(num_args)) {
            return Err(self.runtime_error(format!(
                "incorrect argument count in call to `{}`: expected: {}, received: {}",
                function.name(),
                function.arity().num_args,
                num_args
            )));
        }
        let mut frame =
            CallFrame::with_receiver(receiver, function, num_args, num_args, None::<&[VarId]>);
        frame.initialize_parameters(&self.context.txn);
        for (i, arg) in args.enumerate() {
            let prototype = &frame.function.prototype;
            check_arg_type(
                &self.context.txn,
                &arg,
                prototype.arg_types.get(i),
                prototype.arg_spans.get(i),
                &prototype.name,
            )?;
            frame.push_arg(&self.context.txn, i, arg)?;
        }
        self.stack.push(frame)
    }

    /// Push a frame for a call to `func` on `process`, its arguments read
    /// from the current frame's `list`.
    #[instrument(level = "debug", skip_all)]
    pub(crate) fn push_call_frame(
        &mut self,
        process: Arc<Process>,
        func: Function,
        list: ArgList,
        entry: CallEntry,
    ) -> lpc_rs_errors::Result<()> {
        let receiver = if entry.external() {
            FrameReceiver::new(process, &self.context.txn)
        } else {
            self.receiver_for(process)
        };
        self.push_receiver_frame(receiver, func, list, entry)
    }

    pub(super) fn receiver_for(&self, process: Arc<Process>) -> Arc<FrameReceiver> {
        self.stack
            .last()
            .filter(|frame| Arc::ptr_eq(&frame.process, &process))
            .map_or_else(
                || FrameReceiver::new(process, &self.context.txn),
                |frame| frame.receiver.clone(),
            )
    }

    #[inline]
    fn push_receiver_frame(
        &mut self,
        receiver: Arc<FrameReceiver>,
        func: Function,
        list: ArgList,
        entry: CallEntry,
    ) -> lpc_rs_errors::Result<()> {
        let num_args = self.checked_register_count(self.expanded_arg_count(list)?, &func)?;
        if entry == CallEntry::Initializer {
            for (i, value) in self.arg_values(list)?.iter().enumerate() {
                self.type_check_call_arg(
                    value,
                    func.prototype.arg_types.get(i),
                    func.prototype.arg_spans.get(i),
                    func.name(),
                )?;
            }
        }
        // A simul_efun's prototype can change after a cached caller was compiled against it.
        if num_args < func.arity().num_args
            && let Some(i) = func.prototype.first_ref_param()
            && i >= usize::from(num_args)
        {
            return Err(LpcError::runtime(format!(
                "argument {} of `{}` must be passed by reference",
                i + 1,
                func.name()
            ))
            .or_span(self.caller_span()));
        }
        // A spread's count is known only here.
        if entry.counted() && !func.prototype.accepts_arg_count(usize::from(num_args)) {
            return Err(LpcError::runtime(format!(
                "incorrect argument count in call to `{}`: expected: {}, received: {}",
                func.name(),
                func.arity().num_args,
                num_args
            ))
            .or_span(self.caller_span()));
        }

        let origin = if self.stack.current_frame()?.function.name() == INIT_PROGRAM {
            CallOrigin::Driver
        } else {
            match entry {
                CallEntry::Direct | CallEntry::Virtual => CallOrigin::Local,
                CallEntry::SimulEfun => CallOrigin::Simul,
                CallEntry::Door => CallOrigin::CallOther,
                CallEntry::Initializer => CallOrigin::Driver,
            }
        };
        trace!("pushing new frame; copying arguments: {num_args}");
        self.stack
            .push_new(receiver, func, num_args, num_args, None::<&[VarId]>)?;
        let frame = self.stack.current_frame_mut()?;
        frame.initialize_parameters(&self.context.txn);
        frame.external = entry.external();
        frame.origin = origin;
        if let Err(e) = self.populate_arguments(list) {
            // The half-built frame comes off; the error names the caller.
            let depth = self.stack.len() - 1;
            self.stack.truncate(depth);
            return Err(e.or_span(self.caller_span()));
        }
        Ok(())
    }

    /// The current frame's instruction span, for an error raised before a
    /// callee's frame exists.
    fn caller_span(&self) -> Option<Span> {
        self.stack
            .current_frame()
            .ok()
            .and_then(CallFrame::current_debug_span)
    }

    /// How many arguments `list` passes once its spreads are expanded.
    fn expanded_arg_count(&self, list: ArgList) -> lpc_rs_errors::Result<usize> {
        let args = self.args_of(list)?;
        if !args.iter().any(|arg| matches!(arg, Arg::Spread(_))) {
            return Ok(args.len());
        }
        let frame = self.stack.current_frame()?;
        let mut count = 0;
        for arg in args {
            count += match *arg {
                Arg::Spread(location) => frame.spread_len(&self.context.txn, location)?,
                Arg::Value(_) | Arg::Ref(_) => 1,
            };
        }
        Ok(count)
    }

    /// `count` as a [`RegisterSize`] for `function`'s frame, or a runtime
    /// error when it would not fit one alongside the function's locals and
    /// its `r0`.
    pub(crate) fn checked_register_count(
        &self,
        count: usize,
        function: &Function,
    ) -> lpc_rs_errors::Result<RegisterSize> {
        if !function.is_driver_code() && function.null_mode != self.context.config().null_mode {
            return Err(
                self.runtime_error("compiled function and VM use different NULL_MODE settings")
            );
        }
        let num_locals = usize::from(function.num_locals);
        if count + num_locals + 1 > usize::from(RegisterSize::MAX) {
            let limit = usize::from(RegisterSize::MAX).saturating_sub(num_locals + 1);
            return Err(self.runtime_error(format!(
                "cannot pass {count} arguments to `{}`: the limit is {limit}",
                function.name()
            )));
        }
        Ok(count as RegisterSize)
    }

    /// Copy the arguments the caller's `list` names into the frame above it.
    fn populate_arguments(&mut self, list: ArgList) -> lpc_rs_errors::Result<()> {
        let txn = &self.context.txn;
        let (callee, below) = self.stack.split_last_mut()?;
        let Some(caller) = below.last() else {
            return Err(callee.runtime_bug("a call with no frame to read its arguments from"));
        };
        let mut i = 0;
        for arg in caller.function.args(list) {
            match *arg {
                Arg::Value(loc) => {
                    let value = caller.get_location(txn, loc)?.into_owned();
                    callee.push_arg(txn, i, value)?;
                    i += 1;
                }
                Arg::Ref(loc) => {
                    let cell = caller.ref_cell(loc)?;
                    callee.push_ref(txn, i, cell)?;
                    i += 1;
                }
                Arg::Spread(loc) => {
                    for value in caller.spread_elements(txn, loc)? {
                        callee.push_arg(txn, i, value)?;
                        i += 1;
                    }
                }
            }
        }
        Ok(())
    }

    /// handle runtime type-checks for function pointer calls
    pub(crate) fn type_check_call_arg(
        &self,
        lpc_ref: &LpcRef,
        arg_type: Option<&LpcType>,
        arg_def_span: Option<&Span>,
        function_name: &str,
    ) -> lpc_rs_errors::Result<()> {
        check_arg_type(
            &self.context.txn,
            lpc_ref,
            arg_type,
            arg_def_span,
            function_name,
        )
        .map_err(|e| {
            e.with_span(
                self.stack
                    .current_frame()
                    .ok()
                    .and_then(CallFrame::current_debug_span),
            )
        })
    }

    /// After an efun: drive the callbacks it asked for, if any.
    fn settle_efun(&mut self) -> lpc_rs_errors::Result<Advance> {
        if self.stack.current_frame()?.pending.is_some() {
            self.advance_pending(false)
        } else {
            Ok(Advance::Running)
        }
    }

    /// `settle_efun` with the async arm for a step that suspends.
    async fn settle_efun_async(&mut self) -> lpc_rs_errors::Result<()> {
        if self.stack.current_frame()?.pending.is_some() {
            self.advance_pending_async(false).await
        } else {
            Ok(())
        }
    }

    /// Run `efun`, called by the top frame's instruction with `list`, in
    /// place; its result lands in that frame's register 0.
    pub(crate) fn call_efun_now(
        &mut self,
        efun: Efun,
        list: ArgList,
    ) -> lpc_rs_errors::Result<Advance> {
        let mut ctx = EfunContext::at_call(&mut self.stack, &self.context, efun, list)?;
        let result = match call_efun_sync(efun, &mut ctx) {
            Some(result) => result,
            None => Err(ctx.runtime_bug(format!("`{efun:?}` suspends but reached the sync door"))),
        };

        #[cfg(test)]
        if let Some(snap) = ctx.snapshot.take() {
            self.snapshots.push(snap);
        }

        ctx.finish(result)?;
        self.settle_efun()
    }

    /// Run `efun`, one that can suspend, called by the top frame's
    /// instruction with `list`.
    pub(crate) async fn prepare_and_call_efun(
        &mut self,
        efun: Efun,
        list: ArgList,
    ) -> lpc_rs_errors::Result<()> {
        let mut ctx = EfunContext::at_call(&mut self.stack, &self.context, efun, list)?;
        let result = call_efun(efun, &mut ctx).await;

        #[cfg(test)]
        if let Some(snap) = ctx.snapshot.take() {
            self.snapshots.push(snap);
        }

        ctx.finish(result)?;
        self.settle_efun_async().await
    }

    /// Push the frame an efun fired through a pointer runs in, as `owner`.
    pub(crate) fn push_entry_frame(
        &mut self,
        owner: Arc<Process>,
        origin: CallOrigin,
    ) -> lpc_rs_errors::Result<()> {
        let mut frame = CallFrame::entry(owner, &self.context.txn);
        frame.origin = origin;
        self.stack.push(frame)
    }

    /// The by-reference parameter no pointer call can satisfy, as the error;
    /// the same rule `FunctionPtr::prepare_call` applies to a loaded callee.
    pub(super) fn refuse_ref_params(&self, efun: Efun) -> lpc_rs_errors::Result<()> {
        let prototype = efun.prototype();
        match prototype.first_ref_param() {
            Some(i) => Err(self.runtime_error(format!(
                "argument {} of `{}` must be passed by reference",
                i + 1,
                prototype.name
            ))),
            None => Ok(()),
        }
    }

    /// Run `efun` fired through a pointer with `args`, as `owner`, written
    /// by `origin`, in the entry frame on top.
    pub(crate) async fn call_fired_efun(
        &mut self,
        efun: Efun,
        args: Vec<LpcRef>,
        owner: Arc<Process>,
        origin: Option<Arc<LpcPath>>,
    ) -> lpc_rs_errors::Result<()> {
        let mut ctx = EfunContext::fired(&mut self.stack, &self.context, efun, args, owner, origin);
        let result = call_efun(efun, &mut ctx).await;

        #[cfg(test)]
        if let Some(snap) = ctx.snapshot.take() {
            self.snapshots.push(snap);
        }

        ctx.finish(result)?;
        self.settle_efun_async().await
    }

    /// `call_fired_efun` for an efun that never suspends: no future built.
    pub(crate) fn call_fired_efun_now(
        &mut self,
        efun: Efun,
        args: Vec<LpcRef>,
        owner: Arc<Process>,
        origin: Option<Arc<LpcPath>>,
    ) -> lpc_rs_errors::Result<Advance> {
        let mut ctx = EfunContext::fired(&mut self.stack, &self.context, efun, args, owner, origin);
        let result = match call_efun_sync(efun, &mut ctx) {
            Some(result) => result,
            None => Err(ctx.runtime_bug(format!("`{efun:?}` suspends but reached the sync door"))),
        };

        #[cfg(test)]
        if let Some(snap) = ctx.snapshot.take() {
            self.snapshots.push(snap);
        }

        ctx.finish(result)?;
        self.settle_efun()
    }

    /// The efun a function with an efun prototype names; none is the
    /// driver's bug.
    pub(crate) fn efun_of(&self, function: &ProgramFunction) -> lpc_rs_errors::Result<Efun> {
        Efun::from_name(function.name()).ok_or_else(|| {
            self.runtime_bug(format!(
                "`{}` is typed efun but has no table row",
                function.name()
            ))
        })
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn handle_call_simul_efun(
        &mut self,
        func_name: Ustr,
        list: ArgList,
    ) -> lpc_rs_errors::Result<()> {
        // The path-cell read invalidates this attempt if the binding changes.
        let Some(simul_efuns) = self.context.simul_efuns() else {
            return Err(self.runtime_error(format!(
                "call to simul efun `{func_name}`: no simul-efun object is loaded"
            )));
        };

        let Some(func) = simul_efuns
            .program(&self.context.txn)
            .lookup_function(func_name)
            .filter(|function| function.public())
            .cloned()
        else {
            return Err(self.runtime_error(format!("call to unknown simul efun `{func_name}`")));
        };

        self.push_call_frame(simul_efuns.clone(), func, list, CallEntry::SimulEfun)
    }
}

/// Check a supplied parameter type, accepting null in `NullMode::Zero`.
/// Errors are labelled at `arg_def_span`, without a call site span.
pub(crate) fn check_arg_type(
    txn: &TxnHandle,
    lpc_ref: &LpcRef,
    arg_type: Option<&LpcType>,
    arg_def_span: Option<&Span>,
    function_name: &str,
) -> lpc_rs_errors::Result<()> {
    if txn.null_mode() == NullMode::Zero && lpc_ref.is_null_in(txn) {
        return Ok(());
    }
    let Some(arg_type) = arg_type else {
        return Ok(());
    };
    let ref_type = lpc_ref.as_lpc_type(txn);
    // Declared against found — reversed, a union parameter rejected its own members.
    if arg_type.matches_type(ref_type) {
        return Ok(());
    }
    Err(LpcError::runtime(format!(
        "unexpected argument type to `{function_name}`: {ref_type}. expected {arg_type}."
    ))
    .with_label("defined here", arg_def_span.copied()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::stm::{Snapshot, Transaction, Version};

    #[test]
    fn call_entries_define_external_history_and_argument_counting() {
        for (entry, external, counted) in [
            (CallEntry::Direct, false, true),
            (CallEntry::Virtual, false, false),
            (CallEntry::SimulEfun, true, true),
            (CallEntry::Door, true, false),
            (CallEntry::Initializer, false, false),
        ] {
            assert_eq!(entry.external(), external, "{entry:?}");
            assert_eq!(entry.counted(), counted, "{entry:?}");
        }
    }

    #[test]
    fn runtime_argument_types_respect_null_mode_and_missing_parameter_metadata() {
        let int = LpcType::Int(false);
        let string = LpcType::String(false);
        let any = LpcType::Mixed(false);
        let number = LpcRef::from(7);
        let text = LpcRef::from("wrong type");
        let zero = LpcRef::from(0);

        for mode in [NullMode::Zero, NullMode::Distinct] {
            let mut snapshot = Snapshot::new(Version::new(), imbl::HashMap::new());
            snapshot.null_mode = mode;
            let txn = TxnHandle::new(Transaction::new(snapshot));
            check_arg_type(&txn, &number, Some(&int), None, "target").unwrap();
            check_arg_type(&txn, &number, Some(&any), None, "target").unwrap();
            check_arg_type(&txn, &text, None, None, "target").unwrap();
            let error = check_arg_type(&txn, &text, Some(&int), None, "target")
                .unwrap_err()
                .to_string();
            assert!(
                error.contains("unexpected argument type to `target`"),
                "{error}"
            );
            assert!(error.contains("expected int"), "{error}");
            assert_eq!(
                check_arg_type(&txn, &zero, Some(&string), None, "target").is_ok(),
                mode == NullMode::Zero,
                "{mode:?}"
            );
        }
    }

    fn empty_task() -> (Task<1024>, Arc<Process>) {
        use crate::interpreter::{task_context::TaskContext, vm::global_state::GlobalState};

        let (tx, _receiver) = tokio::sync::mpsc::channel(8);
        let state = Arc::new(GlobalState::new(crate::test_support::test_config(), tx));
        let process = Arc::new(Process::default());
        let context = TaskContext::new(state, process.clone(), None);
        (Task::new(context), process)
    }

    #[tokio::test]
    async fn register_budgets_accept_the_last_fitting_count_and_report_the_exact_limit() {
        use lpc_rs_core::function_arity::FunctionArity;
        use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

        let (task, _process) = empty_task();
        for locals in [0, 1, 7] {
            let prototype = FunctionPrototypeBuilder::default()
                .name("budget")
                .filename(Arc::new("budget.c".into()))
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(0))
                .build()
                .unwrap();
            let function = Function::from(Arc::new(ProgramFunction::new(prototype, locals)));
            let limit = usize::from(RegisterSize::MAX) - usize::from(locals) - 1;
            assert_eq!(task.checked_register_count(0, &function).unwrap(), 0);
            assert_eq!(
                usize::from(task.checked_register_count(limit, &function).unwrap()),
                limit
            );
            let error = task
                .checked_register_count(limit + 1, &function)
                .unwrap_err();
            let expected = format!("the limit is {limit}");
            assert!(error.to_string().contains(&expected), "{error}");
        }
    }

    #[tokio::test]
    async fn cached_simul_calls_report_missing_reference_arguments_at_the_call_site() {
        use lpc_rs_asm::instruction::Instruction;
        use lpc_rs_core::function_arity::FunctionArity;
        use lpc_rs_errors::source_map::SOURCE_MAP;
        use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

        let (mut task, process) = empty_task();
        let source = "updated()";
        let file = SOURCE_MAP
            .write()
            .add("/cached_caller.c".to_owned(), source.to_owned());
        let span = Span::new(file, 0..source.len());
        let caller = FunctionPrototypeBuilder::default()
            .name("cached_caller")
            .filename(Arc::new("cached_caller.c".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let mut caller = ProgramFunction::new(caller, 0);
        caller.arg_lists = vec![vec![]];
        caller.instructions = vec![Instruction::Call(ustr::ustr("updated"), ArgList(0))];
        caller.debug_spans = vec![Some(span)];
        let mut frame = CallFrame::new(
            process,
            Arc::new(caller),
            0,
            None::<&[VarId]>,
            &task.context.txn,
        );
        frame.set_pc(1usize);
        task.stack.push(frame).unwrap();
        let updated = FunctionPrototypeBuilder::default()
            .name("updated")
            .filename(Arc::new("updated_simul.c".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Int(false)])
            .ref_params(vec![true])
            .build()
            .unwrap();
        let mut updated = ProgramFunction::new(updated, 0);
        updated.num_upvalues = 1;
        updated.arg_locations = vec![lpc_rs_core::register::Register(0).as_upvalue()];
        let function = Function::from(Arc::new(updated));
        let error = task
            .push_call_frame(
                Arc::new(Process::default()),
                function,
                ArgList(0),
                CallEntry::SimulEfun,
            )
            .unwrap_err();
        assert!(
            error
                .to_string()
                .contains("argument 1 of `updated` must be passed by reference"),
            "{error}"
        );
        assert_eq!(error.span(), Some(span));
        assert_eq!(task.stack.len(), 1);
    }

    #[tokio::test]
    async fn mixed_value_reference_and_spread_arguments_keep_their_positions() {
        let task = crate::test_support::run_prog(
            r#"
            void fill(int first, int ref output, int middle, int next, int last) {
                output = first * 1000 + middle * 100 + next * 10 + last;
            }
            int create() {
                int output;
                int *middle = ({ 2, 3 });
                fill(1, ref output, middle..., 4);
                return output;
            }
        "#,
        )
        .await;
        assert!(matches!(task.result().unwrap(), LpcRef::Int(value) if value.0 == 1234));
    }
}
