use std::sync::Arc;

use if_chain::if_chain;
use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{RegisterSize, lpc_path::LpcPath, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace};
use ustr::Ustr;

use crate::interpreter::{
    call_frame::CallFrame,
    efun::{Efun, call_efun, call_efun_sync, efun_context::EfunContext},
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::{TxnHandle, VarId},
    task::Task,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    pub(crate) fn handle_call(&mut self, name: Ustr, list: ArgList) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        // Codegen emits `Call` only for a name of this program; a miss is a bug.
        let Some(func) = current_frame.process.program.function(name).cloned() else {
            return Err(self
                .stack
                .runtime_bug(format!("call to unknown local function `{name}`")));
        };
        let process = current_frame.process.clone();

        self.push_call_frame(process, func, list, false)
    }

    /// Push a frame for a call to `func` on `process`, its arguments read
    /// from the current frame's `list`; `external` marks a frame entered
    /// through a door.
    #[instrument(level = "debug", skip_all)]
    pub(crate) fn push_call_frame(
        &mut self,
        process: Arc<Process>,
        func: Arc<ProgramFunction>,
        list: ArgList,
        external: bool,
    ) -> lpc_rs_errors::Result<()> {
        let num_args = RegisterSize::try_from(self.args_of(list)?.len())?;
        // A simul_efun's prototype can change after a cached caller was compiled against it.
        if_chain! {
            if num_args < func.arity().num_args;
            if let Some(i) = func.prototype.first_ref_param();
            if i >= usize::from(num_args);
            then {
                let caller_span = self.stack.current_frame().ok().and_then(CallFrame::current_debug_span);
                return Err(LpcError::runtime(format!(
                    "argument {} of `{}` must be passed by reference",
                    i + 1,
                    func.name()
                ))
                .or_span(caller_span));
            }
        }

        trace!("pushing new frame; copying arguments: {num_args}");
        self.stack
            .push_new(process, func, num_args, num_args, None::<&[VarId]>)?;
        self.stack.current_frame_mut()?.external = external;
        if let Err(e) = self.populate_arguments(list) {
            // The half-built frame comes off; the error names the caller.
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

    /// Copy the arguments the caller's `list` names into the frame above it.
    fn populate_arguments(&mut self, list: ArgList) -> lpc_rs_errors::Result<()> {
        let txn = &self.context.txn;
        let (callee, below) = self.stack.split_last_mut()?;
        let Some(caller) = below.last() else {
            return Err(callee.runtime_bug("a call with no frame to read its arguments from"));
        };
        for (i, arg) in caller.function.args(list).iter().enumerate() {
            match *arg {
                Arg::Value(loc) => {
                    let value = caller.get_location(txn, loc)?.into_owned();
                    callee.push_arg(txn, i, value)?;
                }
                Arg::Ref(loc) => {
                    let cell = caller.ref_cell(loc)?;
                    callee.push_ref(i, cell)?;
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

    /// Run `efun`, called by the top frame's instruction with `list`, in
    /// place; its result lands in that frame's register 0.
    pub(crate) fn call_efun_now(&mut self, efun: Efun, list: ArgList) -> lpc_rs_errors::Result<()> {
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
        Ok(())
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
        Ok(())
    }

    /// Push the frame an efun fired through a pointer runs in: `owner`'s,
    /// written by `origin`.
    pub(crate) fn push_entry_frame(
        &mut self,
        owner: Arc<Process>,
        origin: Option<Arc<LpcPath>>,
    ) -> lpc_rs_errors::Result<()> {
        let mut frame = CallFrame::entry(owner);
        frame.origin = origin;
        self.stack.push(frame)
    }

    /// The by-reference parameter no pointer call can satisfy, as the error.
    pub(super) fn refuse_ref_params(
        &self,
        efun: Efun,
        args: &[LpcRef],
    ) -> lpc_rs_errors::Result<()> {
        let prototype = efun.prototype();
        match (0..args.len()).find(|&i| prototype.is_ref_param(i)) {
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
        self.refuse_ref_params(efun, &args)?;

        let mut ctx = EfunContext::fired(&mut self.stack, &self.context, efun, args, owner, origin);
        let result = call_efun(efun, &mut ctx).await;

        #[cfg(test)]
        if let Some(snap) = ctx.snapshot.take() {
            self.snapshots.push(snap);
        }

        ctx.finish(result)
    }

    /// `call_fired_efun` for an efun that never suspends: no future built.
    pub(crate) fn call_fired_efun_now(
        &mut self,
        efun: Efun,
        args: Vec<LpcRef>,
        owner: Arc<Process>,
        origin: Option<Arc<LpcPath>>,
    ) -> lpc_rs_errors::Result<()> {
        self.refuse_ref_params(efun, &args)?;
        let mut ctx = EfunContext::fired(&mut self.stack, &self.context, efun, args, owner, origin);
        let result = match call_efun_sync(efun, &mut ctx) {
            Some(result) => result,
            None => Err(ctx.runtime_bug(format!("`{efun:?}` suspends but reached the sync door"))),
        };

        #[cfg(test)]
        if let Some(snap) = ctx.snapshot.take() {
            self.snapshots.push(snap);
        }

        ctx.finish(result)
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
        // A caller links by name to whichever resident a task starts with, so a
        // destructed or recompiled simul-efun object is a runtime miss here.
        let Some(simul_efuns) = self.context.simul_efuns() else {
            return Err(self.runtime_error(format!(
                "call to simul efun `{func_name}`: no simul-efun object is loaded"
            )));
        };

        let Some(func) = simul_efuns.program.lookup_function(func_name).cloned() else {
            return Err(self.runtime_error(format!("call to unknown simul efun `{func_name}`")));
        };

        self.push_call_frame(simul_efuns.clone(), func, list, true)
    }
}

/// `lpc_ref` fits a parameter typed `arg_type` (0 always does), else the
/// runtime error, labelled at `arg_def_span`, with no call site span.
pub(crate) fn check_arg_type(
    txn: &TxnHandle,
    lpc_ref: &LpcRef,
    arg_type: Option<&LpcType>,
    arg_def_span: Option<&Span>,
    function_name: &str,
) -> lpc_rs_errors::Result<()> {
    if lpc_ref.eq_in(&NULL, txn) {
        return Ok(());
    }
    let Some(arg_type) = arg_type else {
        return Ok(());
    };
    let ref_type = lpc_ref.as_lpc_type();
    if ref_type.matches_type(*arg_type) {
        return Ok(());
    }
    Err(LpcError::runtime(format!(
        "unexpected argument type to `{function_name}`: {ref_type}. expected {arg_type}."
    ))
    .with_label("defined here", arg_def_span.copied()))
}
