use std::sync::Arc;

use if_chain::if_chain;
use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{RegisterSize, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace};
use ustr::Ustr;

use crate::interpreter::{
    call_frame::CallFrame,
    efun::{call_efun, efun_context::EfunContext},
    lpc_ref::{LpcRef, NULL},
    process::Process,
    task::Task,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    pub(crate) fn handle_call(&mut self, name: Ustr, list: ArgList) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        // Codegen emits `Call` only for a name of this program; a miss is a bug.
        let Some(func) = current_frame.process.program.lookup_function(name).cloned() else {
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
        self.stack.push_new(process, func, num_args, num_args)?;
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
                    callee.push_ref(txn, i, cell)?;
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
        if_chain! {
            if !lpc_ref.eq_in(&NULL, &self.context.txn); // 0 is always allowed
            if let Some(arg_type) = arg_type;
            let ref_type = lpc_ref.as_lpc_type();
            if !ref_type.matches_type(*arg_type);
            then {
                let error = self.runtime_error(format!(
                    "unexpected argument type to `{function_name}`: {ref_type}. expected {arg_type}."
                ))
                .with_label("defined here", arg_def_span.copied());

                return Err(error);
            }
        }

        Ok(())
    }

    /// Create a new [`EfunContext`] and called the named efun.
    pub(crate) async fn prepare_and_call_efun<S>(&mut self, name: S) -> lpc_rs_errors::Result<()>
    where
        S: AsRef<str>,
    {
        let mut ctx = EfunContext::new(&mut self.stack, &self.context);

        let result = call_efun(name.as_ref(), &mut ctx).await;

        #[cfg(test)]
        {
            if let Some(snap) = ctx.snapshot {
                self.snapshots.push(snap);
            }
        }

        // The efun's own frame has no debug span; the caller's is the nearest
        // location for an error built without one.
        if let Err(e) = result {
            let caller = self
                .stack
                .len()
                .checked_sub(2)
                .and_then(|i| self.stack.get(i));
            return Err(e.or_span(caller.and_then(|frame| frame.current_debug_span())));
        }

        self.pop_frame()?;

        Ok(())
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
