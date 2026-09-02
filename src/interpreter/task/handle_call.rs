use std::sync::Arc;

use if_chain::if_chain;
use lpc_rs_core::{RegisterSize, lpc_type::LpcType};
use lpc_rs_errors::{LpcError, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace, warn};
use ustr::Ustr;

use crate::interpreter::stm::VarId;
use crate::{
    interpreter::{
        call_frame::CallFrame,
        efun::{call_efun, efun_context::EfunContext},
        lpc_ref::{LpcRef, NULL},
        process::Process,
        task::{Arg, Task, get_location},
    },
    pop_frame,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    pub(crate) async fn handle_call(&mut self, name: Ustr) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        let process = current_frame.process.clone();
        let func = {
            let function = process.program.lookup_function(name);
            if let Some(func) = function {
                func.clone()
            } else {
                // A function the simul-efun file inherits lands here: its
                // prototype is `Local`-kind, so callers emit `Call`, and it
                // runs in the caller.
                warn!(
                    at = %current_frame.to_stack_trace_format(),
                    "call to unknown local function `{name}`, trying simul_efuns and efuns"
                );

                if_chain! {
                    // See if there is a simul efun with this name
                    if let Some(se) = self.context.simul_efuns();
                    if let Some(func) = se.program.lookup_function(name);
                    then {
                        func.clone()
                    } else {
                        let msg = format!("Call to unknown function `{name}`");
                        return Err(self.runtime_error(msg));
                    }
                }
            }
        };

        let new_frame = self.prepare_new_call_frame(process, func).await?;

        trace!("pushing new frame");

        self.stack.push(new_frame)?;

        Ok(())
    }

    /// Prepare and populate a new [`CallFrame`] for a call to a static function.
    #[instrument(level = "debug", skip_all)]
    pub(crate) async fn prepare_new_call_frame(
        &mut self,
        process: Arc<Process>,
        func: Arc<ProgramFunction>,
    ) -> lpc_rs_errors::Result<CallFrame> {
        let num_args = RegisterSize::try_from(self.args.len())?;
        // A simul_efun's prototype can change after a cached caller was compiled against it.
        if_chain! {
            if self.args.len() < func.arity().num_args as usize;
            if let Some(i) = func.prototype.first_ref_param();
            if i >= self.args.len();
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
        let mut new_frame = CallFrame::with_minimum_arg_capacity(
            process,
            func.clone(),
            num_args,
            num_args,
            // A static function inherits no captures from its caller.
            None::<&[VarId]>,
        );

        trace!("copying arguments to new frame: {num_args}");
        // Looked up only on the error path; do not hoist it onto every call.
        let caller_span = || {
            self.stack
                .current_frame()
                .ok()
                .and_then(CallFrame::current_debug_span)
        };
        for (i, arg) in self.args.iter().enumerate() {
            match *arg {
                Arg::Value(loc) => {
                    let lpc_ref = get_location(&self.stack, &self.context.txn, loc)?.into_owned();
                    new_frame
                        .push_arg(&self.context.txn, i, lpc_ref)
                        .map_err(|e| e.or_span(caller_span()))?;
                }
                Arg::Ref(loc) => {
                    let cell = self.stack.current_frame()?.ref_cell(loc)?;
                    new_frame
                        .push_ref(&self.context.txn, i, cell)
                        .map_err(|e| e.or_span(caller_span()))?;
                }
            }
        }

        Ok(new_frame)
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

        pop_frame!(self);

        Ok(())
    }

    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) async fn handle_call_simul_efun(
        &mut self,
        func_name: Ustr,
    ) -> lpc_rs_errors::Result<()> {
        let Some(simul_efuns) = self.context.simul_efuns() else {
            // This could be legitimately hit in the case an object was compiled with simul_efuns,
            // cached to disk, and then later executed without them.
            // tl;dr objects are dynamically linked.
            return Err(self.runtime_error("Unable to find simul_efuns. Were they configured?"));
        };

        let func = {
            if let Some(func) = simul_efuns.program.lookup_function(func_name) {
                func.clone()
            } else {
                let msg = format!("Call to unknown simul efun `{func_name}`");
                return Err(self.runtime_error(msg));
            }
        };

        let mut new_frame = self
            .prepare_new_call_frame(simul_efuns.clone(), func)
            .await?;
        new_frame.external = true;

        self.stack.push(new_frame)?;

        Ok(())
    }
}
