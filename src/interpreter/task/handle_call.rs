use std::sync::Arc;

use if_chain::if_chain;
use lpc_rs_core::{RegisterSize, lpc_type::LpcType, register::Register};
use lpc_rs_errors::{LpcError, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace};
use ustr::Ustr;

use crate::{
    interpreter::{
        call_frame::CallFrame,
        efun::{call_efun, efun_context::EfunContext},
        lpc_ref::{LpcRef, NULL},
        process::Process,
        task::{Task, get_location},
    },
    pop_frame,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(skip_all)]
    pub(crate) async fn handle_call(&mut self, name: Ustr) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        let process = current_frame.process.clone();
        let func = {
            let function = process.program.lookup_function(name);
            if let Some(func) = function {
                func.clone()
            } else {
                // These shouldn't be reachable due to the CallEfun and CallSimulEfun instructions,
                // but are kept juuuuuust in case.
                {
                    let e = LpcError::new_warning(
                        format!("Call to unknown local function `{name}`. Falling back to legacy SEfun and Efun checks.")
                    ).with_span(current_frame.current_debug_span());
                    e.emit_diagnostics();
                }

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
    #[instrument(skip_all)]
    pub(crate) async fn prepare_new_call_frame(
        &mut self,
        process: Arc<Process>,
        func: Arc<ProgramFunction>,
    ) -> lpc_rs_errors::Result<CallFrame> {
        let num_args = RegisterSize::try_from(self.args.len())?;
        let mut new_frame = CallFrame::with_minimum_arg_capacity(
            process,
            func.clone(),
            num_args,
            num_args,
            None::<&[Register]>, /* static functions do not inherit upvalues from the calling function */
            self.context.global_state.clone_upvalues(),
        );

        trace!("copying arguments to new frame: {num_args}");
        for (i, arg) in self.args.iter().enumerate() {
            let lpc_ref = get_location(&self.stack, &self.context.txn, *arg)?.into_owned();
            new_frame.push_arg(&self.context.txn, i, lpc_ref)?;
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

                return Err(error.into());
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
        if let Err(mut e) = result {
            if e.span.is_none() {
                let caller = self
                    .stack
                    .len()
                    .checked_sub(2)
                    .and_then(|i| self.stack.get(i));
                *e = e.with_span(caller.and_then(|frame| frame.current_debug_span()));
            }
            return Err(e);
        }

        pop_frame!(self);

        Ok(())
    }

    #[instrument(skip_all)]
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

        let new_frame = self
            .prepare_new_call_frame(simul_efuns.clone(), func)
            .await?;

        self.stack.push(new_frame)?;

        Ok(())
    }
}
