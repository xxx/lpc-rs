use std::{path::PathBuf, sync::Arc};

use if_chain::if_chain;
use lpc_rs_core::{
    RegisterSize,
    lpc_path::LpcPath,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace};

use crate::{
    interpreter::{
        call_frame::CallFrame,
        efun::{EFUN_FUNCTIONS, call_efun, efun_context::EfunContext},
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
        lpc_ref::{LpcRef, NULL},
        process::Process,
        task::{ProcessFunctionPair, Task, get_location},
    },
    pop_frame,
    util::process_builder::ProcessCreator,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(skip_all)]
    pub(crate) async fn handle_call(
        &mut self,
        name_idx: RegisterSize,
    ) -> lpc_rs_errors::Result<()> {
        let current_frame = self.stack.current_frame()?;
        let process = current_frame.process.clone();
        let func = {
            let name = process
                .program
                .strings
                .resolve(Self::index_symbol(name_idx))
                .unwrap();
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
            self.context.global_state.upvalues.clone(),
        );

        trace!("copying arguments to new frame: {num_args}");
        // copy argument registers from old frame to new
        if num_args > 0 {
            let mut next_index = 1;
            for (i, arg) in self.args.iter().enumerate() {
                let target_location = func.arg_locations.get(i).copied().unwrap_or_else(|| {
                    // This should only be reached by efun calls, or variables that will go
                    // into an ellipsis function's `argv`.
                    Register(next_index).as_local()
                });
                if let RegisterVariant::Local(r) = target_location {
                    next_index = r.index() + 1;
                }

                let lpc_ref = get_location(&self.stack, *arg).map(|i| i.into_owned())?;

                trace!(
                    "Copying argument {} ({}) to {}",
                    i, lpc_ref, target_location
                );

                new_frame.arg_locations.push(target_location);

                new_frame.set_location(target_location, lpc_ref)
            }
        }

        Ok(new_frame)
    }

    /// An extracted helper to handle pulling the [`Process`] and [`ProgramFunction`] out of a [`FunctionPtr`].
    /// This will create a [`Process`] from a path name, in the case of `call_other` receivers that are strings.
    pub async fn extract_process_and_function(
        &mut self,
        ptr: &FunctionPtr,
    ) -> lpc_rs_errors::Result<Option<ProcessFunctionPair>> {
        let (proc, function) = match &ptr.address {
            FunctionAddress::Local(proc, function) => (proc.clone(), function.clone()),
            FunctionAddress::Dynamic(name) => {
                let lpc_ref = match &*get_location(&self.stack, self.args[0])? {
                    LpcRef::Object(lpc_ref) => lpc_ref.upgrade(),
                    LpcRef::String(string_ref) => {
                        let lookup = {
                            let string = string_ref.read();
                            self.context.lookup_process(string.to_str())
                        };

                        if lookup.is_some() {
                            lookup
                        } else {
                            let path = LpcPath::InGame(PathBuf::from(string_ref.read().to_str()));
                            // This will be initialized later on, if necessary.
                            Some(self.context.create_process_from_path(&path).await?)
                        }
                    }
                    _ => {
                        return Err(
                            self.runtime_error("non-object receiver to function pointer call")
                        );
                    }
                };

                let pair_opt = {
                    if let Some(proc) = lpc_ref {
                        proc.program
                            .lookup_function(name)
                            .map(|func| (proc.clone(), func.clone()))
                    } else {
                        None
                    }
                };

                // short-circuit a 0 return if doing a call_other to a
                // non-existent function, or destructed object
                let Some(pair) = pair_opt else {
                    let frame = self.stack.current_frame_mut()?;
                    frame.registers[0] = NULL;
                    return Ok(None);
                };

                (Arc::downgrade(&pair.0), pair.1)
            }
            FunctionAddress::Efun(name) => {
                // unwrap is safe because this should have been checked in an earlier step
                let pf = EFUN_FUNCTIONS.get(name.as_str()).cloned().unwrap();

                let frame = self.stack.current_frame()?;

                (Arc::downgrade(&frame.process), pf)
            }
            FunctionAddress::SimulEfun(name) => {
                let Some(simul_efuns) = self.context.simul_efuns() else {
                    return Err(self.runtime_bug("simul_efun called without simul_efuns"));
                };

                let Some(function) = simul_efuns.program.lookup_function(name) else {
                    return Err(self.runtime_error(format!("call to unknown simul_efun `{name}`")));
                };

                (Arc::downgrade(simul_efuns), function.clone())
            }
        };

        Ok(Some((proc, function)))
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
            if lpc_ref != &NULL; // 0 is always allowed
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
        let mut ctx = EfunContext::new(self.id, &mut self.stack, &self.context);

        call_efun(name.as_ref(), &mut ctx).await?;

        #[cfg(test)]
        {
            if let Some(snap) = ctx.snapshot {
                self.snapshots.push(snap);
            }
        }

        pop_frame!(self);

        Ok(())
    }

    #[instrument(skip_all)]
    #[inline]
    pub(crate) async fn handle_call_simul_efun(
        &mut self,
        name_idx: RegisterSize,
    ) -> lpc_rs_errors::Result<()> {
        let Some(func_name) = self
            .stack
            .current_frame()?
            .function
            .strings
            .get()
            .unwrap()
            .resolve(Self::index_symbol(name_idx))
        else {
            return Err(self.runtime_bug("Unable to find the name being pointed to."));
        };

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
