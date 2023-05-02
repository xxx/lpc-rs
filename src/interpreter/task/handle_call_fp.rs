use std::sync::Arc;

use lpc_rs_core::{
    register::{Register, RegisterVariant},
    RegisterSize,
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;
use thin_vec::ThinVec;
use tracing::{instrument, trace};

use crate::{
    get_loc,
    interpreter::{
        call_frame::CallFrame,
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
        lpc_ref::{LpcRef, NULL},
        object_flags::ObjectFlags,
        process::Process,
        task::{get_location, set_location, Task},
    },
    set_loc,
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(skip_all)]
    #[inline]
    pub(crate) async fn handle_call_fp(&mut self, location: RegisterVariant) -> Result<()> {
        let num_args = RegisterSize::try_from(self.args.len())?;
        let ptr_arc = {
            let lpc_ref = &*get_loc!(self, location)?;

            if let LpcRef::Function(func) = lpc_ref {
                func.clone() // this is a cheap clone
            } else {
                return Err(
                    self.runtime_error(format!("callfp instruction on non-function: {}", lpc_ref))
                );
            }
        };

        let (passed_args_count, function_is_efun, is_dynamic_receiver, function, proc, upvalues) =
            match self.extract_ptr_data(&ptr_arc, num_args).await {
                Ok(Some(tuple)) => tuple,
                Ok(None) => return Ok(()),
                Err(e) => return Err(e),
            };

        if !proc.flags.test(ObjectFlags::Initialized) {
            let ctx = self.context.clone().with_process(proc.clone());
            Self::initialize_process(ctx).await?;
        }

        let adjusted_num_args = num_args - (is_dynamic_receiver as RegisterSize);

        let max_arg_length = std::cmp::max(adjusted_num_args, function.arity().num_args);
        let max_arg_length = std::cmp::max(max_arg_length, passed_args_count);

        let mut new_frame = CallFrame::with_minimum_arg_capacity(
            proc,
            function.clone(),
            passed_args_count,
            max_arg_length,
            upvalues,
            self.context.upvalues().clone(),
        );
        // let pf = function;

        // for dynamic receivers, skip the first register of the passed args, which contains the receiver itself
        let index = is_dynamic_receiver as usize;
        let from_slice = &self.args[index..(index + adjusted_num_args as usize)];

        let mut from_slice_index = 0;
        let mut next_index = 1;
        {
            // This read() needs to be dropped before the `await`.
            let arg_locations = &function.arg_locations;
            let partial_args = ptr_arc.partial_args.read();

            for i in 0..max_arg_length {
                let target_location = arg_locations.get(i as usize).copied().unwrap_or_else(|| {
                    // This should only be reached by variables that will go
                    // into an ellipsis function's `argv`.
                    Register(next_index).as_local()
                });

                if let RegisterVariant::Local(r) = target_location {
                    next_index = r.index() + 1;
                }

                if let Some(Some(lpc_ref)) = partial_args.get(i as usize) {
                    // if a partially-applied-to arg is present, use it
                    Self::type_check_and_assign_location(
                        self,
                        &mut new_frame,
                        target_location,
                        lpc_ref.clone(),
                        i,
                    )?;
                } else if let Some(location) = from_slice.get(from_slice_index) {
                    // check if the user passed an argument, which will either
                    // fill in the next hole in the partial arguments, or
                    // append to the end

                    let lpc_ref = get_loc!(self, *location)?;
                    Self::type_check_and_assign_location(
                        self,
                        &mut new_frame,
                        target_location,
                        lpc_ref.into_owned(),
                        i,
                    )?;

                    from_slice_index += 1;
                }
            }
        }

        self.stack.push(new_frame)?;

        if function_is_efun {
            self.prepare_and_call_efun(function.name()).await?;
        }

        Ok(())
    }

    fn type_check_and_assign_location(
        task: &Task<STACKSIZE>,
        new_frame: &mut CallFrame,
        loc: RegisterVariant,
        r: LpcRef,
        i: RegisterSize,
    ) -> Result<()> {
        let prototype = &new_frame.function.prototype;
        task.type_check_call_arg(
            &r,
            prototype.arg_types.get(i as usize),
            prototype.arg_spans.get(i as usize),
            &prototype.name,
        )?;

        trace!("Copying argument {} ({}) to {}", i, r, loc);

        new_frame.arg_locations.push(loc);
        new_frame.set_location(loc, r);

        Ok(())
    }

    /// A broken-out function to avoid the ptr's lock being held across awaits.
    async fn extract_ptr_data(
        &mut self,
        ptr: &FunctionPtr,
        num_args: RegisterSize,
    ) -> Result<
        Option<(
            RegisterSize,
            bool,
            bool,
            Arc<ProgramFunction>,
            Arc<Process>,
            Option<ThinVec<Register>>,
        )>,
    > {
        trace!("Calling function ptr: {}", ptr);

        let passed_args_count = num_args
            + ptr
                .partial_args
                .read()
                .iter()
                .fold(0, |sum, arg| sum + arg.is_some() as RegisterSize);
        let function_is_efun = matches!(&ptr.address, FunctionAddress::Efun(_));
        let is_dynamic_receiver = matches!(&ptr.address, FunctionAddress::Dynamic(_));
        let is_call_other = ptr.call_other;

        if let FunctionAddress::Local(receiver, pf) = &ptr.address {
            let Some(receiver) = receiver.upgrade() else {
                return Err(self.runtime_error(format!(
                    "attempted to call a pointer to a function in a destructed object: {}",
                    ptr
                )));
            };

            if !pf.public()
                && !pf.is_closure()
                && (is_call_other || !Arc::ptr_eq(self.context.process(), &receiver))
            {
                set_loc!(self, Register(0).as_local(), NULL)?;
                return Ok(None);
            }
        }

        let Some((proc, function)) = self.extract_process_and_function(ptr).await? else {
            return Ok(None)
        };

        let Some(proc) = proc.upgrade() else {
            return Err(self.runtime_error(format!(
                "attempted to call a pointer to a function in a destructed object: {}",
                ptr
            )));
        };

        let upvalues = if function.is_closure() {
            Some(ptr.upvalue_ptrs.clone())
        } else {
            // Calls to pointers to static functions do not inherit upvalues,
            // same as normal direct calls to them.
            None
        };
        Ok(Some((
            passed_args_count,
            function_is_efun,
            is_dynamic_receiver,
            function,
            proc,
            upvalues,
        )))
    }
}
