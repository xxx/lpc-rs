use std::sync::Arc;

use lpc_rs_errors::{LpcError, Result};

use super::{RunningTask, Task};
use crate::interpreter::{
    call_frame::CallFrame,
    call_origin::CallOrigin,
    function_type::{function_ptr::FunctionPtr, function_ptr::ResolvedCall},
    lpc_ref::LpcRef,
};

pub(crate) struct DriverCallbackError {
    pub error: LpcError,
    pub retire: bool,
}

impl DriverCallbackError {
    fn dispatch(error: LpcError) -> Self {
        Self {
            error,
            retire: true,
        }
    }

    fn execution(error: LpcError) -> Self {
        Self {
            error,
            retire: false,
        }
    }
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Invoke a freshly leased pointer in the caller's transaction with driver origin.
    pub(crate) async fn run_driver_callback(
        &mut self,
        ptr: &Arc<FunctionPtr>,
        args: &[LpcRef],
    ) -> std::result::Result<(), DriverCallbackError> {
        let mut running = RunningTask {
            task: self,
            finished: false,
        };
        let result = running.task.invoke_driver_callback(ptr, args).await;
        running.finished = true;
        result
    }

    async fn invoke_driver_callback(
        &mut self,
        ptr: &Arc<FunctionPtr>,
        args: &[LpcRef],
    ) -> std::result::Result<(), DriverCallbackError> {
        if !ptr
            .owner
            .upgrade()
            .is_some_and(|owner| owner.is_live(self.context.txn()))
        {
            return Err(DriverCallbackError::dispatch(
                self.runtime_error("callback owner is retired"),
            ));
        }
        let ResolvedCall {
            process,
            function,
            args,
        } = ptr
            .prepare_call(args, &self.context, || Ok(self.context.callers.clone()))
            .await
            .map_err(DriverCallbackError::dispatch)?
            .ok_or_else(|| {
                DriverCallbackError::dispatch(self.runtime_error("callback has no target function"))
            })?;
        self.context.process = process.clone();
        if !process.is_initialized(self.context.txn()) {
            Self::initialize_process(
                self.context
                    .nested(self.context.callers.clone(), process.clone())
                    .map_err(DriverCallbackError::execution)?,
            )
            .await
            .map_err(DriverCallbackError::execution)?;
        }
        let count = self
            .checked_register_count(args.len(), &function)
            .map_err(DriverCallbackError::dispatch)?;
        for (i, arg) in args.iter().enumerate() {
            self.type_check_call_arg(
                arg,
                function.prototype.arg_types.get(i),
                function.prototype.arg_spans.get(i),
                &function.prototype.name,
            )
            .map_err(DriverCallbackError::dispatch)?;
        }
        if function.prototype.is_efun() {
            let efun = self
                .efun_of(&function)
                .map_err(DriverCallbackError::dispatch)?;
            self.push_entry_frame(process.clone(), CallOrigin::Driver)
                .map_err(DriverCallbackError::dispatch)?;
            self.call_fired_efun(efun, args, process, ptr.origin.clone())
                .await
                .map_err(DriverCallbackError::execution)?;
        } else {
            let frame: Result<CallFrame> = (|| {
                let mut frame = CallFrame::with_receiver(
                    ptr.frame_receiver(process, self.context.txn()),
                    function,
                    count,
                    count,
                    Some(ptr.upvalue_ptrs.as_slice()),
                );
                frame.initialize_parameters(self.context.txn());
                frame.origin = CallOrigin::Driver;
                for (i, arg) in args.into_iter().enumerate() {
                    frame.push_arg(self.context.txn(), i, arg)?;
                }
                Ok(frame)
            })();
            self.stack
                .push(frame.map_err(DriverCallbackError::dispatch)?)
                .map_err(DriverCallbackError::dispatch)?;
        }
        self.resume().await.map_err(DriverCallbackError::execution)
    }
}
