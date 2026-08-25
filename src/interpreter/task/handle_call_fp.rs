use std::borrow::Cow;

use lpc_rs_core::{RegisterSize, register::RegisterVariant};
use lpc_rs_errors::Result;
use tracing::instrument;

use crate::interpreter::{
    call_frame::CallFrame,
    function_type::function_ptr::ResolvedCall,
    lpc_ref::{LpcRef, NULL},
    task::{Arg, Task, get_location},
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) async fn handle_call_fp(&mut self, location: RegisterVariant) -> Result<()> {
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
            .args
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
        } = match ptr.prepare_call(&passed, &self.context).await {
            Ok(Some(prepared)) => prepared,
            Ok(None) => {
                self.stack.current_frame_mut()?.registers[0] = NULL;
                return Ok(());
            }
            Err(e) => return Err(e.or_span(self.stack.current_frame()?.current_debug_span())),
        };

        if !process.is_initialized(&self.context.txn) {
            Self::initialize_process(self.context.clone().with_process(process.clone())).await?;
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

        let mut new_frame = CallFrame::new(
            process,
            function.clone(),
            RegisterSize::try_from(args.len())?,
            Some(ptr.upvalue_ptrs.clone()),
        );
        for (i, arg) in args.into_iter().enumerate() {
            new_frame.push_arg(&self.context.txn, i, arg)?;
        }

        self.stack.push(new_frame)?;

        if function.prototype.is_efun() {
            self.prepare_and_call_efun(function.name()).await?;
        }

        Ok(())
    }
}
