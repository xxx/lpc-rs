use crate::{
    errors::LpcError,
    interpreter::{lpc_ref::LpcRef, stack_frame::StackFrame},
    Result,
};
use arrayvec::ArrayVec;
use delegate::delegate;

#[derive(Debug)]
pub struct CallStack<const STACKSIZE: usize> {
    /// The call stack
    stack: ArrayVec<StackFrame, STACKSIZE>,
}

impl<const STACKSIZE: usize> CallStack<STACKSIZE> {
    delegate! {
        to self.stack {
            /// Get the number of objects in the space
            pub fn len(&self) -> usize;

            /// is the stack empty?
            pub fn is_empty(&self) -> bool;

            /// truncate the stack down to the specified length, if it's longer than that
            pub fn truncate(&mut self, new_len: usize);
        }
    }

    #[inline]
    pub fn current_frame(&self) -> Result<&StackFrame> {
        self.stack
            .last()
            .ok_or_else(|| LpcError::new("stack is somehow empty"))
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> Result<&mut StackFrame> {
        self.stack
            .last_mut()
            .ok_or_else(|| LpcError::new("stack is somehow empty"))
    }

    /// Push a new frame onto the stack. Will return `Err` in the
    /// case of a stack overflow.
    pub fn push(&mut self, frame: StackFrame) -> Result<()> {
        match self.stack.try_push(frame) {
            Ok(_) => Ok(()),
            Err(_e) => Err(LpcError::new("stack overflow")),
        }
    }

    /// Remove the top item from the call stack, and return a mutable reference to it.
    pub fn pop(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    /// Convenience helper to copy a return value from a given stack frame, back to the current one.
    pub fn set_call_result(&mut self, result: LpcRef) -> Result<()> {
        if !self.stack.is_empty() {
            self.current_frame_mut()?.registers[0] = result;
        }

        Ok(())
    }

    /// Convenience helper to copy a return value from a given stack frame, back to the current one.
    pub fn copy_call_result(&mut self, from: &StackFrame) -> Result<()> {
        if !self.stack.is_empty() {
            self.current_frame_mut()?.registers[0] = from.registers[0].clone();
        }

        Ok(())
    }

    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        let span = match self.current_frame() {
            Ok(frame) => frame.current_debug_span(),
            Err(_) => None,
        };
        LpcError::new(format!("runtime error: {}", msg.as_ref())).with_span(span)
    }
}

impl<const STACKSIZE: usize> Default for CallStack<STACKSIZE> {
    fn default() -> Self {
        Self {
            stack: ArrayVec::<_, STACKSIZE>::new(),
        }
    }
}
