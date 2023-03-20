use std::collections::HashSet;
use std::ops::{Index, IndexMut};

use arrayvec::ArrayVec;
use bit_set::BitSet;
use delegate::delegate;
use qcell::QCellOwner;
use lpc_rs_core::register::Register;
use lpc_rs_errors::{span::Span, LpcError, Result};

use crate::interpreter::{call_frame::CallFrame, lpc_ref::LpcRef};
use crate::interpreter::gc::mark::Mark;
use crate::interpreter::gc::unique_id::UniqueId;

#[derive(Debug, Clone)]
pub struct CallStack<const STACKSIZE: usize> {
    /// The call stack
    stack: ArrayVec<CallFrame, STACKSIZE>,
}

impl<const STACKSIZE: usize> CallStack<STACKSIZE> {
    delegate! {
        to self.stack {
            /// Get a frame by index
            pub fn get(&self, index: usize) -> Option<&CallFrame>;

            /// Get the number of objects in the space
            pub fn len(&self) -> usize;

            /// is the stack empty?
            pub fn is_empty(&self) -> bool;

            /// truncate the stack down to the specified length, if it's longer than that
            pub fn truncate(&mut self, new_len: usize);

            /// get a reference to the top frame in the stack
            pub fn last(&self) -> Option<&CallFrame>;

            /// get a mutable reference to the top frame in the stack
            pub fn last_mut(&mut self) -> Option<&mut CallFrame>;

            /// Get an Iterator over the stack
            pub fn iter(&self) -> std::slice::Iter<'_, CallFrame>;
        }
    }

    #[inline]
    pub fn current_frame(&self) -> Result<&CallFrame> {
        self.stack
            .last()
            .ok_or_else(|| LpcError::new("stack is somehow empty"))
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> Result<&mut CallFrame> {
        self.stack
            .last_mut()
            .ok_or_else(|| LpcError::new("stack is somehow empty"))
    }

    /// Push a new frame onto the stack. Will return `Err` in the
    /// case of a stack overflow.
    pub fn push(&mut self, frame: CallFrame) -> Result<()> {
        match self.stack.try_push(frame) {
            Ok(_) => Ok(()),
            Err(_e) => Err(LpcError::new("stack overflow")),
        }
    }

    /// Remove the top item from the call stack, and return a mutable reference
    /// to it.
    pub fn pop(&mut self) -> Option<CallFrame> {
        self.stack.pop()
    }

    /// Convenience helper to copy a return value from a given stack frame, back
    /// to the current one.
    pub fn set_result(&mut self, result: LpcRef) -> Result<()> {
        if !self.stack.is_empty() {
            self.current_frame_mut()?.registers[0] = result;
        }

        Ok(())
    }

    /// Convenience helper to copy a return value from a given stack frame, back
    /// to the current one.
    pub fn copy_result(&mut self, from: &CallFrame) -> Result<()> {
        self.set_result(from.registers[0].clone())
    }

    /// Create a runtime error, with stack trace, based on the current state.
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        let span = self.current_frame_debug_span();

        LpcError::new(format!("runtime error: {}", msg.as_ref())).with_span(span)
    }

    /// Create a runtime bug, with stack trace, based on the current state.
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> LpcError {
        let span = self.current_frame_debug_span();

        LpcError::new_bug(format!("runtime error: {}", msg.as_ref())).with_span(span)
    }

    /// Convenience helper to get the current frame's debug span
    #[inline]
    fn current_frame_debug_span(&self) -> Option<Span> {
        self.current_frame()
            .map(|f| f.current_debug_span())
            .unwrap_or(None)
    }

    /// Get the stack trace information for the stack
    #[inline]
    pub fn stack_trace(&self) -> Vec<String> {
        self.stack
            .iter()
            .map(|f| f.to_stack_trace_format())
            .collect::<Vec<_>>()
    }
}

impl<const STACKSIZE: usize> Mark for CallStack<STACKSIZE> {
    fn mark(&self, marked: &mut BitSet, processed: &mut HashSet<UniqueId>, cell_key: &QCellOwner) -> Result<()> {
        for frame in self.stack.iter() {
            frame.mark(marked, processed, cell_key)?
        }

        Ok(())
    }
}

impl<const STACKSIZE: usize> Default for CallStack<STACKSIZE> {
    fn default() -> Self {
        Self {
            stack: ArrayVec::<_, STACKSIZE>::new(),
        }
    }
}

impl<const STACKSIZE: usize> Index<usize> for CallStack<STACKSIZE> {
    type Output = CallFrame;

    fn index(&self, index: usize) -> &Self::Output {
        &self.stack[index]
    }
}

impl<const STACKSIZE: usize> IndexMut<usize> for CallStack<STACKSIZE> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.stack[index]
    }
}
