use std::ops::{Index, IndexMut, RangeFrom};

use arrayvec::ArrayVec;
use delegate::delegate;
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error};

use crate::interpreter::{call_frame::CallFrame, lpc_ref::LpcRef};

#[derive(Debug, Clone)]
pub struct CallStack<const STACKSIZE: usize> {
    /// The call stack
    stack: Box<ArrayVec<CallFrame, STACKSIZE>>,
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
            .ok_or_else(|| lpc_error!("stack is somehow empty"))
    }

    #[inline]
    pub fn current_frame_mut(&mut self) -> Result<&mut CallFrame> {
        self.stack
            .last_mut()
            .ok_or_else(|| lpc_error!("stack is somehow empty"))
    }

    /// Push a new frame onto the stack. Will return `Err` in the
    /// case of a stack overflow.
    pub fn push(&mut self, frame: CallFrame) -> Result<()> {
        match self.stack.try_push(frame) {
            Ok(_) => Ok(()),
            Err(_e) => Err(lpc_error!("stack overflow")),
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

    /// Create a runtime error at the current frame's location; `None` span
    /// on an empty stack.
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        match self.stack.last() {
            Some(frame) => frame.runtime_error(msg),
            None => lpc_error!(None, "runtime error: {}", msg.as_ref()),
        }
    }

    /// Create a runtime bug at the current frame's location; `None` span on
    /// an empty stack.
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        match self.stack.last() {
            Some(frame) => frame.runtime_bug(msg),
            None => lpc_bug!(None, "runtime bug: {}", msg.as_ref()),
        }
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

impl<const STACKSIZE: usize> Default for CallStack<STACKSIZE> {
    fn default() -> Self {
        Self {
            stack: Box::new(ArrayVec::<_, STACKSIZE>::new()),
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

impl<const STACKSIZE: usize> Index<RangeFrom<usize>> for CallStack<STACKSIZE> {
    type Output = [CallFrame];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.stack[index]
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::{RegisterSize, lpc_path::LpcPath, lpc_type::LpcType};
    use lpc_rs_function_support::{
        function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunctionBuilder,
    };

    use super::*;
    use crate::interpreter::{process::Process, program::ProgramBuilder};

    fn stack_with_one_frame() -> CallStack<4> {
        let program = ProgramBuilder::default()
            .filename(LpcPath::InGame("/caller".into()))
            .build()
            .unwrap();
        let function = ProgramFunctionBuilder::default()
            .prototype(
                FunctionPrototypeBuilder::default()
                    .name("f")
                    .filename(Arc::new(LpcPath::InGame("/caller".into())))
                    .return_type(LpcType::Void)
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap();
        let frame = CallFrame::new(
            Arc::new(Process::new(program)),
            Arc::new(function),
            0 as RegisterSize,
            None::<&[crate::interpreter::stm::VarId]>,
        );
        let mut stack = CallStack::default();
        stack.push(frame).unwrap();
        stack
    }

    #[test]
    fn error_helpers_carry_their_prefix_and_severity() {
        for stack in [CallStack::<4>::default(), stack_with_one_frame()] {
            let error = stack.runtime_error("x");
            assert_eq!(error.to_string(), "runtime error: x");
            assert!(error.is_error());

            let bug = stack.runtime_bug("x");
            assert_eq!(bug.to_string(), "runtime bug: x");
            assert!(bug.is_bug());
        }
    }
}
