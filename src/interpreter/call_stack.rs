use lpc_rs_core::null_mode::NullMode;
use std::{
    ops::{Index, IndexMut, RangeFrom},
    sync::Arc,
};

use delegate::delegate;
use lpc_rs_core::{RegisterSize, lpc_path::LibRoot};
use lpc_rs_errors::{LpcError, Result, lpc_error};

use crate::interpreter::program::Function;
use crate::interpreter::{
    call_frame::{CallFrame, FrameCells, FrameReceiver},
    lpc_ref::LpcRef,
    process::Process,
    task_context::{Caller, Callers},
};

/// A starting capacity well under `STACKSIZE`; most tasks never push deep
/// enough to grow it.
const INITIAL_CAPACITY: usize = 8;

#[derive(Debug, Clone)]
pub struct CallStack<const STACKSIZE: usize> {
    /// The call stack; grows on demand, `push` refuses past `STACKSIZE` frames.
    stack: Vec<CallFrame>,

    /// An automatic query must not refresh its own target's idle time on retries.
    pub(crate) cleanup_target: Option<Arc<Process>>,
}

impl<const STACKSIZE: usize> CallStack<STACKSIZE> {
    delegate! {
        to self.stack {
            /// Drop every frame in place, keeping the stack's allocation.
            pub fn clear(&mut self);

            /// Get a frame by index
            pub fn get(&self, index: usize) -> Option<&CallFrame>;

            /// Get a mutable reference to a frame by index
            pub fn get_mut(&mut self, index: usize) -> Option<&mut CallFrame>;

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

    #[inline(always)]
    pub fn current_frame(&self) -> Result<&CallFrame> {
        match self.stack.last() {
            Some(frame) => Ok(frame),
            None => Err(Self::empty_stack()),
        }
    }

    #[inline(always)]
    pub fn current_frame_mut(&mut self) -> Result<&mut CallFrame> {
        match self.stack.last_mut() {
            Some(frame) => Ok(frame),
            None => Err(Self::empty_stack()),
        }
    }

    /// The top frame and every frame below it.
    #[inline(always)]
    pub fn split_last_mut(&mut self) -> Result<(&mut CallFrame, &[CallFrame])> {
        match self.stack.split_last_mut() {
            Some((top, below)) => Ok((top, below)),
            None => Err(Self::empty_stack()),
        }
    }

    /// The error for a frame lookup on an empty stack.
    #[cold]
    #[inline(never)]
    fn empty_stack() -> LpcError {
        lpc_error!("stack is somehow empty")
    }

    /// The error for a push past `STACKSIZE`.
    #[cold]
    #[inline(never)]
    fn overflow() -> LpcError {
        lpc_error!("stack overflow")
    }

    /// Push a new frame onto the stack. Will return `Err` in the
    /// case of a stack overflow.
    pub fn push(&mut self, frame: CallFrame) -> Result<()> {
        if self.stack.len() >= STACKSIZE {
            return Err(Self::overflow());
        }

        self.record_activity(&frame.process);
        self.stack.push(frame);

        Ok(())
    }

    /// Push a frame for `function`, built in its slot: a frame moved
    /// through memory reloads its narrow stores wide, a stall per call.
    #[inline]
    pub(crate) fn push_new<V>(
        &mut self,
        receiver: Arc<FrameReceiver>,
        function: Function,
        called_with_num_args: RegisterSize,
        arg_capacity: RegisterSize,
        upvalue_ptrs: Option<V>,
    ) -> Result<()>
    where
        V: Into<FrameCells>,
    {
        if self.stack.len() >= STACKSIZE {
            return Err(Self::overflow());
        }

        self.record_activity(&receiver.process);
        self.stack.push(CallFrame::with_receiver(
            receiver,
            function,
            called_with_num_args,
            arg_capacity,
            upvalue_ptrs,
        ));

        Ok(())
    }

    /// Remove the top item from the call stack, and return a mutable reference
    /// to it.
    pub fn pop(&mut self) -> Option<CallFrame> {
        self.stack.pop()
    }

    fn record_activity(&self, process: &Arc<Process>) {
        if let Some(cleanup) = &process.cleanup
            && !self
                .cleanup_target
                .as_ref()
                .is_some_and(|target| Arc::ptr_eq(target, process))
            && !self
                .stack
                .last()
                .is_some_and(|frame| Arc::ptr_eq(&frame.process, process))
        {
            cleanup.touch();
        }
    }

    /// Create a runtime error at the current frame's location; `None` span
    /// on an empty stack.
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        match self.stack.last() {
            Some(frame) => frame.runtime_error(msg),
            None => LpcError::runtime(msg),
        }
    }

    /// Create a runtime bug at the current frame's location; `None` span on
    /// an empty stack.
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> LpcError {
        match self.stack.last() {
            Some(frame) => frame.runtime_bug(msg),
            None => LpcError::runtime_bug(msg),
        }
    }

    /// The defining file of the top frame's code, as an in-game path with
    /// its extension (`/secure/master.c`); the selected null on an empty stack. The
    /// `calling_program` efun answers the caller's file, not this one.
    pub fn calling_program(&self, lib_dir: &str, mode: NullMode) -> LpcRef {
        match self.last() {
            Some(frame) => {
                let path = &frame.function.prototype.filename;
                LpcRef::from(LibRoot::new(lib_dir).source_name(path).to_string())
            }
            None => LpcRef::nil(mode),
        }
    }

    /// The frames that called through a door to reach the frame at
    /// `index`, innermost first: beneath each external frame at or below
    /// it is the frame that called through. Ends at the entry frame; the
    /// task's context has the rest.
    pub fn door_crossers(&self, index: usize) -> impl Iterator<Item = &CallFrame> {
        self.stack[..=index]
            .iter()
            .enumerate()
            .rev()
            .filter(|(_, frame)| frame.external)
            .filter_map(|(i, _)| self.stack.get(i.checked_sub(1)?))
    }

    /// Capture every frame through `index` ahead of `tail` for a nested task.
    pub fn chain(&self, index: usize, tail: Callers) -> Arc<Caller> {
        let rest = self.stack[..=index].windows(2).fold(tail, |rest, pair| {
            Some(Caller::link_frame(&pair[0], pair[1].external, rest))
        });
        Caller::link_frame(&self.stack[index], true, rest)
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
            stack: Vec::with_capacity(INITIAL_CAPACITY),
            cleanup_target: None,
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

    fn named_frame(name: &'static str) -> CallFrame {
        let program = ProgramBuilder::default()
            .filename(LpcPath::in_game("/caller".into()))
            .build()
            .unwrap();
        let function = ProgramFunctionBuilder::default()
            .prototype(
                FunctionPrototypeBuilder::default()
                    .name(name)
                    .filename(Arc::new(LpcPath::in_game("/caller.c".into())))
                    .return_type(LpcType::Void)
                    .build()
                    .unwrap(),
            )
            .build()
            .unwrap();
        CallFrame::new(
            Arc::new(Process::new(program)),
            Arc::new(function),
            0 as RegisterSize,
            None::<&[crate::interpreter::stm::VarId]>,
            &crate::interpreter::stm::TxnHandle::default(),
        )
    }

    fn stack_with_one_frame() -> CallStack<4> {
        let mut stack = CallStack::default();
        stack.push(named_frame("f")).unwrap();
        stack
    }

    #[test]
    fn pop_removes_frames_in_lifo_order_and_preserves_the_caller() {
        let mut stack = CallStack::<4>::default();
        stack.push(named_frame("caller")).unwrap();
        stack.push(named_frame("callee")).unwrap();
        assert_eq!(stack.pop().unwrap().function.name(), "callee");
        assert_eq!(stack.len(), 1);
        assert_eq!(stack.current_frame().unwrap().function.name(), "caller");
        assert_eq!(stack.pop().unwrap().function.name(), "caller");
        assert!(stack.pop().is_none());
        assert!(stack.current_frame().is_err());
    }

    #[test]
    fn external_callers_skip_local_frames_and_the_entry_boundary() {
        let mut stack = CallStack::<4>::default();
        for (name, external) in [
            ("entry", true),
            ("external", true),
            ("local", false),
            ("nested", true),
        ] {
            let mut frame = named_frame(name);
            frame.external = external;
            stack.push(frame).unwrap();
        }
        assert_eq!(
            stack
                .door_crossers(3)
                .map(|frame| frame.function.name())
                .collect::<Vec<_>>(),
            ["local", "entry"],
        );
        assert_eq!(
            stack
                .door_crossers(2)
                .map(|frame| frame.function.name())
                .collect::<Vec<_>>(),
            ["entry"],
        );
        assert_eq!(stack.door_crossers(0).count(), 0);
    }

    #[test]
    fn program_names_and_stack_traces_report_the_active_frames() {
        let mut stack = CallStack::<4>::default();
        assert!(stack.stack_trace().is_empty());
        assert_eq!(
            stack.calling_program("/lib", NullMode::Zero),
            LpcRef::from(0)
        );
        assert_eq!(
            stack.calling_program("/lib", NullMode::Distinct),
            LpcRef::Nil
        );
        stack.push(named_frame("caller")).unwrap();
        stack.push(named_frame("callee")).unwrap();
        assert_eq!(
            stack.calling_program("/lib", NullMode::Zero),
            LpcRef::from("/caller.c")
        );
        assert_eq!(
            stack.stack_trace(),
            ["(unknown) in caller()", "(unknown) in callee()"],
        );
    }

    #[test]
    fn error_helpers_carry_their_prefix_and_severity() {
        for stack in [CallStack::<4>::default(), stack_with_one_frame()] {
            let error = stack.runtime_error("x");
            assert_eq!(error.to_string(), "runtime error: x");
            assert_eq!(error.severity(), lpc_rs_errors::LpcErrorSeverity::Error);

            let bug = stack.runtime_bug("x");
            assert_eq!(bug.to_string(), "runtime bug: x");
            assert!(bug.is_bug());
        }
    }
}
