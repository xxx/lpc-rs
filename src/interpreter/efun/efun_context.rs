use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::task_context::TaskContext;
use crate::interpreter::lpc_ref::LpcRef;
use crate::interpreter::lpc_value::LpcValue;
use crate::parser::span::Span;
use crate::errors::LpcError;
use delegate::delegate;
use std::path::PathBuf;
use std::rc::Rc;
use crate::util::config::Config;
use crate::Result;
use crate::interpreter::process::Process;
use crate::interpreter::memory::Memory;
use std::cell::RefCell;
use crate::interpreter::program::Program;

/// A structure to hold various pieces of interpreter state, to be passed to Efuns
#[derive(Debug)]
pub struct EfunContext<'task> {
    frame: &'task mut StackFrame,
    task_context: &'task TaskContext,
    snapshot: Option<Box<EfunContext<'task>>>,
    memory: &'task Memory,
}

impl<'task> EfunContext<'task> {
    pub fn new(frame: &'task mut StackFrame, task_context: &'task TaskContext, memory: &'task Memory) -> Self {
        Self {
            frame,
            task_context,
            memory,
            snapshot: None
        }
    }

    delegate! {
        to self.task_context {
            /// Get the in-game directory of the current process
            pub fn in_game_cwd(&self) -> Result<PathBuf>;

            /// Get pointer to the current [`Config`] that's in-use
            pub fn config(&self) -> Rc<Config>;

            /// Increment the current task instruction count, checking for too-long-evaluations
            pub fn increment_instruction_count(&self, amount: usize) -> Result<usize>;

            /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
            /// then insert it into the object space.
            pub fn insert_clone(&self, program: Rc<Program>) -> Rc<RefCell<Process>>;
        }

        to self.memory {
            /// Wrap an [`LpcValue`]  with an [`LpcRef`]
            pub fn value_to_ref(&self, value: LpcValue) -> LpcRef;
        }
    }

    /// Get a reference to the current [`StackFrame`]
    #[inline]
    pub fn frame(&mut self) -> &StackFrame {
        &self.frame
    }

    #[inline]
    pub fn memory(&self) -> &Memory {
        &self.memory
    }

    #[inline]
    pub fn return_efun_result(&mut self, result: LpcRef) {
        self.frame.registers[0] = result;
    }

    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        self.frame.current_debug_span()
    }

    #[inline]
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        self.frame.runtime_error(msg)
    }

    /// Resolve a register
    #[inline]
    pub fn resolve_lpc_ref<I>(&self, register: I) -> LpcRef
    where
        I: Into<usize>,
    {
        self.frame.resolve_lpc_ref(register)
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T) -> Option<Rc<RefCell<Process>>>
    where
        T: AsRef<str>,
    {
        self.task_context.lookup_process(path)
    }

    /// Get a clone of the task context
    #[inline]
    pub fn clone_task_context(&self) -> TaskContext {
        self.task_context.clone()
    }

    /// Get a reference to the [`Process`] that contains the call to this efun
    #[inline]
    pub fn process(&self) -> &Rc<RefCell<Process>> {
        &self.frame.process
    }

    /// Directly insert the passed [`Process`] into the object space, with in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        self.task_context.insert_process(process);
    }
}
