use std::{borrow::Cow, fmt::Debug, path::PathBuf, rc::Rc};

use delegate::delegate;
use lpc_rs_core::register::RegisterVariant;
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_utils::config::Config;
use qcell::{QCell, QCellOwner};

use crate::interpreter::{
    call_frame::CallFrame, call_stack::CallStack, gc::gc_bank::GcRefBank, lpc_ref::LpcRef,
    lpc_value::LpcValue, memory::Memory, process::Process, program::Program, task::get_location,
    task_context::TaskContext,
};

/// A structure to hold various pieces of interpreter state, to be passed to
/// Efuns when they're called
#[derive(Debug)]
pub struct EfunContext<'task, const N: usize> {
    stack: &'task mut CallStack<N>,
    task_context: &'task TaskContext,
    memory: &'task Memory,

    /// Allow the user to take a snapshot of the callstack, for testing and
    /// debugging
    #[cfg(test)]
    pub snapshot: Option<CallStack<N>>,
}

impl<'task, const N: usize> EfunContext<'task, N> {
    pub fn new(
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
        memory: &'task Memory,
    ) -> Self {
        Self {
            stack,
            task_context,
            memory,

            #[cfg(test)]
            snapshot: None,
        }
    }

    delegate! {
        to self.task_context {
            /// Get the in-game directory of the current process
            pub fn in_game_cwd(&self, cell_key: &QCellOwner) -> PathBuf;

            /// Get pointer to the current [`Config`] that's in-use
            pub fn config(&self) -> Rc<Config>;

            /// Increment the current task instruction count, checking for too-long-evaluations
            pub fn increment_instruction_count(&self, amount: usize) -> Result<usize>;

            /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
            /// then insert it into the object space.
            pub fn insert_clone(&self, program: Rc<Program>, cell_key: &mut QCellOwner) -> Rc<QCell<Process>>;

            /// Get access to the [`Vm`](crate::interpreter::vm::Vm)'s upvalues (i.e. all of them)
            #[call(upvalues)]
            pub fn vm_upvalues(&self) -> &Rc<QCell<GcRefBank>>;
        }

        to self.memory {
            /// Wrap an [`LpcValue`]  with an [`LpcRef`]
            pub fn value_to_ref(&self, value: LpcValue) -> LpcRef;
        }
    }

    /// Get a reference to the current [`CallFrame`]
    #[inline]
    pub fn frame(&self) -> &CallFrame {
        self.stack.last().unwrap()
    }

    #[inline]
    pub fn memory(&self) -> &Memory {
        self.memory
    }

    #[inline]
    pub fn return_efun_result(&mut self, result: LpcRef) {
        let frame = self.stack.last_mut().unwrap();
        frame.registers[0] = result;
    }

    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        self.frame().current_debug_span()
    }

    #[inline]
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        self.frame().runtime_error(msg)
    }

    /// Resolve a local register
    #[inline]
    pub fn resolve_local_register<I>(&self, register: I) -> LpcRef
    where
        I: Into<usize>,
    {
        self.frame().registers[register.into()].clone()
    }

    /// Resolve any RegisterVariant
    #[inline]
    pub fn resolve_register_variant(
        &self,
        variant: RegisterVariant,
        cell_key: &QCellOwner,
    ) -> Result<Cow<LpcRef>> {
        get_location(self.stack, variant, cell_key)
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T, cell_key: &QCellOwner) -> Option<Rc<QCell<Process>>>
    where
        T: AsRef<str>,
    {
        self.task_context.lookup_process(path, cell_key)
    }

    /// Get a clone of the task context
    #[inline]
    pub fn clone_task_context(&self) -> TaskContext {
        self.task_context.clone()
    }

    /// Get a reference to the [`Process`] that contains the call to this efun
    #[inline]
    pub fn process(&self) -> &Rc<QCell<Process>> {
        &self.frame().process
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P, cell_key: &mut QCellOwner)
    where
        P: Into<Rc<QCell<Process>>>,
    {
        self.task_context.insert_process(process, cell_key);
    }

    /// Return a clone of the current stack, for snapshotting
    #[cfg(test)]
    pub fn clone_stack(&self) -> CallStack<N> {
        self.stack.clone()
    }
}
