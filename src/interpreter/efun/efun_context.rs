use std::{borrow::Cow, fmt::Debug, future::Future, path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use async_trait::async_trait;
use delegate::delegate;
use lpc_rs_core::{register::RegisterVariant, RegisterSize};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use tokio::sync::mpsc::Sender;

use crate::{
    compiler::Compiler,
    interpreter::{
        call_frame::CallFrame,
        call_outs::CallOuts,
        call_stack::CallStack,
        gc::gc_bank::GcRefBank,
        heap::Heap,
        lpc_ref::LpcRef,
        process::Process,
        program::Program,
        task::{get_location},
        task_context::{TaskContext, TaskContextBuilder},
        vm::vm_op::VmOp,
    },
    util::{with_compiler::WithCompiler},
};
use crate::interpreter::object_space::ObjectSpace;
use crate::interpreter::task::task_template::TaskTemplate;
use crate::util::process_builder::{ProcessCreator, ProcessInitializer};

/// A structure to hold various pieces of interpreter state, to be passed to
/// Efuns when they're called
#[derive(Debug)]
pub struct EfunContext<'task, const N: usize> {
    stack: &'task mut CallStack<N>,
    task_context: &'task TaskContext,
    memory: Arc<Heap>,

    /// Allow the user to take a snapshot of the callstack, for testing and
    /// debugging
    #[cfg(test)]
    pub snapshot: Option<CallStack<N>>,
}

impl<'task, const N: usize> EfunContext<'task, N> {
    pub fn new(
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
        memory: Arc<Heap>,
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
            pub fn in_game_cwd(&self) -> PathBuf;

            /// Get pointer to the current [`Config`] that's in-use
            pub fn config(&self) -> &Arc<Config>;

            /// Convert the passed [`Program`] into a [`Process`], set its clone ID,
            /// then insert it into the object space.
            pub fn insert_clone(&self, program: Arc<Program>) -> Arc<Process>;

            /// Get access to the [`Vm`](crate::interpreter::vm::Vm)'s upvalues (i.e. all of them)
            #[call(upvalues)]
            pub fn vm_upvalues(&self) -> &Arc<RwLock<GcRefBank>>;

            /// Get access to the [`Vm`](crate::interpreter::vm::Vm)'s call outs
            pub fn call_outs(&self) -> &Arc<RwLock<CallOuts>>;

            /// Get access to the `tx` channel, to talk to the [`Vm`](crate::interpreter::vm::Vm)
            pub fn tx(&self) -> Sender<VmOp>;
        }
    }

    /// Get a reference to the current [`CallFrame`]
    #[inline]
    pub fn frame(&self) -> &CallFrame {
        self.stack.last().unwrap()
    }

    #[inline]
    pub fn memory(&self) -> &Arc<Heap> {
        &self.memory
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
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        self.frame().runtime_error(msg)
    }

    #[inline]
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        self.frame().runtime_bug(msg)
    }

    /// Resolve a local register
    #[inline]
    pub fn resolve_local_register<I>(&self, register: I) -> &LpcRef
    where
        I: Into<RegisterSize>,
    {
        &self.frame().registers[register.into()]
    }

    /// Resolve a local register
    #[inline]
    pub fn try_resolve_local_register<I>(&self, register: I) -> Option<&LpcRef>
    where
        I: Into<usize>,
    {
        self.frame().registers.get(register.into())
    }

    /// Resolve any RegisterVariant
    #[inline]
    pub fn resolve_register_variant(&self, variant: RegisterVariant) -> Result<Cow<LpcRef>> {
        get_location(self.stack, variant)
    }

    /// Lookup the process with the passed path.
    #[inline]
    pub fn lookup_process<T>(&self, path: T) -> Option<Arc<Process>>
    where
        T: AsRef<str>,
    {
        self.task_context.lookup_process(path)
    }

    /// Get a clone of the task context
    #[inline]
    pub fn task_context_builder(&self) -> TaskContextBuilder {
        TaskContextBuilder::from(self.task_context)
    }

    /// Get a reference to the [`Process`] that contains the call to this efun
    #[inline]
    pub fn process(&self) -> &Arc<Process> {
        &self.frame().process
    }

    /// Directly insert the passed [`Process`] into the object space, with
    /// in-game local filename.
    #[inline]
    pub fn insert_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        self.task_context.insert_process(process);
    }

    ///Remove the passed [`Process`] from the object space
    #[inline]
    pub fn remove_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        self.task_context.remove_process(process);
    }

    /// Get a reference to `this_player` from the context
    #[inline]
    pub fn this_player(&self) -> &ArcSwapAny<Option<Arc<Process>>> {
        &self.task_context.this_player
    }

    /// Get the current `chain_count` from the context.
    #[inline]
    pub fn chain_count(&self) -> u8 {
        self.task_context.chain_count
    }

    /// Return a clone of the current stack, for snapshotting
    #[cfg(test)]
    pub fn clone_stack(&self) -> CallStack<N> {
        self.stack.clone()
    }
}

#[async_trait]
impl<'task, const N: usize> WithCompiler for EfunContext<'task, N> {
    async fn with_async_compiler<F, U, T>(&self, f: F) -> Result<T>
    where
        F: FnOnce(Compiler) -> U + Send,
        U: Future<Output = Result<T>> + Send,
    {
        self.task_context.with_async_compiler(f).await
    }
}

#[async_trait]
impl<'task, const N: usize> ProcessCreator for EfunContext<'task, N> {
    #[inline]
    fn process_creator_data(&self) -> &ObjectSpace {
        self.task_context.process_creator_data()
    }
}

#[async_trait]
impl<'task, const N: usize> ProcessInitializer for EfunContext<'task, N> {
    #[inline]
    fn process_initializer_data(&self) -> TaskTemplate {
        self.task_context.process_initializer_data()
    }
}
