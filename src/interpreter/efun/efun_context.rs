use std::{borrow::Cow, fmt::Debug, future::Future, path::PathBuf, sync::Arc};

use arc_swap::ArcSwapAny;
use async_trait::async_trait;
use delegate::delegate;
use lpc_rs_core::{RegisterSize, lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_utils::config::Config;
use tokio::sync::mpsc::Sender;

use crate::{
    compiler::Compiler,
    interpreter::{
        call_frame::CallFrame,
        call_outs::CallOuts,
        call_stack::CallStack,
        lpc_ref::LpcRef,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        stm::{Effect, TxnHandle},
        task::{Task, get_location, task_id::TaskId, task_template::TaskTemplate},
        task_context::{ObjectLookup, TaskContext, TaskContextBuilder},
        vm::vm_op::VmOp,
    },
    util::{
        process_builder::{ProcessCreator, ProcessInitializer},
        with_compiler::WithCompiler,
    },
};

/// A structure to hold various pieces of interpreter state, to be passed to
/// Efuns when they're called
#[derive(Debug)]
pub struct EfunContext<'task, const N: usize> {
    task_id: TaskId,
    stack: &'task mut CallStack<N>,
    task_context: &'task TaskContext,
    /// The caller task's transaction
    txn: TxnHandle,

    /// Allow the user to take a snapshot of the callstack, for testing and
    /// debugging
    #[cfg(test)]
    pub snapshot: Option<CallStack<N>>,
}

impl<'task, const N: usize> EfunContext<'task, N> {
    pub fn new(
        task_id: TaskId,
        stack: &'task mut CallStack<N>,
        task_context: &'task TaskContext,
    ) -> Self {
        Self {
            task_id,
            stack,
            task_context,
            // The efun runs inside the caller's transaction: it shares
            // the handle, so its reads/writes join the caller's attempt.
            txn: task_context.txn().clone(),

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

            // /// Get access to the [`Vm`](crate::interpreter::vm::Vm)'s upvalues (i.e. all of them)
            // #[call(upvalues)]
            // pub fn vm_upvalues(&self) -> &Arc<RwLock<GcRefBank>>;
            //
            // /// Get access to the [`Vm`](crate::interpreter::vm::Vm)'s call outs
            // pub fn call_outs(&self) -> &RwLock<CallOuts>;

            /// Get access to the `tx` channel, to talk to the [`Vm`](crate::interpreter::vm::Vm)
            pub fn tx(&self) -> Sender<VmOp>;
        }
    }

    pub fn with_call_outs<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&CallOuts) -> R,
    {
        self.task_context.with_call_outs(f)
    }

    pub fn with_call_outs_mut<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&mut CallOuts) -> R,
    {
        self.task_context.with_call_outs_mut(f)
    }

    /// Find or create (but don't initialize) an object by path.
    /// Transactional: a create writes the object's cell and records a deferred
    /// physical insert, so the object is usable within this transaction but
    /// physically visible only after commit.
    pub async fn create_object(&self, path: &LpcPath) -> Result<Arc<Process>> {
        if let ObjectLookup::Found(proc) = self.find_object(path) {
            return Ok(proc);
        }

        let debug_span = self.current_debug_span();
        let process = self.create_process_from_path(path).await.map_err(|mut e| {
            *e = e.with_span(debug_span);
            e
        })?;

        self.insert_process_transactional(&process);

        Ok(process)
    }

    /// Find or initialize an object by path.
    /// Transactional: a new object gets a cell write and deferred physical
    /// insert, and its initializer runs in a sub-task that joins this
    /// transaction's commit.
    pub async fn load_object(&self, path: &LpcPath) -> Result<Arc<Process>> {
        if let ObjectLookup::Found(proc) = self.find_object(path) {
            return Ok(proc);
        }

        let debug_span = self.current_debug_span();
        let process = self.create_process_from_path(path).await.map_err(|mut e| {
            *e = e.with_span(debug_span);
            e
        })?;

        self.insert_process_transactional(&process);
        self.init_process_transactional(&process)
            .await
            .map_err(|mut e| {
                let span = self.current_debug_span();
                *e = e.with_span(span);
                e
            })?;

        Ok(process)
    }

    /// Return the [`TaskId`] of the current `Task`
    #[inline]
    pub fn task_id(&self) -> TaskId {
        self.task_id
    }

    /// Get a reference to the current [`CallFrame`]
    #[inline]
    pub fn frame(&self) -> &CallFrame {
        self.stack.last().unwrap()
    }

    /// Place the passed `result` into the correct location to return from an efun.
    #[inline]
    pub fn return_efun_result(&mut self, result: LpcRef) {
        let frame = self.stack.last_mut().unwrap();
        frame.registers[0] = result;
    }

    /// Get the current debug span
    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        self.frame().current_debug_span()
    }

    /// Get the current debug span of the previous frame
    #[inline]
    pub fn previous_debug_span(&self) -> Option<Span> {
        if self.stack.len() > 1 {
            self.stack[self.stack.len() - 2].current_debug_span()
        } else {
            None
        }
    }

    /// A helper to generate an [`LpcError`] for runtime errors
    #[inline]
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        self.frame().runtime_error(msg)
    }

    /// A helper to generate an [`LpcError`] for runtime bugs
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

    /// The transaction this efun's task runs in. `pub(crate)`:
    /// `TxnHandle` is not part of the public API.
    #[inline]
    pub(crate) fn txn(&self) -> &TxnHandle {
        &self.txn
    }

    /// The object space this efun's task commits into.
    #[inline]
    pub(crate) fn object_space(&self) -> &ObjectSpace {
        self.task_context.object_space()
    }

    /// Record a physical side effect on this efun's attempt.
    pub(crate) fn record_effect(&self, effect: Effect) {
        self.txn.record_effect(effect);
    }

    /// Find an object by path, transactionally. Delegates to
    /// [`TaskContext::find_object`]; does not initialize or create.
    pub fn find_object(&self, path: &LpcPath) -> ObjectLookup {
        self.task_context.find_object(path)
    }

    /// Create a [`Process`] from `path`, compiling the file but *without*
    /// inserting it physically. The transactional caller performs the (cell +
    /// deferred-physical) insert, so the object is not physically visible
    /// until its transaction commits.
    pub async fn create_process_from_path(&self, path: &LpcPath) -> Result<Arc<Process>> {
        self.task_context.compile_process(path).await
    }

    /// Insert an object transactionally (cell write + deferred physical
    /// insert). Delegates to [`TaskContext::insert_process_transactional`].
    pub(crate) fn insert_process_transactional(&self, process: &Arc<Process>) {
        self.task_context.insert_process_transactional(process);
    }

    /// Initialize a newly created object in a sub-task that joins this
    /// transaction's commit. Mirrors the old `ProcessInitializer` path
    /// (insert *before* init, to prevent infinite loops, then run the
    /// initializer in a joined sub-task), but the insert is the transactional
    /// one, and the sub-task's writes ride this transaction's single commit.
    pub async fn init_process_transactional(&self, process: &Arc<Process>) -> Result<()> {
        let new_task_context = self
            .task_context_builder()
            .process(process.clone())
            .chain_count(self.chain_count() + 1)
            .build()
            .map_err(|e| -> Box<LpcError> {
                Box::new(LpcError::new_bug(format!(
                    "{e}: could not build sub-task context"
                )))
            })?;

        Task::<N>::initialize_sub_process(self.task_id(), new_task_context).await?;

        Ok(())
    }

    /// Convert the passed [`Program`] into a clone [`Process`], write its
    /// cell, and record a deferred physical insert. The clone is usable
    /// immediately (via the cell) but physically visible only after commit.
    pub fn insert_clone(&self, program: Arc<Program>) -> Arc<Process> {
        let process = self.object_space().create_clone_process(program);
        self.insert_process_transactional(&process);
        process
    }

    /// Remove the passed [`Process`] from the object space, transactionally:
    /// drop its cell (so it reads back as absent to this attempt, and a
    /// concurrent reader of the cell conflicts and re-runs) and record a
    /// deferred physical removal applied at commit.
    #[inline]
    pub fn remove_process<P>(&self, process: P)
    where
        P: Into<Arc<Process>>,
    {
        let process = process.into();
        let key = self.object_space().process_key(&process);
        let var_id = self.object_space().cell_id(&key);
        self.txn().with(|t| t.drop_var(var_id));
        self.record_effect(Effect::RemoveObject { key });
    }

    /// Resolve any RegisterVariant
    #[inline]
    pub fn resolve_register_variant(&self, variant: RegisterVariant) -> Result<Cow<'_, LpcRef>> {
        get_location(self.stack, self.txn(), variant)
    }

    /// Get a clone of the task context
    #[inline]
    pub fn task_context_builder(&self) -> TaskContextBuilder {
        TaskContextBuilder::from(self.task_context)
    }

    pub fn new_task_template(&self) -> TaskTemplate {
        TaskTemplate::from(self.task_context)
    }

    /// Get a reference to the [`Process`] that contains the call to this efun
    #[inline]
    pub fn process(&self) -> &Arc<Process> {
        &self.frame().process
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

impl<'task, const N: usize> From<&EfunContext<'task, N>> for TaskTemplate {
    fn from(value: &EfunContext<'task, N>) -> Self {
        TaskTemplate::from(value.task_context)
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

#[cfg(test)]
mod tests {
    use lpc_rs_core::{lpc_type::LpcType, register::Register};
    use lpc_rs_function_support::{
        function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunctionBuilder,
    };

    use super::*;
    use crate::{
        interpreter::{
            program::ProgramBuilder,
            vm::global_state::{GlobalState, GlobalStateBuilder},
        },
        test_support::test_config,
    };

    /// A fresh, uncommitted `EfunContext` whose call stack holds one real
    /// frame. The committer is spawned (as in the other efun fixtures) so
    /// effect flushing and cell mints behave as in a VM. `frame()` must
    /// resolve, because `create_object` records the caller's debug span.
    fn efun_context() -> (TaskContext, CallStack<10>) {
        let (tx, _rx) = tokio::sync::mpsc::channel::<VmOp>(128);
        let (committer_tx, committer_handle) = GlobalState::spawn_committer();
        let global_state = GlobalStateBuilder::default()
            .config(test_config())
            .tx(tx)
            .committer_tx(committer_tx)
            .committer_handle(Some(committer_handle))
            .build()
            .expect("global state builder");
        let upvalues = global_state.clone_upvalues();
        let program = ProgramBuilder::default()
            .filename(LpcPath::InGame("/caller".into()))
            .build()
            .expect("program builder");
        let process = Arc::new(Process::new(program));
        let task_context = TaskContextBuilder::default()
            .global_state(global_state)
            .process(process.clone())
            .build()
            .expect("task context builder");

        let function = ProgramFunctionBuilder::default()
            .prototype(
                FunctionPrototypeBuilder::default()
                    .name("efun_test")
                    .filename(Arc::new(LpcPath::InGame("/caller".into())))
                    .return_type(LpcType::Void)
                    .build()
                    .expect("prototype builder"),
            )
            .build()
            .expect("function builder");
        let frame = CallFrame::new(
            process,
            Arc::new(function),
            0 as RegisterSize,
            None::<&[Register]>,
            upvalues,
        );
        let mut stack = CallStack::default();
        stack.push(frame).expect("push entry frame");

        (task_context, stack)
    }

    // `destruct` + re-create of a prototype, repeated in one transaction. The
    // removal is deferred to commit, so until then the physical map and the
    // committed world both still hold the destructed process; every
    // re-create must yield a distinct process identity (no resurrection from
    // either source), and a find with no intervening destruct returns the
    // same identity again. Object identity is only observable in Rust
    // (LPC `==` compares the weak handles, not the processes; a prototype
    // has no clone id to compare), so this asserts `Arc` identity.
    #[tokio::test]
    async fn destruct_and_recreate_cycles_yield_fresh_objects() {
        let (task_context, mut stack) = efun_context();
        let ctx = EfunContext::new(TaskId::default(), &mut stack, &task_context);

        let path = LpcPath::new_in_game(
            "/example",
            task_context.in_game_cwd(),
            &*test_config().lib_dir,
        );

        // Baseline: create once, and a second find (no destruct in between)
        // returns the same object.
        let p1 = ctx.create_object(&path).await.expect("first create");
        let ObjectLookup::Found(p1b) = ctx.find_object(&path) else {
            panic!("second find, no destruct, should be Found");
        };
        assert!(
            Arc::ptr_eq(&p1, &p1b),
            "find without a destruct returns the same object"
        );

        // Cycle 1: destruct + re-create. The re-created object must be a
        // distinct identity, even though the physical map and committed world
        // still hold `p1` (the effects haven't landed).
        ctx.remove_process(p1.clone());
        let p2 = ctx
            .create_object(&path)
            .await
            .expect("re-create after destruct");
        assert!(
            !Arc::ptr_eq(&p1, &p2),
            "re-created object must not resurrect the destructed one"
        );

        // Cycle 2: the same sequence again; all three identities distinct.
        ctx.remove_process(p2.clone());
        let p3 = ctx.create_object(&path).await.expect("second re-create");
        assert!(
            !Arc::ptr_eq(&p2, &p3) && !Arc::ptr_eq(&p1, &p3),
            "second re-create must be a fresh object, distinct from both predecessors"
        );

        // And a plain find (no destruct) after the cycles returns the live
        // (most recent) object's identity.
        let ObjectLookup::Found(p3b) = ctx.find_object(&path) else {
            panic!("find after the cycles, should be Found");
        };
        assert!(
            Arc::ptr_eq(&p3, &p3b),
            "find returns the live object's identity"
        );
    }
}
