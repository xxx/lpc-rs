pub mod apply_function;
pub mod eval_loop;
mod handle_call;
mod handle_call_fp;
mod handle_call_other;
mod handle_data;
pub mod initialize_program;
pub mod into_task_context;
mod location;
pub mod task_id;
pub mod task_state;
pub mod task_template;

#[cfg(test)]
mod tests;

use std::{
    fmt::{Debug, Display},
    sync::{Arc, Weak},
    time::Duration,
};

use async_recursion::async_recursion;
use bit_set::BitSet;
use educe::Educe;
pub(crate) use location::{apply_in_location, get_location, get_location_in_frame, set_location};
use lpc_rs_asm::address::Address;
use lpc_rs_core::{
    LpcIntInner, RegisterSize,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use parking_lot::RwLock;
use string_interner::{DefaultSymbol, Symbol};
use thin_vec::{ThinVec, thin_vec};
use tokio::{task::JoinHandle, time::timeout};
use tracing::{error, instrument, trace, warn};

use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    gc::{gc_bank::GcVarIdBank, mark::Mark},
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
    lpc_string::LpcString,
    object_flags::ObjectFlags,
    process::Process,
    program::Program,
    stm::{
        AttemptBody, CommitProtocol, Effect, LiveSnapshot, RetryStats, Transaction, TxnHandle,
        commit_changeset, flush_effects, run_attempts, start_txn,
    },
    task::{task_id::TaskId, task_state::TaskState},
    task_context::TaskContext,
    vm::global_state::GlobalState,
};

// this is just to shut clippy up
type ProcessFunctionPair = (Weak<Process>, Arc<ProgramFunction>);

#[macro_export]
macro_rules! pop_frame {
    ($task:expr) => {{
        let opt = $task.pop_frame();
        if let Some(ref frame) = opt {
            $task.stack.copy_result(&frame)?;

            if $task.stack.is_empty() {
                $task.context.set_result(frame.registers[0].clone())?;
            }
        }

        opt
    }};
}

/// A type to track where `catch` calls need to go if there is an error
#[derive(Debug, Clone)]
struct CatchPoint {
    /// The index of the stack frame that contains this `catch`
    frame_index: usize,

    /// The address to jump in the current function, if there is an error
    address: Address,

    /// The register to put the error in, within the above [`StackFrame`]
    register: RegisterVariant,
}

/// The inputs needed to (re)start a task's entry call, hoisted out of the
/// `CallStack` so a rejected commit can rebuild the task from scratch.
#[derive(Debug, Clone)]
pub struct TaskSeed {
    pub process: Arc<Process>,
    pub function: Arc<ProgramFunction>,
    pub args: Vec<LpcRef>,
}

impl TaskSeed {
    /// Build the entry [`CallFrame`] for one attempt, copying `self.args`
    /// into the frame's registers exactly as `prepare_function_call` does.
    pub(crate) fn build_call_frame(
        &self,
        upvalue_ptrs: Option<&[Register]>,
        vm_upvalues: Arc<RwLock<GcVarIdBank>>,
    ) -> Result<CallFrame> {
        let mut frame = CallFrame::new(
            self.process.clone(),
            self.function.clone(),
            RegisterSize::try_from(self.args.len())?,
            upvalue_ptrs,
            vm_upvalues,
        );
        if !self.args.is_empty() {
            frame.registers[1..=self.args.len()].clone_from_slice(&self.args);
        }
        Ok(frame)
    }
}

/// An abstraction to allow for isolated running to completion of a specified
/// function. It represents a single thread of execution
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct Task<const STACKSIZE: usize> {
    pub id: TaskId,

    /// The call stack
    pub stack: CallStack<STACKSIZE>,

    /// Stack of [`CatchPoint`]s
    catch_points: ThinVec<CatchPoint>,

    /// The arg vector, populated prior to executing any of the `Call`-family [`Instruction`](lpc_rs_asm::instruction::Instruction)s
    pub args: ThinVec<RegisterVariant>,

    /// The vector used to collect arguments when creating a partially-applied function pointer
    pub partial_args: ThinVec<Option<RegisterVariant>>,

    /// The vector used to collect members of a soon-to-be-created array
    array_items: ThinVec<RegisterVariant>,

    /// The context of this task
    pub context: TaskContext,

    /// This task's transaction handle. One top-level task = one
    /// transaction; nested sub-tasks join it by cloning the handle.
    /// Re-based onto a fresh transaction per attempt.
    pub(crate) txn: TxnHandle,

    /// True when `txn` was adopted from a caller's live attempt: this task
    /// joins it and must never open, re-base, or commit a transaction of
    /// its own. Decided at construction — a top-level task's own attempts
    /// re-base its handle onto a joinable one, so the handle's state can't
    /// be the branch key on retry.
    joins_parent: bool,

    /// The seed for this task's attempts: the function entry each re-run
    /// rebuilds from. `None` for sub-tasks; set by the top-level entry
    /// points.
    seed: Option<TaskSeed>,

    /// The per-attempt execution timeout, set by the top-level entry point.
    timeout_ms: Option<u64>,

    /// The current state of the task
    pub state: TaskState,

    /// Store the most recently popped frame, for testing
    #[cfg(test)]
    pub popped_frame: Option<CallFrame>,

    /// Store a snapshot of a specific state, for testing
    #[cfg(test)]
    pub snapshots: ThinVec<CallStack<STACKSIZE>>,
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Create a new Task
    #[instrument(skip_all)]
    pub fn new(task_context: TaskContext) -> Self {
        Self::new_sub_task(TaskId::new(), task_context)
    }

    /// Create a new [`Task`], as a subtask of the given [`TaskId`].
    /// A subtask should _never_ execute simultaneously with any other Task with
    /// the same [`TaskId`], as that can lead to deadlocks.
    pub fn new_sub_task(parent_id: TaskId, task_context: TaskContext) -> Self {
        // Adopt the caller's transaction handle: a sub-task joins its
        // caller's transaction, not a fresh one. A
        // joinable handle is a live attempt; the empty default is not,
        // so this also distinguishes top-level tasks.
        let txn = task_context.txn().clone();
        let joins_parent = txn.joinable();
        Self {
            id: parent_id,
            stack: CallStack::default(),
            catch_points: thin_vec![],
            args: ThinVec::with_capacity(4),
            partial_args: thin_vec![],
            array_items: ThinVec::with_capacity(10),
            context: task_context,
            txn,
            joins_parent,
            seed: None,
            timeout_ms: None,
            state: TaskState::New,

            #[cfg(test)]
            popped_frame: None,

            #[cfg(test)]
            snapshots: thin_vec![],
        }
    }

    /// Rebuild the task to a blank slate for a retry re-run
    fn reset(&mut self) {
        self.stack = CallStack::default();
        self.catch_points.clear();
        self.args.clear();
        self.partial_args.clear();
        self.array_items.clear();
        self.context.reset();
        // A joiner keeps the parent's live handle (its run defers to the
        // parent's commit); a top-level task resets to an empty handle,
        // which `open_attempt` re-bases onto a fresh attempt.
        if !self.joins_parent {
            self.txn = TxnHandle::empty();
        }
        self.state = TaskState::New;

        #[cfg(test)]
        {
            self.popped_frame = None;
        }
    }

    /// Convenience helper to get a Program initialized.
    /// This will also insert it into the object space.
    #[instrument(skip_all)]
    pub async fn initialize_program<P>(
        program: P,
        global_state: Arc<GlobalState>,
        this_player: Option<Arc<Process>>,
        upvalue_ptrs: Option<&[Register]>,
    ) -> Result<Task<STACKSIZE>>
    where
        P: Into<Arc<Program>>,
    {
        let program = program.into();
        let process: Arc<Process> = Process::new(program).into();
        let context = TaskContext::new(
            global_state,
            process.clone(),
            this_player,
            upvalue_ptrs.map(ThinVec::from),
        );

        context.insert_process(process);

        Self::initialize_process(context).await
    }

    /// Initialize a [`Process`] by calling its initializer function, using the
    /// given [`TaskContext`]. This creates a new unique Task ID.
    /// It's assumed that the process has already been inserted into the [`ObjectSpace`](crate::interpreter::object_space::ObjectSpace)
    pub async fn initialize_process(context: TaskContext) -> Result<Task<STACKSIZE>> {
        Self::initialize_sub_process(TaskId::new(), context).await
    }

    /// Initialize a [`Process`] by calling its initializer function, using the
    /// given [`TaskContext`], using the specified Task ID.
    /// It's assumed that the process has already been inserted into the [`ObjectSpace`](crate::interpreter::object_space::ObjectSpace)
    pub async fn initialize_sub_process(
        task_id: TaskId,
        context: TaskContext,
    ) -> Result<Task<STACKSIZE>> {
        debug_assert!(!context.process.flags.test(ObjectFlags::Initialized));

        let Some(initializer) = context.process.program.initializer.clone() else {
            let msg = "Init function not found on cloned object? This should never happen.";

            error!(msg);
            return Err(lpc_bug!(msg));
        };

        // We mark ourselves as initialized before actually initializing, to avoid
        // infinite loops where this_object() is used in global initialization.
        // this flag is physical world state, so a rejected
        // commit (init rolled back) leaves it set and a re-run skips re-init.
        context.process.flags.set(ObjectFlags::Initialized);

        let max_execution_time = context.config().max_execution_time;
        let mut task = Task::new_sub_task(task_id, context);
        task.timed_eval(initializer, &[], max_execution_time)
            .await?;

        Ok(task)
    }

    /// Spawn a new tokio task to evaluate `f` to completion, or an error, with timeout.
    pub async fn spawn_eval<const N: usize>(
        mut task: Task<N>,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
    ) -> JoinHandle<Result<Task<N>>> {
        let args = args.to_vec();

        tokio::spawn(async move {
            let max_execution_time = task.context.config().max_execution_time;
            match task.timed_eval(f, &args, max_execution_time).await {
                Ok(_) => Ok(task),
                Err(e) => Err(e),
            }
        })
    }

    /// Run one attempt: open a transaction against the current world, and
    /// run to completion (one timeout, if given). Returns the attempt's
    /// [`LiveSnapshot`] (release it only after the commit reply resolves),
    /// or `None` for a joiner.
    async fn open_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        timeout_ms: Option<u64>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = if self.joins_parent {
            None
        } else {
            Some(start_txn(tx).await?)
        };

        let seed = self
            .seed
            .clone()
            .expect("top-level attempt has a seed; joiners never open");

        self.reset();

        let frame = seed.build_call_frame(
            self.context.upvalue_ptrs.as_deref(),
            self.context.global_state.clone_upvalues(),
        )?;
        self.stack.push(frame)?;

        // Re-base both the task's handle and the context's handle onto
        // this attempt's fresh transaction (same `Arc`): contexts derived
        // from this one (sub-task spawn sites) see the live attempt. A
        // joiner keeps the adopted parent handle instead.
        if let Some(live) = &live {
            self.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        }
        self.context.txn = self.txn.clone();

        // One timeout per attempt; the committer's conflict rule is the sole
        // serialization control.
        let outcome = match timeout_ms {
            Some(ms) => timeout(Duration::from_millis(ms), self.resume()).await,
            None => Ok(self.resume().await),
        };
        let run_result = match outcome {
            Ok(run) => run,
            Err(_) => {
                return Err(lpc_error!(
                    "evaluation limit of {}ms has been reached",
                    timeout_ms.unwrap_or(0)
                ));
            }
        };

        if let Err(e) = run_result {
            // A failed run holds nothing the committer needs.
            drop(live);
            return Err(e);
        }

        Ok(live)
    }

    /// One task's attempts through the shared runner
    /// ([`stm::run_attempts`]): timeout and effect flush stay here; the
    /// committer protocol lives in the runner.
    async fn retry_loop(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        timeout_ms: Option<u64>,
    ) -> (Result<()>, RetryStats) {
        self.timeout_ms = timeout_ms;
        run_attempts(tx, self).await
    }

    /// Evaluate `f` to completion, or an error. No timeouts are applied.
    #[instrument(skip_all)]
    pub async fn eval(&mut self, f: Arc<ProgramFunction>, args: &[LpcRef]) -> Result<()> {
        self.timed_eval(f, args, 0).await
    }

    /// Evaluate `f` to completion, or an error, with a timeout.
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn timed_eval(
        &mut self,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
        timeout_ms: u64,
    ) -> Result<()> {
        let loop_timeout = {
            if timeout_ms == 0 {
                None
            } else {
                Some(timeout_ms)
            }
        };
        let seed = TaskSeed {
            process: self.context.process().clone(),
            function: f,
            args: args.to_vec(),
        };
        self.seed = Some(seed);
        let tx = self.context.global_state.committer_tx.clone();
        let (res, stats) = self.retry_loop(&tx, loop_timeout).await;
        trace!(
            attempts = stats.attempts,
            conflicts = stats.conflicts,
            ?stats.duration,
            "task timed_eval finished"
        );
        res
    }

    /// Evaluate `f` to completion, or an error, in the context of an arbitrary process
    #[async_recursion]
    pub async fn eval_function(
        &mut self,
        process: Arc<Process>,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
    ) -> Result<()> {
        self.prepare_function_call(process, f, args).await?;

        self.resume().await
    }

    /// Prepare to call a function. This is intended to be used when a Task is first created and enqueued.
    #[instrument(skip_all)]
    async fn prepare_function_call(
        &mut self,
        process: Arc<Process>,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
    ) -> Result<()> {
        let mut frame = CallFrame::new(
            process,
            f,
            RegisterSize::try_from(args.len())?,
            self.context.upvalue_ptrs.as_deref(),
            self.context.global_state.clone_upvalues(),
        );

        // TODO: This is probably not correct. See behavior in prepare_new_call_frame
        if !args.is_empty() {
            frame.registers[1..=args.len()].clone_from_slice(args);
        }

        self.stack.push(frame)
    }

    /// Set the state to handle a caught error.
    /// Panics if there aren't actually any catch points.
    #[instrument(skip_all)]
    fn catch_error(&mut self, error: Box<LpcError>) -> Result<()> {
        let catch_point = self.catch_points.last().unwrap();
        let result_index = catch_point.register.index();
        let frame_index = catch_point.frame_index;
        let new_pc = catch_point.address;

        let truncate_len = frame_index + 2;

        // clear away stack frames that won't be executed any further, which lie between
        // the error and the catch point's stack frame.
        // Does nothing if you're already in the correct stack frame, or one away.
        // The +2 is because truncate takes a length, not an index.
        self.stack.truncate(truncate_len);

        // If these aren't equal, we're already in the correct stack frame.
        // That only happens when the stack has one frame.
        if self.stack.len() == truncate_len {
            // Pop the final frame via pop_frame(), to keep other state changes to a single
            // code path, (e.g. changing the current process)
            self.pop_frame();
        }

        if self.stack.is_empty() {
            return Err(self.runtime_bug("stack is empty after popping to catch point?"));
        }

        // set up the catch point's return value
        let value = LpcString::from(error.to_string());
        let lpc_ref = value.into();
        set_location(
            &mut self.stack,
            &self.txn,
            Register(result_index).as_local(),
            lpc_ref,
        )?;
        let frame = self.stack.current_frame_mut()?;

        // jump to the corresponding catchend instruction
        frame.set_pc(new_pc);

        Ok(())
    }

    #[instrument(skip_all)]
    fn binary_operation<F>(
        &mut self,
        r1: RegisterVariant,
        r2: RegisterVariant,
        r3: RegisterVariant,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &LpcRef, &TxnHandle) -> Result<LpcRef>,
    {
        let ref1 = &*get_location(&self.stack, &self.txn, r1)?;
        let ref2 = &*get_location(&self.stack, &self.txn, r2)?;

        match operation(ref1, ref2, &self.txn) {
            Ok(result) => {
                set_location(&mut self.stack, &self.txn, r3, result)?;
            }
            Err(mut e) => {
                let frame = self.stack.current_frame()?;
                *e = e.with_span(frame.current_debug_span());
                return Err(e);
            }
        }

        Ok(())
    }

    /// Binary operations that return a boolean value (e.g. comparisons)
    #[instrument(skip_all)]
    fn binary_boolean_operation<F>(
        &mut self,
        r1: RegisterVariant,
        r2: RegisterVariant,
        r3: RegisterVariant,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &LpcRef) -> bool,
    {
        let ref1 = &*get_location(&self.stack, &self.txn, r1)?;
        let ref2 = &*get_location(&self.stack, &self.txn, r2)?;

        let out = operation(ref1, ref2) as LpcIntInner;

        set_location(&mut self.stack, &self.txn, r3, LpcRef::Int(LpcInt(out)))
    }

    /// convenience helper to generate runtime errors
    #[inline]
    fn runtime_error<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        self.stack.runtime_error(msg)
    }

    /// convenience helper to generate runtime bugs
    #[inline]
    fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> Box<LpcError> {
        self.stack.runtime_bug(msg)
    }

    #[inline]
    fn array_index_error<T>(&self, index: T, length: usize) -> Box<LpcError>
    where
        T: Display,
    {
        self.runtime_error(format!(
            "Attempting to access index {index} in an array of length {length}"
        ))
    }
    /// Pop the top frame from the stack, and return it.
    /// Use the `pop_frame!` macro instead for most uses.
    #[inline]
    #[allow(clippy::let_and_return)]
    fn pop_frame(&mut self) -> Option<CallFrame> {
        let frame = self.stack.pop();

        #[cfg(test)]
        {
            self.popped_frame = frame.clone();
        }

        frame
    }

    // /// Negotiate how much space needs to be made for a call to a function pointer.
    // ///
    // /// # Arguments
    // ///
    // /// num_args: the number of arguments actually passed to the function for this call
    // /// partial_args: the arguments that were passed to the function when the function pointer was created
    // ///
    // /// # Returns
    // ///
    // /// The maximum number of arguments that space needs to be made for.
    // #[instrument(skip_all)]
    // #[inline]
    // fn calculate_max_arg_length<T>(num_args: usize, partial_args: &[Option<T>]) -> usize {
    //     let none_args = partial_args.iter().filter(|a| a.is_none()).count();
    //     partial_args.len() + num_args.saturating_sub(none_args)
    // }

    #[inline]
    fn index_symbol(index: RegisterSize) -> DefaultSymbol {
        DefaultSymbol::try_from_usize(index as usize).unwrap()
    }

    #[inline]
    pub fn result(&self) -> Option<LpcRef> {
        self.context.result()
    }

    /// `#[cfg(test)]` entry point: run against an explicit committer (used by
    /// the synthetic-abort test to inject a rejection) and return the stats.
    /// `pub(crate)`: only the crate's own tests call it, and `RetryStats` is
    /// `pub(crate)`, so a `pub` item would leak a more-private type.
    #[cfg(test)]
    pub(crate) async fn eval_with_committer(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        seed: &TaskSeed,
    ) -> (Result<()>, RetryStats) {
        self.seed = Some(seed.clone());
        self.retry_loop(tx, None).await
    }
}

#[async_trait::async_trait]
impl<const STACKSIZE: usize> AttemptBody for Task<STACKSIZE> {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        self.open_attempt(tx, self.timeout_ms).await
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(
        std::result::Result<(), crate::interpreter::stm::Changeset>,
        Vec<Effect>,
    )> {
        // Clone the attempt's transaction out of the handle; the task
        // keeps its copy for the next re-run.
        let (_world, changeset) = self.txn.with(|t| t.clone()).into_parts();
        let commit = commit_changeset(tx, changeset).await?;
        let effects = self.txn.take_effects();
        Ok((commit, effects))
    }

    async fn deliver(&mut self, effects: Vec<Effect>) -> Result<()> {
        if !effects.is_empty() {
            flush_effects(
                self.context.config(),
                self.context.object_space(),
                self.context.global_state.call_outs(),
                effects,
            )
            .await;
        }
        Ok(())
    }
}

impl<const STACKSIZE: usize> Mark for Task<STACKSIZE> {
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        self.stack.mark(marked, processed)?;

        // Uncommitted (and committed-through-us) values in the in-flight
        // changeset are roots: they may not exist anywhere else yet.
        self.txn.with(|t| {
            for value in t.written_values() {
                value.mark(marked, processed)?;
            }
            Ok(())
        })
    }
}

// impl<const STACKSIZE: usize> Drop for Task<STACKSIZE> {
//     fn drop(&mut self) {
//         self.context.process.lock.try_release(self.id);
//     }
// }

#[cfg(test)]
mod stm_retry_tests {
    use indoc::indoc;
    use lpc_rs_utils::debug_log::DebugLog;

    use super::*;
    use crate::{
        compile_time_config::MAX_CALL_STACK_SIZE,
        interpreter::{
            lpc_int::LpcInt,
            stm::{CommitProtocol, Committer},
        },
        test_support::{run_prog, run_prog_with_config},
    };

    /// Run the program's `foo()` through the retry loop against `tx`.
    async fn eval_foo(
        task: &mut Task<MAX_CALL_STACK_SIZE>,
        tx: &flume::Sender<CommitProtocol>,
    ) -> (Result<()>, RetryStats) {
        let process = task.context.process().clone();
        let f = process
            .program
            .unmangled_functions
            .get("foo")
            .cloned()
            .expect("program should define foo()");
        let seed = TaskSeed {
            process,
            function: f,
            args: Vec::new(),
        };
        task.eval_with_committer(tx, &seed).await
    }

    const CODE: &str = indoc! { r##"
            int foo() { return 10; }
        "##};

    macro_rules! assert_result_is_ten {
        ($task:expr) => {
            let LpcRef::Int(LpcInt(v)) = $task.result().expect("result should be set") else {
                panic!("result is not an int");
            };
            assert_eq!(v, 10, "foo() must return 10");
        };
    }

    #[tokio::test]
    async fn clean_run_commits_in_one_attempt() {
        let mut task = run_prog(CODE).await;
        let (tx, rx) = flume::bounded(4);
        let committer_tx = tx.clone(); // keep `tx` for the final `Close`
        let handle = std::thread::spawn(move || Committer::new().run(committer_tx, rx));

        let (res, stats) = eval_foo(&mut task, &tx).await;
        assert!(res.is_ok());
        assert_eq!(stats.attempts, 1);
        assert_eq!(stats.conflicts, 0);
        assert_result_is_ten!(task);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let _ = handle.join();
    }

    #[tokio::test]
    async fn task_reruns_to_same_result_after_synthetic_abort() {
        let mut task = run_prog(CODE).await;
        // A committer that rejects the FIRST commit: the task's only commit
        // in a clean run. With the transaction empty, the rejection can only
        // come from this hook, so the test isolates the re-run path.
        let (tx, rx) = flume::bounded(4);
        let committer_tx = tx.clone(); // keep `tx` for the final `Close`
        let handle = std::thread::spawn(move || {
            let committer = Committer::new();
            committer.run_with_rejections(committer_tx, rx, 1)
        });

        let (res, stats) = eval_foo(&mut task, &tx).await;
        assert!(res.is_ok());
        assert_eq!(stats.attempts, 2, "first commit rejected, re-run commits");
        assert_eq!(stats.conflicts, 1);
        // The load-bearing assertion: the re-run from the seed produced the
        // same result as a clean run (see clean_run_commits_in_one_attempt).
        assert_result_is_ten!(task);

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let _ = handle.join();
    }

    /// Forwards bytes written to it (via `AsyncWriteExt::write_all`) to a
    /// channel, so a test can read exactly what a [`DebugLog`] emitted.
    struct CapturingWriter(tokio::sync::mpsc::Sender<Vec<u8>>);

    impl tokio::io::AsyncWrite for CapturingWriter {
        fn poll_write(
            self: std::pin::Pin<&mut Self>,
            _cx: &mut std::task::Context<'_>,
            buf: &[u8],
        ) -> std::task::Poll<std::io::Result<usize>> {
            let _ = self.get_mut().0.try_send(buf.to_vec());
            std::task::Poll::Ready(Ok(buf.len()))
        }

        fn poll_flush(
            self: std::pin::Pin<&mut Self>,
            _cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<std::io::Result<()>> {
            std::task::Poll::Ready(Ok(()))
        }

        fn poll_shutdown(
            self: std::pin::Pin<&mut Self>,
            _cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<std::io::Result<()>> {
            std::task::Poll::Ready(Ok(()))
        }
    }

    /// A retried task's physical output must fire exactly once: the
    /// first attempt is rejected (and its recorded output discarded), the
    /// re-run records it again, and only the committed attempt's output is
    /// delivered.
    #[tokio::test]
    async fn retried_task_output_emits_exactly_once() {
        use lpc_rs_utils::config::ConfigBuilder;

        const CODE: &str = indoc! { r##"
                int foo() {
                    write("one\n");
                    return 10;
                }
            "##};

        let (out_tx, mut out_rx) = tokio::sync::mpsc::channel::<Vec<u8>>(16);
        let log = DebugLog::new(CapturingWriter(out_tx));
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .simul_efun_file("/secure/simul_efuns")
            .debug_log(log)
            .build()
            .unwrap();

        let mut task = run_prog_with_config(CODE, std::sync::Arc::new(config)).await;

        // A committer that rejects the FIRST commit, so `foo()` runs twice:
        // attempt 1 records its output then is discarded, attempt 2 commits.
        let (tx, rx) = flume::bounded(4);
        let committer_tx = tx.clone();
        let handle = std::thread::spawn(move || {
            let committer = Committer::new();
            committer.run_with_rejections(committer_tx, rx, 1)
        });

        let (res, stats) = eval_foo(&mut task, &tx).await;
        assert!(res.is_ok(), "task failed: {:?}", res);
        assert_eq!(stats.attempts, 2, "first commit rejected, re-run commits");
        assert_eq!(stats.conflicts, 1);

        // The load-bearing assertion: the debug log received the write
        // exactly once, despite the task running to completion twice. The
        // drain is bounded: the flush happened before `eval_foo` returned,
        // so the messages are already buffered; the receiver's EOF would
        // only come when the task's `GlobalState` drops, which we don't do.
        let mut seen = Vec::new();
        while let Ok(Some(chunk)) =
            tokio::time::timeout(Duration::from_millis(100), out_rx.recv()).await
        {
            seen.extend(chunk);
        }
        let got = String::from_utf8(seen).expect("utf-8 output");
        assert_eq!(
            got, "one\n",
            "aborted attempt's output must not be delivered; the committed \
            attempt's output must be delivered exactly once"
        );

        tx.send(CommitProtocol::Close)
            .expect("committer channel closed");
        drop(tx);
        let _ = handle.join();
    }
}
