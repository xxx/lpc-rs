pub mod apply_function;
pub mod eval_loop;
mod handle_call;
mod handle_call_fp;
mod handle_call_other;
mod handle_data;
mod location;
pub mod task_template;

#[cfg(test)]
mod tests;

use std::{
    fmt::{Debug, Display},
    sync::Arc,
    time::Duration,
};

use async_recursion::async_recursion;
use educe::Educe;
pub(crate) use location::{bump_in_location, get_location, set_location};
use lpc_rs_asm::address::Address;
use lpc_rs_core::{
    LpcIntInner, RegisterSize,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use thin_vec::{ThinVec, thin_vec};
use tokio::time::timeout;
use tracing::{error, instrument, warn};

#[cfg(test)]
use crate::interpreter::stm::RetryStats;
use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
    lpc_string::LpcString,
    process::Process,
    stm::{
        AttemptBody, CommitProtocol, Effect, LiveSnapshot, Transaction, TxnHandle, VarId,
        commit_changeset, flush_effects, run_attempts, start_txn,
    },
    task_context::TaskContext,
};

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
    /// An initializer run: each attempt claims the marker first and is a
    /// no-op when it is already held.
    pub initializes: bool,
}

impl TaskSeed {
    /// Build the entry [`CallFrame`] for one attempt; `self.args` land in
    /// registers `1..=len`. This path (`call_other`, process init) can only
    /// ever seed plain values, so a `ref` parameter here is always a refusal.
    pub(crate) fn build_call_frame(&self, upvalue_ptrs: Option<&[VarId]>) -> Result<CallFrame> {
        if let Some(i) = self.function.prototype.first_ref_param() {
            return Err(LpcError::runtime(format!(
                "argument {} of `{}` must be passed by reference",
                i + 1,
                self.function.name()
            )));
        }

        let mut frame = CallFrame::new(
            self.process.clone(),
            self.function.clone(),
            RegisterSize::try_from(self.args.len())?,
            upvalue_ptrs,
        );
        if !self.args.is_empty() {
            // `Bank`'s own `Index` impls stop the indexing autoderef.
            (*frame.registers)[1..=self.args.len()].clone_from_slice(&self.args);
        }
        Ok(frame)
    }
}

/// One argument of the pending call: a value to copy into the callee, or
/// a cell the callee aliases.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Arg {
    /// A value, copied into the callee's own register.
    Value(RegisterVariant),
    /// A cell the callee aliases in place of minting its own.
    Ref(RegisterVariant),
}

/// An abstraction to allow for isolated running to completion of a specified
/// function. It represents a single thread of execution
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct Task<const STACKSIZE: usize> {
    /// The call stack
    pub stack: CallStack<STACKSIZE>,

    /// Stack of [`CatchPoint`]s
    catch_points: ThinVec<CatchPoint>,

    /// The arg vector, populated prior to executing any of the `Call`-family [`Instruction`](lpc_rs_asm::instruction::Instruction)s
    pub args: ThinVec<Arg>,

    /// The vector used to collect arguments when creating a partially-applied function pointer
    pub partial_args: ThinVec<Option<RegisterVariant>>,

    /// The vector used to collect members of a soon-to-be-created array
    array_items: ThinVec<RegisterVariant>,

    /// The context of this task; its `txn` is the transaction this task runs in.
    pub context: TaskContext,

    /// True when the handle was adopted from a caller's live attempt: this task
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

    /// The most recently popped frame other than the initializer's driver
    /// frame, for testing.
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
        // A joinable handle is a caller's live attempt; the empty default is not.
        let joins_parent = task_context.txn().joinable();
        Self {
            stack: CallStack::default(),
            catch_points: thin_vec![],
            args: ThinVec::with_capacity(4),
            partial_args: thin_vec![],
            array_items: ThinVec::with_capacity(10),
            context: task_context,
            joins_parent,
            seed: None,
            timeout_ms: None,

            #[cfg(test)]
            popped_frame: None,

            #[cfg(test)]
            snapshots: thin_vec![],
        }
    }

    /// Rebuild the task to a blank slate for a retry re-run
    fn reset(&mut self) {
        // Cleared, not replaced: the ~3KB boxed stack survives the retry.
        self.stack.clear();
        self.catch_points.clear();
        self.args.clear();
        self.partial_args.clear();
        self.array_items.clear();
        self.context.reset();

        #[cfg(test)]
        {
            self.popped_frame = None;
        }
    }

    /// Initialize a [`Process`] by calling its initializer function, using the
    /// given [`TaskContext`].
    /// It's assumed that the process has already been inserted into the [`ObjectSpace`](crate::interpreter::object_space::ObjectSpace).
    /// The task is returned unevaluated when another transaction already
    /// initialized the process.
    pub async fn initialize_process(context: TaskContext) -> Result<Task<STACKSIZE>> {
        let Some(initializer) = context.process.program.initializer.clone() else {
            let msg = format!(
                "Init function not found for `{}`. This should never happen.",
                context.process.filename()
            );

            error!("{msg}");
            return Err(lpc_bug!("{}", msg));
        };

        let max_execution_time = context.config().max_execution_time;
        let mut task = Task::new(context);
        let seed = TaskSeed {
            process: task.context.process().clone(),
            function: initializer,
            args: Vec::new(),
            initializes: true,
        };
        task.timed_eval_seed(seed, max_execution_time).await?;

        Ok(task)
    }

    /// Open a transaction against the current world and run one attempt to
    /// completion under `self.timeout_ms`. Returns the attempt's
    /// [`LiveSnapshot`], to be released only after the commit reply, or
    /// `None` for a joiner.
    async fn open_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        let live = if self.joins_parent {
            None
        } else {
            Some(start_txn(tx).await?)
        };

        if let Some(live) = &live {
            self.context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
            self.context.clear_commands();
        }

        let seed = self
            .seed
            .clone()
            .expect("top-level attempt has a seed; joiners never open");

        self.reset();

        // Claimed inside the attempt, so a rejected initialization re-runs
        // instead of staying marked.
        if seed.initializes && !seed.process.claim_init(&self.context.txn) {
            return Ok(live);
        }

        let frame = seed.build_call_frame(self.context.upvalue_ptrs.as_deref())?;
        self.stack.push(frame)?;

        // One timeout per attempt; the committer's conflict rule is the sole
        // serialization control.
        let outcome = match self.timeout_ms {
            Some(ms) => timeout(Duration::from_millis(ms), self.resume()).await,
            None => Ok(self.resume().await),
        };
        let run_result = match outcome {
            Ok(run) => run,
            Err(_) => {
                return Err(lpc_error!(
                    "evaluation limit of {}ms has been reached",
                    self.timeout_ms.unwrap_or(0)
                )
                .with_stack_trace(self.stack.stack_trace()));
            }
        };

        if let Err(e) = run_result {
            // A failed run holds nothing the committer needs.
            drop(live);
            return Err(e);
        }

        Ok(live)
    }

    /// Evaluate `f` to completion, or an error; a `timeout_ms` of 0 means
    /// no timeout.
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn timed_eval(
        &mut self,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
        timeout_ms: u64,
    ) -> Result<()> {
        let seed = TaskSeed {
            process: self.context.process().clone(),
            function: f,
            args: args.to_vec(),
            initializes: false,
        };
        self.timed_eval_seed(seed, timeout_ms).await
    }

    /// Run `seed` to completion through the committer's retry loop, or an
    /// error; a `timeout_ms` of 0 means no timeout.
    #[async_recursion]
    async fn timed_eval_seed(&mut self, seed: TaskSeed, timeout_ms: u64) -> Result<()> {
        self.timeout_ms = (timeout_ms != 0).then_some(timeout_ms);
        self.seed = Some(seed);
        let tx = self.context.global_state.committer_tx.clone();
        let telemetry = self.context.global_state.attempt_telemetry.clone();
        let commit_watch = self.context.global_state.commit_watch.clone();
        let (res, _) = run_attempts(&tx, &telemetry, Some(commit_watch), self).await;
        res
    }

    /// Set the state to handle a caught error.
    /// Panics if there aren't actually any catch points.
    #[instrument(skip_all)]
    fn catch_error(&mut self, error: LpcError) -> Result<()> {
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
            &self.context.txn,
            Register(result_index).as_local(),
            lpc_ref,
        )?;
        let frame = self.stack.current_frame_mut()?;

        // jump to the corresponding catchend instruction
        frame.set_pc(new_pc);

        Ok(())
    }

    #[instrument(level = "debug", skip_all)]
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
        let ref1 = &*get_location(&self.stack, &self.context.txn, r1)?;
        let ref2 = &*get_location(&self.stack, &self.context.txn, r2)?;

        match operation(ref1, ref2, &self.context.txn) {
            Ok(result) => {
                set_location(&mut self.stack, &self.context.txn, r3, result)?;
            }
            Err(e) => {
                let frame = self.stack.current_frame()?;
                return Err(e.or_span(frame.current_debug_span()));
            }
        }

        Ok(())
    }

    #[instrument(level = "debug", skip_all)]
    fn unary_operation<F>(
        &mut self,
        r1: RegisterVariant,
        r2: RegisterVariant,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &TxnHandle) -> Result<LpcRef>,
    {
        let ref1 = &*get_location(&self.stack, &self.context.txn, r1)?;

        match operation(ref1, &self.context.txn) {
            Ok(result) => {
                set_location(&mut self.stack, &self.context.txn, r2, result)?;
            }
            Err(e) => {
                let frame = self.stack.current_frame()?;
                return Err(e.or_span(frame.current_debug_span()));
            }
        }

        Ok(())
    }

    /// Binary operations that return a boolean value (e.g. comparisons)
    #[instrument(level = "debug", skip_all)]
    fn binary_boolean_operation<F>(
        &mut self,
        r1: RegisterVariant,
        r2: RegisterVariant,
        r3: RegisterVariant,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &LpcRef, &TxnHandle) -> bool,
    {
        let ref1 = &*get_location(&self.stack, &self.context.txn, r1)?;
        let ref2 = &*get_location(&self.stack, &self.context.txn, r2)?;

        let out = operation(ref1, ref2, &self.context.txn) as LpcIntInner;

        set_location(
            &mut self.stack,
            &self.context.txn,
            r3,
            LpcRef::Int(LpcInt(out)),
        )
    }

    /// convenience helper to generate runtime errors
    #[inline]
    fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        self.stack.runtime_error(msg)
    }

    /// convenience helper to generate runtime bugs
    #[inline]
    fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> LpcError {
        self.stack.runtime_bug(msg)
    }

    #[inline]
    fn array_index_error<T>(&self, index: T, length: usize) -> LpcError
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
        if frame
            .as_ref()
            .is_some_and(|f| f.function.name() != lpc_rs_core::INIT_PROGRAM)
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
    pub fn result(&self) -> Option<LpcRef> {
        self.context.result()
    }

    /// Run against an explicit committer, so a test can inject a rejection,
    /// and return the stats.
    #[cfg(test)]
    pub(crate) async fn eval_with_committer(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        seed: &TaskSeed,
    ) -> (Result<()>, RetryStats) {
        self.seed = Some(seed.clone());
        self.timeout_ms = None;
        run_attempts(
            tx,
            &crate::interpreter::stm::AttemptTelemetry::default(),
            None,
            self,
        )
        .await
    }
}

#[async_trait::async_trait]
impl<const STACKSIZE: usize> AttemptBody for Task<STACKSIZE> {
    async fn begin_attempt(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
    ) -> Result<Option<LiveSnapshot>> {
        self.open_attempt(tx).await
    }

    async fn commit_phase(
        &mut self,
        tx: &flume::Sender<CommitProtocol>,
        _live: LiveSnapshot,
    ) -> Result<(
        std::result::Result<(), crate::interpreter::stm::Conflict>,
        Vec<Effect>,
    )> {
        // Clone the changeset, not take it: the result's payload is read
        // through this handle after the commit, against the pre-commit
        // snapshot plus this changeset.
        let changeset = self.context.txn.with(|t| t.clone_changeset());
        let commit = commit_changeset(tx, changeset).await?;
        let effects = self.context.txn.with(|t| t.take_effects());
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
            initializes: false,
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

    const SCAN_CODE: &str = indoc! { r##"
        int hits;
        int foo() { int n; sscanf("hits 10", "hits %d", n); hits += n; return hits; }
    "##};

    /// `n` is the `ref` `sscanf` writes each attempt; a leaked first-attempt
    /// `hits += n` would read 20 instead of 10.
    #[tokio::test]
    async fn a_retried_task_writes_its_ref_arguments_once() {
        let mut task = run_prog(SCAN_CODE).await;
        let (tx, rx) = flume::bounded(4);
        let committer_tx = tx.clone();
        let handle =
            std::thread::spawn(move || Committer::new().run_with_rejections(committer_tx, rx, 1));
        let (res, stats) = eval_foo(&mut task, &tx).await;
        assert!(res.is_ok());
        assert_eq!(stats.attempts, 2, "first commit rejected, re-run commits");
        assert_eq!(stats.conflicts, 1);
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
