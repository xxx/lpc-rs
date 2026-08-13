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
use string_interner::{DefaultSymbol, Symbol};
use thin_vec::{ThinVec, thin_vec};
use tokio::{task::JoinHandle, time::timeout};
use tracing::{error, instrument, warn};

use crate::interpreter::{
    call_frame::CallFrame,
    call_stack::CallStack,
    gc::mark::Mark,
    gil::run_with_gil,
    lpc_int::LpcInt,
    lpc_ref::LpcRef,
    lpc_string::LpcString,
    object_flags::ObjectFlags,
    process::Process,
    program::Program,
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

    /// The arg vector, populated prior to executing any of the `Call`-family [`Instruction`]s
    pub args: ThinVec<RegisterVariant>,

    /// The vector used to collect arguments when creating a partially-applied function pointer
    pub partial_args: ThinVec<Option<RegisterVariant>>,

    /// The vector used to collect members of a soon-to-be-created array
    array_items: ThinVec<RegisterVariant>,

    /// The context of this task
    pub context: TaskContext,

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
        Self {
            id: parent_id,
            stack: CallStack::default(),
            catch_points: thin_vec![],
            args: ThinVec::with_capacity(4),
            partial_args: thin_vec![],
            array_items: ThinVec::with_capacity(10),
            context: task_context,
            state: TaskState::New,

            #[cfg(test)]
            popped_frame: None,

            #[cfg(test)]
            snapshots: thin_vec![],
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
    /// It's assumed that the process has already been inserted into the [`ObjectSpace`]
    pub async fn initialize_process(context: TaskContext) -> Result<Task<STACKSIZE>> {
        Self::initialize_sub_process(TaskId::new(), context).await
    }

    /// Initialize a [`Process`] by calling its initializer function, using the
    /// given [`TaskContext`], using the specified Task ID.
    /// It's assumed that the process has already been inserted into the [`ObjectSpace`]
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

    /// Evaluate `f` to completion, or an error. No timeouts are applied.
    ///
    /// # Arguments
    /// `f` - the function to call
    /// `args` - the slice of arguments to pass to the function
    ///
    /// # Returns
    ///
    /// `Ok(())` if successful, or an [`LpcError`] if not
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn eval(&mut self, f: Arc<ProgramFunction>, args: &[LpcRef]) -> Result<()> {
        let process = self.context.process().clone();

        let state_clone = self.context.global_state.clone();
        run_with_gil(&state_clone, self.eval_function(process, f, args)).await
        // run_with_gil(&self.context.global_state, async move {
        //     self.eval_function(process, f, args).await
        // })
        // self.eval_function(process, f, args).await
    }

    /// Evaluate `f` to completion, or an error, with a timeout.
    ///
    /// # Arguments
    /// `f` - the function to call
    /// `args` - the slice of arguments to pass to the function
    ///
    /// # Returns
    ///
    /// `Ok(())` if successful, or an [`LpcError`] if not
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn timed_eval(
        &mut self,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
        timeout_ms: u64,
    ) -> Result<()> {
        if timeout_ms == 0 {
            return self.eval(f, args).await;
        }

        let process = self.context.process().clone();

        let state_clone = self.context.global_state.clone();
        let result = run_with_gil(
            &state_clone,
            timeout(
                Duration::from_millis(timeout_ms),
                self.eval_function(process, f, args),
            ),
        )
        .await;

        match result {
            Ok(Ok(_)) => Ok(()),
            Ok(Err(e)) => Err(e),
            Err(_) => Err(lpc_error!(
                "evaluation limit of {}ms has been reached",
                timeout_ms
            )),
        }
    }

    /// Evaluate `f` to completion, or an error, in the context of an arbitrary process
    ///
    /// # Arguments
    /// `process`: the process that owns the function to call.
    /// `f` - the function to call
    /// `args` - the slice of arguments to pass to the function
    ///
    /// # Returns
    ///
    /// `Ok(())` if successful, or an [`LpcError`] if not
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
            self.context.global_state.upvalues.clone(),
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
        set_location(&mut self.stack, Register(result_index).as_local(), lpc_ref)?;
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
        F: Fn(&LpcRef, &LpcRef) -> Result<LpcRef>,
    {
        let ref1 = &*get_location(&self.stack, r1)?;
        let ref2 = &*get_location(&self.stack, r2)?;

        match operation(ref1, ref2) {
            Ok(result) => {
                set_location(&mut self.stack, r3, result)?;
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
        let ref1 = &*get_location(&self.stack, r1)?;
        let ref2 = &*get_location(&self.stack, r2)?;

        let out = operation(ref1, ref2) as LpcIntInner;

        set_location(&mut self.stack, r3, LpcRef::Int(LpcInt(out)))
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
    pub fn result(&self) -> Option<&LpcRef> {
        self.context.result()
    }
}

impl<const STACKSIZE: usize> Mark for Task<STACKSIZE> {
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        self.stack.mark(marked, processed)
    }
}

// impl<const STACKSIZE: usize> Drop for Task<STACKSIZE> {
//     fn drop(&mut self) {
//         self.context.process.lock.try_release(self.id);
//     }
// }
