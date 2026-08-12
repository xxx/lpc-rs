pub mod apply_function;
pub mod eval_loop;
mod handle_call_fp;
mod handle_call_other;
pub mod initialize_program;
pub mod into_task_context;
mod location;
pub mod task_id;
pub mod task_state;
pub mod task_template;

use std::{
    fmt::{Debug, Display},
    path::PathBuf,
    sync::{Arc, Weak},
    time::Duration,
};

use async_recursion::async_recursion;
use bit_set::BitSet;
use educe::Educe;
use if_chain::if_chain;
pub(crate) use location::{apply_in_location, get_location, get_location_in_frame, set_location};
use lpc_rs_asm::address::Address;
use lpc_rs_core::{
    LpcIntInner, RegisterSize,
    function_receiver::FunctionReceiver,
    lpc_path::LpcPath,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, Result, lpc_bug, lpc_error, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use parking_lot::RwLock;
use string_interner::{DefaultSymbol, Symbol};
use thin_vec::{ThinVec, thin_vec};
use tokio::{task::JoinHandle, time::timeout};
use tracing::{error, instrument, trace, warn};
use ustr::ustr;

use crate::{
    interpreter::{
        call_frame::CallFrame,
        call_stack::CallStack,
        efun::{EFUN_FUNCTIONS, call_efun, efun_context::EfunContext},
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
        gc::{mark::Mark, unique_id::UniqueId},
        lpc_array::LpcArray,
        lpc_int::LpcInt,
        lpc_ref::{LpcRef, NULL},
        lpc_string::LpcString,
        object_flags::ObjectFlags,
        process::Process,
        program::Program,
        task::{task_id::TaskId, task_state::TaskState},
        task_context::TaskContext,
        vm::global_state::GlobalState,
    },
    util::process_builder::ProcessCreator,
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

#[macro_export]
macro_rules! get_loc {
    ($self:expr, $loc:expr) => {{ get_location(&$self.stack, $loc) }};
}

#[macro_export]
macro_rules! set_loc {
    ($self:expr, $loc:expr, $val:expr) => {{ set_location(&mut $self.stack, $loc, $val) }};
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

        self.eval_function(process, f, args).await
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

        match timeout(
            Duration::from_millis(timeout_ms),
            self.eval_function(process, f, args),
        )
        .await
        {
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
            self.context.upvalues().clone(),
        );

        // TODO: This is probably not correct. See behavior in prepare_new_call_frame
        if !args.is_empty() {
            frame.registers[1..=args.len()].clone_from_slice(args);
        }

        self.stack.push(frame)
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_aconst(&mut self, location: RegisterVariant) -> Result<()> {
        let items = &self.array_items;
        let vars = items
            .iter()
            .map(|i| get_loc!(self, *i).map(|i| i.into_owned()))
            .collect::<Result<Vec<_>>>()?;
        let new_ref = LpcArray::new(vars).into();

        set_loc!(self, location, new_ref)
    }

    #[instrument(skip_all)]
    async fn handle_call(&mut self, name_idx: RegisterSize) -> Result<()> {
        let current_frame = self.stack.current_frame()?;
        let process = current_frame.process.clone();
        let func = {
            let name = process
                .program
                .strings
                .resolve(Self::index_symbol(name_idx))
                .unwrap();
            let function = process.program.lookup_function(name);
            if let Some(func) = function {
                func.clone()
            } else {
                // These shouldn't be reachable due to the CallEfun and CallSimulEfun instructions,
                // but are kept juuuuuust in case.
                {
                    let e = LpcError::new_warning(
                        format!("Call to unknown local function `{name}`. Falling back to legacy SEfun and Efun checks.")
                    ).with_span(current_frame.current_debug_span());
                    e.emit_diagnostics();
                }

                if_chain! {
                    // See if there is a simul efun with this name
                    if let Some(se) = self.context.simul_efuns();
                    if let Some(func) = se.program.lookup_function(name);
                    then {
                        func.clone()
                    } else {
                        let msg = format!("Call to unknown function `{name}`");
                        return Err(self.runtime_error(msg));
                    }
                }
            }
        };

        let new_frame = self.prepare_new_call_frame(process, func).await?;

        trace!("pushing new frame");

        self.stack.push(new_frame)?;

        Ok(())
    }

    /// Prepare and populate a new [`CallFrame`] for a call to a static function.
    #[instrument(skip_all)]
    async fn prepare_new_call_frame(
        &mut self,
        process: Arc<Process>,
        func: Arc<ProgramFunction>,
    ) -> Result<CallFrame> {
        let num_args = RegisterSize::try_from(self.args.len())?;
        let mut new_frame = CallFrame::with_minimum_arg_capacity(
            process,
            func.clone(),
            num_args,
            num_args,
            None::<&[Register]>, /* static functions do not inherit upvalues from the calling function */
            self.context.upvalues().clone(),
        );

        trace!("copying arguments to new frame: {num_args}");
        // copy argument registers from old frame to new
        if num_args > 0 {
            let mut next_index = 1;
            for (i, arg) in self.args.iter().enumerate() {
                let target_location = func.arg_locations.get(i).copied().unwrap_or_else(|| {
                    // This should only be reached by efun calls, or variables that will go
                    // into an ellipsis function's `argv`.
                    Register(next_index).as_local()
                });
                if let RegisterVariant::Local(r) = target_location {
                    next_index = r.index() + 1;
                }

                let lpc_ref = get_loc!(self, *arg).map(|i| i.into_owned())?;

                trace!(
                    "Copying argument {} ({}) to {}",
                    i, lpc_ref, target_location
                );

                new_frame.arg_locations.push(target_location);

                new_frame.set_location(target_location, lpc_ref)
            }
        }

        Ok(new_frame)
    }

    /// An extracted helper to handle pulling the [`Process`] and [`ProgramFunction`] out of a [`FunctionPtr`].
    /// This will create a [`Process`] from a path name, in the case of `call_other` receivers that are strings.
    pub async fn extract_process_and_function(
        &mut self,
        ptr: &FunctionPtr,
    ) -> Result<Option<ProcessFunctionPair>> {
        let (proc, function) = match &ptr.address {
            FunctionAddress::Local(proc, function) => (proc.clone(), function.clone()),
            FunctionAddress::Dynamic(name) => {
                let lpc_ref = match &*get_loc!(self, self.args[0])? {
                    LpcRef::Object(lpc_ref) => lpc_ref.upgrade(),
                    LpcRef::String(string_ref) => {
                        let lookup = {
                            let string = string_ref.read();
                            self.context.lookup_process(string.to_str())
                        };

                        if lookup.is_some() {
                            lookup
                        } else {
                            let path = LpcPath::InGame(PathBuf::from(string_ref.read().to_str()));
                            // This will be initialized later on, if necessary.
                            Some(self.context.create_process_from_path(&path).await?)
                        }
                    }
                    _ => {
                        return Err(
                            self.runtime_error("non-object receiver to function pointer call")
                        );
                    }
                };

                let pair_opt = {
                    if let Some(proc) = lpc_ref {
                        proc.program
                            .lookup_function(name)
                            .map(|func| (proc.clone(), func.clone()))
                    } else {
                        None
                    }
                };

                // short-circuit a 0 return if doing a call_other to a
                // non-existent function, or destructed object
                let Some(pair) = pair_opt else {
                    let frame = self.stack.current_frame_mut()?;
                    frame.registers[0] = NULL;
                    return Ok(None);
                };

                (Arc::downgrade(&pair.0), pair.1)
            }
            FunctionAddress::Efun(name) => {
                // unwrap is safe because this should have been checked in an earlier step
                let pf = EFUN_FUNCTIONS.get(name.as_str()).cloned().unwrap();

                let frame = self.stack.current_frame()?;

                (Arc::downgrade(&frame.process), pf)
            }
            FunctionAddress::SimulEfun(name) => {
                let Some(simul_efuns) = self.context.simul_efuns() else {
                    return Err(self.runtime_bug("simul_efun called without simul_efuns"));
                };

                let Some(function) = simul_efuns.program.lookup_function(name) else {
                    return Err(self.runtime_error(format!("call to unknown simul_efun `{name}`")));
                };

                (Arc::downgrade(simul_efuns), function.clone())
            }
        };

        Ok(Some((proc, function)))
    }

    /// handle runtime type-checks for function pointer calls
    fn type_check_call_arg(
        &self,
        lpc_ref: &LpcRef,
        arg_type: Option<&LpcType>,
        arg_def_span: Option<&Span>,
        function_name: &str,
    ) -> Result<()> {
        if_chain! {
            if lpc_ref != &NULL; // 0 is always allowed
            if let Some(arg_type) = arg_type;
            let ref_type = lpc_ref.as_lpc_type();
            if !ref_type.matches_type(*arg_type);
            then {
                let error = self.runtime_error(format!(
                    "unexpected argument type to `{function_name}`: {ref_type}. expected {arg_type}."
                ))
                .with_label("defined here", arg_def_span.copied());

                return Err(error.into());
            }
        }

        Ok(())
    }

    /// Create a new [`EfunContext`] and called the named efun.
    async fn prepare_and_call_efun<S>(&mut self, name: S) -> Result<()>
    where
        S: AsRef<str>,
    {
        let mut ctx = EfunContext::new(self.id, &mut self.stack, &self.context);

        call_efun(name.as_ref(), &mut ctx).await?;

        #[cfg(test)]
        {
            if let Some(snap) = ctx.snapshot {
                self.snapshots.push(snap);
            }
        }

        pop_frame!(self);

        Ok(())
    }

    #[instrument(skip_all)]
    #[inline]
    async fn handle_call_simul_efun(&mut self, name_idx: RegisterSize) -> Result<()> {
        let Some(func_name) = self
            .stack
            .current_frame()?
            .function
            .strings
            .get()
            .unwrap()
            .resolve(Self::index_symbol(name_idx))
        else {
            return Err(self.runtime_bug("Unable to find the name being pointed to."));
        };

        let Some(simul_efuns) = self.context.simul_efuns() else {
            // This could be legitimately hit in the case an object was compiled with simul_efuns,
            // cached to disk, and then later executed without them.
            // tl;dr objects are dynamically linked.
            return Err(self.runtime_error("Unable to find simul_efuns. Were they configured?"));
        };

        let func = {
            if let Some(func) = simul_efuns.program.lookup_function(func_name) {
                func.clone()
            } else {
                let msg = format!("Call to unknown simul efun `{func_name}`");
                return Err(self.runtime_error(msg));
            }
        };

        let new_frame = self
            .prepare_new_call_frame(simul_efuns.clone(), func)
            .await?;

        self.stack.push(new_frame)?;

        Ok(())
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_functionptrconst(
        &mut self,
        location: RegisterVariant,
        receiver: FunctionReceiver,
        name_idx: RegisterSize,
    ) -> Result<()> {
        let call_other = match receiver {
            FunctionReceiver::Var(_) | FunctionReceiver::Dynamic => true,
            FunctionReceiver::Local | FunctionReceiver::Efun | FunctionReceiver::SimulEfun => false,
        };

        let Some(func_name) = self
            .stack
            .current_frame()?
            .function
            .strings
            .get()
            .unwrap()
            .resolve(Self::index_symbol(name_idx))
        else {
            return Err(self.runtime_bug("Unable to find the name being pointed to."));
        };

        let address = match receiver {
            FunctionReceiver::Efun => FunctionAddress::Efun(ustr(func_name)),
            FunctionReceiver::SimulEfun => FunctionAddress::SimulEfun(ustr(func_name)),
            FunctionReceiver::Dynamic => FunctionAddress::Dynamic(ustr(func_name)),
            FunctionReceiver::Local => {
                let frame = self.stack.current_frame()?;
                let process = frame.process.clone();

                let func = {
                    let Some(func) = process.program.lookup_function(func_name) else {
                        return Err(self.runtime_error(format!(
                            "Unable to find function `{}` in local process `{}`.",
                            func_name,
                            process.filename()
                        )));
                    };

                    func.clone()
                };

                FunctionAddress::Local(Arc::downgrade(&process), func)
            }
            FunctionReceiver::Var(location) => {
                let receiver_ref = &*get_loc!(self, location)?;
                match receiver_ref {
                    LpcRef::Object(weak_process) => {
                        let Some(process) = weak_process.upgrade() else {
                            return Err(self.runtime_error("called object is no longer available"));
                        };

                        let func = {
                            let Some(func) = process.program.lookup_function(func_name) else {
                                return Err(self.runtime_error(format!(
                                    "Unable to find function `{}` in remote process `{}`.",
                                    func_name,
                                    process.filename()
                                )));
                            };

                            func.clone()
                        };
                        let weak_process = (*weak_process).clone();
                        FunctionAddress::Local(weak_process, func)
                    }
                    LpcRef::String(s) => {
                        let process = {
                            let path = s.read();

                            let Some(process) = self.context.lookup_process(&*path) else {
                                return Err(self
                                    .runtime_error(format!("Unable to find object `{}`.", path)));
                            };

                            process
                        };

                        let func = {
                            let Some(func) = process.program.lookup_function(func_name) else {
                                return Err(self.runtime_error(format!(
                                    "Unable to find function `{}` in remote process `{}`.",
                                    func_name,
                                    process.filename()
                                )));
                            };

                            func.clone()
                        };

                        FunctionAddress::Local(Arc::downgrade(&process), func)
                    }
                    _ => {
                        return Err(self.runtime_error(format!(
                            "Unable to find the receiver for function `{}`.",
                            func_name
                        )));
                    }
                }
            }
        };

        let partial_args = self
            .partial_args
            .iter()
            .map(|arg| {
                arg.map(|register| Ok(get_loc!(self, register)?.into_owned()))
                    .transpose()
            })
            .collect::<Result<ThinVec<Option<LpcRef>>>>()?;

        let frame = self.stack.current_frame()?;
        let fp = FunctionPtr {
            owner: Arc::downgrade(&frame.process),
            address,
            partial_args: RwLock::new(partial_args),
            call_other,
            // Function pointers inherit the current upvalue_ptrs
            upvalue_ptrs: frame.upvalue_ptrs.clone(),
            unique_id: UniqueId::new(),
        };

        let new_ref = fp.into();

        set_loc!(self, location, new_ref)
    }

    // #[instrument(skip_all)]
    // fn capture_environment(&mut self) -> Result<Vec<Register>> {
    //     let frame = self.stack.current_frame_mut()?;
    //     let mut upvalues = self.context.upvalues().write();
    //
    //     trace!("ptrs: {:?}", frame.upvalue_ptrs);
    //     trace!("upvalues: {:?}", upvalues);
    //
    //     frame
    //         .upvalue_ptrs
    //         .iter()
    //         .map(|ptr| {
    //             let upvalue = upvalues
    //                 .get(ptr.index() as usize)
    //                 .cloned()
    //                 .unwrap_or_default();
    //             let new_index = RegisterSize::try_from(upvalues.insert(upvalue))?;
    //             Ok(Register(new_index))
    //         })
    //         .collect::<Result<Vec<Register>>>()
    // }

    #[instrument(skip_all)]
    #[inline]
    fn handle_load(
        &mut self,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
        destination: RegisterVariant,
    ) -> Result<()> {
        let container_ref = get_loc!(self, container_loc)?.into_owned();
        let lpc_ref = get_loc!(self, index_loc)?.into_owned();

        match container_ref {
            LpcRef::Array(vec_ref) => {
                let vec = vec_ref.read();

                if let LpcRef::Int(i) = lpc_ref {
                    let idx = if i.0 >= 0 {
                        i.0
                    } else {
                        vec.len() as LpcIntInner + i.0
                    };

                    if idx >= 0 {
                        if let Some(v) = vec.get(idx as usize) {
                            set_loc!(self, destination, v.clone())?;
                        } else {
                            return Err(self.array_index_error(idx, vec.len()));
                        }
                    } else {
                        return Err(self.array_index_error(idx, vec.len()));
                    }
                } else {
                    return Err(self.array_index_error(lpc_ref, vec.len()));
                }

                Ok(())
            }
            LpcRef::String(string_ref) => {
                let lock = string_ref.read();
                let string = lock.to_str();

                if let LpcRef::Int(i) = lpc_ref {
                    let idx = if i.0 >= 0 {
                        i.0
                    } else {
                        string.len() as LpcIntInner + i.0
                    };

                    if idx >= 0 {
                        if let Some(v) = string.chars().nth(idx as usize) {
                            set_loc!(self, destination, LpcRef::Int(LpcInt(v as LpcIntInner)))?;
                        } else {
                            set_loc!(self, destination, NULL)?;
                        }
                    } else {
                        set_loc!(self, destination, NULL)?;
                    }
                } else {
                    return Err(self.runtime_error(format!(
                        "Attempting to access index {} in a string of length {}",
                        lpc_ref,
                        string.len()
                    )));
                }

                Ok(())
            }
            LpcRef::Mapping(map_ref) => {
                let map = map_ref.read();

                let var = if let Some(v) = map.get(&lpc_ref) {
                    v.clone()
                } else {
                    NULL
                };

                set_loc!(self, destination, var)?;

                Ok(())
            }
            x => Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x))),
        }
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_load_mapping_key(
        &mut self,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
        destination: RegisterVariant,
    ) -> Result<()> {
        let var = {
            let container_ref = &*get_loc!(self, container_loc)?;
            let lpc_ref = &*get_loc!(self, index_loc)?;

            match container_ref {
                LpcRef::Mapping(map_ref) => {
                    let map = map_ref.read();

                    let index = match lpc_ref {
                        LpcRef::Int(i) => i.0,
                        _ => {
                            return Err(
                                self.runtime_error(format!("Invalid index type: {}", lpc_ref))
                            );
                        }
                    };

                    if let Some((key, _)) = map.get_index(index as usize) {
                        key.clone()
                    } else {
                        NULL
                    }
                }
                x => {
                    return Err(
                        self.runtime_error(format!("Invalid attempt to take index of `{}`", x))
                    );
                }
            }
        };

        set_loc!(self, destination, var)
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_sconst(&mut self, location: RegisterVariant, index: usize) -> Result<()> {
        let function_strings = self.stack.current_frame()?.function.strings.get();
        const MSG: &str = "the `strings` reference was never assigned to the function.";
        debug_assert!(function_strings.is_some(), "{}", MSG); // This is very bad if it happens.
        let Some(strings) = function_strings else {
            return Err(self.runtime_bug(MSG));
        };
        let lpc_string = LpcString::Static(index, strings.clone());

        trace!(?lpc_string, "Storing static string");

        let new_ref = lpc_string.into();

        set_loc!(self, location, new_ref)
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_store(
        &mut self,
        value_loc: RegisterVariant,
        container_loc: RegisterVariant,
        index_loc: RegisterVariant,
    ) -> Result<()> {
        let mut container = get_loc!(self, container_loc)?.into_owned();
        let index = &*get_loc!(self, index_loc)?;
        let array_idx = if let LpcRef::Int(i) = index { i.0 } else { 0 };

        match container {
            LpcRef::Array(vec_ref) => {
                let mut vec = vec_ref.write();

                let len = vec.len();

                // handle negative indices
                let idx = if array_idx >= 0 {
                    array_idx
                } else {
                    len as LpcIntInner + array_idx
                };

                if idx >= 0 && (idx as usize) < len {
                    vec[idx as usize] = (*get_loc!(self, value_loc)?).clone();
                } else {
                    return Err(self.array_index_error(idx, len));
                }

                Ok(())
            }
            LpcRef::Mapping(ref mut map_ref) => {
                let mut map = map_ref.write();

                map.insert(index.clone(), get_loc!(self, value_loc)?.into_owned());

                Ok(())
            }
            x => Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x))),
        }
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
        set_loc!(self, Register(result_index).as_local(), lpc_ref)?;
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
                set_loc!(self, r3, result)?;
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

        set_loc!(self, r3, LpcRef::Int(LpcInt(out)))
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

#[cfg(test)]
mod tests;
