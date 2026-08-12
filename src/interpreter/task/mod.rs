pub mod apply_function;
pub mod initialize_program;
pub mod into_task_context;
pub mod task_id;
pub mod task_state;
pub mod task_template;

mod handle_call_fp;
mod handle_call_other;

use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    path::PathBuf,
    sync::{Arc, Weak},
    time::Duration,
};

use async_recursion::async_recursion;
use bit_set::BitSet;
use decorum::Total;
use educe::Educe;
use if_chain::if_chain;
use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_asm::{address::Address, instruction::Instruction};
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
        lpc_mapping::LpcMapping,
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

/// Resolve any type RegisterVariant into an LpcRef, for the current frame
#[inline]
pub fn get_location<const N: usize>(
    stack: &CallStack<N>,
    location: RegisterVariant,
) -> Result<Cow<'_, LpcRef>> {
    let frame = stack.current_frame()?;

    get_location_in_frame(frame, location)
}

/// Resolve any type RegisterVariant into an LpcRef, for the passed frame
#[instrument(skip(frame))]
#[inline]
pub fn get_location_in_frame(
    frame: &CallFrame,
    location: RegisterVariant,
) -> Result<Cow<'_, LpcRef>> {
    match location {
        RegisterVariant::Local(reg) => {
            let registers = &frame.registers;
            Ok(Cow::Borrowed(&registers[reg]))
        }
        RegisterVariant::Global(reg) => {
            let proc = &frame.process;
            Ok(Cow::Owned(proc.globals.read()[reg].clone()))
        }
        RegisterVariant::Upvalue(upv) => {
            let upvalue_ptrs = &frame.upvalue_ptrs;
            let reg = upvalue_ptrs[upv.index() as usize];

            let vm_upvalues = &frame.vm_upvalues.read();
            trace!("upvalue data: idx = {}, len = {}", reg, vm_upvalues.len());
            Ok(Cow::Owned(vm_upvalues[reg].clone()))
        }
    }
}

#[inline]
fn set_location<const N: usize>(
    stack: &mut CallStack<N>,
    location: RegisterVariant,
    lpc_ref: LpcRef,
) -> Result<()> {
    let frame = stack.current_frame_mut()?;
    frame.set_location(location, lpc_ref);
    Ok(())
}

/// Apply an operation to a location, in-place.
fn apply_in_location<F, const N: usize>(
    stack: &mut CallStack<N>,
    location: RegisterVariant,
    func: F,
) -> Result<()>
where
    F: FnOnce(&mut LpcRef) -> Result<()>,
{
    match location {
        RegisterVariant::Local(reg) => {
            let frame = stack.current_frame_mut()?;
            let registers = &mut frame.registers;
            func(&mut registers[reg])
        }
        RegisterVariant::Global(reg) => {
            let frame = stack.current_frame()?;

            let proc = &frame.process;
            func(&mut proc.globals.write()[reg])
        }
        RegisterVariant::Upvalue(reg) => {
            let frame = stack.current_frame()?;
            let upvalues = &frame.upvalue_ptrs;
            let idx = upvalues[reg.index() as usize];

            let vm_upvalues = &mut frame.vm_upvalues.write();
            func(&mut vm_upvalues[idx])
        }
    }
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

    /// Resume execution of a New or Paused Task. Assumes the stack has already been set up
    #[instrument(skip_all)]
    #[async_recursion]
    pub async fn resume(&mut self) -> Result<()> {
        self.state = TaskState::Running;

        let f = &self.stack.current_frame()?.function.clone();

        if f.prototype.is_efun() {
            // call the efun, then we're done with this Task
            self.prepare_and_call_efun(f.name()).await?;
        } else {
            let mut halted = false;

            let mut c = 0 as RegisterSize;

            while !halted {
                halted = match self.eval_one_instruction().await {
                    Ok(x) => x,
                    Err(mut e) => {
                        if !self.catch_points.is_empty() {
                            self.catch_error(e)?;

                            false
                        } else {
                            let stack_trace = self.stack.stack_trace();
                            return Err({
                                *e = e.with_stack_trace(stack_trace);
                                e
                            });
                        }
                    }
                };

                // Ensure infinite loops and the like don't monopolize the runtime.
                c += 1;
                if c == 1000 {
                    c = 0;
                    tokio::task::yield_now().await;
                }
            }
        }

        assert!(self.stack.is_empty());
        self.state = TaskState::Complete;
        Ok(())
    }

    /// Evaluate the instruction at the current value of the program counter.
    /// This is the main interpretation function for the VM.
    ///
    /// # Returns
    ///
    /// A [`Result`], with a boolean indicating whether we are at the end of input
    #[instrument(skip_all)]
    #[inline]
    #[async_recursion]
    async fn eval_one_instruction(&mut self) -> Result<bool> {
        if self.stack.is_empty() {
            self.state = TaskState::Complete;

            return Ok(true);
        }

        let instruction = {
            let frame = match self.stack.current_frame_mut() {
                Ok(x) => x,
                Err(_) => {
                    self.state = TaskState::Error;

                    warn!("Expected to get an instruction, but there are no more frames.");

                    return Ok(true);
                }
            };

            let Some(instruction) = frame.instruction() else {
                self.state = TaskState::Error;

                warn!("No more instructions. Missing Ret instruction?");

                return Ok(true);
            };
            trace!("about to evaluate: {}", instruction);

            frame.inc_pc();

            instruction
        };

        match instruction {
            Instruction::AConst(location) => {
                self.handle_aconst(location)?;
            }
            Instruction::And(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitand(y))?;
            }
            Instruction::BitwiseNot(r1, r2) => {
                let frame = self.stack.current_frame().unwrap();
                let debug_span = frame.current_debug_span();
                let lpc_ref = &*get_loc!(self, r1)?;
                match lpc_ref.bitnot() {
                    Ok(result) => {
                        set_loc!(self, r2, result)?;
                    }
                    Err(mut e) => {
                        *e = e.with_span(debug_span);
                        return Err(e);
                    }
                }
            }
            Instruction::Call(name_idx) => {
                self.handle_call(name_idx).await?;
            }
            Instruction::CallEfun(name_idx) => {
                let process = self.stack.current_frame()?.process.clone();
                let (pf, name) = {
                    let (name, pf) = EFUN_FUNCTIONS.get_index(name_idx as usize).unwrap();

                    (pf.clone(), name)
                };

                let new_frame = self.prepare_new_call_frame(process, pf).await?;

                self.stack.push(new_frame)?;

                self.prepare_and_call_efun(name).await?;
            }
            Instruction::CallFp(location) => {
                self.handle_call_fp(location).await?;
            }
            Instruction::CallOther(receiver, name) => {
                self.handle_call_other(receiver, name).await?;
            }
            Instruction::CallSimulEfun(name_idx) => {
                self.handle_call_simul_efun(name_idx).await?;
            }
            Instruction::CatchEnd => {
                self.catch_points.pop();
            }
            Instruction::CatchStart(r, address) => {
                let catch_point = CatchPoint {
                    frame_index: self.stack.len() - 1,
                    register: r,
                    address,
                };

                self.catch_points.push(catch_point);
            }
            Instruction::ClearArgs => {
                self.args.clear();
            }
            Instruction::ClearArrayItems => {
                self.array_items.clear();
            }
            Instruction::ClearPartialArgs => {
                self.partial_args.clear();
            }
            Instruction::Copy(r1, r2) => {
                let new_ref = get_location(&self.stack, r1)?.into_owned();
                set_loc!(self, r2, new_ref)?;
            }
            Instruction::Dec(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.dec())?;
            }
            Instruction::EqEq(r1, r2, r3) => {
                let out = (get_loc!(self, r1)? == get_loc!(self, r2)?) as LpcIntInner;

                set_loc!(self, r3, LpcRef::Int(out.into()))?;
            }
            Instruction::FConst(r, f) => {
                set_loc!(self, r, LpcRef::Float(f.into()))?;
            }
            Instruction::FunctionPtrConst {
                location,
                receiver,
                name_index,
            } => {
                self.handle_functionptrconst(location, receiver, name_index)?;
            }
            Instruction::Gt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x > y)?;
            }
            Instruction::Gte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x >= y)?;
            }
            Instruction::IAdd(r1, r2, r3) => match get_loc!(self, r1)?.add(&*get_loc!(self, r2)?) {
                Ok(result) => {
                    set_loc!(self, r3, result)?;
                }
                Err(mut e) => {
                    let frame = self.stack.current_frame()?;
                    *e = e.with_span(frame.current_debug_span());
                    return Err(e);
                }
            },
            Instruction::IConst(r, i) => {
                set_loc!(self, r, LpcRef::Int(i.into()))?;
            }
            Instruction::IConst0(r) => {
                set_loc!(self, r, NULL)?;
            }
            Instruction::IConst1(r) => {
                set_loc!(self, r, LpcRef::Int(1.into()))?;
            }
            Instruction::IDiv(r1, r2, r3) => match get_loc!(self, r1)?.div(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, result)?,
                Err(mut e) => {
                    let frame = self.stack.current_frame()?;
                    *e = e.with_span(frame.current_debug_span());
                    return Err(e);
                }
            },
            Instruction::IMod(r1, r2, r3) => match get_loc!(self, r1)?.rem(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, result)?,
                Err(mut e) => {
                    let frame = self.stack.current_frame()?;
                    *e = e.with_span(frame.current_debug_span());
                    return Err(e);
                }
            },
            Instruction::IMul(r1, r2, r3) => match get_loc!(self, r1)?.mul(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, result)?,
                Err(mut e) => {
                    let frame = self.stack.current_frame()?;
                    *e = e.with_span(frame.current_debug_span());
                    return Err(e);
                }
            },
            Instruction::Inc(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.inc())?;
            }
            Instruction::ISub(r1, r2, r3) => match get_loc!(self, r1)?.sub(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, result)?,
                Err(mut e) => {
                    let frame = self.stack.current_frame()?;
                    *e = e.with_span(frame.current_debug_span());
                    return Err(e);
                }
            },
            Instruction::Jmp(address) => {
                let frame = self.stack.current_frame_mut()?;
                frame.set_pc(address);
            }
            Instruction::Jnz(r1, address) => {
                let v = &*get_loc!(self, r1)?;

                // TODO: re-decide of 0.0 floats should match here and with Jz
                if v != &NULL && v != &LpcRef::Float(Total::from(0.0).into()) {
                    let frame = self.stack.current_frame_mut()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Jz(r1, address) => {
                let v = &*get_loc!(self, r1)?;

                if v == &NULL || v == &LpcRef::Float(Total::from(0.0).into()) {
                    let frame = self.stack.current_frame_mut()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Load(container, index, destination) => {
                self.handle_load(container, index, destination)?;
            }
            Instruction::LoadMappingKey(container, index, destination) => {
                self.handle_load_mapping_key(container, index, destination)?;
            }
            Instruction::Lt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x < y)?;
            }
            Instruction::Lte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x <= y)?;
            }
            Instruction::MAdd(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.add(y))?;
            }
            Instruction::MapConst(r) => {
                let mut register_map = IndexMap::with_capacity(self.array_items.len() / 2);

                debug_assert!(
                    self.array_items.len().is_multiple_of(2),
                    "Odd number of items in `array` when creating a mapping constant"
                );
                for chunk in &self.array_items.iter().copied().chunks(2) {
                    let (key, value) = chunk.into_iter().collect_tuple().unwrap();
                    register_map.insert(
                        get_loc!(self, key)?.into_owned(),
                        get_loc!(self, value)?.into_owned(),
                    );
                }

                let new_ref = LpcMapping::new(register_map.into_iter().collect()).into();

                set_loc!(self, r, new_ref)?;
            }
            Instruction::MMul(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.mul(y))?;
            }
            Instruction::MSub(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.sub(y))?;
            }
            Instruction::Not(r1, r2) => {
                let matched = match &*get_loc!(self, r1)? {
                    LpcRef::Int(x) => LpcRef::Int(LpcInt((*x == 0) as LpcIntInner)),
                    LpcRef::Float(x) => LpcRef::Int(LpcInt((*x == 0.0) as LpcIntInner)),

                    // These rest always have a value at runtime.
                    // Any null / undefined values would be LpcRef::Ints, handled above.
                    LpcRef::String(_)
                    | LpcRef::Array(_)
                    | LpcRef::Mapping(_)
                    | LpcRef::Object(_)
                    | LpcRef::Function(_) => NULL,
                };

                set_loc!(self, r2, matched)?;
            }
            Instruction::NotEq(r1, r2, r3) => {
                let out = (get_loc!(self, r1)? != get_loc!(self, r2)?) as LpcIntInner;

                set_loc!(self, r3, LpcRef::Int(LpcInt(out)))?;
            }
            Instruction::Or(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitor(y))?;
            }
            Instruction::PopulateArgv(r, num_args, _num_locals) => {
                let frame = self.stack.current_frame()?;
                let arg_locations = &frame.arg_locations;
                let num_args = usize::from(num_args);
                let refs = {
                    if arg_locations.len() < num_args {
                        vec![]
                    } else {
                        let ellipsis_vars = &arg_locations[num_args..];
                        ellipsis_vars
                            .iter()
                            .map(|x| get_location_in_frame(frame, *x).map(|v| v.into_owned()))
                            .collect::<Result<Vec<_>>>()?
                    }
                };

                let new_ref = LpcArray::new(refs).into();

                set_location(&mut self.stack, r, new_ref)?;
            }
            Instruction::PopulateDefaults => {
                // let default_addresses = &self.defaults;
                let frame = self.stack.current_frame_mut()?;
                let func = &frame.function;
                let num_args = func.arity().num_args;
                let num_default_args = func.arity().num_default_args;
                // let non_default_args = num_args - num_default_args;
                let called_args = frame.called_with_num_args;
                let defaults_to_init = (num_args.saturating_sub(called_args)).min(num_default_args);

                let jump = num_default_args - defaults_to_init;
                frame.set_pc(frame.pc() + jump as usize);
            }
            Instruction::PushArg(r) => self.args.push(r),
            Instruction::PushArrayItem(r1) => {
                self.array_items.push(r1);
            }
            Instruction::PushPartialArg(r) => {
                self.partial_args.push(r);
            }
            Instruction::Range(r1, r2, r3, r4) => {
                // r4 = r1[r2..r3]

                let resolve_range = |start: i64, end: i64, len: usize| -> (usize, usize) {
                    let to_idx = |i: LpcIntInner| {
                        // We handle the potential overflow just below.
                        if i >= 0 {
                            i as usize
                        } else {
                            (len as LpcIntInner + i) as usize
                        }
                    };
                    let real_start = to_idx(start);
                    let mut real_end = to_idx(end);

                    if real_end >= len {
                        real_end = len - 1;
                    }

                    (real_start, real_end)
                };

                let return_value = |new_ref, stack| -> Result<()> {
                    set_location(stack, r4, new_ref)?;

                    Ok(())
                };

                let get_new_value = |stack| -> Result<LpcRef> {
                    let lpc_ref = &*get_location(stack, r1)?;

                    match lpc_ref {
                        LpcRef::Array(v_ref) => {
                            let vec = v_ref.read();

                            if vec.is_empty() {
                                return Ok(LpcArray::new(vec![]).into());
                            }

                            let index1 = &*get_location(stack, r2)?;
                            let index2 = &*get_location(stack, r3)?;

                            if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                let (real_start, real_end) =
                                    resolve_range(start.0, end.0, vec.len());

                                if real_start <= real_end {
                                    let slice = &vec[real_start..=real_end];
                                    let mut new_vec = vec![NULL; slice.len()];
                                    new_vec.clone_from_slice(slice);
                                    Ok(LpcArray::new(new_vec).into())
                                } else {
                                    Ok(LpcArray::new(vec![]).into())
                                }
                            } else {
                                let frame = self.stack.current_frame()?;
                                Err(lpc_error!(
                                    frame.current_debug_span(),
                                    "Invalid code was generated for a Range instruction.",
                                ))
                            }
                        }
                        LpcRef::String(v_ref) => {
                            let string = v_ref.read();

                            if string.is_empty() {
                                return Ok(LpcString::from("").into());
                            }

                            let index1 = &*get_location(stack, r2)?;
                            let index2 = &*get_location(stack, r3)?;

                            if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                let (real_start, real_end) =
                                    resolve_range(start.0, end.0, string.len());

                                if real_start <= real_end {
                                    let len = real_end - real_start + 1;
                                    let new_string: String =
                                        string.chars().skip(real_start).take(len).collect();
                                    Ok(LpcString::from(new_string).into())
                                } else {
                                    Ok(LpcString::from("").into())
                                }
                            } else {
                                let frame = self.stack.current_frame()?;
                                Err(lpc_error!(
                                    frame.current_debug_span(),
                                    "Invalid code was generated for a Range instruction.",
                                ))
                            }
                        }
                        LpcRef::Float(_)
                        | LpcRef::Int(_)
                        | LpcRef::Mapping(_)
                        | LpcRef::Object(_)
                        | LpcRef::Function(_) => {
                            let frame = self.stack.current_frame()?;
                            Err(lpc_error!(
                                frame.current_debug_span(),
                                "Range's receiver isn't actually an array or string?",
                            ))
                        }
                    }
                };

                let new_ref = get_new_value(&self.stack)?;
                return_value(new_ref, &mut self.stack)?;
            }
            Instruction::Ret => {
                pop_frame!(self).map(|frame| {
                    trace!("Returning from function: {}", frame.function.name());
                });

                // halt at the end of all input
                if self.stack.is_empty() {
                    self.state = TaskState::Complete;
                    return Ok(true);
                }
            }
            Instruction::Sizeof(r1, r2) => {
                let lpc_ref = &*get_loc!(self, r1)?;

                let new_ref = match lpc_ref {
                    LpcRef::Array(x) => {
                        let vec = x.read();

                        LpcRef::Int(LpcInt(vec.len() as LpcIntInner))
                    }
                    LpcRef::Mapping(x) => {
                        let map = x.read();

                        LpcRef::Int(LpcInt(map.len() as LpcIntInner))
                    }
                    LpcRef::String(x) => {
                        let string = x.read();

                        LpcRef::Int(LpcInt(string.len() as LpcIntInner))
                    }
                    LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::Object(_) | LpcRef::Function(_) => {
                        NULL
                    }
                };

                set_loc!(self, r2, new_ref)?;
            }
            Instruction::Store(value_loc, container_loc, index_loc) => {
                // r2[r3] = r1;
                self.handle_store(value_loc, container_loc, index_loc)?;
            }
            Instruction::SConst(location, index) => {
                self.handle_sconst(location, index)?;
            }
            Instruction::Shl(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.shl(y))?;
            }
            Instruction::Shr(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.shr(y))?;
            }
            Instruction::Xor(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitxor(y))?;
            }
        }

        Ok(false)
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
