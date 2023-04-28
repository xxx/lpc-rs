pub mod apply_function;
pub mod initialize_program;
pub mod into_task_context;
pub mod task_id;
pub mod task_state;
pub mod task_template;

use std::{
    borrow::Cow,
    fmt::{Debug, Display},
    sync::{Arc, Weak},
    time::Duration,
};

use async_recursion::async_recursion;
use bit_set::BitSet;
use decorum::Total;
use educe::Educe;
use futures::future::join_all;
use if_chain::if_chain;
use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_asm::{address::Address, instruction::Instruction};
use lpc_rs_core::{
    function_receiver::FunctionReceiver,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
    LpcIntInner,
};
use lpc_rs_errors::{lpc_bug, span::Span, LpcError, Result, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use string_interner::{DefaultSymbol, Symbol};
use thin_vec::{thin_vec, ThinVec};
use tokio::{sync::mpsc::Sender, task::JoinHandle, time::timeout};
use tracing::{error, instrument, trace, warn};
use ustr::ustr;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        call_frame::CallFrame,
        call_outs::CallOuts,
        call_stack::CallStack,
        efun::{call_efun, efun_context::EfunContext, EFUN_FUNCTIONS},
        function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
        gc::{gc_bank::GcRefBank, mark::Mark, unique_id::UniqueId},
        heap::Heap,
        into_lpc_ref::IntoLpcRef,
        lpc_array::LpcArray,
        lpc_int::LpcInt,
        lpc_mapping::LpcMapping,
        lpc_ref::{LpcRef, NULL},
        lpc_string::LpcString,
        object_flags::ObjectFlags,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        task::{task_id::TaskId, task_state::TaskState},
        task_context::TaskContext,
        vm::vm_op::VmOp,
    },
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
) -> Result<Cow<LpcRef>> {
    let frame = stack.current_frame()?;

    get_location_in_frame(frame, location)
}

/// Resolve any type RegisterVariant into an LpcRef, for the passed frame
#[instrument(skip(frame))]
#[inline]
pub fn get_location_in_frame(frame: &CallFrame, location: RegisterVariant) -> Result<Cow<LpcRef>> {
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
            let idx = upvalue_ptrs[upv.index()];

            let vm_upvalues = &frame.vm_upvalues.read();
            trace!("upvalue data: idx = {}, len = {}", idx, vm_upvalues.len());
            Ok(Cow::Owned(vm_upvalues[idx].clone()))
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
            let idx = upvalues[reg.index()];

            let vm_upvalues = &mut frame.vm_upvalues.write();
            func(&mut vm_upvalues[idx])
        }
    }
}

macro_rules! get_loc {
    ($self:expr, $loc:expr) => {{
        get_location(&$self.stack, $loc)
    }};
}

macro_rules! set_loc {
    ($self:expr, $loc:expr, $val:expr) => {{
        set_location(&mut $self.stack, $loc, $val)
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
    pub stack: Box<CallStack<STACKSIZE>>,

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
        Self {
            id: TaskId::new(),
            stack: Box::new(CallStack::default()),
            catch_points: thin_vec![],
            args: ThinVec::with_capacity(10),
            partial_args: ThinVec::with_capacity(10),
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
    #[allow(clippy::too_many_arguments)]
    #[instrument(skip_all)]
    pub async fn initialize_program<P, C, O, M, U, A>(
        program: P,
        config: C,
        object_space: O,
        memory: M,
        vm_upvalues: U,
        call_outs: A,
        this_player: Option<Arc<Process>>,
        upvalue_ptrs: Option<&[Register]>,
        tx: Sender<VmOp>,
    ) -> Result<Task<STACKSIZE>>
    where
        P: Into<Arc<Program>>,
        C: Into<Arc<Config>> + Debug,
        O: Into<Arc<ObjectSpace>>,
        M: Into<Arc<Heap>>,
        U: Into<Arc<RwLock<GcRefBank>>>,
        A: Into<Arc<RwLock<CallOuts>>>,
    {
        let program = program.into();
        let process: Arc<Process> = Process::new(program).into();
        let context = TaskContext::new(
            config,
            process.clone(),
            object_space,
            memory.into(),
            vm_upvalues,
            call_outs,
            this_player,
            upvalue_ptrs.map(|v| v.to_vec()),
            tx,
        );

        context.insert_process(process);

        Self::initialize_process(context).await
    }

    /// Initialize a [`Process`] by calling its initializer function, using the
    /// given [`TaskContext`].
    /// It's assumed that the process has already been inserted into the [`ObjectSpace`]
    pub async fn initialize_process(context: TaskContext) -> Result<Task<STACKSIZE>> {
        debug_assert!(!context.process.flags.test(ObjectFlags::INITIALIZED));

        let Some(initializer) = context.process.program.initializer.clone() else {
            let msg = "Init function not found on cloned object? This should never happen.";

            error!(msg);
            return Err(lpc_bug!(msg));
        };

        // We mark ourselves as initialized before actually initializing, to avoid
        // infinite loops where this_object() is used in global initialization.
        context.process.flags.set(ObjectFlags::INITIALIZED);

        let max_execution_time = context.config().max_execution_time;
        let mut task = Task::new(context);
        task.timed_eval(initializer, &[], max_execution_time)
            .await?;

        // TODO: do we need an error flag for initialization failures?

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
            let max_execution_time = task.context.config.max_execution_time;
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
                "evaluation limit of {}ms has been reached", timeout_ms
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
    pub async fn prepare_function_call(
        &mut self,
        process: Arc<Process>,
        f: Arc<ProgramFunction>,
        args: &[LpcRef],
    ) -> Result<()> {
        let mut frame = CallFrame::new(
            process,
            f,
            args.len(),
            self.context.upvalue_ptrs.as_deref(),
            self.context.upvalues().clone(),
        );
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
            debug_assert!(self.stack.is_empty());
            self.state = TaskState::Complete;
        } else {
            let mut halted = false;

            let mut c = 0_u16;

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
                self.binary_operation(r1, r2, r3, |x, y, _memory| x.bitand(y))?;
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
            Instruction::Call(name) => {
                self.handle_call(name)?;
            }
            Instruction::CallEfun(name_idx) => {
                let process = self.stack.current_frame()?.process.clone();
                let (pf, name) = {
                    let (name, pf) = EFUN_FUNCTIONS.get_index(name_idx).unwrap();

                    (pf.clone(), name)
                };

                let new_frame = self.prepare_new_call_frame(process, pf)?;

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
                self.handle_call_simul_efun(name_idx)?;
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
                name_index: name,
            } => {
                self.handle_functionptrconst(location, receiver, name)?;
            }
            Instruction::Gt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x > y)?;
            }
            Instruction::Gte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x >= y)?;
            }
            Instruction::IAdd(r1, r2, r3) => {
                match get_loc!(self, r1)?.add(&*get_loc!(self, r2)?, &self.context.memory) {
                    Ok(result) => {
                        set_loc!(self, r3, result)?;
                    }
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
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
            Instruction::IMul(r1, r2, r3) => {
                match get_loc!(self, r1)?.mul(&*get_loc!(self, r2)?, &self.context.memory) {
                    Ok(result) => set_loc!(self, r3, result)?,
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
            Instruction::Inc(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.inc())?;
            }
            Instruction::ISub(r1, r2, r3) => {
                match get_loc!(self, r1)?.sub(&*get_loc!(self, r2)?, &self.context.memory) {
                    Ok(result) => set_loc!(self, r3, result)?,
                    Err(mut e) => {
                        let frame = self.stack.current_frame()?;
                        *e = e.with_span(frame.current_debug_span());
                        return Err(e);
                    }
                }
            }
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
                self.binary_operation(r1, r2, r3, |x, y, memory| x.add(y, memory))?;
            }
            Instruction::MapConst(r) => {
                let mut register_map = IndexMap::with_capacity(self.array_items.len() / 2);

                debug_assert!(
                    self.array_items.len() % 2 == 0,
                    "Odd number of items in `array` when creating a mapping constant"
                );
                for chunk in &self.array_items.iter().copied().chunks(2) {
                    let (key, value) = chunk.into_iter().collect_tuple().unwrap();
                    register_map.insert(
                        get_loc!(self, key)?.into_owned(),
                        get_loc!(self, value)?.into_owned(),
                    );
                }

                let new_ref = LpcMapping::new(register_map.into_iter().collect())
                    .into_lpc_ref(&self.context.memory);

                set_loc!(self, r, new_ref)?;
            }
            Instruction::MMul(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, memory| x.mul(y, memory))?;
            }
            Instruction::MSub(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, memory| x.sub(y, memory))?;
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
                self.binary_operation(r1, r2, r3, |x, y, _memory| x.bitor(y))?;
            }
            Instruction::PopulateArgv(r, num_args, _num_locals) => {
                let frame = self.stack.current_frame()?;
                let arg_locations = &frame.arg_locations;
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

                let new_ref = LpcArray::new(refs).into_lpc_ref(&self.context.memory);

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
                frame.set_pc(frame.pc() + jump);
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
                                return Ok(LpcArray::new(vec![]).into_lpc_ref(&self.context.memory));
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
                                    Ok(LpcArray::new(new_vec).into_lpc_ref(&self.context.memory))
                                } else {
                                    Ok(LpcArray::new(vec![]).into_lpc_ref(&self.context.memory))
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
                                return Ok(LpcString::from("").into_lpc_ref(&self.context.memory));
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
                                    Ok(LpcString::from(new_string)
                                        .into_lpc_ref(&self.context.memory))
                                } else {
                                    Ok(LpcString::from("").into_lpc_ref(&self.context.memory))
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
                self.binary_operation(r1, r2, r3, |x, y, _memory| x.shl(y))?;
            }
            Instruction::Shr(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _memory| x.shr(y))?;
            }
            Instruction::Xor(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y, _memory| x.bitxor(y))?;
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
        let new_ref = LpcArray::new(vars).into_lpc_ref(&self.context.memory);

        set_loc!(self, location, new_ref)
    }

    #[instrument(skip_all)]
    fn handle_call<'task>(&mut self, name_idx: usize) -> Result<()> {
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

        let new_frame = self.prepare_new_call_frame(process, func)?;

        trace!("pushing new frame");

        self.stack.push(new_frame)?;

        Ok(())
    }

    /// Prepare and populate a new [`CallFrame`] for a call to a static function.
    fn prepare_new_call_frame(
        &mut self,
        process: Arc<Process>,
        func: Arc<ProgramFunction>,
    ) -> Result<CallFrame> {
        let num_args = self.args.len();
        let mut new_frame = CallFrame::with_minimum_arg_capacity(
            process,
            func.clone(),
            num_args,
            num_args,
            None, // static functions do not inherit upvalues from the calling function
            self.context.upvalues().clone(),
        );

        trace!("copying arguments to new frame: {num_args}");
        // copy argument registers from old frame to new
        if num_args > 0_usize {
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
                    i,
                    lpc_ref,
                    target_location
                );

                new_frame.arg_locations.push(target_location);

                new_frame.set_location(target_location, lpc_ref)
            }
        }

        Ok(new_frame)
    }

    #[instrument(skip_all)]
    #[inline]
    async fn handle_call_fp(&mut self, location: RegisterVariant) -> Result<()> {
        let num_args = self.args.len();
        let func = {
            let lpc_ref = &*get_loc!(self, location)?;

            if let LpcRef::Function(func) = lpc_ref {
                func.clone() // this is a cheap clone
            } else {
                return Err(
                    self.runtime_error(format!("callfp instruction on non-function: {}", lpc_ref))
                );
            }
        };

        let (
            mut new_frame,
            is_dynamic_receiver,
            function_is_efun,
            adjusted_num_args,
            max_arg_length,
        ) = {
            let ptr = func.read();

            trace!("Calling function ptr: {}", ptr);

            let passed_args_count = num_args
                + ptr
                    .partial_args
                    .iter()
                    .fold(0, |sum, arg| sum + arg.is_some() as usize);
            let function_is_efun = matches!(&ptr.address, FunctionAddress::Efun(_));
            let is_dynamic_receiver = matches!(&ptr.address, FunctionAddress::Dynamic(_));
            let is_call_other = ptr.call_other;

            if let FunctionAddress::Local(receiver, pf) = &ptr.address {
                let Some(receiver) = receiver.upgrade() else {
                    return Err(self.runtime_error(format!(
                        "attempted to call a pointer to a function in a destructed object: {}",
                        ptr
                    )));
                };

                if !pf.public()
                    && !pf.is_closure()
                    && (is_call_other || !Arc::ptr_eq(self.context.process(), &receiver))
                {
                    return set_loc!(self, Register(0).as_local(), NULL);
                }
            }

            let Some((proc, function)) = self.extract_process_and_function(&ptr)? else {
                return Ok(())
            };

            let Some(proc) = proc.upgrade() else {
                return Err(self.runtime_error(format!(
                    "attempted to call a pointer to a function in a destructed object: {}",
                    ptr
                )));
            };

            let adjusted_num_args = num_args - (is_dynamic_receiver as usize);

            let max_arg_length = std::cmp::max(adjusted_num_args, function.arity().num_args);
            let max_arg_length = std::cmp::max(max_arg_length, passed_args_count);

            let upvalues = if function.is_closure() {
                Some(&ptr.upvalue_ptrs)
            } else {
                // Calls to pointers to static functions do not inherit upvalues,
                // same as normal direct calls to them.
                None
            };

            (
                CallFrame::with_minimum_arg_capacity(
                    proc,
                    function,
                    passed_args_count,
                    max_arg_length,
                    upvalues.map(|f| f.as_slice()),
                    self.context.upvalues().clone(),
                ),
                is_dynamic_receiver,
                function_is_efun,
                adjusted_num_args,
                max_arg_length,
            )
        };

        // for dynamic receivers, skip the first register of the passed args, which contains the receiver itself
        let index = is_dynamic_receiver as usize;
        let from_slice = &self.args[index..(index + adjusted_num_args)];

        fn type_check_and_assign_location<const STACKSIZE: usize>(
            task: &Task<STACKSIZE>,
            new_frame: &mut CallFrame,
            loc: RegisterVariant,
            r: LpcRef,
            i: usize,
        ) -> Result<()> {
            let prototype = &new_frame.function.prototype;
            task.type_check_call_arg(
                &r,
                prototype.arg_types.get(i),
                prototype.arg_spans.get(i),
                &prototype.name,
            )?;

            trace!("Copying argument {} ({}) to {}", i, r, loc);

            new_frame.arg_locations.push(loc);
            new_frame.set_location(loc, r);

            Ok(())
        }

        let mut from_slice_index = 0;
        let mut next_index = 1;
        {
            // This read() needs to be dropped before the `await`.
            let ptr = func.read();
            let arg_locations = &new_frame.function.clone().arg_locations;

            for i in 0..max_arg_length {
                let target_location = arg_locations.get(i).copied().unwrap_or_else(|| {
                    // This should only be reached by variables that will go
                    // into an ellipsis function's argv.
                    Register(next_index).as_local()
                });

                if let RegisterVariant::Local(r) = target_location {
                    next_index = r.index() + 1;
                }

                if let Some(Some(lpc_ref)) = ptr.partial_args.get(i) {
                    // if a partially-applied arg is present, use it
                    type_check_and_assign_location(
                        self,
                        &mut new_frame,
                        target_location,
                        lpc_ref.clone(),
                        i,
                    )?;
                } else if let Some(location) = from_slice.get(from_slice_index) {
                    // check if the user passed an argument, which will
                    // fill in the next hole in the partial arguments, or
                    // append to the end

                    let lpc_ref = get_loc!(self, *location)?;
                    type_check_and_assign_location(
                        self,
                        &mut new_frame,
                        target_location,
                        lpc_ref.into_owned(),
                        i,
                    )?;

                    from_slice_index += 1;
                }
            }
        }

        let pf = new_frame.function.clone();
        self.stack.push(new_frame)?;

        if function_is_efun {
            self.prepare_and_call_efun(pf.name()).await?;
        }

        Ok(())
    }

    /// An extracted helper to handle pulling the [`Process`] and [`ProgramFunction`] out of a [`FunctionPtr`].
    pub fn extract_process_and_function(
        &mut self,
        ptr: &FunctionPtr,
    ) -> Result<Option<ProcessFunctionPair>> {
        let (proc, function) = match &ptr.address {
            FunctionAddress::Local(proc, function) => (proc.clone(), function.clone()),
            FunctionAddress::Dynamic(name) => {
                let LpcRef::Object(lpc_ref) = &*get_loc!(self, self.args[0])? else {
                    return Err(self.runtime_error("non-object receiver to function pointer call"));
                };

                let pair_opt = {
                    if let Some(proc) = lpc_ref.upgrade() {
                        proc.program
                            .lookup_function(name)
                            .map(|func| ((**lpc_ref).clone(), func.clone()))
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

                pair
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

    async fn prepare_and_call_efun<S>(&mut self, name: S) -> Result<()>
    where
        S: AsRef<str>,
    {
        let mut ctx = EfunContext::new(
            &mut self.stack,
            &self.context,
            self.context.memory().clone(),
        );

        call_efun(name.as_ref(), &mut ctx).await?;

        #[cfg(test)]
        {
            if ctx.snapshot.is_some() {
                self.snapshots.push(ctx.snapshot.unwrap());
            }
        }

        pop_frame!(self);

        Ok(())
    }

    #[instrument(skip_all)]
    #[inline]
    #[async_recursion]
    async fn handle_call_other(
        &mut self,
        receiver: RegisterVariant,
        name_location: RegisterVariant,
    ) -> Result<()> {
        // set up result_ref in a block, as `registers` is a long-lived reference that
        // doesn't work as mutable, but needs to be written to at the very end.
        let result_ref = {
            // figure out which function we're calling
            let receiver_ref = &*get_location(&self.stack, receiver)?;
            let name_ref = &*get_location(&self.stack, name_location)?;
            let pool_ref = if let LpcRef::String(r) = name_ref {
                r
            } else {
                let str = format!("Invalid name passed to `call_other`: {}", name_ref);
                return Err(self.runtime_error(str));
            };

            let function_name = pool_ref.clone();

            trace!(
                "Calling call_other: {}->{}",
                receiver_ref,
                function_name.read()
            );

            // An inner helper function to actually calculate the result, for easy re-use
            // when using `call_other` with arrays and mappings.
            #[async_recursion]
            async fn resolve_result<T>(
                receiver_ref: &LpcRef,
                function_name: T,
                args: &[LpcRef],
                task_context: &TaskContext,
            ) -> Result<LpcRef>
            where
                T: AsRef<str> + Send + Sync,
            {
                let resolved = Task::<MAX_CALL_STACK_SIZE>::resolve_call_other_receiver(
                    receiver_ref,
                    function_name.as_ref(),
                    task_context,
                )
                .await;

                if let Some(receiver) = resolved {
                    let new_context = task_context.clone().with_process(receiver.clone());
                    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(new_context);

                    // unwrap() is ok because resolve_call_other_receiver() checks
                    // for the function's presence.
                    let function = receiver
                        .program
                        .lookup_function(function_name.as_ref())
                        .unwrap()
                        .clone();

                    let result = if function.public() {
                        let max_execution_time = task_context.config.max_execution_time;
                        task.timed_eval(function, args, max_execution_time).await?;

                        let Some(r) = task.context.into_result() else {
                            return Err(lpc_bug!("resolve_result finished the task, but it has no result? wtf."));
                        };

                        r
                    } else {
                        NULL
                    };

                    Ok(result)
                } else {
                    Ok(NULL)
                }
            }

            let args = self
                .args
                .iter()
                .map(|i| get_loc!(self, *i).map(|r| r.into_owned()))
                .collect::<Result<Vec<_>>>()?;

            let function_name = Arc::new(function_name.read().clone());

            match &receiver_ref {
                LpcRef::String(_) | LpcRef::Object(_) => {
                    resolve_result(receiver_ref, &*function_name, &args, &self.context).await?
                }
                LpcRef::Array(r) => {
                    let refs = r.read().iter().cloned().collect_vec();
                    let args = Arc::new(args);

                    let futures = refs.iter().map(|lpc_ref| {
                        let fname = function_name.clone();
                        let args = args.clone();
                        let ctx = &self.context;

                        async move {
                            resolve_result(lpc_ref, &*fname, args.as_ref(), ctx)
                                .await
                                .unwrap_or(NULL)
                        }
                    });

                    let array_value = join_all(futures).await;
                    LpcArray::new(array_value).into_lpc_ref(self.context.memory())
                }
                LpcRef::Mapping(m) => {
                    let map = m
                        .read()
                        .iter()
                        .map(|(k, v)| (k.clone(), v.clone()))
                        .collect_vec();
                    let args = Arc::new(args);

                    let futures = map.iter().map(|(key_ref, value_ref)| {
                        let fname = function_name.clone();
                        let args = args.clone();
                        let ctx = &self.context;

                        async move {
                            (
                                key_ref.clone(),
                                resolve_result(value_ref, &*fname, &args, ctx)
                                    .await
                                    .unwrap_or(NULL),
                            )
                        }
                    });

                    let with_results = join_all(futures)
                        .await
                        .into_iter()
                        .collect::<IndexMap<_, _>>();

                    LpcMapping::new(with_results.into_iter().collect())
                        .into_lpc_ref(self.context.memory())
                }
                _ => {
                    return Err(self.runtime_error(format!(
                        "What are you trying to call `{function_name}` on?"
                    )))
                }
            }
        };

        let registers = &mut self.stack.current_frame_mut()?.registers;
        registers[0] = result_ref;

        Ok(())
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_call_simul_efun(&mut self, name_idx: usize) -> Result<()> {
        let Some(func_name) = self.stack.current_frame()?.function.strings.get().unwrap().resolve(Self::index_symbol(name_idx)) else {
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
                let msg = format!("Call to unknown function `{func_name}`");
                return Err(self.runtime_error(msg));
            }
        };

        let new_frame = self.prepare_new_call_frame(simul_efuns.clone(), func)?;

        self.stack.push(new_frame)?;

        Ok(())
    }

    #[instrument(skip_all)]
    #[inline]
    fn handle_functionptrconst(
        &mut self,
        location: RegisterVariant,
        receiver: FunctionReceiver,
        name_idx: usize,
    ) -> Result<()> {
        let call_other = match receiver {
            FunctionReceiver::Var(_) | FunctionReceiver::Dynamic => true,
            FunctionReceiver::Local | FunctionReceiver::Efun | FunctionReceiver::SimulEfun => false,
        };

        let Some(func_name) = self.stack.current_frame()?.function.strings.get().unwrap().resolve(Self::index_symbol(name_idx)) else {
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
                        let weak_process = (**weak_process).clone();
                        FunctionAddress::Local(weak_process, func)
                    }
                    LpcRef::String(s) => {
                        let process = {
                            let path = s.read();

                            let Some(process) = self.context.lookup_process(&*path) else {
                                return Err(self.runtime_error(format!(
                                    "Unable to find object `{}`.",
                                    path
                                )));
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
            .collect::<Result<Vec<Option<LpcRef>>>>()?;

        let frame = self.stack.current_frame()?;
        let fp = FunctionPtr {
            owner: Arc::downgrade(&frame.process),
            address,
            partial_args,
            call_other,
            // Function pointers inherit the creating function's upvalues
            upvalue_ptrs: frame.upvalue_ptrs.clone(),
            unique_id: UniqueId::new(),
        };

        let new_ref = fp.into_lpc_ref(self.context.memory());

        set_loc!(self, location, new_ref)
    }

    #[instrument(skip_all)]
    fn capture_environment(&mut self) -> Result<Vec<Register>> {
        let frame = self.stack.current_frame_mut()?;
        let mut upvalues = self.context.upvalues().write();

        trace!("ptrs: {:?}", frame.upvalue_ptrs);
        trace!("upvalues: {:?}", upvalues);

        frame
            .upvalue_ptrs
            .iter()
            .map(|ptr| {
                let upvalue = upvalues.get(ptr.index()).cloned().unwrap_or_default();
                let new_index = upvalues.insert(upvalue);
                Ok(Register(new_index))
            })
            .collect::<Result<Vec<Register>>>()
    }

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
                            )
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
                    )
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

        let new_ref = lpc_string.into_lpc_ref(self.context.memory());

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

    #[instrument(skip_all)]
    async fn resolve_call_other_receiver<T>(
        receiver_ref: &LpcRef,
        name: T,
        context: &TaskContext,
    ) -> Option<Arc<Process>>
    where
        T: AsRef<str>,
    {
        let process = match receiver_ref {
            LpcRef::String(s) => {
                let r = s.read();
                let str = r.to_str();

                match context.lookup_process(str) {
                    Some(proc) => proc,
                    None => {
                        // TODO: this should create a new master object if the path points to one
                        return None;
                    }
                }
            }
            LpcRef::Object(proc) => {
                let Some(proc) = proc.upgrade() else {
                    return None;
                };

                proc
            }
            _ => return None,
        };

        // If uninitialized, it's time to set that up. Note that we do this regardless
        // of whether the function exists or not, because this is a primary way of
        // initializing objects. If you've ever seen a call_other to teleledningsanka()
        // or some other knowingly undefined function in old lib code, this is why.
        let result = if !process.flags.test(ObjectFlags::INITIALIZED) {
            let ctx = context.clone().with_process(process);
            let Ok(task) = Self::initialize_process(ctx).await else {
                return None;
            };

            task.context.process
        } else {
            process
        };

        // Only switch the process if there's actually a function to
        // call by this name on the other side.
        if result.program.contains_function(name) {
            Some(result)
        } else {
            None
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

        // clear away stack frames that won't be executed any further, which lie between
        // the error and the catch point's stack frame.
        // Does nothing if you're already in the correct stack frame, or one away.
        self.stack.truncate(frame_index + 2);

        // If these aren't equal, we're already in the correct stack frame.
        if self.stack.len() == frame_index + 2 {
            // Pop the final frame via pop_frame(), to keep other state changes to a single
            // code path, (e.g. changing the current process)
            self.pop_frame();
        }

        if self.stack.is_empty() {
            return Err(self.runtime_bug("stack is empty after popping to catch point?"));
        }

        // set up the catch point's return value
        let value = LpcString::from(error.to_string());
        let lpc_ref = value.into_lpc_ref(self.context.memory());
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
        F: Fn(&LpcRef, &LpcRef, &Heap) -> Result<LpcRef>,
    {
        let ref1 = &*get_location(&self.stack, r1)?;
        let ref2 = &*get_location(&self.stack, r2)?;

        match operation(ref1, ref2, &self.context.memory) {
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

    /// Negotiate how much space needs to be made for a call to a function pointer.
    ///
    /// # Arguments
    ///
    /// num_args: the number of arguments actually passed to the function for this call
    /// partial_args: the arguments that were passed to the function when the function pointer was created
    ///
    /// # Returns
    ///
    /// The maximum number of arguments that space needs to be made for.
    #[instrument(skip_all)]
    #[inline]
    fn calculate_max_arg_length<T>(num_args: usize, partial_args: &[Option<T>]) -> usize {
        let none_args = partial_args.iter().filter(|a| a.is_none()).count();
        partial_args.len() + num_args.saturating_sub(none_args)
    }

    #[inline]
    fn index_symbol(index: usize) -> DefaultSymbol {
        DefaultSymbol::try_from_usize(index).unwrap()
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

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        fmt::Formatter,
        hash::{Hash, Hasher},
    };

    use indoc::indoc;
    use lpc_rs_core::{LpcFloatInner, LpcIntInner};
    use tokio::sync::mpsc;

    use super::*;
    use crate::{
        interpreter::gc::gc_bank::GcBank,
        test_support::{compile_prog, run_prog},
    };

    #[allow(dead_code)]
    fn format_slice<I>(slice: &[I]) -> String
    where
        I: Display,
    {
        let mut ret = String::new();
        ret.push_str("[\n");

        for i in slice {
            ret.push_str(&format!("  {i},\n"));
        }

        ret.push(']');

        ret
    }

    #[allow(dead_code)]
    fn format_map<'a, M, K, V>(map: M) -> String
    where
        M: IntoIterator<Item = (&'a K, &'a V)>,
        K: Display + 'a,
        V: Display + 'a,
    {
        let mut ret = String::new();
        ret.push_str("{\n");

        for (k, v) in map {
            ret.push_str(&format!("  {k}: {v},\n"));
        }

        ret.push('}');

        ret
    }

    /// A type to make it easier to set up test expectations for register contents
    #[derive(Debug, Eq, Clone)]
    enum BareVal {
        String(String),
        Int(LpcIntInner),
        Float(LpcFloatInner),
        Array(Vec<BareVal>),
        Mapping(HashMap<BareVal, BareVal>),
        Object(String),                         // Just the filename
        Function(String, Vec<Option<BareVal>>), // name and args
    }

    impl BareVal {
        pub fn from_lpc_ref(lpc_ref: &LpcRef) -> Self {
            match lpc_ref {
                LpcRef::Float(x) => BareVal::Float(x.0),
                LpcRef::Int(x) => BareVal::Int(x.0),
                LpcRef::String(x) => {
                    let s = x.read();
                    BareVal::String(s.to_string())
                }
                LpcRef::Array(x) => {
                    let a = x.read();
                    let array = a.iter().map(BareVal::from_lpc_ref).collect::<Vec<_>>();
                    BareVal::Array(array)
                }
                LpcRef::Mapping(x) => {
                    let m = x.read();
                    let mapping = m
                        .iter()
                        .map(|(k, v)| (BareVal::from_lpc_ref(k), BareVal::from_lpc_ref(v)))
                        .collect::<HashMap<_, _>>();
                    BareVal::Mapping(mapping)
                }
                LpcRef::Object(o) => {
                    if let Some(o) = o.upgrade() {
                        let filename = o.filename().into_owned();
                        BareVal::Object(filename)
                    } else {
                        BareVal::Int(0)
                    }
                }
                LpcRef::Function(x) => {
                    let fp = x.read();
                    let args = fp
                        .partial_args
                        .iter()
                        .map(|item| item.as_ref().map(BareVal::from_lpc_ref))
                        .collect::<Vec<_>>();

                    BareVal::Function(fp.name().into(), args)
                }
            }
        }

        pub fn equal_to_lpc_ref(&self, other: &LpcRef) -> bool {
            self == &BareVal::from_lpc_ref(other)
        }

        pub fn assert_equal(&self, other: &LpcRef) {
            assert_eq!(self, &BareVal::from_lpc_ref(other));
        }

        pub fn assert_vec_equal(a: &[BareVal], b: &[LpcRef]) {
            assert_eq!(
                a.len(),
                b.len(),
                "Vectors {:?} and {:?} are of different lengths",
                a,
                b
            );
            for (a, b) in a.iter().zip(b.iter()) {
                a.assert_equal(b);
            }
        }
    }

    impl PartialEq<&LpcRef> for BareVal {
        fn eq(&self, lpc_ref: &&LpcRef) -> bool {
            &BareVal::from_lpc_ref(lpc_ref) == self
        }
    }

    impl Hash for BareVal {
        fn hash<H: Hasher>(&self, state: &mut H) {
            match self {
                BareVal::Float(x) => x.hash(state),
                BareVal::Int(x) => x.hash(state),
                BareVal::String(x) => x.hash(state),
                BareVal::Array(x) => std::ptr::hash(&**x, state),
                BareVal::Mapping(x) => std::ptr::hash(x, state),
                BareVal::Object(x) => std::ptr::hash(x, state),
                BareVal::Function(x, y) => {
                    x.hash(state);
                    y.hash(state);
                }
            }
        }
    }

    impl PartialEq for BareVal {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (BareVal::Float(x), BareVal::Float(y)) => x == y,
                (BareVal::Int(x), BareVal::Int(y)) => x == y,
                (BareVal::String(x), BareVal::String(y)) => x == y,
                (BareVal::Array(x), BareVal::Array(y)) => x == y,
                (BareVal::Mapping(x), BareVal::Mapping(y)) => x == y,
                (BareVal::Object(x), BareVal::Object(y)) => x == y,
                (BareVal::Function(x, y), BareVal::Function(a, b)) => x == a && y == b,
                _ => false,
            }
        }
    }

    impl Display for BareVal {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            match self {
                BareVal::Float(x) => write!(f, "{x}"),
                BareVal::Int(x) => write!(f, "{x}"),
                BareVal::String(x) => write!(f, "\"{x}\""),
                BareVal::Array(x) => write!(f, "{}", format_slice(x)),
                BareVal::Mapping(x) => write!(f, "{}", format_map(x)),
                BareVal::Object(x) => write!(f, "object({x})"),
                BareVal::Function(x, y) => {
                    write!(f, "function({x}")?;
                    for arg in y {
                        match arg {
                            Some(x) => write!(f, ", {x}")?,
                            None => write!(f, ", <partial>")?,
                        }
                    }
                    write!(f, ")")
                }
            }
        }
    }

    mod test_instructions {
        use super::*;
        use crate::interpreter::{bank::RefBank, task::tests::BareVal::*};

        async fn snapshot_registers(code: &str) -> RefBank {
            let mut task = run_prog(code).await;
            let mut stack = task.snapshots.pop().unwrap();

            // The top of the stack in the snapshot is the object initialization frame,
            // which is not what we care about here, so we get the second-to-top frame
            // instead.
            let index = stack.len() - 2;

            std::mem::take(&mut stack[index].registers)
        }

        mod test_aconst {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed *a = ({ 12, 4.3, "hello", ({ 1, 2, 3 }) });
                "##};
                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(12),
                    Float(LpcFloatInner::from(4.3)),
                    String("hello".into()),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)]),
                    Array(vec![
                        Int(12),
                        Float(LpcFloatInner::from(4.3)),
                        String("hello".into()),
                        Array(vec![Int(1), Int(2), Int(3)]),
                    ]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_and {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 & 27;
                    mixed b = 0 & a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(11), Int(0), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_andand {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 && 333;
                    mixed b = 0;
                    mixed c = b && a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(123), Int(333), Int(333), Int(0), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_bitwise_not {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    int a = ~0;
                    int b = 7;
                    int c = ~b;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(-1), Int(7), Int(-8)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_call {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = tacos();
                    int tacos() { return 666; }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(666), Int(666)];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn calls_correct_function() {
                let code = indoc! { r##"
                    inherit "/std/object";
                    mixed mine = public_function();
                    mixed parents = ::public_function();

                    string public_function() {
                        return "my public_function";
                    }
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let proc = ctx.process();
                let values = proc.global_variable_values();
                String("my public_function".into()).assert_equal(values.get("mine").unwrap());
                String("/std/object public".into()).assert_equal(values.get("parents").unwrap());
            }

            #[tokio::test]
            async fn calls_correct_function_with_efuns() {
                let code = indoc! { r##"
                    object ob = clone_object("/std/object");
                    mixed this_one = file_name(ob);
                    mixed efun_one = efun::file_name(ob);

                    string file_name(object ob) {
                        return "file_name_override";
                    }
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let proc = ctx.process();
                let values = proc.global_variable_values();
                assert_eq!(
                    String("file_name_override".into()),
                    values.get("this_one").unwrap()
                );
                assert_eq!(
                    String("/std/object#0".into()),
                    values.get("efun_one").unwrap()
                );
            }

            #[tokio::test]
            async fn calls_correct_function_with_simul_efuns() {
                // this is deprecated behavior that emits a warning, but probably won't ever be removed completely.
                let code = indoc! { r##"
                    string this_one = simul_efun("marf");
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let proc = ctx.process();
                let values = proc.global_variable_values();
                String("this is a simul_efun: marf".into())
                    .assert_equal(values.get("this_one").unwrap());
            }
        }

        mod test_call_efun {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = this_object();
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Object("/my_file".into()), Object("/my_file".into())];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_call_simul_efun {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = simul_efun("marf");
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("this is a simul_efun: marf".into()),
                    String("marf".into()),
                    String("this is a simul_efun: marf".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_call_fp {
            use claims::assert_ok;
            use tokio::sync::mpsc;

            use super::*;
            use crate::interpreter::task::initialize_program::InitializeProgramBuilder;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    function q = tacos;
                    int a = q(666);
                    int tacos(int j) { return j + 1; }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(667),
                    Function("tacos".into(), vec![]),
                    Int(666),
                    Int(667),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_partial_applications() {
                let code = indoc! { r##"
                    function q = &tacos(, "adding some!");
                    int a = q(666, 4);
                    string tacos(int j, string s, int k) {
                        return s + " " +  (j + k);
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("adding some! 670".into()),
                    String("adding some!".into()),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
                    Int(666),
                    Int(4),
                    String("adding some! 670".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_partial_applications_with_no_added_args() {
                let code = indoc! { r##"
                    function q = &tacos("my_string!");
                    int a = q();
                    string tacos(string s) {
                        return s + " awesome!" ;
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("my_string! awesome!".into()),
                    String("my_string!".into()),
                    Function("tacos".into(), vec![Some(String("my_string!".into()))]),
                    String("my_string! awesome!".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_partial_applications_with_default_arguments() {
                let code = indoc! { r##"
                    function q = &tacos(, "adding some!");
                    int a = q(666, 4);
                    int b = q(123);
                    string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("adding some! 223".into()),
                    String("adding some!".into()),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
                    Int(666),
                    Int(4),
                    String("adding some! 670".into()),
                    Int(123),
                    String("adding some! 223".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_partial_applications_with_default_arguments_and_ellipsis()
            {
                let code = indoc! { r##"
                    function q = &tacos(, "adding some!", , 666, 123);
                    int a = q(42, 4, "should be in argv");
                    int b = q(69);
                    int tacos(int j, string s, int k = 100, ...) {
                        dump("argv!");
                        dump(argv);
                        return j + k;
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(69),
                    String("adding some!".into()),
                    Int(666),
                    Int(123),
                    Function(
                        "tacos".to_string(),
                        vec![
                            None,
                            Some(String("adding some!".into())),
                            None,
                            Some(Int(666)),
                            Some(Int(123)),
                        ],
                    ),
                    Int(42),
                    Int(4),
                    String("should be in argv".into()),
                    Int(46),
                    Int(69),
                    Int(69),
                ];
                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_dynamic_receivers() {
                let code = indoc! { r##"
                    function q = &->name(, "awesome!");

                    int a = q(this_object(), 666);
                    int b = q(clone_object("/std/widget"), 42);

                    string name(int rank, string reaction) {
                        return "me: " + rank + ". " + reaction;
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("widget: 42. awesome!".into()),
                    String("awesome!".into()),
                    Function("name".into(), vec![None, Some(String("awesome!".into()))]),
                    Object("/my_file".into()),
                    Int(666),
                    String("me: 666. awesome!".into()),
                    String("/std/widget".into()),
                    Object("/std/widget#0".into()),
                    Int(42),
                    String("widget: 42. awesome!".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn is_0_for_call_other_private_functions() {
                let code = indoc! { r##"
                    function q = &(this_object())->tacos(, "adding some!");
                    int a = q(666, 4);
                    int b = q(123);
                    private string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("adding some!".into()),
                    Object("/my_file".into()),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
                    Int(666),
                    Int(4),
                    Int(0),
                    Int(123),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn is_normal_call_for_local_private_functions() {
                let code = indoc! { r##"
                    function q = tacos;
                    int a = q(4);
                    private int tacos(int j) {
                        return j;
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(4), Function("tacos".into(), vec![]), Int(4), Int(4)];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn checks_types() {
                let code = indoc! { r##"
                    function q = &tacos("foo");
                    int a = q();
                    private int tacos(int j) {
                        return j;
                    }
                "##};

                let object_space = ObjectSpace::default();
                let upvalues = GcBank::default();
                let (tx, _rx) = mpsc::channel(128);
                let call_outs = Arc::new(RwLock::new(CallOuts::new(tx.clone())));

                let (program, config, process) = compile_prog(code).await;
                let space_cell = object_space.into();
                ObjectSpace::insert_process(&space_cell, process);
                let vm_upvalues = Arc::new(RwLock::new(upvalues));

                let result = InitializeProgramBuilder::<32>::default()
                    .program(program)
                    .config(config)
                    .object_space(space_cell.clone())
                    .vm_upvalues(vm_upvalues.clone())
                    .call_outs(call_outs.clone())
                    .tx(tx.clone())
                    .build()
                    .await;

                assert_eq!(
                    result.unwrap_err().to_string(),
                    "runtime error: unexpected argument type to `tacos`: string. expected int."
                );

                let code = indoc! { r##"
                    function q = &tacos(5, , 666);

                    int a = q(123.4);

                    private int tacos(int i, string s, int j) {
                        return i + j;
                    }
                "##};

                let (program, config, process) = compile_prog(code).await;
                ObjectSpace::insert_process(&space_cell, process);

                let result = InitializeProgramBuilder::<10>::default()
                    .program(program)
                    .config(config)
                    .object_space(space_cell.clone())
                    .vm_upvalues(vm_upvalues.clone())
                    .call_outs(call_outs.clone())
                    .tx(tx.clone())
                    .build()
                    .await;

                assert_eq!(
                    result.unwrap_err().to_string(),
                    "runtime error: unexpected argument type to `tacos`: float. expected string."
                );

                let code = indoc! { r##"
                    function f = taco_maker();

                    string name = f("carne asada");

                    private function taco_maker() {
                        return (: [string name, float price = 1.00] name :);
                    }
                "##};

                let (program, config, process) = compile_prog(code).await;
                let object_space = ObjectSpace::default();
                let space_cell = object_space.into();
                ObjectSpace::insert_process(&space_cell, process);

                let result = InitializeProgramBuilder::<20>::default()
                    .program(program)
                    .config(config)
                    .object_space(space_cell)
                    .vm_upvalues(vm_upvalues)
                    .call_outs(call_outs)
                    .tx(tx)
                    .build()
                    .await;

                assert_ok!(result);
            }
        }

        mod test_call_other {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    int tacos() { return 666; }
                "##};

                let task = run_prog(code).await;
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(666),
                    Object("/my_file".into()),
                    String("tacos".into()),
                    Int(666),
                ];

                BareVal::assert_vec_equal(&expected, registers);
            }

            #[tokio::test]
            async fn returns_0_for_private_functions() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    private int tacos() { return 666; }
                "##};

                let task = run_prog(code).await;
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Object("/my_file".into()),
                    String("tacos".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, registers);
            }

            #[tokio::test]
            async fn returns_0_for_protected_functions() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    protected int tacos() { return 666; }
                "##};

                let task = run_prog(code).await;
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Object("/my_file".into()),
                    String("tacos".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, registers);
            }

            #[tokio::test]
            async fn returns_0_for_unknown_receiver() {
                let code = indoc! { r##"
                    mixed q = "/foobarbaz"->tacos();
                "##};

                let task = run_prog(code).await;
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("/foobarbaz".into()),
                    String("tacos".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, registers);
            }
        }

        mod test_catch {
            use super::*;

            #[tokio::test]
            async fn stores_the_error_string() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(10 / j);

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(0),
                    String("Runtime Error: Division by zero".into()),
                    Int(10),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_zero_when_no_error() {
                let code = indoc! { r##"
                    void create() {
                        int j = 5;
                        catch(10 / j);

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(5),
                    Int(0),
                    Int(10),
                    Int(2),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_catch_end {
            use super::*;

            #[tokio::test]
            async fn pops_the_catch_point() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(catch(catch(catch(10 / j))));
                    }
                "##};

                let task = run_prog(code).await;

                assert!(task.catch_points.is_empty());
            }
        }

        mod test_dec {
            use super::*;

            #[tokio::test]
            async fn stores_the_value_for_pre() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        --j;

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![Int(0), Int(-1), String("snapshot_stack".into()), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_pre_when_global() {
                let code = indoc! { r##"
                    int j = 5;
                    int k = --j;
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let expected = vec![Int(4), Int(4)];

                let proc = ctx.process();

                BareVal::assert_vec_equal(
                    &expected,
                    &proc.globals.read().iter().cloned().collect::<Vec<_>>(),
                );
            }

            #[tokio::test]
            async fn stores_the_value_for_post() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        j--;

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(-1),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_post_when_global() {
                let code = indoc! { r##"
                    int j = 5;
                    int k = j--;
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let expected = vec![Int(4), Int(5)];

                let proc = ctx.process();

                BareVal::assert_vec_equal(
                    &expected,
                    &proc.globals.read().iter().cloned().collect::<Vec<_>>(),
                );
            }
        }

        mod test_eq_eq {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 2 == 2;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(2), Int(2), Int(1)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_fconst {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    float π = 4.13;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Float(4.13.into())];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_functionptrconst {
            use super::*;

            #[tokio::test]
            async fn stores_the_value_for_efuns() {
                let code = indoc! { r##"
                    function f = dump;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Function("dump".to_string(), vec![])];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_simul_efuns() {
                let code = indoc! { r##"
                    function f = simul_efun;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Function("simul_efun".to_string(), vec![])];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_call_other() {
                let code = indoc! { r##"
                    function f = &(this_object())->tacco();

                    void tacco() {
                        dump("tacco!");
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Object("/my_file".into()),
                    Object("/my_file".into()),
                    Function("tacco".to_string(), vec![]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_call_other_string_receiver() {
                let code = indoc! { r##"
                    function f = &("/secure/simul_efuns")->simul_efun();
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("/secure/simul_efuns".into()),
                    Function("simul_efun".to_string(), vec![]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_with_args() {
                let code = indoc! { r##"
                    function f = &tacco(1, 666);

                    void tacco(int a, int b) {
                        dump(a + b);
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(666),
                    Function("tacco".to_string(), vec![Some(Int(1)), Some(Int(666))]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_with_partial_applications() {
                let code = indoc! { r##"
                    function f = &tacco(1, , , 42, );

                    void tacco(int a, int b, int c, int d, int e) {
                        dump(a + b - c * (d + e));
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(42),
                    Function(
                        "tacco".to_string(),
                        vec![Some(Int(1)), None, None, Some(Int(42)), None],
                    ),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_closures() {
                let code = indoc! { r##"
                    function f = maker();

                    function maker() {
                        int i = 666;
                        return (: i + $1 :);
                    }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Function("closure-0".to_string(), vec![]),
                    Function("closure-0".to_string(), vec![]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_gt {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 > 1199;
                    mixed r = 1199 > 1200;
                    mixed s = 1200 > 1200;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1200),
                    Int(1199),
                    Int(1),
                    Int(1199),
                    Int(1200),
                    Int(0),
                    Int(1200),
                    Int(1200),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_gte {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 >= 1199;
                    mixed r = 1199 >= 1200;
                    mixed s = 1200 >= 1200;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1200),
                    Int(1199),
                    Int(1),
                    Int(1199),
                    Int(1200),
                    Int(0),
                    Int(1200),
                    Int(1200),
                    Int(1),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_iadd {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    int q = 16 + 34;
                    int r = 12 + -4;
                    int s = q + r;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(50),
                    Int(8),
                    Int(58),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_iconst {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 666;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(666)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_iconst0 {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 0;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_iconst1 {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(1)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_idiv {
            use super::*;
            use crate::interpreter::task::initialize_program::InitializeProgramBuilder;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 / 2;
                    mixed r = 12 / -4;
                    mixed s = q / r;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(8),
                    Int(-3),
                    Int(-2),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn errors_on_division_by_zero() {
                let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q / r;
                "##};

                let (program, _, _) = compile_prog(code).await;
                let (tx, _rx) = mpsc::channel(128);

                let r = InitializeProgramBuilder::<10>::default()
                    .program(program)
                    .tx(tx)
                    .build()
                    .await;

                assert_eq!(
                    r.unwrap_err().to_string(),
                    "Runtime Error: Division by zero"
                )
            }
        }

        mod test_imod {
            use super::*;
            use crate::interpreter::task::initialize_program::InitializeProgramBuilder;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 % 7;
                    mixed r = 12 % -7;
                    mixed s = q % r;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(2),
                    Int(5),
                    Int(2),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn errors_on_division_by_zero() {
                let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q % r;
                "##};

                let (program, _, _) = compile_prog(code).await;
                let (tx, _rx) = mpsc::channel(128);

                let r = InitializeProgramBuilder::<20>::default()
                    .program(program)
                    .tx(tx)
                    .build()
                    .await;

                assert_eq!(
                    r.unwrap_err().to_string(),
                    "Runtime Error: Remainder division by zero"
                )
            }
        }

        mod test_imul {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    int q = 16 * 2;
                    int r = 12 * -4;
                    int s = q * r;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(32), Int(-48), Int(-1536)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_inc {
            use super::*;

            #[tokio::test]
            async fn stores_the_value_for_pre() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        ++j;

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![Int(0), Int(1), String("snapshot_stack".into()), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_pre_when_global() {
                let code = indoc! { r##"
                    int j = 0;
                    int k = ++j;
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let expected = vec![Int(1), Int(1)];

                let proc = ctx.process();

                BareVal::assert_vec_equal(
                    &expected,
                    &proc.globals.read().iter().cloned().collect::<Vec<_>>(),
                );
            }

            #[tokio::test]
            async fn stores_the_value_for_post() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        j++;

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_post_when_global() {
                let code = indoc! { r##"
                    int j = 5;
                    int k = j++;
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;

                let expected = vec![Int(6), Int(5)];

                let proc = ctx.process();

                BareVal::assert_vec_equal(
                    &expected,
                    &proc.globals.read().iter().cloned().collect::<Vec<_>>(),
                );
            }
        }

        mod test_isub {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    int q = 16 - 2;
                    int r = 12 - -4;
                    int s = q - r;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(14), Int(16), Int(-2)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_jmp {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        mixed j;
                        int i = 12;
                        if (i > 10) {
                            j = 69;
                        } else {
                            j = 3;
                        }

                        // Store a snapshot, so we can test this even though this stack
                        // frame would otherwise have been popped off into the aether.
                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(69),
                    Int(12),
                    Int(10),
                    Int(1),
                    Int(69),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_jnz {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        int j;
                        do {
                            j += 1;
                        } while(j < 8);

                        // Store a snapshot, so we can test this even though this stack
                        // frame would otherwise have been popped off into the aether.
                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(8),
                    Int(1),
                    Int(8),
                    Int(8),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_jz {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                            int i = 12;
                            int j = i > 12 ? 10 : 1000;
                        "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(12),
                    Int(1000),
                    Int(12),
                    Int(0),
                    Int(0),
                    Int(1000),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_load {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    int *i = ({ 1, 2, 3 });
                    int j = i[1];
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)]),
                    Int(1),
                    Int(2),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_lt {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 < 1199;
                    mixed r = 1199 < 1200;
                    mixed s = 1200 < 1200;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1200),
                    Int(1199),
                    Int(0),
                    Int(1199),
                    Int(1200),
                    Int(1),
                    Int(1200),
                    Int(1200),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_lte {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 <= 1199;
                    mixed r = 1199 <= 1200;
                    mixed s = 1200 <= 1200;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1200),
                    Int(1199),
                    Int(0),
                    Int(1199),
                    Int(1200),
                    Int(1),
                    Int(1200),
                    Int(1200),
                    Int(1),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_mapconst {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = ([
                        "asdf": 123,
                        456: 4.13
                    ]);
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let mut hashmap = HashMap::new();
                hashmap.insert(String("asdf".into()), Int(123));
                hashmap.insert(Int(456), Float(4.13.into()));

                let expected = vec![
                    Int(0),
                    String("asdf".into()),
                    Int(123),
                    Int(456),
                    Float(4.13.into()),
                    Mapping(hashmap),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_madd {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = "abc";
                    mixed b = 123;
                    mixed c = a + b;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("abc".into()),
                    Int(123),
                    String("abc123".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_mmul {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = "abc";
                    mixed b = 4;
                    mixed c = a * b;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("abc".into()),
                    Int(4),
                    String("abcabcabcabc".into()),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_msub {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = ({ 1, 1, 2, 3 });
                    mixed b = a - ({ 1 });
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(1), Int(2), Int(3)]),
                    Int(1),
                    Array(vec![Int(1)]),
                    Array(vec![Int(2), Int(3)]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_not {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = !2;
                    mixed b = !!4;
                    float c = !0.00;
                    float d = !0.01;
                    string e = !"";
                    string f = !"asdf";
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(2),
                    Int(0),
                    Int(4),
                    Int(0),
                    Int(1),
                    Float(Total::from(0.0)),
                    Int(1),
                    Float(Total::from(0.01)),
                    Int(0),
                    String("".into()),
                    Int(0),
                    String("asdf".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_or {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 | 27;
                    mixed b = 0 | a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(31), Int(0), Int(31)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_oror {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 || 333;
                    mixed b = 0;
                    mixed c = b || a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(123), Int(123), Int(0), Int(0), Int(123)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_populate_argv {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        do_thing(1, 2, 3, "foo", ({ "bar", "baz", 4.13 }), ([ "a": 123 ]));
                    }

                    void do_thing(int a, int b, ...) {
                        dump(argv);
                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let mut mapping = HashMap::new();
                mapping.insert(String("a".into()), Int(123));

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Array(vec![
                        Int(3),
                        String("foo".into()),
                        Array(vec![
                            String("bar".into()),
                            String("baz".into()),
                            Float(4.13.into()),
                        ]),
                        Mapping(mapping.clone()),
                    ]),
                    String("snapshot_stack".into()),
                    Array(vec![
                        String("bar".into()),
                        String("baz".into()),
                        Float(4.13.into()),
                    ]),
                    Mapping(mapping),
                    Int(0),
                    Int(0),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn test_creates_empty_array() {
                let code = indoc! { r##"
                    void create() {
                        function f = (: [int i = 69, ...] dump(i, argv); argv :);
                        f();
                    }
                "##};

                let task = run_prog(code).await;
                let ctx = task.context;
                Array(vec![]).assert_equal(ctx.result().unwrap());
            }
        }

        mod test_populate_defaults {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        do_thing(45, 34, 7.77);
                    }

                    void do_thing(int a, int b, float d = 6.66, string s = "snuh", mixed *muh = ({ "a string", 3, 2.44 })) {
                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let mut mapping = HashMap::new();
                mapping.insert(String("a".into()), Int(123));

                let expected = vec![
                    Int(0),
                    Int(45),
                    Int(34),
                    Float(7.77.into()),
                    String("snuh".into()),
                    Array(vec![String("a string".into()), Int(3), Float(2.44.into())]),
                    String("snapshot_stack".into()),
                    Int(0),
                    Int(0),
                    String("snuh".into()),
                    String("a string".into()),
                    Int(3),
                    Float(2.44.into()),
                    Array(vec![String("a string".into()), Int(3), Float(2.44.into())]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_range {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = ({ 1, 2, 3 })[1..];
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)]),
                    Int(1),
                    Int(-1),
                    Array(vec![Int(2), Int(3)]),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_regcopy {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 4;
                    mixed b = a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(4)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_ret {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    int create() { return 666; }
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(666), // return value from create()
                    Int(666), // The copy of the call return value into its own register
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_shl {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 12345 << 6;
                    mixed b = 0 << a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(790080), Int(0), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_shr {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 12345 >> 6;
                    mixed b = 0 >> a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(192), Int(0), Int(0)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_sizeof {
            use std::sync::Arc;

            use lpc_rs_asm::instruction::Instruction::{SConst, Sizeof};
            use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType, INIT_PROGRAM};
            use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;
            use once_cell::sync::OnceCell;
            use string_interner::StringInterner;

            use super::*;
            use crate::{
                interpreter::task::initialize_program::InitializeProgramBuilder,
                test_support::test_config,
            };

            #[tokio::test]
            async fn stores_the_value_for_arrays() {
                let code = indoc! { r##"
                    int a = sizeof(({ 1, 2, 3 }));
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)]),
                    Int(3),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_mappings() {
                let code = indoc! { r##"
                    int a = sizeof(([ "a": 1, 'b': 2, 3: ({ 4, 5, 6 }), 0: 0 ]));
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let mut mapping = HashMap::new();
                mapping.insert(String("a".into()), Int(1));
                mapping.insert(Int(98), Int(2));
                mapping.insert(Int(3), Array(vec![Int(4), Int(5), Int(6)]));
                mapping.insert(Int(0), Int(0));

                let expected = vec![
                    Int(0),
                    String("a".into()),
                    Int(1),
                    Int(98),
                    Int(2),
                    Int(3),
                    Int(4),
                    Int(5),
                    Int(6),
                    Array(vec![Int(4), Int(5), Int(6)]),
                    Int(0),
                    Int(0),
                    Mapping(mapping),
                    Int(4),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }

            #[tokio::test]
            async fn stores_the_value_for_strings() {
                let config = Arc::new(test_config());
                let path = Arc::new(LpcPath::new_in_game("/my_file.c", "/", &*config.lib_dir));

                let prototype = FunctionPrototypeBuilder::default()
                    .name(INIT_PROGRAM)
                    .filename(path.clone())
                    .return_type(LpcType::Void)
                    .build()
                    .unwrap();
                let initializer = ProgramFunction {
                    prototype,
                    num_locals: 2,
                    num_upvalues: 0,
                    instructions: vec![
                        SConst(Register(1).as_local(), 0),
                        Sizeof(Register(1).as_local(), Register(2).as_local()),
                    ],
                    debug_spans: vec![None, None],
                    labels: Some(HashMap::new()),
                    local_variables: Default::default(),
                    arg_locations: Default::default(),
                    strings: OnceCell::with_value(
                        StringInterner::from_iter(["Hello, world!"].into_iter()).into(),
                    ),
                }
                .into();

                let program = Program {
                    filename: path,
                    functions: Box::new(IndexMap::new()),
                    initializer: Some(initializer),
                    // num_init_registers: 2,
                    ..Default::default()
                };

                let (tx, _rx) = mpsc::channel(128);

                let task = InitializeProgramBuilder::<20>::default()
                    .program(program)
                    .config(config)
                    .tx(tx)
                    .build()
                    .await
                    .expect("failed to initialize");

                let registers = &task.stack.last().unwrap().registers;

                let expected = vec![Int(0), String("Hello, world!".into()), Int(13)];

                BareVal::assert_vec_equal(&expected, registers);
            }
        }

        mod test_store {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        mixed a = ({ 1, 2, 3 });
                        a[2] = 678;

                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code).await;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(678)]),
                    Int(678),
                    Int(2),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_sconst {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    string foo = "lolwut";
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), String("lolwut".into())];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }

        mod test_xor {
            use super::*;

            #[tokio::test]
            async fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 ^ 27;
                    mixed b = 0 ^ a;
                "##};

                let task = run_prog(code).await;
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(20), Int(0), Int(20)];

                BareVal::assert_vec_equal(&expected, &registers);
            }
        }
    }

    mod test_limits {

        use super::*;
        use crate::interpreter::task::initialize_program::InitializeProgramBuilder;

        #[tokio::test]
        async fn errors_on_stack_overflow() {
            let code = indoc! { r##"
                int kab00m = marf();

                int marf() {
                    return marf();
                }
            "##};

            let (program, _, _) = compile_prog(code).await;
            let (tx, _rx) = mpsc::channel(128);

            let r = InitializeProgramBuilder::<20>::default()
                .program(program)
                .tx(tx)
                .build()
                .await;

            assert_eq!(r.unwrap_err().to_string(), "stack overflow");
        }

        #[tokio::test]
        async fn errors_on_too_long_evaluation() {
            let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};

            let (program, _, _) = compile_prog(code).await;
            let (tx, _rx) = mpsc::channel(128);

            let r = InitializeProgramBuilder::<20>::default()
                .program(program)
                .tx(tx)
                .build()
                .await;

            assert_eq!(
                r.unwrap_err().to_string(),
                "evaluation limit of 300ms has been reached"
            );
        }
    }

    mod test_globals {
        use super::*;
        use crate::interpreter::task::tests::BareVal::*;

        #[tokio::test]
        async fn test_frame_globals() {
            let code = indoc! { r##"
                int i = 0;
                function inc = (: i++ :);
                int j = inc();
                int k = inc();
            "##};

            let task = run_prog(code).await;
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                Int(1),
                Int(0),
                Function("closure-0".to_string(), vec![]),
                Int(0),
                Int(1),
            ];

            BareVal::assert_vec_equal(&expected, &registers);

            let proc = task.context.process();

            let expected = vec![
                Int(2),
                Function("closure-0".to_string(), vec![]),
                Int(0),
                Int(1),
            ];
            BareVal::assert_vec_equal(&expected, &proc.globals.read());
        }
    }

    mod test_upvalues {
        use super::*;
        use crate::interpreter::task::tests::BareVal::*;

        async fn check_local_vars<T>(code: &str, vars: &IndexMap<&str, T>)
        where
            T: Into<BareVal> + Clone,
        {
            let mut task = run_prog(code).await;

            let snapshot = &mut task.snapshots.pop().unwrap();
            snapshot.pop(); // pop off the init frame

            let frame = snapshot.pop().unwrap();

            let frame_vars = frame.local_variables();

            for (k, v) in vars {
                let v: BareVal = v.clone().into();
                let found = frame_vars
                    .iter()
                    .filter(|v| &v.name == k)
                    .collect::<Vec<_>>();
                assert!(
                    found.iter().any(|local| v.equal_to_lpc_ref(&local.value)),
                    "key: {k}, value: {v}, found: {:?}",
                    found.iter().map(|v| &v.value).collect::<Vec<_>>()
                );
                // assert_eq!(&v, frame_vars.get(*k).unwrap(), "key: {}", k);
            }
        }

        async fn check_vm_upvalues<T>(code: &str, upvalues: &[T])
        where
            T: Into<BareVal> + Clone,
        {
            let mut task = run_prog(code).await;

            let snapshot = &mut task.snapshots.pop().unwrap();
            snapshot.pop(); // pop off the init frame

            let frame = snapshot.pop().unwrap();

            assert_eq!(
                upvalues.len(),
                frame.vm_upvalues.read().len(),
                "frame upvalues: {:?}\nvm upvalues: {:?}",
                upvalues
                    .iter()
                    .map(|i| i.clone().into())
                    .collect::<Vec<BareVal>>(),
                frame.vm_upvalues.read().iter().collect::<Vec<_>>()
            );

            let vm_upvalues = frame.vm_upvalues.read();
            for (i, v) in upvalues.iter().enumerate() {
                let v: BareVal = v.clone().into();
                v.assert_equal(&vm_upvalues[i]);
            }
        }

        async fn check_frame_upvalue_ptrs<T>(code: &str, upvalue_ptrs: &[T])
        where
            T: Into<Register> + Copy,
        {
            let mut task = run_prog(code).await;

            let snapshot = &mut task.snapshots.pop().unwrap();
            snapshot.pop(); // pop off the init frame

            let frame = snapshot.pop().unwrap();

            assert_eq!(upvalue_ptrs.len(), frame.upvalue_ptrs.len());

            for (i, v) in upvalue_ptrs.iter().enumerate() {
                let v: Register = (*v).into();
                assert_eq!(v, frame.upvalue_ptrs[i], "index: {i}");
            }
        }

        #[tokio::test]
        async fn test_local_captures() {
            let code = indoc! { r##"
                void create() {
                    int i = 0;
                    function inc = (: i++; debug("snapshot_stack"); i :);
                    int j = inc();
                    int k = inc();
                }
            "##};

            let expected = vec![Int(2)];
            check_vm_upvalues(code, &expected).await;

            let expected = vec![Register(0)];
            check_frame_upvalue_ptrs(code, &expected).await;

            let expected: IndexMap<&str, BareVal> = IndexMap::new();
            check_local_vars(code, &expected).await;
        }

        #[tokio::test]
        async fn test_shared_captures() {
            let code = indoc! { r##"
                void create() {
                    int i = 0;
                    function inc = (: i++ :);
                    int j = inc();
                    int k = inc();
                    debug("snapshot_stack");
                }
            "##};

            let expected = IndexMap::from([
                ("i", Int(2)),
                ("j", Int(0)),
                ("k", Int(1)),
                ("inc", Function("closure-0".to_string(), vec![])),
            ]);

            check_local_vars(code, &expected).await;
        }

        #[tokio::test]
        async fn test_arg_captures() {
            let code = indoc! { r##"
                void create() {
                    function add = make_adder(10);
                    int j = add(5);
                    int k = add(-20);
                    int l = add();
                    function add2 = make_adder(666);
                    int m = add2(1);
                    int n = add2();
                    debug("snapshot_stack");
                }

                function make_adder(int i) {
                    return (: [int j = i] j + $1 :);
                }
            "##};

            let expected = vec![Int(10), Int(666)];
            check_vm_upvalues(code, &expected).await;

            let expected = IndexMap::from([
                ("j", Int(10)),
                ("k", Int(-40)),
                ("l", Int(20)),
                ("m", Int(2)),
                ("n", Int(1332)),
                ("add", Function("closure-0".into(), vec![])),
                ("add2", Function("closure-0".into(), vec![])),
            ]);
            check_local_vars(code, &expected).await;
        }

        #[tokio::test]
        async fn test_higher_order() {
            let code = indoc! { r##"
                void create() {
                    function make_counter = make_make_counter(0);

                    function counter1 = make_counter();
                    function counter2 = make_counter();
                    function counter3 = make_counter(100);

                    int c1 = counter1();
                    int c2 = counter2(4);
                    int c3 = counter3();

                    debug("snapshot_stack");
                }

                function make_make_counter(int default_value) {
                    int counter = default_value;
                    return (: [int count_by = 1]
                        return (: [int j = count_by] counter += j :);
                    :);
                }
            "##};

            // let expected = vec![Int(105), Int(1), Int(1), Int(100)];
            // check_vm_upvalues(code, &expected);

            let expected = IndexMap::from([
                ("c1", Int(1)),
                ("c2", Int(5)),
                ("c3", Int(105)),
                ("make_counter", Function("closure-1".into(), vec![])),
                ("counter1", Function("closure-0".into(), vec![])),
                ("counter2", Function("closure-0".into(), vec![])),
                ("counter3", Function("closure-0".into(), vec![])),
            ]);

            check_local_vars(code, &expected).await;
        }

        #[tokio::test]
        async fn test_higher_order_with_implicit_vars() {
            let code = indoc! { r##"
                void create() {
                    function make = make_maker();

                    function made1 = make(1);
                    function made2 = make(2);

                    int c1 = made1();
                    int c2 = made2(69);

                    debug("snapshot_stack");
                }

                function make_maker() {
                    return (: [int i]
                        return (: $1 :); // This should *not* capture `i`
                    :);
                }
            "##};

            let expected: Vec<BareVal> = vec![];
            check_vm_upvalues(code, &expected).await;

            let expected = IndexMap::from([
                ("c1", Int(0)),
                ("c2", Int(69)),
                ("make", Function("closure-1".into(), vec![])),
                ("made1", Function("closure-0".into(), vec![])),
                ("made2", Function("closure-0".into(), vec![])),
            ]);

            check_local_vars(code, &expected).await;
        }

        #[tokio::test]
        async fn test_higher_order_with_partial_application() {
            let code = indoc! { r##"
                void create() {
                    function partial = &make_maker(,666);

                    function maker = partial("hello");

                    function made1 = maker(1, 2);
                    function made2 = (: maker(3, $1) :); // closure-0
                    made2 = made2(77);

                    int c1 = made1(-4);
                    int c2 = made2(69);

                    debug("snapshot_stack");
                }

                function make_maker(string str, int i) {
                    return (: [int j, int k] // closure-2
                        return (: [int l] str + i + " " + j + " " + k + " " + l :); // closure-1
                    :);
                }
            "##};

            let expected: Vec<BareVal> = vec![
                Function("closure-2".into(), vec![]),
                String("hello".into()),
                Int(666),
                Int(1),
                Int(2),
                Int(3),
                Int(77),
            ];
            check_vm_upvalues(code, &expected).await;

            let expected = IndexMap::from([
                ("c1", String("hello666 1 2 -4".into())),
                ("c2", String("hello666 3 77 69".into())),
                (
                    "partial",
                    Function("make_maker".into(), vec![None, Some(Int(666))]),
                ),
                ("maker", Function("closure-2".into(), vec![])),
                ("made1", Function("closure-1".into(), vec![])),
                ("made2", Function("closure-1".into(), vec![])),
            ]);

            check_local_vars(code, &expected).await;
        }

        #[tokio::test]
        async fn test_upvalued_ellipsis() {
            let code = indoc! { r##"
                void create() {
                    function partial = &make_maker(,666);

                    function maker = partial("hello");

                    function made1 = maker(123, 456);
                    function made2 = (: maker("world", $1) :); // closure-0
                    made2 = made2(77);

                    int c1 = made1(0);
                    int c2 = made2(1);

                    debug("snapshot_stack");
                }

                function make_maker(string str, int _i) {
                    return (: [...] // closure-2
                        dump("maker", argv);
                        return (: [int i] dump(str, argv[i]); argv[i] :); // closure-1
                    :);
                }
            "##};

            let expected: Vec<BareVal> = vec![
                Function("closure-2".into(), vec![]),
                String("hello".into()),
                Array(vec![Int(123), Int(456)]),
                Array(vec![String("world".into()), Int(77)]),
            ];
            check_vm_upvalues(code, &expected).await;

            let expected = IndexMap::from([
                ("c1", Int(123)),
                ("c2", Int(77)),
                (
                    "partial",
                    Function("make_maker".into(), vec![None, Some(Int(666))]),
                ),
                ("maker", Function("closure-2".into(), vec![])),
                ("made1", Function("closure-1".into(), vec![])),
                ("made2", Function("closure-1".into(), vec![])),
            ]);

            check_local_vars(code, &expected).await;
        }
    }

    mod test_gc {
        use super::*;
        use crate::interpreter::gc::sweep::KeylessSweep;

        #[tokio::test]
        async fn test_gc_is_accurate() {
            let code = indoc! { r##"
                int k = 0;

                void create() {
                    function stored = store();
                    function stored2 = store();
                    function stored3 = store();

                    int i = stored();
                    int j = stored2();
                    int l = stored3();
                }

                function store() {
                    int i = k++;

                    return (: i :);
                }
            "##};

            let task = run_prog(code).await;
            let ctx = &task.context;
            assert!(!ctx.upvalues().read().is_empty());

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();
            task.mark(&mut marked, &mut processed).unwrap();
            ctx.upvalues().write().keyless_sweep(&marked).unwrap();

            assert!(ctx.upvalues().read().is_empty());
        }
    }
}
