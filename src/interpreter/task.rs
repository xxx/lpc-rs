use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use decorum::Total;
use if_chain::if_chain;
use indexmap::IndexMap;
use qcell::{QCell, QCellOwner};
use lpc_rs_asm::instruction::{Address, Instruction};
use lpc_rs_core::{
    call_namespace::CallNamespace,
    function::{FunctionName, FunctionReceiver, FunctionTarget},
    function_arity::FunctionArity,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
    LpcInt, EFUN, INIT_PROGRAM,
};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::config::Config;
use tracing::{instrument, trace};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        call_frame::CallFrame,
        call_stack::CallStack,
        efun::{call_efun, efun_context::EfunContext, EFUN_PROTOTYPES},
        function_type::{FunctionAddress, FunctionPtr},
        lpc_ref::{LpcRef, NULL},
        lpc_value::LpcValue,
        memory::Memory,
        object_space::ObjectSpace,
        process::Process,
        program::Program,
        register_bank::RegisterBank,
        task_context::TaskContext,
    },
    try_extract_value,
};

macro_rules! pop_frame {
    ($task:expr, $context:expr) => {{
        let opt = $task.pop_frame();
        if let Some(ref frame) = opt {
            $task.stack.copy_result(&frame)?;

            if $task.stack.is_empty() {
                $context.set_result(frame.registers[0].clone());
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
#[inline]
pub fn get_location_in_frame(frame: &CallFrame, location: RegisterVariant) -> Result<Cow<LpcRef>> {
    match location {
        RegisterVariant::Local(reg) => {
            let registers = &frame.registers;
            Ok(Cow::Borrowed(&registers[reg]))
        }
        RegisterVariant::Global(reg) => {
            let proc = frame.process.borrow();
            Ok(Cow::Owned(proc.globals[reg].clone()))
        }
        RegisterVariant::Upvalue(upv) => {
            let upvalues = &frame.upvalues;
            let idx = upvalues[upv.index()];

            let proc = frame.process.borrow();
            Ok(Cow::Owned(proc.upvalues[idx].clone()))
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

            let mut proc = frame.process.borrow_mut();
            func(&mut proc.globals[reg])
        }
        RegisterVariant::Upvalue(reg) => {
            let frame = stack.current_frame()?;
            let upvalues = &frame.upvalues;
            let idx = upvalues[reg.index()];

            let mut proc = frame.process.borrow_mut();
            func(&mut proc.upvalues[idx])
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
#[derive(Debug, Clone)]
pub struct Task<'pool, const STACKSIZE: usize> {
    /// The call stack
    pub stack: CallStack<STACKSIZE>,

    /// Stack of [`CatchPoint`]s
    catch_points: Vec<CatchPoint>,

    /// A pointer to a memory pool to allocate new values from
    memory: Cow<'pool, Memory>,

    /// The arg vector, populated prior to any of the `Call`-family instructions
    pub args: Vec<RegisterVariant>,

    /// Store the most recently popped frame, for testing
    #[cfg(test)]
    pub popped_frame: Option<CallFrame>,

    /// Store a snapshot of a specific state, for testing
    #[cfg(test)]
    pub snapshots: Vec<CallStack<STACKSIZE>>,
}

impl<'pool, const STACKSIZE: usize> Task<'pool, STACKSIZE> {
    #[instrument(skip_all)]
    pub fn new<T>(memory: T) -> Self
    where
        T: Into<Cow<'pool, Memory>> + Debug,
    {
        Self {
            memory: memory.into(),
            stack: CallStack::default(),
            catch_points: vec![],
            args: Vec::with_capacity(10),

            #[cfg(test)]
            popped_frame: None,

            #[cfg(test)]
            snapshots: vec![],
        }
    }

    /// Convenience helper to get a Program initialized.
    #[instrument(skip_all)]
    pub fn initialize_program<C, O>(
        &mut self,
        program: Program,
        config: C,
        object_space: O,
        cell_key: &mut QCellOwner
    ) -> Result<TaskContext>
    where
        C: Into<Rc<Config>> + Debug,
        O: Into<Rc<QCell<ObjectSpace>>>,
    {
        let init_function = {
            let function = program.lookup_function(INIT_PROGRAM, &CallNamespace::Local);
            if function.is_none() {
                return Err(LpcError::new("Init function not found?"));
            }

            function.unwrap().clone()
        };
        let process: Rc<RefCell<Process>> = Process::new(program).into();
        let context = TaskContext::new(config, process.clone(), object_space, cell_key);
        context.insert_process(process, cell_key);

        self.eval(init_function, &[], context, cell_key)
    }

    /// Evaluate `f` to completion, or an error
    ///
    /// # Arguments
    /// `f` - the function to call
    /// `args` - the slice of arguments to pass to the function
    /// `task_context` the [`TaskContext`] that will be used for this evaluation
    ///
    /// # Returns
    ///
    /// A [`Result`]
    #[instrument(skip_all)]
    pub fn eval<F>(
        &mut self,
        f: F,
        args: &[LpcRef],
        task_context: TaskContext,
        cell_key: &mut QCellOwner
    ) -> Result<TaskContext>
    where
        F: Into<Rc<ProgramFunction>>,
    {
        let function = f.into();
        let process = task_context.process();

        let mut frame = CallFrame::new(process, function, args.len(), None);
        if !args.is_empty() {
            frame.registers[1..=args.len()].clone_from_slice(args);
        }

        self.stack.push(frame)?;

        let mut halted = false;

        while !halted {
            halted = match self.eval_one_instruction(&task_context, cell_key) {
                Ok(x) => x,
                Err(e) => {
                    if !self.catch_points.is_empty() {
                        self.catch_error(e)?;
                        false
                    } else {
                        let stack_trace = self.stack.stack_trace();
                        return Err(e.with_stack_trace(stack_trace));
                    }
                }
            };
        }

        Ok(task_context)
    }

    /// Evaluate the instruction at the current value of the PC
    /// The boolean represents whether we are at the end of input (i.e. we
    /// should halt the machine)
    #[instrument(skip_all)]
    fn eval_one_instruction(&mut self, task_context: &TaskContext, cell_key: &mut QCellOwner) -> Result<bool> {
        if self.stack.is_empty() {
            return Ok(true);
        }

        task_context.increment_instruction_count(1)?;

        let instruction = {
            let frame = match self.stack.current_frame() {
                Ok(x) => x,
                Err(_) => return Ok(true),
            };

            let instruction = match frame.instruction() {
                // TODO: Get rid of this clone (once Instruction is Copy, this will go away)
                Some(i) => {
                    trace!("about to evaluate: {}", i);
                    i.clone()
                }
                None => {
                    trace!("No more instructions");
                    return Ok(true);
                }
            };

            frame.inc_pc();

            instruction
        };

        match instruction {
            Instruction::AConst(..) => {
                self.handle_aconst(&instruction)?;
            }
            Instruction::And(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitand(y))?;
            }
            Instruction::Arg(r) => self.args.push(r),
            Instruction::BitwiseNot(r1, r2) => {
                let frame = self.stack.current_frame().unwrap();
                let debug_span = frame.current_debug_span();
                let lpc_ref = &*get_loc!(self, r1)?;
                match lpc_ref.bitnot() {
                    Ok(result) => {
                        let var = self.memory.value_to_ref(result);

                        set_loc!(self, r2, var)?;
                    }
                    Err(e) => {
                        return Err(e.with_span(debug_span));
                    }
                }
            }
            Instruction::Call { .. } => {
                self.handle_call(&instruction, task_context, cell_key)?;
            }
            Instruction::CallFp { location, num_args } => {
                self.handle_call_fp(task_context, &location, &num_args, cell_key)?;
            }
            Instruction::CallOther { .. } => {
                self.handle_call_other(&instruction, task_context, cell_key)?;
            }
            Instruction::CatchEnd => {
                self.catch_points.pop();
            }
            Instruction::CatchStart(r, label) => {
                let frame = self.stack.current_frame()?;
                let address = match frame.lookup_label(&label) {
                    Some(x) => *x,
                    None => {
                        return Err(
                            self.runtime_error(format!("Missing address for label `{label}`"))
                        )
                    }
                };

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
            Instruction::Dec(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.dec())?;
            }
            Instruction::EqEq(r1, r2, r3) => {
                let out = (get_loc!(self, r1)? == get_loc!(self, r2)?) as LpcInt;

                set_loc!(self, r3, LpcRef::Int(out))?;
            }
            Instruction::FConst(r, f) => {
                set_loc!(self, r, LpcRef::Float(f))?;
            }
            Instruction::FunctionPtrConst {
                location,
                target,
                applied_arguments,
                arity,
            } => {
                self.handle_functionptrconst(
                    task_context,
                    location,
                    target,
                    applied_arguments,
                    arity,
                )?;
            }
            Instruction::Gt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x > y)?;
            }
            Instruction::Gte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x >= y)?;
            }
            Instruction::IAdd(r1, r2, r3) => match get_loc!(self, r1)?.add(&*get_loc!(self, r2)?) {
                Ok(result) => {
                    let out = self.memory.value_to_ref(result);

                    set_loc!(self, r3, out)?;
                }
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::IConst(r, i) => {
                set_loc!(self, r, LpcRef::Int(i))?;
            }
            Instruction::IConst0(r) => {
                set_loc!(self, r, LpcRef::Int(0))?;
            }
            Instruction::IConst1(r) => {
                set_loc!(self, r, LpcRef::Int(1))?;
            }
            Instruction::IDiv(r1, r2, r3) => match get_loc!(self, r1)?.div(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::IMod(r1, r2, r3) => match get_loc!(self, r1)?.rem(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::IMul(r1, r2, r3) => match get_loc!(self, r1)?.mul(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::Inc(r1) => {
                apply_in_location(&mut self.stack, r1, |x| x.inc())?;
            }
            Instruction::ISub(r1, r2, r3) => match get_loc!(self, r1)?.sub(&*get_loc!(self, r2)?) {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::Jmp(label) => {
                let frame = self.stack.current_frame()?;
                let address = match frame.lookup_label(&label) {
                    Some(x) => *x,
                    None => {
                        return Err(
                            self.runtime_error(format!("Missing address for label `{label}`"))
                        )
                    }
                };
                frame.set_pc(address);
            }
            Instruction::Jnz(r1, label) => {
                let v = &*get_loc!(self, r1)?;

                if v != &LpcRef::Int(0) && v != &LpcRef::Float(Total::from(0.0)) {
                    let frame = self.stack.current_frame()?;
                    let address = match frame.lookup_label(&label) {
                        Some(x) => *x,
                        None => {
                            return Err(
                                self.runtime_error(format!("Missing address for label `{label}`"))
                            )
                        }
                    };
                    let frame = self.stack.current_frame()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Jz(r1, ref label) => {
                let v = &*get_loc!(self, r1)?;

                if v == &LpcRef::Int(0) || v == &LpcRef::Float(Total::from(0.0)) {
                    let frame = self.stack.current_frame()?;
                    frame.set_pc_from_label(label)?;
                }
            }
            Instruction::Load(..) => {
                self.handle_load(&instruction)?;
            }
            Instruction::LoadMappingKey(..) => {
                self.handle_load_mapping_key(&instruction)?;
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
            Instruction::MapConst(r, map) => {
                let mut register_map = IndexMap::new();

                for (key, value) in map {
                    register_map.insert(
                        get_loc!(self, key)?.into_owned(),
                        get_loc!(self, value)?.into_owned(),
                    );
                }

                let new_ref = self.memory.value_to_ref(LpcValue::from(register_map));

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
                    LpcRef::Int(x) => LpcRef::Int((*x == 0) as LpcInt),
                    LpcRef::Float(x) => LpcRef::Int((*x == 0.0) as LpcInt),

                    // These rest always have a value at runtime.
                    // Any null / undefined values would be LpcRef::Ints, handled above.
                    LpcRef::String(_)
                    | LpcRef::Array(_)
                    | LpcRef::Mapping(_)
                    | LpcRef::Object(_)
                    | LpcRef::Function(_) => LpcRef::Int(0),
                };

                set_loc!(self, r2, matched)?;
            }
            Instruction::Or(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x.bitor(y))?;
            }
            Instruction::PopulateArgv(r, num_args, _num_locals) => {
                let frame = self.stack.current_frame()?;
                let arg_locations = &frame.arg_locations;
                let ellipsis_vars = &arg_locations[num_args..];
                let refs = ellipsis_vars
                    .iter()
                    .map(|x| get_location_in_frame(frame, *x).map(|v| v.into_owned()))
                    .collect::<Result<Vec<_>>>()?;
                let new_ref = self.memory.value_to_ref(LpcValue::from(refs));

                set_location(&mut self.stack, r, new_ref)?;
            }
            Instruction::PopulateDefaults(default_addresses) => {
                let frame = self.stack.current_frame()?;
                let func = &frame.function;
                let declared_args = func.arity().num_args;
                let called_args = frame.called_with_num_args;

                if called_args < declared_args {
                    let difference = declared_args - called_args;
                    debug_assert!(difference <= default_addresses.len());
                    let index = default_addresses.len() - difference;
                    let address = default_addresses[index];
                    frame.set_pc(address);
                }
            }
            Instruction::Range(r1, r2, r3, r4) => {
                // r4 = r1[r2..r3]

                let resolve_range = |start: i64, end: i64, len: usize| -> (usize, usize) {
                    let to_idx = |i: LpcInt| {
                        // We handle the potential overflow just below.
                        if i >= 0 {
                            i as usize
                        } else {
                            (len as LpcInt + i) as usize
                        }
                    };
                    let real_start = to_idx(start);
                    let mut real_end = to_idx(end);

                    if real_end >= len {
                        real_end = len - 1;
                    }

                    (real_start, real_end)
                };

                let return_value = |value, memory: &Memory, stack| -> Result<()> {
                    let new_ref = memory.value_to_ref(value);
                    set_location(stack, r4, new_ref)?;

                    Ok(())
                };

                let get_new_value = |stack| {
                    let lpc_ref = &*get_location(stack, r1)?;

                    match lpc_ref {
                        LpcRef::Array(v_ref) => {
                            let value = v_ref.borrow();
                            let vec = try_extract_value!(*value, LpcValue::Array);

                            if vec.is_empty() {
                                return Ok(LpcValue::from(vec![]));
                            }

                            let index1 = &*get_location(stack, r2)?;
                            let index2 = &*get_location(stack, r3)?;

                            if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                let (real_start, real_end) = resolve_range(*start, *end, vec.len());

                                if real_start <= real_end {
                                    let slice = &vec[real_start..=real_end];
                                    let mut new_vec = vec![NULL; slice.len()];
                                    new_vec.clone_from_slice(slice);
                                    Ok(LpcValue::from(new_vec))
                                } else {
                                    Ok(LpcValue::from(vec![]))
                                }
                            } else {
                                let frame = self.stack.current_frame()?;
                                Err(LpcError::new(
                                    "Invalid code was generated for a Range instruction.",
                                )
                                .with_span(frame.current_debug_span()))
                            }
                        }
                        LpcRef::String(v_ref) => {
                            let value = v_ref.borrow();
                            let string = try_extract_value!(*value, LpcValue::String);

                            if string.is_empty() {
                                return Ok(LpcValue::from(""));
                            }

                            let index1 = &*get_location(stack, r2)?;
                            let index2 = &*get_location(stack, r3)?;

                            if let (LpcRef::Int(start), LpcRef::Int(end)) = (&index1, &index2) {
                                let (real_start, real_end) =
                                    resolve_range(*start, *end, string.len());

                                if real_start <= real_end {
                                    let len = real_end - real_start + 1;
                                    let new_string: String =
                                        string.chars().skip(real_start).take(len).collect();
                                    Ok(LpcValue::from(new_string))
                                } else {
                                    Ok(LpcValue::from(""))
                                }
                            } else {
                                let frame = self.stack.current_frame()?;
                                Err(LpcError::new(
                                    "Invalid code was generated for a Range instruction.",
                                )
                                .with_span(frame.current_debug_span()))
                            }
                        }
                        LpcRef::Float(_)
                        | LpcRef::Int(_)
                        | LpcRef::Mapping(_)
                        | LpcRef::Object(_)
                        | LpcRef::Function(_) => {
                            let frame = self.stack.current_frame()?;
                            Err(LpcError::new(
                                "Range's receiver isn't actually an array or string?",
                            )
                            .with_span(frame.current_debug_span()))
                        }
                    }
                };

                let new_val = { get_new_value(&self.stack)? };
                return_value(new_val, &self.memory, &mut self.stack)?;
            }

            Instruction::RegCopy(r1, r2) => {
                let new_ref = get_location(&self.stack, r1)?.into_owned();
                set_loc!(self, r2, new_ref)?;
            }
            Instruction::Ret => {
                pop_frame!(self, task_context).map(|frame| {
                    trace!("Returning from function: {}", frame.function.name());
                });

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
            }
            Instruction::Sizeof(r1, r2) => {
                let lpc_ref = &*get_loc!(self, r1)?;

                let value = match lpc_ref {
                    LpcRef::Array(x) => {
                        let borrowed = x.borrow();
                        let vec = try_extract_value!(*borrowed, LpcValue::Array);

                        LpcValue::from(vec.len() as LpcInt)
                    }
                    LpcRef::Mapping(x) => {
                        let borrowed = x.borrow();
                        let map = try_extract_value!(*borrowed, LpcValue::Mapping);

                        LpcValue::from(map.len() as LpcInt)
                    }
                    LpcRef::String(x) => {
                        let borrowed = x.borrow();
                        let string = try_extract_value!(*borrowed, LpcValue::String);

                        LpcValue::from(string.len() as LpcInt)
                    }
                    LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::Object(_) | LpcRef::Function(_) => {
                        LpcValue::from(0)
                    }
                };

                let lpc_ref = self.memory.value_to_ref(value);

                set_loc!(self, r2, lpc_ref)?;
            }
            Instruction::Store(..) => {
                // r2[r3] = r1;
                self.handle_store(&instruction)?;
            }
            Instruction::SConst(..) => {
                self.handle_sconst(&instruction)?;
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
    fn handle_aconst(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::AConst(r, vec) => {
                let vars = vec
                    .iter()
                    .map(|i| get_loc!(self, *i).map(|i| i.into_owned()))
                    .collect::<Result<Vec<_>>>()?;
                let new_ref = self.memory.value_to_ref(LpcValue::from(vars));

                set_loc!(self, *r, new_ref)
            }
            _ => Err(self.runtime_error("non-AConst instruction passed to `handle_aconst`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_call<'task>(&mut self, instruction: &Instruction, task_context: &TaskContext, cell_key: &mut QCellOwner) -> Result<()> {
        let Instruction::Call {
            name,
            namespace,
            num_args,
        } = instruction else {
            return Err(self.runtime_error("non-Call instruction passed to `handle_call`"));
        };

        let current_frame = self.stack.current_frame()?;
        let process = current_frame.process.clone();
        let num_args = *num_args;
        let func = {
            let borrowed = process.borrow();

            let function = borrowed.lookup_function(name, namespace);
            if let Some(func) = function {
                func.clone()
            } else {
                if_chain! {
                    // See if there is a simul efun with this name
                    if let Some(func) = task_context.simul_efuns();
                    let b = func.borrow();
                    if let Some(func) = b.lookup_function(name, &CallNamespace::Local);
                    then {
                        func.clone()
                    } else {
                        // See if there is a normal efun with this name
                        if let Some(prototype) = EFUN_PROTOTYPES.get(name.as_str()) {
                            let func = ProgramFunction::new(prototype.clone(), 0);

                            Rc::new(func)
                        } else {
                            let msg = format!("Call to unknown function `{name}`");
                            return Err(self.runtime_error(msg));
                        }
                    }
                }
            }
        };
        trace!("Calling function: {}", func);

        let mut new_frame = CallFrame::with_minimum_arg_capacity(
            process,
            func.clone(),
            num_args,
            num_args,
            None, // static functions do not inherit upvalues from the calling function
        );

        // copy argument registers from old frame to new
        if num_args > 0_usize {
            let mut next_index = 1;
            for (i, arg) in self.args.iter().enumerate() {
                let target_location = func.arg_locations.get(i).copied().unwrap_or_else(|| {
                    // This should only be reached by variables that will go
                    // into an ellipsis function's argv.
                    Register(next_index).as_local()
                });
                if let RegisterVariant::Local(r) = target_location {
                    next_index = r.index() + 1;
                }

                let val = get_loc!(self, *arg).map(|i| i.into_owned())?;

                trace!("Copying argument {} ({}) to {}", i, val, target_location);

                new_frame.arg_locations.push(target_location);

                new_frame.set_location(target_location, val)
            }
        }

        let function_is_efun = EFUN_PROTOTYPES.contains_key(name.as_str())
            && (!current_frame
                .process
                .borrow()
                .contains_function(name, namespace)
                || namespace.as_str() == EFUN);

        self.stack.push(new_frame)?;

        if function_is_efun {
            return self.prepare_and_call_efun(name.as_str(), task_context, cell_key);
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn handle_call_fp(
        &mut self,
        task_context: &TaskContext,
        location: &RegisterVariant,
        num_args: &usize,
        cell_key: &mut QCellOwner,
    ) -> Result<()> {
        let func = {
            let lpc_ref = &*get_loc!(self, *location)?;

            if let LpcRef::Function(func) = lpc_ref {
                func.clone() // this is a cheap clone
            } else {
                return Err(
                    self.runtime_error(format!("callfp instruction on non-function: {lpc_ref}"))
                );
            }
        };

        // TODO: reduce the amount of time this borrow is live
        let borrowed = func.borrow();
        let ptr = try_extract_value!(*borrowed, LpcValue::Function);

        trace!("Calling function ptr: {}", ptr);

        let arity = ptr.arity;
        let partial_args = &ptr.partial_args;
        let passed_args_count = *num_args
            + partial_args
                .iter()
                .fold(0, |sum, arg| sum + arg.is_some() as usize);
        let function_is_efun = matches!(&ptr.address, FunctionAddress::Efun(_));
        let dynamic_receiver = matches!(&ptr.address, FunctionAddress::Dynamic(_));
        // for dynamic receivers, skip the first register of the passed args,
        // which contains the receiver itself
        let index = dynamic_receiver as usize;
        let adjusted_num_args = *num_args - (dynamic_receiver as usize);
        let max_arg_length = Self::calculate_max_arg_length(adjusted_num_args, partial_args, arity);

        if let FunctionAddress::Local(receiver, pf) = &ptr.address {
            if !pf.public() && (ptr.call_other || !Rc::ptr_eq(&task_context.process(), receiver)) {
                return set_loc!(self, Register(0).as_local(), NULL);
            }
        }

        let (proc, function) = match &ptr.address {
            FunctionAddress::Local(proc, function) => (proc.clone(), function.clone()),
            FunctionAddress::Dynamic(name) => {
                let LpcRef::Object(lpc_ref) = &*get_loc!(self, self.args[0])? else {
                    return Err(self.runtime_error("non-object receiver to function pointer call"));
                };

                let pair_opt = {
                    let b = lpc_ref.borrow();
                    let cell = try_extract_value!(*b, LpcValue::Object);
                    let proc = cell.borrow();
                    proc.lookup_function(name, &CallNamespace::Local)
                        .map(|func| (cell.clone(), func.clone()))
                };

                // short-circuit a 0 return if doing a call_other to a non-existent function
                let Some(pair) = pair_opt else {
                    let frame = self.stack.current_frame_mut()?;
                    frame.registers[0] = NULL;
                    return Ok(());
                    // return Err(self.runtime_error(format!("call to unknown function `{name}`")));
                };

                pair
            }
            FunctionAddress::Efun(name) => {
                // unwrap is safe because this should have been checked in an earlier step
                let prototype = EFUN_PROTOTYPES.get(name.as_str()).unwrap();
                let pf = ProgramFunction::new(prototype.clone(), 0);

                let frame = self.stack.current_frame()?;

                (frame.process.clone(), Rc::new(pf))
            }
        };

        let new_registers = RegisterBank::initialized_for_function(&function, max_arg_length);

        let upvalues = if function.is_closure() {
            Some(&ptr.upvalues)
        } else {
            // Calls to pointers to static functions do not inherit upvalues,
            // same as normal direct calls to them.
            None
        };

        let mut new_frame =
            CallFrame::with_registers(proc, function, passed_args_count, new_registers, upvalues);

        // negotiate the passed & partially-applied arguments
        if arity.num_args > 0_usize || arity.ellipsis || (dynamic_receiver && *num_args > 0) {
            let from_slice = &self.args[index..(index + adjusted_num_args)];

            let mut type_check_and_assign_location = |loc, r, i| -> Result<()> {
                let prototype = &new_frame.function.prototype;
                self.type_check_call_arg(
                    &r,
                    prototype.arg_types.get(i),
                    prototype.arg_spans.get(i),
                    &prototype.name,
                )?;

                trace!("Copying argument {} ({}) to {}", i, r, loc);

                new_frame.arg_locations.push(loc);
                new_frame.set_location(loc, r);

                Ok(())
            };

            let mut from_slice_index = 0;
            let mut next_index = 1;
            let b = func.borrow();
            let ptr = try_extract_value!(&*b, LpcValue::Function);
            let arg_locations = match &ptr.address {
                FunctionAddress::Local(_, func) => Cow::Borrowed(&func.arg_locations),
                FunctionAddress::Dynamic(_) | FunctionAddress::Efun(_) => Cow::Owned(vec![]),
            };

            for i in 0..max_arg_length {
                let target_location = arg_locations.get(i).copied().unwrap_or_else(|| {
                    // This should only be reached by variables that will go
                    // into an ellipsis function's argv.
                    Register(next_index).as_local()
                });

                if let RegisterVariant::Local(r) = target_location {
                    next_index = r.index() + 1;
                }

                if let Some(Some(lpc_ref)) = partial_args.get(i) {
                    // if a partially-applied arg is present, use it
                    type_check_and_assign_location(target_location, lpc_ref.clone(), i)?;
                } else if let Some(location) = from_slice.get(from_slice_index) {
                    // check if the user passed an argument, which will
                    // fill in the next hole in the partial arguments, or
                    // append to the end

                    let lpc_ref = get_loc!(self, *location)?;
                    type_check_and_assign_location(target_location, lpc_ref.into_owned(), i)?;

                    from_slice_index += 1;
                }
            }
        }

        let pf = new_frame.function.clone();
        self.stack.push(new_frame)?;

        if function_is_efun {
            self.prepare_and_call_efun(pf.name(), task_context, cell_key)?;
        }

        Ok(())
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

                return Err(error);
            }
        }

        Ok(())
    }

    fn prepare_and_call_efun(&mut self, name: &str, task_context: &TaskContext, cell_key: &mut QCellOwner) -> Result<()>
    // where
    //     'pool: 'task,
    {
        let mut ctx = EfunContext::new(&mut self.stack, task_context, &self.memory);

        call_efun(name, &mut ctx, cell_key)?;

        #[cfg(test)]
        {
            if ctx.snapshot.is_some() {
                self.snapshots.push(ctx.snapshot.unwrap());
            }
        }

        pop_frame!(self, task_context);

        Ok(())
    }

    #[instrument(skip_all)]
    fn handle_call_other(
        &mut self,
        instruction: &Instruction,
        task_context: &TaskContext,
        cell_key: &mut QCellOwner
    ) -> Result<()> {
        match instruction {
            Instruction::CallOther {
                receiver,
                name,
                num_args: _,
            } => {
                // set up result_ref in a block, as `registers` is a long-lived reference that
                // doesn't work as mutable, but needs to be written to at the very end.
                let result_ref = {
                    // figure out which function we're calling
                    let receiver_ref = &*get_location(&self.stack, *receiver)?;
                    let name_ref = &*get_location(&self.stack, *name)?;
                    let pool_ref = if let LpcRef::String(r) = name_ref {
                        r
                    } else {
                        let str = format!("Invalid name passed to `call_other`: {name_ref}");
                        return Err(self.runtime_error(str));
                    };
                    let borrowed = pool_ref.borrow();
                    let function_name = try_extract_value!(*borrowed, LpcValue::String);

                    trace!("Calling call_other: {receiver_ref}->{function_name}");

                    // An inner helper function to actually calculate the result, for easy re-use
                    // when using `call_other` with arrays and mappings.
                    fn resolve_result(
                        receiver_ref: &LpcRef,
                        function_name: &str,
                        args: &[LpcRef],
                        task_context: &TaskContext,
                        memory: &Memory,
                        cell_key: &mut QCellOwner,
                    ) -> Result<LpcRef> {
                        let resolved = Task::<MAX_CALL_STACK_SIZE>::resolve_call_other_receiver(
                            receiver_ref,
                            function_name,
                            task_context,
                            &CallNamespace::Local,
                            cell_key
                        );

                        if let Some(pr) = resolved {
                            let value = LpcValue::from(pr);
                            let result = match value {
                                LpcValue::Object(receiver) => {
                                    let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(memory);

                                    let new_context =
                                        task_context.clone().with_process(receiver.clone());
                                    // unwrap() is ok because resolve_call_other_receiver() checks
                                    // for the function's presence.
                                    let function = receiver
                                        .borrow()
                                        // TODO: namespace needs to be made available to this
                                        // instruction
                                        .lookup_function(function_name, &CallNamespace::Local)
                                        .unwrap()
                                        .clone();

                                    if function.public() {
                                        let eval_context =
                                            task.eval(function, args, new_context, cell_key)?;
                                        task_context.increment_instruction_count(
                                            eval_context.instruction_count(),
                                        )?;

                                        eval_context.into_result()
                                    } else {
                                        NULL
                                    }
                                }
                                _ => NULL,
                            };

                            Ok(result)
                        } else {
                            Err(LpcError::new("Unable to find the receiver."))
                        }
                    }

                    // let registers = &self.stack.current_frame()?.registers;

                    let args = self
                        .args
                        .iter()
                        .map(|i| get_loc!(self, *i).map(|r| r.into_owned()))
                        .collect::<Result<Vec<_>>>()?;

                    match &receiver_ref {
                        LpcRef::String(_) | LpcRef::Object(_) => resolve_result(
                            receiver_ref,
                            function_name,
                            &args,
                            task_context,
                            &self.memory,
                            cell_key,
                        )?,
                        LpcRef::Array(r) => {
                            let b = r.borrow();
                            let array = try_extract_value!(*b, LpcValue::Array);

                            let array_value: LpcValue = array
                                .iter()
                                .map(|lpc_ref| {
                                    resolve_result(
                                        lpc_ref,
                                        function_name,
                                        &args,
                                        task_context,
                                        &self.memory,
                                        cell_key,
                                    )
                                    .unwrap_or(NULL)
                                })
                                .collect::<Vec<_>>()
                                .into();
                            self.memory.value_to_ref(array_value)
                        }
                        LpcRef::Mapping(m) => {
                            let b = m.borrow();
                            let map = try_extract_value!(*b, LpcValue::Mapping);

                            let with_results: LpcValue = map
                                .iter()
                                .map(|(key_ref, value_ref)| {
                                    (
                                        key_ref.clone(),
                                        resolve_result(
                                            value_ref,
                                            function_name,
                                            &args,
                                            task_context,
                                            &self.memory,
                                            cell_key
                                        )
                                        .unwrap_or(NULL),
                                    )
                                })
                                .collect::<IndexMap<_, _>>()
                                .into();

                            self.memory.value_to_ref(with_results)
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
            _ => Err(self.runtime_error("non-CallOther instruction passed to `handle_call_other`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_functionptrconst(
        &mut self,
        task_context: &TaskContext,
        location: RegisterVariant,
        target: FunctionTarget,
        applied_arguments: Vec<Option<RegisterVariant>>,
        arity: FunctionArity,
    ) -> Result<()> {
        let call_other = if let FunctionTarget::Local(_, ref rcvr) = target {
            !matches!(rcvr, FunctionReceiver::Local)
        } else {
            false
        };

        let address = match target {
            FunctionTarget::Efun(func_name) => FunctionAddress::Efun(func_name),
            FunctionTarget::Local(func_name, FunctionReceiver::Argument) => match func_name {
                FunctionName::Var(_) => {
                    return Err(self.runtime_error(concat!(
                        "A function pointer with `&` as the receiver somehow has a ",
                        "var function name. This should not be reachable if ",
                        "semantic checks have passed."
                    )));
                }
                FunctionName::Literal(name) => FunctionAddress::Dynamic(name),
            },
            FunctionTarget::Local(func_name, func_receiver) => {
                let proc = match func_receiver {
                    FunctionReceiver::Var(receiver_reg) => {
                        let receiver_ref = &*get_loc!(self, receiver_reg)?;
                        match receiver_ref {
                            LpcRef::Object(x) => {
                                let b = x.borrow();
                                let process = try_extract_value!(*b, LpcValue::Object);
                                process.clone()
                            }
                            LpcRef::String(_) => todo!(),
                            LpcRef::Array(_)
                            | LpcRef::Mapping(_)
                            | LpcRef::Float(_)
                            | LpcRef::Int(_)
                            | LpcRef::Function(_) => {
                                return Err(self.runtime_error("Receiver was not object or string"));
                            }
                        }
                    }
                    FunctionReceiver::Local => {
                        let frame = self.stack.current_frame()?;
                        let proc = &frame.process;

                        proc.clone()
                    }
                    FunctionReceiver::Argument => {
                        unreachable!("This is specified in an earlier arm of the parent `match`");
                    }
                };

                let s = Self::resolve_function_name(&self.stack, &func_name)?;
                let frame = self.stack.current_frame()?;
                let proc_ref = &frame.process;
                let borrowed_proc = proc_ref.borrow();
                // look in the Local namespace first
                let func = borrowed_proc.lookup_function(&*s, &CallNamespace::Local);
                match func {
                    Some(program_function) => {
                        FunctionAddress::Local(proc, program_function.clone())
                    }
                    None => {
                        if_chain! {
                            // check simul efuns, which use the `Local` FunctionTarget
                            if let Some(rc) = task_context.simul_efuns();
                            let b = rc.borrow();
                            if let Some(func) = b.lookup_function(&*s, &CallNamespace::Local);
                            then {
                                FunctionAddress::Local(proc, func.clone())
                            } else {
                                return Err(
                                    self.runtime_error(format!("unknown local target `{s}`"))
                                );
                            }
                        }
                    }
                }
            }
        };

        let partial_args: Vec<Option<LpcRef>> = applied_arguments
            .iter()
            .map(|arg| {
                // TODO: This should not use unwrap
                arg.map(|register| get_loc!(self, register).unwrap().into_owned())
            })
            .collect();

        let frame = self.stack.current_frame()?;
        let fp = FunctionPtr {
            owner: frame.process.clone(),
            address,
            partial_args,
            arity,
            call_other,
            // Function pointers inherit the creating function's upvalues
            upvalues: frame.upvalues.clone(),
        };

        // panic!("search here");

        let new_ref = self.memory.value_to_ref(LpcValue::from(fp));

        set_loc!(self, location, new_ref)
    }

    #[instrument(skip_all)]
    fn handle_load(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::Load(r1, r2, r3) => {
                let container_ref = get_loc!(self, *r1)?.into_owned();
                let lpc_ref = get_loc!(self, *r2)?.into_owned();

                match container_ref {
                    LpcRef::Array(vec_ref) => {
                        let value = vec_ref.borrow();
                        let vec = try_extract_value!(*value, LpcValue::Array);

                        if let LpcRef::Int(i) = lpc_ref {
                            let idx = if i >= 0 { i } else { vec.len() as LpcInt + i };

                            if idx >= 0 {
                                if let Some(v) = vec.get(idx as usize) {
                                    set_loc!(self, *r3, v.clone())?;
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
                        let value = string_ref.borrow();
                        let string = try_extract_value!(*value, LpcValue::String);

                        if let LpcRef::Int(i) = lpc_ref {
                            let idx = if i >= 0 {
                                i
                            } else {
                                string.len() as LpcInt + i
                            };

                            if idx >= 0 {
                                if let Some(v) = string.chars().nth(idx as usize) {
                                    set_loc!(self, *r3, LpcRef::Int(v as i64))?;
                                } else {
                                    set_loc!(self, *r3, LpcRef::Int(0))?;
                                }
                            } else {
                                set_loc!(self, *r3, LpcRef::Int(0))?;
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
                        let value = map_ref.borrow();
                        let map = try_extract_value!(*value, LpcValue::Mapping);

                        let var = if let Some(v) = map.get(&lpc_ref) {
                            v.clone()
                        } else {
                            NULL
                        };

                        set_loc!(self, *r3, var)?;

                        Ok(())
                    }
                    x => Err(self.runtime_error(format!("Invalid attempt to take index of `{x}`"))),
                }
            }
            _ => Err(self.runtime_error("non-Load instruction passed to `handle_load`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_load_mapping_key(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::LoadMappingKey(r1, r2, r3) => {
                let var = {
                    let container_ref = &*get_loc!(self, *r1)?;
                    let lpc_ref = &*get_loc!(self, *r2)?;

                    match container_ref {
                        LpcRef::Mapping(map_ref) => {
                            let value = map_ref.borrow();
                            let map = try_extract_value!(*value, LpcValue::Mapping);

                            let index = match lpc_ref {
                                LpcRef::Int(i) => *i,
                                _ => {
                                    return Err(self
                                        .runtime_error(format!("Invalid index type: {lpc_ref}")))
                                }
                            };

                            if let Some((key, _)) = map.get_index(index as usize) {
                                key.clone()
                            } else {
                                NULL
                            }
                        }
                        x => {
                            return Err(self
                                .runtime_error(format!("Invalid attempt to take index of `{x}`")))
                        }
                    }
                };

                set_loc!(self, *r3, var)
            }
            _ => Err(self.runtime_error("non-Load instruction passed to `handle_load`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_sconst(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::SConst(r, s) => {
                let new_ref = self.memory.value_to_ref(LpcValue::from(s));

                set_loc!(self, *r, new_ref)
            }
            _ => Err(self.runtime_error("non-SConst instruction passed to `handle_sconst`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_store(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::Store(r1, r2, r3) => {
                let mut container = get_loc!(self, *r2)?.into_owned();
                let index = &*get_loc!(self, *r3)?;
                let array_idx = if let LpcRef::Int(i) = index { *i } else { 0 };

                match container {
                    LpcRef::Array(vec_ref) => {
                        let mut r = vec_ref.borrow_mut();
                        let vec = match *r {
                            LpcValue::Array(ref mut v) => v,
                            _ => return Err(self.runtime_bug(
                                "LpcRef with a non-Array reference as its value. This indicates a bug in the interpreter.")
                            )
                        };

                        let len = vec.len();

                        // handle negative indices
                        let idx = if array_idx >= 0 {
                            array_idx
                        } else {
                            len as LpcInt + array_idx
                        };

                        if idx >= 0 && (idx as usize) < len {
                            vec[idx as usize] = (*get_loc!(self, *r1)?).clone();
                        } else {
                            return Err(self.array_index_error(idx, len));
                        }

                        Ok(())
                    }
                    LpcRef::Mapping(ref mut map_ref) => {
                        let mut r = map_ref.borrow_mut();
                        let map = match *r {
                            LpcValue::Mapping(ref mut m) => m,
                            _ => return Err(self.runtime_bug(
                                "LpcRef with a non-Mapping reference as its value. This indicates a bug in the interpreter.")
                            )
                        };

                        map.insert(index.clone(), get_loc!(self, *r1)?.into_owned());

                        Ok(())
                    }
                    x => Err(self.runtime_error(format!("Invalid attempt to take index of `{x}`"))),
                }
            }
            _ => Err(self.runtime_error("non-Store instruction passed to `handle_store`")),
        }
    }

    #[instrument(skip_all)]
    fn resolve_call_other_receiver(
        receiver_ref: &LpcRef,
        name: &str,
        context: &TaskContext,
        namespace: &CallNamespace,
        cell_key: &QCellOwner,
    ) -> Option<Rc<RefCell<Process>>> {
        let process = match receiver_ref {
            LpcRef::String(s) => {
                let r = s.borrow();
                let str = if let LpcValue::String(ref s) = *r {
                    s
                } else {
                    return None;
                };

                match context.lookup_process(str, cell_key) {
                    Some(proc) => proc,
                    None => return None,
                }
            }
            LpcRef::Object(o) => {
                let r = o.borrow();
                if let LpcValue::Object(ref proc) = *r {
                    proc.clone()
                } else {
                    return None;
                }
            }
            _ => return None,
        };

        // Only switch the process if there's actually a function to
        // call by this name on the other side.
        if process.borrow().contains_function(name, namespace) {
            Some(process)
        } else {
            None
        }
    }

    /// Set the state to handle a caught error.
    /// Panics if there aren't actually any catch points.
    #[instrument(skip_all)]
    fn catch_error(&mut self, error: LpcError) -> Result<()> {
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
            return Err(self.runtime_error("stack is empty after popping to catch point?"));
        }

        // set up the catch point's return value
        let value = LpcValue::from(error.to_string());
        let lpc_ref = self.memory.value_to_ref(value);
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
        F: Fn(&LpcRef, &LpcRef) -> Result<LpcValue>,
    {
        let ref1 = &*get_location(&self.stack, r1)?;
        let ref2 = &*get_location(&self.stack, r2)?;

        match operation(ref1, ref2) {
            Ok(result) => {
                let var = self.memory.value_to_ref(result);

                set_loc!(self, r3, var)?;
            }
            Err(e) => {
                let frame = self.stack.current_frame()?;
                return Err(e.with_span(frame.current_debug_span()));
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

        let out = operation(ref1, ref2) as LpcInt;

        set_loc!(self, r3, LpcRef::Int(out))
    }

    /// Get the true name of the function to call.
    #[instrument(skip(stack))]
    fn resolve_function_name<'a, const N: usize>(
        stack: &'a CallStack<N>,
        name: &'a FunctionName,
    ) -> Result<Cow<'a, str>> {
        match name {
            FunctionName::Var(reg) => {
                let name_ref = &*get_location(stack, *reg)?;

                if let LpcRef::String(s) = name_ref {
                    let b = s.borrow();
                    let str = try_extract_value!(*b, LpcValue::String);
                    Ok(str.clone().into())
                } else {
                    Err(LpcError::new(
                        "runtime error: found function var that didn't resolve to a string?",
                    ))
                }
            }
            FunctionName::Literal(s) => Ok(s.into()),
        }
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

    #[instrument(skip_all)]
    fn calculate_max_arg_length<T>(
        num_args: usize,
        partial_args: &[Option<T>],
        arity: FunctionArity,
    ) -> usize {
        let none_args = partial_args.iter().filter(|a| a.is_none()).count();
        let dynamic_len = partial_args.len() + num_args.saturating_sub(none_args);
        std::cmp::max(dynamic_len, arity.num_args)
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
    use lpc_rs_core::{LpcFloat, LpcInt};
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;
    use crate::{
        extract_value,
        test_support::{compile_prog, run_prog},
    };

    // #[ctor::ctor]
    // fn init() {
    //     tracing::subscriber::set_global_default(
    //         tracing_subscriber::fmt()
    //             .with_max_level(tracing::Level::TRACE)
    //             .with_writer(std::io::stdout)
    //             .finish(),
    //     )
    //         .expect("setting tracing default failed");
    // }

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

    /// A type to make it easier to set up test expectations for register
    /// contents
    #[derive(Debug, Eq, Clone)]
    enum BareVal {
        String(String),
        Int(LpcInt),
        Float(LpcFloat),
        Array(Vec<BareVal>),
        Mapping(HashMap<BareVal, BareVal>),
        Object(String),                         // Just the filename
        Function(String, Vec<Option<BareVal>>), // name and args
    }

    impl From<&LpcRef> for BareVal {
        fn from(lpc_ref: &LpcRef) -> Self {
            match lpc_ref {
                LpcRef::Float(x) => BareVal::Float(*x),
                LpcRef::Int(x) => BareVal::Int(*x),
                LpcRef::String(x) => {
                    let xb = x.borrow();
                    let s = extract_value!(&*xb, LpcValue::String);
                    BareVal::String(s.clone())
                }
                LpcRef::Array(x) => {
                    let xb = x.borrow();
                    let a = extract_value!(&*xb, LpcValue::Array);
                    let array = a.iter().map(|item| item.into()).collect::<Vec<_>>();
                    BareVal::Array(array)
                }
                LpcRef::Mapping(x) => {
                    let xb = x.borrow();
                    let m = extract_value!(&*xb, LpcValue::Mapping);
                    let mapping = m
                        .into_iter()
                        .map(|(k, v)| (k.into(), v.into()))
                        .collect::<HashMap<_, _>>();
                    BareVal::Mapping(mapping)
                }
                LpcRef::Object(x) => {
                    let xb = x.borrow();
                    let o = extract_value!(&*xb, LpcValue::Object);
                    let filename = o.borrow().filename().into_owned();
                    BareVal::Object(filename)
                }
                LpcRef::Function(x) => {
                    let xb = x.borrow();
                    let fp = extract_value!(&*xb, LpcValue::Function);
                    let args = fp
                        .partial_args
                        .iter()
                        .map(|item| item.as_ref().map(|r| r.into()))
                        .collect::<Vec<_>>();

                    BareVal::Function(fp.name().into(), args)
                }
            }
        }
    }

    impl PartialEq<LpcRef> for BareVal {
        fn eq(&self, other: &LpcRef) -> bool {
            &BareVal::from(other) == self
        }
    }

    impl PartialEq<RegisterBank> for Vec<BareVal> {
        fn eq(&self, other: &RegisterBank) -> bool {
            self == &other.registers
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
        use crate::interpreter::task::tests::BareVal::*;

        fn snapshot_registers(code: &str, cell_key: &mut QCellOwner) -> RegisterBank {
            let (mut task, _) = run_prog(code, cell_key);
            let mut stack = task.snapshots.pop().unwrap();

            // The top of the stack in the snapshot is the object initialization frame,
            // which is not what we care about here, so we get the second-to-top frame
            // instead.
            let index = stack.len() - 2;

            std::mem::take(&mut stack[index].registers)
        }

        mod test_aconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed *a = ({ 12, 4.3, "hello", ({ 1, 2, 3 }) });
                "##};
                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(12),
                    Float(LpcFloat::from(4.3)),
                    String("hello".into()),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)]),
                    Array(vec![
                        Int(12),
                        Float(LpcFloat::from(4.3)),
                        String("hello".into()),
                        Array(vec![Int(1), Int(2), Int(3)]),
                    ]),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_and {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 & 27;
                    mixed b = 0 & a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(11), Int(0), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_andand {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 && 333;
                    mixed b = 0;
                    mixed c = b && a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(123), Int(333), Int(333), Int(0), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_bitwise_not {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    int a = ~0;
                    int b = 7;
                    int c = ~b;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(-1), Int(7), Int(-8)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_call {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = tacos();
                    int tacos() { return 666; }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(666), Int(666)];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn calls_correct_function() {
                let code = indoc! { r##"
                    inherit "/std/object";
                    mixed mine = public_function();
                    mixed parents = ::public_function();

                    string public_function() {
                        return "my public_function";
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let proc = ctx.process();
                let borrowed = proc.borrow();
                let values = borrowed.global_variable_values();
                assert_eq!(
                    &String("my public_function".into()),
                    *values.get("mine").unwrap()
                );
                assert_eq!(
                    &String("/std/object public".into()),
                    *values.get("parents").unwrap()
                );
            }

            #[test]
            fn calls_correct_function_with_efuns() {
                let code = indoc! { r##"
                    object ob = clone_object("/std/object");
                    mixed this_one = file_name(ob);
                    mixed efun_one = efun::file_name(ob);

                    string file_name(object ob) {
                        return "file_name_override";
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let proc = ctx.process();
                let borrowed = proc.borrow();
                let values = borrowed.global_variable_values();
                assert_eq!(
                    &String("file_name_override".into()),
                    *values.get("this_one").unwrap()
                );
                assert_eq!(
                    &String("/std/object#0".into()),
                    *values.get("efun_one").unwrap()
                );
            }

            #[test]
            fn calls_correct_function_with_simul_efuns() {
                let code = indoc! { r##"
                    string this_one = simul_efun("marf");
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let proc = ctx.process();
                let borrowed = proc.borrow();
                let values = borrowed.global_variable_values();
                assert_eq!(
                    &String("this is a simul_efun: marf".into()),
                    *values.get("this_one").unwrap()
                );
            }
        }

        mod test_call_fp {
            use claim::assert_ok;

            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    function q = tacos;
                    int a = q(666);
                    int tacos(int j) { return j + 1; }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(667),
                    Function("tacos".into(), vec![]),
                    Int(666),
                    Int(667),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_partial_applications() {
                let code = indoc! { r##"
                    function q = &tacos(, "adding some!");
                    int a = q(666, 4);
                    string tacos(int j, string s, int k) {
                        return s + " " +  (j + k);
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_partial_applications_with_no_added_args() {
                let code = indoc! { r##"
                    function q = &tacos("my_string!");
                    int a = q();
                    string tacos(string s) {
                        return s + " awesome!" ;
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("my_string! awesome!".into()),
                    String("my_string!".into()),
                    Function("tacos".into(), vec![Some(String("my_string!".into()))]),
                    String("my_string! awesome!".into()),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_partial_applications_with_default_arguments() {
                let code = indoc! { r##"
                    function q = &tacos(, "adding some!");
                    int a = q(666, 4);
                    int b = q(123);
                    string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_partial_applications_with_default_arguments_and_ellipsis() {
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

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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
                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_dynamic_receivers() {
                let code = indoc! { r##"
                    function q = &->name(, "awesome!");

                    int a = q(this_object(), 666);
                    int b = q(clone_object("/std/widget"), 42);

                    string name(int rank, string reaction) {
                        return "me: " + rank + ". " + reaction;
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn is_0_for_call_other_private_functions() {
                let code = indoc! { r##"
                    function q = &(this_object())->tacos(, "adding some!");
                    int a = q(666, 4);
                    int b = q(123);
                    private string tacos(int j, string s, int k = 100) {
                        return s + " " +  (j + k);
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn is_normal_call_for_local_private_functions() {
                let code = indoc! { r##"
                    function q = tacos;
                    int a = q(4);
                    private int tacos(int j) {
                        return j;
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(4), Function("tacos".into(), vec![]), Int(4), Int(4)];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn checks_types() {
                let code = indoc! { r##"
                    function q = &tacos("foo");
                    int a = q();
                    private int tacos(int j) {
                        return j;
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let mut object_space = ObjectSpace::default();
                let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(Memory::default());

                let (program, config, process) = compile_prog(code);
                object_space.insert_process(process);

                let result = task.initialize_program(program, config, cell_key.cell(object_space), &mut cell_key);

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

                let (program, config, process) = compile_prog(code);
                let mut object_space = ObjectSpace::default();
                object_space.insert_process(process);

                let result = task.initialize_program(program, config, cell_key.cell(object_space), &mut cell_key);

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

                let (program, config, process) = compile_prog(code);
                let mut object_space = ObjectSpace::default();
                object_space.insert_process(process);

                let result = task.initialize_program(program, config, cell_key.cell(object_space), &mut cell_key);

                assert_ok!(result);
            }
        }

        mod test_call_other {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    int tacos() { return 666; }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(666),
                    Object("/my_file".into()),
                    String("tacos".into()),
                    Int(666),
                ];

                assert_eq!(&expected, registers);
            }

            #[test]
            fn returns_0_for_private_functions() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    private int tacos() { return 666; }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Object("/my_file".into()),
                    String("tacos".into()),
                    Int(0),
                ];

                assert_eq!(&expected, registers);
            }

            #[test]
            fn returns_0_for_protected_functions() {
                let code = indoc! { r##"
                    mixed q = this_object()->tacos();
                    protected int tacos() { return 666; }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = &task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Object("/my_file".into()),
                    String("tacos".into()),
                    Int(0),
                ];

                assert_eq!(&expected, registers);
            }
        }

        mod test_catch {
            use super::*;

            #[test]
            fn stores_the_error_string() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(10 / j);

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

                let expected = vec![
                    Int(0),
                    Int(0),
                    String("Runtime Error: Division by zero".into()),
                    Int(10),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                assert_eq!(expected, registers);
            }

            #[test]
            fn stores_zero_when_no_error() {
                let code = indoc! { r##"
                    void create() {
                        int j = 5;
                        catch(10 / j);

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

                let expected = vec![
                    Int(0),
                    Int(5),
                    Int(0),
                    Int(10),
                    Int(2),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                assert_eq!(expected, registers);
            }
        }

        mod test_catch_end {
            use super::*;

            #[test]
            fn pops_the_catch_point() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(catch(catch(catch(10 / j))));
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);

                assert!(task.catch_points.is_empty());
            }
        }

        mod test_dec {
            use super::*;

            #[test]
            fn stores_the_value_for_pre() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        --j;

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

                let expected = vec![Int(0), Int(-1), String("snapshot_stack".into()), Int(0)];

                assert_eq!(expected, registers);
            }

            #[test]
            fn stores_the_value_for_pre_when_global() {
                let code = indoc! { r##"
                    int j = 5;
                    int k = --j;
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let expected = vec![Int(4), Int(4)];

                let proc = ctx.process();
                let proc = proc.borrow();

                assert_eq!(&expected, &proc.globals.iter().cloned().collect::<Vec<_>>());
            }

            #[test]
            fn stores_the_value_for_post() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        j--;

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

                let expected = vec![
                    Int(0),
                    Int(-1),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                assert_eq!(expected, registers);
            }

            #[test]
            fn stores_the_value_for_post_when_global() {
                let code = indoc! { r##"
                    int j = 5;
                    int k = j--;
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let expected = vec![Int(4), Int(5)];

                let proc = ctx.process();
                let proc = proc.borrow();

                assert_eq!(&expected, &proc.globals.iter().cloned().collect::<Vec<_>>());
            }
        }

        mod test_eq_eq {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 2 == 2;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(2), Int(2), Int(1)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_fconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    float π = 4.13;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Float(4.13.into())];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_functionptrconst {
            use super::*;

            #[test]
            fn stores_the_value_for_efuns() {
                let code = indoc! { r##"
                    function f = dump;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Function("dump".to_string(), vec![])];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_simul_efuns() {
                let code = indoc! { r##"
                    function f = simul_efun;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Function("simul_efun".to_string(), vec![])];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_call_other() {
                let code = indoc! { r##"
                    function f = &(this_object())->tacco();

                    void tacco() {
                        dump("tacco!");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Object("/my_file".into()),
                    Object("/my_file".into()),
                    Function("tacco".to_string(), vec![]),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_with_args() {
                let code = indoc! { r##"
                    function f = &tacco(1, 666);

                    void tacco(int a, int b) {
                        dump(a + b);
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(666),
                    Function("tacco".to_string(), vec![Some(Int(1)), Some(Int(666))]),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_with_partial_applications() {
                let code = indoc! { r##"
                    function f = &tacco(1, , , 42, );

                    void tacco(int a, int b, int c, int d, int e) {
                        dump(a + b - c * (d + e));
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_closures() {
                let code = indoc! { r##"
                    function f = maker();

                    function maker() {
                        int i = 666;
                        return (: i + $1 :);
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Function("closure-0".to_string(), vec![]),
                    Function("closure-0".to_string(), vec![]),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_gt {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 > 1199;
                    mixed r = 1199 > 1200;
                    mixed s = 1200 > 1200;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_gte {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 >= 1199;
                    mixed r = 1199 >= 1200;
                    mixed s = 1200 >= 1200;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_iadd {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    int q = 16 + 34;
                    int r = 12 + -4;
                    int s = q + r;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(50),
                    Int(8),
                    Int(58),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_iconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 666;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(666)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_iconst0 {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 0;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_iconst1 {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(1)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_idiv {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 / 2;
                    mixed r = 12 / -4;
                    mixed s = q / r;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(8),
                    Int(-3),
                    Int(-2),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn errors_on_division_by_zero() {
                let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q / r;
                "##};

                let mut cell_key = QCellOwner::new();
                let mut task: Task<10> = Task::new(Memory::default());
                let (program, _, _) = compile_prog(code);
                let r = task.initialize_program(program, Config::default(), cell_key.cell(ObjectSpace::default()), &mut cell_key);

                assert_eq!(
                    r.unwrap_err().to_string(),
                    "Runtime Error: Division by zero"
                )
            }
        }

        mod test_imod {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 % 7;
                    mixed r = 12 % -7;
                    mixed s = q % r;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(2),
                    Int(5),
                    Int(2),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn errors_on_division_by_zero() {
                let code = indoc! { r##"
                    mixed q = 5;
                    mixed r = 0;
                    mixed s = q % r;
                "##};

                let mut cell_key = QCellOwner::new();
                let mut task: Task<10> = Task::new(Memory::default());
                let (program, _, _) = compile_prog(code);
                let r = task.initialize_program(program, Config::default(), cell_key.cell(ObjectSpace::default()), &mut cell_key);

                assert_eq!(
                    r.unwrap_err().to_string(),
                    "Runtime Error: Remainder division by zero"
                )
            }
        }

        mod test_imul {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    int q = 16 * 2;
                    int r = 12 * -4;
                    int s = q * r;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(32), Int(-48), Int(-1536)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_inc {
            use super::*;

            #[test]
            fn stores_the_value_for_pre() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        ++j;

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

                let expected = vec![Int(0), Int(1), String("snapshot_stack".into()), Int(0)];

                assert_eq!(expected, registers);
            }

            #[test]
            fn stores_the_value_for_pre_when_global() {
                let code = indoc! { r##"
                    int j = 0;
                    int k = ++j;
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let expected = vec![Int(1), Int(1)];

                let proc = ctx.process();
                let proc = proc.borrow();

                assert_eq!(&expected, &proc.globals.iter().cloned().collect::<Vec<_>>());
            }

            #[test]
            fn stores_the_value_for_post() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        j++;

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                assert_eq!(expected, registers);
            }

            #[test]
            fn stores_the_value_for_post_when_global() {
                let code = indoc! { r##"
                    int j = 5;
                    int k = j++;
                "##};

                let mut cell_key = QCellOwner::new();
                let (_task, ctx) = run_prog(code, &mut cell_key);

                let expected = vec![Int(6), Int(5)];

                let proc = ctx.process();
                let proc = proc.borrow();

                assert_eq!(&expected, &proc.globals.iter().cloned().collect::<Vec<_>>());
            }
        }

        mod test_isub {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    int q = 16 - 2;
                    int r = 12 - -4;
                    int s = q - r;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(14), Int(16), Int(-2)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_jmp {
            use super::*;

            #[test]
            fn stores_the_value() {
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

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

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

                assert_eq!(expected, registers);
            }
        }

        mod test_jnz {
            use super::*;

            #[test]
            fn stores_the_value() {
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

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

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

                assert_eq!(expected, registers);
            }
        }

        mod test_jz {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                            int i = 12;
                            int j = i > 12 ? 10 : 1000;
                        "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_load {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    int *i = ({ 1, 2, 3 });
                    int j = i[1];
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_lt {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 < 1199;
                    mixed r = 1199 < 1200;
                    mixed s = 1200 < 1200;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_lte {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 1200 <= 1199;
                    mixed r = 1199 <= 1200;
                    mixed s = 1200 <= 1200;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_mapconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = ([
                        "asdf": 123,
                        456: 4.13
                    ]);
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_madd {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = "abc";
                    mixed b = 123;
                    mixed c = a + b;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("abc".into()),
                    Int(123),
                    String("abc123".into()),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_mmul {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = "abc";
                    mixed b = 4;
                    mixed c = a * b;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("abc".into()),
                    Int(4),
                    String("abcabcabcabc".into()),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_msub {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = ({ 1, 1, 2, 3 });
                    mixed b = a - ({ 1 });
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_not {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = !2;
                    mixed b = !!4;
                    float c = !0.00;
                    float d = !0.01;
                    string e = !"";
                    string f = !"asdf";
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_or {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 | 27;
                    mixed b = 0 | a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(31), Int(0), Int(31)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_oror {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 || 333;
                    mixed b = 0;
                    mixed c = b || a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(123), Int(123), Int(0), Int(0), Int(123)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_populate_argv {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        do_thing(1, 2, 3, "foo", ({ "bar", "baz", 4.13 }), ([ "a": 123 ]));
                    }

                    void do_thing(int a, int b, ...) {
                        dump(argv);
                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

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

                assert_eq!(expected, registers);
            }
        }

        mod test_populate_defaults {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        do_thing(45, 34, 7.77);
                    }

                    void do_thing(int a, int b, float d = 6.66, string s = "snuh", mixed *muh = ({ "a string", 3, 2.44 })) {
                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

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

                assert_eq!(expected, registers);
            }
        }

        mod test_range {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = ({ 1, 2, 3 })[1..];
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }
        }

        mod test_regcopy {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 4;
                    mixed b = a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(4)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_ret {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    int create() { return 666; }
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(666), // return value from create()
                    Int(666), // The copy of the call return value into its own register
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_shl {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 12345 << 6;
                    mixed b = 0 << a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(790080), Int(0), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_shr {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 12345 >> 6;
                    mixed b = 0 >> a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(192), Int(0), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_sizeof {
            use lpc_rs_asm::instruction::Instruction::{SConst, Sizeof};
            use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType};
            use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

            use super::*;
            use crate::test_support::test_config;

            #[test]
            fn stores_the_value_for_arrays() {
                let code = indoc! { r##"
                    int a = sizeof(({ 1, 2, 3 }));
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)]),
                    Int(3),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_mappings() {
                let code = indoc! { r##"
                    int a = sizeof(([ "a": 1, 'b': 2, 3: ({ 4, 5, 6 }), 0: 0 ]));
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
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

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_strings() {
                let config = Rc::new(test_config());
                let path = LpcPath::new_in_game("/my_file.c", "/", &config.lib_dir);

                let mut functions = HashMap::new();
                functions.insert(
                    INIT_PROGRAM.to_string(),
                    ProgramFunction {
                        prototype: FunctionPrototypeBuilder::default()
                            .name(INIT_PROGRAM)
                            .return_type(LpcType::Void)
                            .build()
                            .unwrap(),
                        num_locals: 2,
                        num_upvalues: 0,
                        instructions: vec![
                            SConst(Register(1).as_local(), "Hello, world!".into()),
                            Sizeof(Register(1).as_local(), Register(2).as_local()),
                        ],
                        debug_spans: vec![None, None],
                        labels: Default::default(),
                        local_variables: Default::default(),
                        arg_locations: Default::default(),
                    }
                    .into(),
                );

                let program = Program {
                    filename: path,
                    functions,
                    num_init_registers: 2,
                    ..Default::default()
                };

                let mut cell_key = QCellOwner::new();
                let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(Memory::default());

                let object_space = ObjectSpace::default();

                let _ctx = task
                    .initialize_program(program, config, cell_key.cell(object_space), &mut cell_key)
                    .expect("failed to initialize");

                let registers = &task.stack.last().unwrap().registers;

                let expected = vec![Int(0), String("Hello, world!".into()), Int(13)];

                assert_eq!(&expected, registers);
            }
        }

        mod test_store {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        mixed a = ({ 1, 2, 3 });
                        a[2] = 678;

                        debug("snapshot_stack");
                    }
                "##};

                let mut cell_key = QCellOwner::new();
                let registers = snapshot_registers(code, &mut cell_key);

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

                assert_eq!(expected, registers);
            }
        }

        mod test_sconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    string foo = "lolwut";
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), String("lolwut".into())];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_xor {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 ^ 27;
                    mixed b = 0 ^ a;
                "##};

                let mut cell_key = QCellOwner::new();
                let (task, _) = run_prog(code, &mut cell_key);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(20), Int(0), Int(20)];

                assert_eq!(&expected, &registers);
            }
        }
    }

    mod test_limits {
        use super::*;

        #[test]
        fn errors_on_stack_overflow() {
            let code = indoc! { r##"
                int kab00m = marf();

                int marf() {
                    return marf();
                }
            "##};

            let mut cell_key = QCellOwner::new();
            let mut task: Task<5> = Task::new(Memory::default());
            let (program, _, _) = compile_prog(code);
            let r = task.initialize_program(
                program, Config::default(), cell_key.cell(ObjectSpace::default()), &mut cell_key
            );

            assert_eq!(r.unwrap_err().to_string(), "stack overflow");
        }

        #[test]
        fn errors_on_too_long_evaluation() {
            let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};

            let config = ConfigBuilder::default()
                .max_task_instructions(10_usize)
                .build()
                .unwrap();
            let (program, _, _) = compile_prog(code);
            let mut cell_key = QCellOwner::new();
            let mut task: Task<5> = Task::new(Memory::default());
            let r = task.initialize_program(
                program, config, cell_key.cell(ObjectSpace::default()), &mut cell_key
            );

            assert_eq!(
                r.unwrap_err().to_string(),
                "evaluation limit of `10` instructions has been reached."
            );
        }
    }

    mod test_globals {
        use super::*;
        use crate::interpreter::task::tests::BareVal::*;

        #[test]
        fn test_frame_globals() {
            let code = indoc! { r##"
                int i = 0;
                function inc = (: i++ :);
                int j = inc();
                int k = inc();
            "##};

            let mut cell_key = QCellOwner::new();
            let (task, ctx) = run_prog(code, &mut cell_key);
            let registers = task.popped_frame.unwrap().registers;

            let expected = vec![
                Int(1),
                Int(0),
                Function("closure-0".to_string(), vec![]),
                Int(0),
                Int(1),
            ];

            assert_eq!(&expected, &registers);

            let proc = ctx.process();
            let proc = proc.borrow();

            let expected = vec![
                Int(2),
                Function("closure-0".to_string(), vec![]),
                Int(0),
                Int(1),
            ];
            assert_eq!(&expected, &proc.globals);
        }
    }

    mod test_upvalues {
        use super::*;
        use crate::interpreter::task::tests::BareVal::*;

        fn check_local_vars<T>(code: &str, vars: &IndexMap<&str, T>)
        where
            T: Into<BareVal> + Clone,
        {
            let mut cell_key = QCellOwner::new();
            let (mut task, _ctx) = run_prog(code, &mut cell_key);

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
                    found.iter().any(|local| v == local.value),
                    "key: {k}, value: {v}, found: {found:?}"
                );
                // assert_eq!(&v, frame_vars.get(*k).unwrap(), "key: {}", k);
            }
        }

        fn check_proc_upvalues<T>(code: &str, upvalues: &[T])
        where
            T: Into<BareVal> + Clone,
        {
            let mut cell_key = QCellOwner::new();
            let (mut task, _ctx) = run_prog(code, &mut cell_key);

            let snapshot = &mut task.snapshots.pop().unwrap();
            snapshot.pop(); // pop off the init frame

            let frame = snapshot.pop().unwrap();
            let proc = frame.process.borrow();

            assert_eq!(
                upvalues.len(),
                proc.upvalues.len(),
                "upvalues: {:?}\nproc upvalues: {:?}",
                upvalues
                    .iter()
                    .map(|i| i.clone().into())
                    .collect::<Vec<BareVal>>(),
                proc.upvalues
            );

            for (i, v) in upvalues.iter().enumerate() {
                let v: BareVal = v.clone().into();
                assert_eq!(v, proc.upvalues[i], "index: {i}");
            }
        }

        fn check_frame_upvalues<T>(code: &str, upvalues: &[T])
        where
            T: Into<Register> + Copy,
        {
            let mut cell_key = QCellOwner::new();
            let (mut task, _ctx) = run_prog(code, &mut cell_key);

            let snapshot = &mut task.snapshots.pop().unwrap();
            snapshot.pop(); // pop off the init frame

            let frame = snapshot.pop().unwrap();

            assert_eq!(upvalues.len(), frame.upvalues.len());

            for (i, v) in upvalues.iter().enumerate() {
                let v: Register = (*v).into();
                assert_eq!(v, frame.upvalues[i], "index: {i}");
            }
        }

        #[test]
        fn test_local_captures() {
            let code = indoc! { r##"
                void create() {
                    int i = 0;
                    function inc = (: i++; debug("snapshot_stack"); i :);
                    int j = inc();
                    int k = inc();
                }
            "##};

            let expected = vec![Int(2)];
            check_proc_upvalues(code, &expected);

            let expected = vec![Register(0)];
            check_frame_upvalues(code, &expected);

            let expected: IndexMap<&str, BareVal> = IndexMap::new();
            check_local_vars(code, &expected);
        }

        #[test]
        fn test_shared_captures() {
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

            check_local_vars(code, &expected);
        }

        #[test]
        fn test_arg_captures() {
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
            check_proc_upvalues(code, &expected);

            let expected = IndexMap::from([
                ("j", Int(10)),
                ("k", Int(-40)),
                ("l", Int(20)),
                ("m", Int(2)),
                ("n", Int(1332)),
                ("add", Function("closure-0".into(), vec![])),
                ("add2", Function("closure-0".into(), vec![])),
            ]);
            check_local_vars(code, &expected);
        }

        #[test]
        fn test_higher_order() {
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

            let expected = vec![Int(105), Int(1), Int(1), Int(100)];
            check_proc_upvalues(code, &expected);

            let expected = IndexMap::from([
                ("c1", Int(1)),
                ("c2", Int(5)),
                ("c3", Int(105)),
                ("make_counter", Function("closure-1".into(), vec![])),
                ("counter1", Function("closure-0".into(), vec![])),
                ("counter2", Function("closure-0".into(), vec![])),
                ("counter3", Function("closure-0".into(), vec![])),
            ]);

            check_local_vars(code, &expected);
        }

        #[test]
        fn test_higher_order_with_implicit_vars() {
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
            check_proc_upvalues(code, &expected);

            let expected = IndexMap::from([
                ("c1", Int(0)),
                ("c2", Int(69)),
                ("make", Function("closure-1".into(), vec![])),
                ("made1", Function("closure-0".into(), vec![])),
                ("made2", Function("closure-0".into(), vec![])),
            ]);

            check_local_vars(code, &expected);
        }

        #[test]
        fn test_higher_order_with_partial_application() {
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
            check_proc_upvalues(code, &expected);

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

            check_local_vars(code, &expected);
        }

        #[test]
        fn test_upvalued_ellipsis() {
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
            check_proc_upvalues(code, &expected);

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

            check_local_vars(code, &expected);
        }
    }
}
