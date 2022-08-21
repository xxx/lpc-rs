use std::{
    borrow::Cow,
    cell::RefCell,
    fmt::{Debug, Display},
    rc::Rc,
};

use decorum::Total;
use if_chain::if_chain;
use indexmap::IndexMap;
use lpc_rs_asm::instruction::{Address, Instruction};
use lpc_rs_core::{
    call_namespace::CallNamespace,
    function::{FunctionName, FunctionReceiver, FunctionTarget},
    function_arity::FunctionArity,
    register::{Register, RegisterVariant},
    LpcInt, EFUN, INIT_PROGRAM,
};
use lpc_rs_errors::{LpcError, Result};
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
        lpc_ref::LpcRef,
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

#[inline]
fn get_location<const N: usize>(
    stack: &CallStack<N>,
    location: RegisterVariant,
) -> Result<&LpcRef> {
    match location {
        RegisterVariant::Local(reg) => {
            let frame = stack.current_frame()?;
            let registers = &frame.registers;
            Ok(&registers[reg])
        }
        RegisterVariant::Upvalue(_upv) => todo!(),
    }
}

#[inline]
fn set_location<const N: usize>(
    stack: &mut CallStack<N>,
    location: RegisterVariant,
    value: LpcRef,
) -> Result<()> {
    match location {
        RegisterVariant::Local(reg) => {
            let frame = stack.current_frame_mut()?;
            let registers = &mut frame.registers;
            registers[reg] = value;
            Ok(())
        }
        RegisterVariant::Upvalue(_upv) => todo!(),
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
    /// TODO: update this for closures
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

    /// Store the most recently popped frame, for testing
    #[cfg(test)]
    pub popped_frame: Option<CallFrame>,

    /// Store a snapshot of a specific state, for testing
    #[cfg(test)]
    pub snapshot: Option<CallStack<STACKSIZE>>,
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

            #[cfg(test)]
            popped_frame: None,

            #[cfg(test)]
            snapshot: None,
        }
    }

    /// Convenience helper to get a Program initialized.
    #[instrument(skip_all)]
    pub fn initialize_program<C, O>(
        &mut self,
        program: Program,
        config: C,
        object_space: O,
    ) -> Result<TaskContext>
    where
        C: Into<Rc<Config>> + Debug,
        O: Into<Rc<RefCell<ObjectSpace>>> + Debug,
    {
        let init_function = {
            let function = program.lookup_function(INIT_PROGRAM, &CallNamespace::Local);
            if function.is_none() {
                return Err(LpcError::new("Init function not found?"));
            }

            function.unwrap().clone()
        };
        let process: Rc<RefCell<Process>> = Process::new(program).into();
        let context = TaskContext::new(config, process.clone(), object_space);
        context.insert_process(process);

        self.eval(init_function, &[], context)
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
    ) -> Result<TaskContext>
    where
        F: Into<Rc<ProgramFunction>> + Debug,
    {
        let function = f.into();
        let process = task_context.process();

        let mut frame = CallFrame::new(process, function, args.len());
        if !args.is_empty() {
            frame.registers[1..=args.len()].clone_from_slice(args);
        }

        self.stack.push(frame)?;

        let mut halted = false;

        while !halted {
            halted = match self.eval_one_instruction(&task_context) {
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
    fn eval_one_instruction(&mut self, task_context: &TaskContext) -> Result<bool> {
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
                self.binary_operation(r1, r2, r3, |x, y| x & y)?;
            }
            Instruction::BitwiseNot(r1, r2) => {
                let frame = self.stack.current_frame().unwrap();
                let debug_span = frame.current_debug_span();
                let lpc_ref = get_loc!(self, r1)?;
                match !lpc_ref {
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
                self.handle_call(&instruction, task_context)?;
            }
            Instruction::CallFp {
                location,
                num_args,
                initial_arg,
            } => {
                self.handle_call_fp(task_context, &location, &num_args, &initial_arg)?;
            }
            Instruction::CallOther { .. } => {
                self.handle_call_other(&instruction, task_context)?;
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
                            self.runtime_error(format!("Missing address for label `{}`", label))
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
            Instruction::Dec(r1) => {
                // TODO: update for upvalues
                let frame = self.stack.current_frame_mut()?;
                let registers = &mut frame.registers;

                registers[r1.index()].dec()?;
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
                let call_other = if let FunctionTarget::Local(_, ref rcvr) = target {
                    !matches!(rcvr, FunctionReceiver::Local)
                } else {
                    false
                };

                let address = match target {
                    FunctionTarget::Efun(func_name) => FunctionAddress::Efun(func_name),
                    FunctionTarget::Local(func_name, FunctionReceiver::Argument) => match func_name
                    {
                        FunctionName::Var(_) => {
                            return Err(
                                    self.runtime_error(
                                        concat!(
                                            "A function pointer with `&` as the receiver receiver somehow has a ",
                                            "var function name. This should not be reachable if ",
                                            "semantic checks have passed."
                                        )
                                    )
                                );
                        }
                        FunctionName::Literal(name) => FunctionAddress::Dynamic(name),
                    },
                    FunctionTarget::Local(func_name, func_receiver) => {
                        let proc = match func_receiver {
                            FunctionReceiver::Var(receiver_reg) => {
                                let receiver_ref = get_loc!(self, receiver_reg)?;
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
                                        return Err(
                                            self.runtime_error("Receiver was not object or string")
                                        );
                                    }
                                }
                            }
                            FunctionReceiver::Local => {
                                let frame = self.stack.current_frame()?;
                                let proc = &frame.process;

                                proc.clone()
                            }
                            FunctionReceiver::Argument => {
                                unreachable!(
                                    "This is specified in an earlier arm of the parent `match`"
                                );
                            }
                        };

                        let s = Self::resolve_function_name(&self.stack, &func_name)?;
                        let frame = self.stack.current_frame()?;
                        let proc_ref = &frame.process;
                        let borrowed_proc = proc_ref.borrow();
                        // look locally
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
                                            self.runtime_error(format!("unknown local target `{}`", s))
                                        );
                                    }
                                }
                            }
                        }
                    }
                };

                // TODO: This unwrap should go away
                let partial_args: Vec<Option<LpcRef>> = applied_arguments
                    .iter()
                    .map(|arg| {
                        arg.map(|register| get_loc!(self, register).unwrap())
                            .cloned()
                    })
                    .collect();

                let fp = FunctionPtr {
                    owner: Rc::new(Default::default()),
                    address,
                    partial_args,
                    arity,
                    call_other,
                };

                let new_ref = self.memory.value_to_ref(LpcValue::from(fp));

                set_loc!(self, location, new_ref)?;
            }
            Instruction::GLoad(r1, r2) => {
                // load from global r1, into local r2
                let process = task_context.process();
                let global = process.borrow().globals[r1.index()].borrow().clone();
                set_loc!(self, r2, global)?;
            }
            Instruction::GStore(r1, r2) => {
                // store local r1 into global r2
                let process = task_context.process();
                let globals = &mut process.borrow_mut().globals;
                globals[r2.index()].replace(get_loc!(self, r1)?.clone());
            }
            Instruction::Gt(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x > y)?;
            }
            Instruction::Gte(r1, r2, r3) => {
                self.binary_boolean_operation(r1, r2, r3, |x, y| x >= y)?;
            }
            Instruction::IAdd(r1, r2, r3) => match get_loc!(self, r1)? + get_loc!(self, r2)? {
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
            Instruction::IDiv(r1, r2, r3) => match get_loc!(self, r1)? / get_loc!(self, r2)? {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::IMod(r1, r2, r3) => match get_loc!(self, r1)? % get_loc!(self, r2)? {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::IMul(r1, r2, r3) => match get_loc!(self, r1)? * get_loc!(self, r2)? {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::Inc(r1) => {
                // TODO: update this for upvalues
                let frame = self.stack.current_frame_mut()?;
                let registers = &mut frame.registers;

                registers[r1.index()].inc()?;
            }
            Instruction::ISub(r1, r2, r3) => match get_loc!(self, r1)? - get_loc!(self, r2)? {
                Ok(result) => set_loc!(self, r3, self.memory.value_to_ref(result))?,
                Err(e) => {
                    let frame = self.stack.current_frame()?;
                    return Err(e.with_span(frame.current_debug_span()));
                }
            },
            Instruction::Jmp(label) => {
                let frame = self.stack.current_frame_mut()?;
                let address = match frame.lookup_label(&label) {
                    Some(x) => *x,
                    None => {
                        return Err(
                            self.runtime_error(format!("Missing address for label `{}`", label))
                        )
                    }
                };
                frame.set_pc(address);
            }
            Instruction::Jnz(r1, label) => {
                let v = get_loc!(self, r1)?;

                if v != &LpcRef::Int(0) && v != &LpcRef::Float(Total::from(0.0)) {
                    let frame = self.stack.current_frame()?;
                    let address = match frame.lookup_label(&label) {
                        Some(x) => *x,
                        None => {
                            return Err(self
                                .runtime_error(format!("Missing address for label `{}`", label)))
                        }
                    };
                    let frame = self.stack.current_frame()?;
                    frame.set_pc(address);
                }
            }
            Instruction::Jz(r1, ref label) => {
                let v = get_loc!(self, r1)?;

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
                self.binary_operation(r1, r2, r3, |x, y| x + y)?;
            }
            Instruction::MapConst(r, map) => {
                let mut register_map = IndexMap::new();

                for (key, value) in map {
                    register_map
                        .insert(get_loc!(self, key)?.clone(), get_loc!(self, value)?.clone());
                }

                let new_ref = self.memory.value_to_ref(LpcValue::from(register_map));

                set_loc!(self, r, new_ref)?;
            }
            Instruction::MMul(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x * y)?;
            }
            Instruction::MSub(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x - y)?;
            }
            Instruction::Not(r1, r2) => {
                let matched = match get_loc!(self, r1)? {
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
                self.binary_operation(r1, r2, r3, |x, y| x | y)?;
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                // note that argv population has no interaction with upvalues
                let registers = &mut self.stack.current_frame_mut()?.registers;
                let ellipsis_start_index = num_args + 1; // +1 is for the reserved r0
                let ellipsis_end_index = registers.len() - num_locals;
                let ellipsis_vars = &registers[ellipsis_start_index..ellipsis_end_index];
                let new_ref = self.memory.value_to_ref(LpcValue::from(ellipsis_vars));

                registers[r.index()] = new_ref;
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
                    let lpc_ref = get_location(stack, r1)?;

                    match lpc_ref {
                        LpcRef::Array(v_ref) => {
                            let value = v_ref.borrow();
                            let vec = try_extract_value!(*value, LpcValue::Array);

                            if vec.is_empty() {
                                return Ok(LpcValue::from(vec![]));
                            }

                            let index1 = get_location(stack, r2)?;
                            let index2 = get_location(stack, r3)?;

                            if let (LpcRef::Int(start), LpcRef::Int(end)) = (index1, index2) {
                                let (real_start, real_end) = resolve_range(*start, *end, vec.len());

                                if real_start <= real_end {
                                    let slice = &vec[real_start..=real_end];
                                    let mut new_vec = vec![LpcRef::Int(0); slice.len()];
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

                            let index1 = get_location(stack, r2)?;
                            let index2 = get_location(stack, r3)?;

                            if let (LpcRef::Int(start), LpcRef::Int(end)) = (index1, index2) {
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
                let new_ref = get_location(&self.stack, r1)?.clone();
                set_loc!(self, r2, new_ref)?;
            }
            Instruction::Ret => {
                pop_frame!(self, task_context);

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
            }
            Instruction::Sizeof(r1, r2) => {
                let lpc_ref = get_loc!(self, r1)?;

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
                self.binary_operation(r1, r2, r3, |x, y| x << y)?;
            }
            Instruction::Shr(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x >> y)?;
            }
            Instruction::Xor(r1, r2, r3) => {
                self.binary_operation(r1, r2, r3, |x, y| x ^ y)?;
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
                    .map(|i| get_loc!(self, *i).cloned())
                    .collect::<Result<Vec<_>>>()?;
                let new_ref = self.memory.value_to_ref(LpcValue::from(vars));

                set_loc!(self, *r, new_ref)
            }
            _ => Err(self.runtime_error("non-AConst instruction passed to `handle_aconst`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_call(&mut self, instruction: &Instruction, task_context: &TaskContext) -> Result<()> {
        match instruction {
            Instruction::Call {
                name,
                namespace,
                num_args,
                initial_arg,
            } => {
                let frame = self.stack.current_frame()?;
                let process = frame.process.clone();
                let borrowed = process.borrow();
                let num_args = *num_args;

                let function = borrowed.lookup_function(name, namespace);
                let mut new_frame = if let Some(func) = function {
                    CallFrame::with_minimum_arg_capacity(
                        process.clone(),
                        func.clone(),
                        num_args,
                        num_args,
                    )
                } else {
                    if_chain! {
                        if let Some(func) = task_context.simul_efuns();
                        let b = func.borrow();
                        if let Some(func) = b.lookup_function(name, &CallNamespace::Local);
                        then {
                            CallFrame::with_minimum_arg_capacity(
                                process.clone(),
                                func.clone(),
                                num_args,
                                num_args,
                            )
                        } else {
                            if let Some(prototype) = EFUN_PROTOTYPES.get(name.as_str()) {
                                let func = ProgramFunction::new(prototype.clone(), 0);

                                CallFrame::with_minimum_arg_capacity(
                                    process.clone(),
                                    Rc::new(func),
                                    num_args,
                                    num_args,
                                )
                            } else {
                                let msg = format!("Call to unknown function `{}`", name);
                                return Err(self.runtime_error(msg));
                            }
                        }
                    }
                };

                // copy argument registers from old frame to new
                if num_args > 0_usize {
                    let index = initial_arg.index();
                    let registers = &frame.registers;
                    new_frame.registers[1..=num_args]
                        .clone_from_slice(&registers[index..(index + num_args)]);
                }

                let function_is_efun = EFUN_PROTOTYPES.contains_key(name.as_str())
                    && (!frame.process.borrow().contains_function(name, namespace)
                        || namespace.as_str() == EFUN);

                self.stack.push(new_frame)?;

                if function_is_efun {
                    let mut ctx = EfunContext::new(&mut self.stack, task_context, &self.memory);

                    call_efun(name.as_str(), &mut ctx)?;

                    #[cfg(test)]
                    {
                        if ctx.snapshot.is_some() {
                            self.snapshot = ctx.snapshot;
                        }
                    }

                    pop_frame!(self, task_context);
                }

                Ok(())
            }

            _ => Err(self.runtime_error("non-Call instruction passed to `handle_call`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_call_fp(
        &mut self,
        task_context: &TaskContext,
        location: &RegisterVariant,
        num_args: &usize,
        initial_arg: &RegisterVariant,
    ) -> Result<()> {
        let func = {
            let lpc_ref = get_loc!(self, *location)?;

            if let LpcRef::Function(func) = lpc_ref {
                func.clone() // this is a cheap clone
            } else {
                return Err(self.runtime_error("callfp instruction on non-function"));
            }
        };

        // TODO: reduce the amount of time this borrow is live
        let borrowed = func.borrow();
        let ptr = try_extract_value!(*borrowed, LpcValue::Function);

        let arity = ptr.arity;
        let partial_args = &ptr.partial_args;
        let passed_args_count = *num_args
            + partial_args
                .iter()
                .fold(0, |sum, arg| sum + arg.is_some() as usize);
        let function_is_efun = matches!(&ptr.address, FunctionAddress::Efun(_));
        let dynamic_receiver = matches!(&ptr.address, FunctionAddress::Dynamic(_));
        // for dynamic receivers, skip the first arg register, which contains the
        // receiver
        let index = initial_arg.index() + (dynamic_receiver as usize);
        let adjusted_num_args = *num_args - (dynamic_receiver as usize);
        let max_arg_length = Self::calculate_max_arg_length(adjusted_num_args, partial_args, arity);

        if let FunctionAddress::Local(receiver, pf) = &ptr.address {
            if !pf.public() && (ptr.call_other || !Rc::ptr_eq(&task_context.process(), receiver)) {
                return set_loc!(self, Register(0).as_register(), LpcRef::Int(0));
            }
        }

        let (proc, function) = match &ptr.address {
            FunctionAddress::Local(proc, function) => (proc.clone(), function.clone()),
            FunctionAddress::Dynamic(name) => {
                let lpc_ref = get_loc!(self, *initial_arg)?;

                if let LpcRef::Object(lpc_ref) = lpc_ref {
                    let b = lpc_ref.borrow();
                    let cell = try_extract_value!(*b, LpcValue::Object);
                    let proc = cell.borrow();
                    let func = proc.lookup_function(name, &CallNamespace::Local);

                    if let Some(func) = func {
                        (cell.clone(), func.clone())
                    } else {
                        return Err(
                            self.runtime_error(format!("call to unknown function `{}", name))
                        );
                    }
                } else {
                    return Err(self.runtime_error("non-object receiver to function pointer call"));
                }
            }
            FunctionAddress::Efun(name) => {
                // unwrap is safe because this should have been checked in an earlier step
                let prototype = EFUN_PROTOTYPES.get(name.as_str()).unwrap();
                let pf = ProgramFunction::new(prototype.clone(), 0);

                let frame = self.stack.current_frame()?;

                (frame.process.clone(), Rc::new(pf))
            }
        };

        let mut new_registers = RegisterBank::initialized_for_function(&*function, max_arg_length);

        // negotiate the passed & partially-applied arguments
        if arity.num_args > 0_usize || (dynamic_receiver && *num_args > 0) {
            let frame = self.stack.current_frame()?;
            let registers = &frame.registers;

            if !partial_args.is_empty() {
                let mut from_index = 0;

                let from_slice = &registers[index..(index + adjusted_num_args)];
                let to_slice = &mut new_registers[1..=max_arg_length];

                for (i, item) in to_slice.into_iter().enumerate().take(max_arg_length) {
                    if let Some(Some(x)) = partial_args.get(i) {
                        // if a partially-appliable arg is present, use it
                        let _ = std::mem::replace(item, x.clone());
                    } else if let Some(x) = from_slice.get(from_index) {
                        // check if the user passed an argument to
                        // fill in a hole in the partial arguments
                        let _ = std::mem::replace(item, x.clone());
                        from_index += 1;
                    }
                }
            } else {
                // just copy argument registers from old frame to new
                new_registers[1..=adjusted_num_args]
                    .clone_from_slice(&registers[index..(index + adjusted_num_args)]);
            }
        }

        // Type-check the args.
        // TODO: Maybe make this optional somehow?
        let arg_types = &function.prototype.arg_types;
        for (i, lpc_ref) in new_registers[1..].into_iter().enumerate() {
            if_chain! {
                if let Some(arg_type) = arg_types.get(i);
                let ref_type = lpc_ref.as_lpc_type();
                if !ref_type.matches_type(*arg_type);
                then {
                    let arg_spans = &function.prototype.arg_spans;
                    let arg_def_span = arg_spans.get(i).map(|s| *s);
                    let error = self.runtime_error(format!(
                        "unexpected argument type to `{}`: {}. expected {}.",
                        function.prototype.name, ref_type, arg_type
                    ))
                    .with_label("defined here", arg_def_span);

                    return Err(error);
                }
            }
        }

        let new_frame = CallFrame::with_registers(proc, function, passed_args_count, new_registers);

        let fc = new_frame.function.clone();
        let name = fc.name();
        self.stack.push(new_frame)?;

        if function_is_efun {
            let mut ctx = EfunContext::new(&mut self.stack, task_context, &self.memory);

            call_efun(name, &mut ctx)?;

            #[cfg(test)]
            {
                if ctx.snapshot.is_some() {
                    self.snapshot = ctx.snapshot;
                }
            }

            pop_frame!(self, task_context);
        }

        Ok(())
    }

    #[instrument(skip_all)]
    fn handle_call_other(
        &mut self,
        instruction: &Instruction,
        task_context: &TaskContext,
    ) -> Result<()> {
        match instruction {
            Instruction::CallOther {
                receiver,
                name,
                num_args,
                initial_arg,
            } => {
                // set up result_ref in a block, as `registers` is a long-lived reference that
                // doesn't work as mutable, but needs to be written to at the very end.
                let result_ref = {
                    // figure out which function we're calling
                    let receiver_ref = get_location(&self.stack, *receiver)?;
                    let name_ref = get_location(&self.stack, *name)?;
                    let pool_ref = if let LpcRef::String(r) = name_ref {
                        r
                    } else {
                        let str = format!("Invalid name passed to `call_other`: {}", name_ref);
                        return Err(self.runtime_error(str));
                    };
                    let borrowed = pool_ref.borrow();
                    let function_name = try_extract_value!(*borrowed, LpcValue::String);

                    let initial_index = initial_arg.index();

                    // An inner helper function to actually calculate the result, for easy re-use
                    // when using `call_other` with arrays and mappings.
                    fn resolve_result(
                        receiver_ref: &LpcRef,
                        function_name: &str,
                        registers: &RegisterBank,
                        initial_index: usize,
                        num_args: usize,
                        task_context: &TaskContext,
                        memory: &Memory,
                    ) -> Result<LpcRef> {
                        let resolved = Task::<MAX_CALL_STACK_SIZE>::resolve_call_other_receiver(
                            receiver_ref,
                            function_name,
                            task_context,
                            &CallNamespace::Local,
                        );

                        if let Some(pr) = resolved {
                            let value = LpcValue::from(pr);
                            let result = match value {
                                LpcValue::Object(receiver) => {
                                    let args =
                                        &registers[initial_index..(initial_index + num_args)];

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

                                    if !function.public() {
                                        LpcRef::Int(0)
                                    } else {
                                        let eval_context =
                                            task.eval(function, args, new_context)?;
                                        task_context.increment_instruction_count(
                                            eval_context.instruction_count(),
                                        )?;

                                        eval_context.into_result()
                                    }
                                }
                                _ => LpcRef::Int(0),
                            };

                            Ok(result)
                        } else {
                            Err(LpcError::new("Unable to find the receiver."))
                        }
                    }

                    let registers = &self.stack.current_frame()?.registers;

                    match &receiver_ref {
                        LpcRef::String(_) | LpcRef::Object(_) => resolve_result(
                            receiver_ref,
                            function_name,
                            registers,
                            initial_index,
                            *num_args,
                            task_context,
                            &*self.memory,
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
                                        registers,
                                        initial_index,
                                        *num_args,
                                        task_context,
                                        &*self.memory,
                                    )
                                    .unwrap_or(LpcRef::Int(0))
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
                                            registers,
                                            initial_index,
                                            *num_args,
                                            task_context,
                                            &*self.memory,
                                        )
                                        .unwrap_or(LpcRef::Int(0)),
                                    )
                                })
                                .collect::<IndexMap<_, _>>()
                                .into();

                            self.memory.value_to_ref(with_results)
                        }
                        _ => {
                            return Err(self.runtime_error(format!(
                                "What are you trying to call `{}` on?",
                                function_name
                            )))
                        }
                    }
                };

                let registers = &mut self.stack.current_frame_mut()?.registers;
                registers[0] = result_ref;

                Ok(())
            }
            _ => Err(self.runtime_error("non-Call instruction passed to `handle_call`")),
        }
    }

    #[instrument(skip_all)]
    fn handle_load(&mut self, instruction: &Instruction) -> Result<()> {
        match instruction {
            Instruction::Load(r1, r2, r3) => {
                let container_ref = get_loc!(self, *r1)?.clone();
                let lpc_ref = get_loc!(self, *r2)?.clone();

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
                            LpcRef::Int(0)
                        };

                        set_loc!(self, *r3, var)?;

                        Ok(())
                    }
                    x => {
                        Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x)))
                    }
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
                    let container_ref = get_loc!(self, *r1)?;
                    let lpc_ref = get_loc!(self, *r2)?;

                    match container_ref {
                        LpcRef::Mapping(map_ref) => {
                            let value = map_ref.borrow();
                            let map = try_extract_value!(*value, LpcValue::Mapping);

                            let index = match lpc_ref {
                                LpcRef::Int(i) => *i,
                                _ => {
                                    return Err(self
                                        .runtime_error(format!("Invalid index type: {}", lpc_ref)))
                                }
                            };

                            if let Some((key, _)) = map.get_index(index as usize) {
                                key.clone()
                            } else {
                                LpcRef::Int(0)
                            }
                        }
                        x => {
                            return Err(self.runtime_error(format!(
                                "Invalid attempt to take index of `{}`",
                                x
                            )))
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
                let mut container = get_loc!(self, *r2)?.clone();
                let index = get_loc!(self, *r3)?;
                let array_idx = if let LpcRef::Int(i) = index { *i } else { 0 };

                match container {
                    LpcRef::Array(vec_ref) => {
                        let mut r = vec_ref.borrow_mut();
                        let vec = match *r {
                            LpcValue::Array(ref mut v) => v,
                            _ => return Err(self.runtime_error(
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
                            vec[idx as usize] = get_loc!(self, *r1)?.clone();
                        } else {
                            return Err(self.array_index_error(idx, len));
                        }

                        Ok(())
                    }
                    LpcRef::Mapping(ref mut map_ref) => {
                        let mut r = map_ref.borrow_mut();
                        let map = match *r {
                            LpcValue::Mapping(ref mut m) => m,
                            _ => return Err(self.runtime_error(
                                "LpcRef with a non-Mapping reference as its value. This indicates a bug in the interpreter.")
                            )
                        };

                        map.insert(index.clone(), get_loc!(self, *r1)?.clone());

                        Ok(())
                    }
                    x => {
                        Err(self.runtime_error(format!("Invalid attempt to take index of `{}`", x)))
                    }
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
    ) -> Option<Rc<RefCell<Process>>> {
        let process = match receiver_ref {
            LpcRef::String(s) => {
                let r = s.borrow();
                let str = if let LpcValue::String(ref s) = *r {
                    s
                } else {
                    return None;
                };

                match context.lookup_process(str) {
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
        set_loc!(self, Register(result_index).as_register(), lpc_ref)?;
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
        let ref1 = get_location(&self.stack, r1)?;
        let ref2 = get_location(&self.stack, r2)?;

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
        let ref1 = get_location(&self.stack, r1)?;
        let ref2 = get_location(&self.stack, r2)?;

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
                let name_ref = get_location(stack, *reg)?;

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

    #[inline]
    fn array_index_error<T>(&self, index: T, length: usize) -> LpcError
    where
        T: Display,
    {
        self.runtime_error(format!(
            "Attempting to access index {} in an array of length {}",
            index, length
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
        hash::{Hash, Hasher},
    };

    use indoc::indoc;
    use lpc_rs_core::{LpcFloat, LpcInt};

    use super::*;
    use crate::{
        extract_value,
        test_support::{compile_prog, run_prog},
    };

    /// A type to make it easier to set up test expectations for register
    /// contents
    #[derive(Debug, PartialEq, Eq, Clone)]
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
                    let array = a.into_iter().map(|item| item.into()).collect::<Vec<_>>();
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
                BareVal::Mapping(x) => std::ptr::hash(&*x, state),
                BareVal::Object(x) => std::ptr::hash(&*x, state),
                BareVal::Function(x, y) => {
                    x.hash(state);
                    y.hash(state);
                }
            }
        }
    }

    mod test_instructions {
        use super::*;
        use crate::interpreter::task::tests::BareVal::*;

        fn snapshot_registers(code: &str) -> RegisterBank {
            let (task, _) = run_prog(code);
            let mut stack = task.snapshot.unwrap();

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(12),
                    Float(LpcFloat::from(4.3)),
                    String("hello".into()),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)].into()),
                    Array(
                        vec![
                            Int(12),
                            Float(LpcFloat::from(4.3)),
                            String("hello".into()),
                            Array(vec![Int(1), Int(2), Int(3)].into()),
                        ]
                        .into(),
                    ),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(11), Int(0), Int(11), Int(0)];

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(123),
                    Int(333),
                    Int(333),
                    Int(0),
                    Int(0),
                    Int(0),
                    Int(0),
                ];

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(-1), Int(7), Int(7), Int(-8)];

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

                let (task, _) = run_prog(code);
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

                let (_task, ctx) = run_prog(code);

                let proc = ctx.process();
                let borrowed = proc.borrow();
                let values = borrowed.global_variable_values();
                assert_eq!(
                    String("my public_function".into()),
                    *values.get("mine").unwrap().borrow()
                );
                assert_eq!(
                    String("/std/object public".into()),
                    *values.get("parents").unwrap().borrow()
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

                let (_task, ctx) = run_prog(code);

                let proc = ctx.process();
                let borrowed = proc.borrow();
                let values = borrowed.global_variable_values();
                assert_eq!(
                    String("file_name_override".into()),
                    *values.get("this_one").unwrap().borrow()
                );
                assert_eq!(
                    String("/std/object#0".into()),
                    *values.get("efun_one").unwrap().borrow()
                );
            }

            #[test]
            fn calls_correct_function_with_simul_efuns() {
                let code = indoc! { r##"
                    string this_one = simul_efun("marf");
                "##};

                let (_task, ctx) = run_prog(code);

                let proc = ctx.process();
                let borrowed = proc.borrow();
                let values = borrowed.global_variable_values();
                assert_eq!(
                    String("this is a simul_efun: marf".into()),
                    *values.get("this_one").unwrap().borrow()
                );
            }
        }

        mod test_call_fp {
            use itertools::Itertools;

            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    function q = tacos;
                    int a = q(666);
                    int tacos(int j) { return j + 1; }
                "##};

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(667),
                    Function("tacos".into(), vec![]),
                    Function("tacos".into(), vec![]),
                    Int(666),
                    Function("tacos".into(), vec![]),
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

                let (task, _) = run_prog(code);
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
                    Int(666),
                    Int(4),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    String("my_string! awesome!".into()),
                    String("my_string!".into()),
                    Function("tacos".into(), vec![Some(String("my_string!".into()))]),
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

                let (task, _) = run_prog(code);
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
                    Int(666),
                    Int(4),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
                    String("adding some! 670".into()),
                    Int(123),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
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

                let (task, _) = run_prog(code);
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
                    Int(42),
                    Int(4),
                    String("should be in argv".into()),
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
                    Int(46),
                    Int(69),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                println!(
                    "{:#?}",
                    registers.iter().map(|a| BareVal::from(a)).collect_vec()
                );

                let expected = vec![
                    String("widget: 42. awesome!".into()),
                    String("awesome!".into()),
                    Function("name".into(), vec![None, Some(String("awesome!".into()))]),
                    Object("/my_file".into()),
                    Int(666),
                    Object("/my_file".into()),
                    Int(666),
                    Function("name".into(), vec![None, Some(String("awesome!".into()))]),
                    String("me: 666. awesome!".into()),
                    String("/std/widget".into()),
                    Object("/std/widget#0".into()),
                    Int(42),
                    Object("/std/widget#0".into()),
                    Int(42),
                    Function("name".into(), vec![None, Some(String("awesome!".into()))]),
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

                let (task, _) = run_prog(code);
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
                    Int(666),
                    Int(4),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
                    Int(0),
                    Int(123),
                    Function(
                        "tacos".into(),
                        vec![None, Some(String("adding some!".into()))],
                    ),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(4),
                    Function("tacos".into(), vec![]),
                    Function("tacos".into(), vec![]),
                    Int(4),
                    Function("tacos".into(), vec![]),
                    Int(4),
                ];

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

                let mut object_space = ObjectSpace::default();
                let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(Memory::default());

                let (program, config, process) = compile_prog(code);
                object_space.insert_process(process);

                let result = task.initialize_program(program, config, object_space);

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

                let result = task.initialize_program(program, config, object_space);

                assert_eq!(
                    result.unwrap_err().to_string(),
                    "runtime error: unexpected argument type to `tacos`: float. expected string."
                );
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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

                let registers = snapshot_registers(code);

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

                let registers = snapshot_registers(code);

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

                let (task, _) = run_prog(code);

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

                let registers = snapshot_registers(code);

                let expected = vec![Int(0), Int(-1), String("snapshot_stack".into()), Int(0)];

                assert_eq!(expected, registers);
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

                let registers = snapshot_registers(code);

                let expected = vec![
                    Int(0),
                    Int(-1),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                assert_eq!(expected, registers);
            }
        }

        mod test_eq_eq {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 2 == 2;
                "##};

                let (task, _) = run_prog(code);
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
                    float π = 3.14;
                "##};

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Float(3.14.into())];

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Function("dump".to_string(), vec![]),
                    Function("dump".to_string(), vec![]),
                ];

                assert_eq!(&expected, &registers);
            }

            #[test]
            fn stores_the_value_for_simul_efuns() {
                let code = indoc! { r##"
                    function f = simul_efun;
                "##};

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Function("simul_efun".to_string(), vec![]),
                    Function("simul_efun".to_string(), vec![]),
                ];

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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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
        }

        mod test_gload {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 3.14;
                    mixed j = q + 1.1;
                "##};

                let (task, ctx) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Float(3.14.into()),
                    Float(3.14.into()),
                    Float(1.1.into()),
                    Float(4.24.into()),
                ];

                assert_eq!(&expected, &registers);

                let global_registers = ctx
                    .process()
                    .borrow()
                    .globals
                    .iter()
                    .map(|global| (*global.borrow()).clone())
                    .collect::<Vec<_>>();

                let global_expected = vec![Float(3.14.into()), Float(4.24.into())];

                assert_eq!(&global_expected, &global_registers);
            }
        }

        mod test_gstore {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 3.14;
                "##};

                let (task, ctx) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Float(3.14.into())];

                assert_eq!(&expected, &registers);

                let global_registers = ctx
                    .process()
                    .borrow()
                    .globals
                    .iter()
                    .map(|global| (*global.borrow()).clone())
                    .collect::<Vec<_>>();

                let global_expected = vec![Float(3.14.into())];

                assert_eq!(&global_expected, &global_registers);
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(50),
                    Int(8),
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(8),
                    Int(-3),
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

                let mut task: Task<10> = Task::new(Memory::default());
                let (program, _, _) = compile_prog(code);
                let r = task.initialize_program(program, Config::default(), ObjectSpace::default());

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    // the constant expressions are folded at parse time
                    Int(2),
                    Int(5),
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

                let mut task: Task<10> = Task::new(Memory::default());
                let (program, _, _) = compile_prog(code);
                let r = task.initialize_program(program, Config::default(), ObjectSpace::default());

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(32), Int(-48), Int(32), Int(-48), Int(-1536)];

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

                let registers = snapshot_registers(code);

                let expected = vec![Int(0), Int(1), String("snapshot_stack".into()), Int(0)];

                assert_eq!(expected, registers);
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

                let registers = snapshot_registers(code);

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(0),
                    String("snapshot_stack".into()),
                    Int(0),
                ];

                assert_eq!(expected, registers);
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(14), Int(16), Int(14), Int(16), Int(-2)];

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

                let registers = snapshot_registers(code);

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

                let registers = snapshot_registers(code);

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(12),
                    Int(1000),
                    Int(12),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)].into()),
                    Array(vec![Int(1), Int(2), Int(3)].into()),
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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
                        456: 3.14
                    ]);
                "##};

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let mut hashmap = HashMap::new();
                hashmap.insert(String("asdf".into()), Int(123));
                hashmap.insert(Int(456), Float(3.14.into()));

                let expected = vec![
                    Int(0),
                    String("asdf".into()),
                    Int(123),
                    Int(456),
                    Float(3.14.into()),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("abc".into()),
                    Int(123),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    String("abc".into()),
                    Int(4),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(1), Int(2), Int(3)].into()),
                    Array(vec![Int(1), Int(1), Int(2), Int(3)].into()),
                    Int(1),
                    Array(vec![Int(1)].into()),
                    Array(vec![Int(2), Int(3)].into()),
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(31), Int(0), Int(31), Int(31)];

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(123),
                    Int(123),
                    Int(0),
                    Int(0),
                    Int(0),
                    Int(123),
                    Int(123),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_populate_argv {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    void create() {
                        do_thing(1, 2, 3, "foo", ({ "bar", "baz", 3.14 }), ([ "a": 123 ]));
                    }

                    void do_thing(int a, int b, ...) {
                        dump(argv);
                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code);

                let mut mapping = HashMap::new();
                mapping.insert(String("a".into()), Int(123));

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Array(
                        vec![
                            Int(3),
                            String("foo".into()),
                            Array(vec![
                                String("bar".into()),
                                String("baz".into()),
                                Float(3.14.into()),
                            ]),
                            Mapping(mapping.clone()),
                        ]
                        .into(),
                    ),
                    String("snapshot_stack".into()),
                    Array(vec![
                        String("bar".into()),
                        String("baz".into()),
                        Float(3.14.into()),
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

                let registers = snapshot_registers(code);

                let mut mapping = HashMap::new();
                mapping.insert(String("a".into()), Int(123));

                let expected = vec![
                    Int(0),
                    Int(45),
                    Int(34),
                    Float(7.77.into()),
                    String("snuh".into()),
                    Array(vec![String("a string".into()), Int(3), Float(2.44.into())].into()),
                    String("snapshot_stack".into()),
                    Int(0),
                    Int(0),
                    String("snuh".into()),
                    String("a string".into()),
                    Int(3),
                    Float(2.44.into()),
                    Array(vec![String("a string".into()), Int(3), Float(2.44.into())].into()),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(3)].into()),
                    Int(1),
                    Int(-1),
                    Array(vec![Int(2), Int(3)].into()),
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(4), Int(4), Int(4)];

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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(790080), Int(0), Int(790080), Int(0)];

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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(192), Int(0), Int(192), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_sizeof {
            use lpc_rs_asm::instruction::Instruction::{SConst, Sizeof};
            use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType};
            use lpc_rs_function_support::function_prototype::FunctionPrototype;

            use super::*;
            use crate::test_support::test_config;

            #[test]
            fn stores_the_value_for_arrays() {
                let code = indoc! { r##"
                    int a = sizeof(({ 1, 2, 3 }));
                "##};

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
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
                let path = LpcPath::new_in_game("/my_file.c", "/", config.lib_dir());

                let mut functions = HashMap::new();
                functions.insert(
                    INIT_PROGRAM.to_string(),
                    ProgramFunction {
                        prototype: FunctionPrototype::new(
                            INIT_PROGRAM,
                            LpcType::Void,
                            Default::default(),
                            Default::default(),
                            None,
                            vec![],
                            vec![],
                        ),
                        num_locals: 2,
                        instructions: vec![
                            SConst(Register(1).as_register(), "Hello, world!".into()),
                            Sizeof(Register(1).as_register(), Register(2).as_register()),
                        ],
                        debug_spans: vec![None, None],
                        labels: Default::default(),
                    }
                    .into(),
                );

                let program = Program {
                    filename: path,
                    functions,
                    global_variables: Default::default(),
                    num_globals: 0,
                    num_init_registers: 2,
                    pragmas: Default::default(),
                    inherits: vec![],
                    inherit_names: Default::default(),
                };

                let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(Memory::default());

                let object_space = ObjectSpace::default();

                let _ctx = task
                    .initialize_program(program, config, object_space)
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

                        // Store a snapshot, so we can test this even though this stack
                        // frame would otherwise have been popped off into the aether.
                        debug("snapshot_stack");
                    }
                "##};

                let registers = snapshot_registers(code);

                let expected = vec![
                    Int(0),
                    Int(1),
                    Int(2),
                    Int(3),
                    Array(vec![Int(1), Int(2), Int(678)].into()),
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

                let (task, _) = run_prog(code);
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

                let (task, _) = run_prog(code);
                let registers = task.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(20), Int(0), Int(20), Int(20)];

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

            let mut task: Task<5> = Task::new(Memory::default());
            let (program, _, _) = compile_prog(code);
            let r = task.initialize_program(program, Config::default(), ObjectSpace::default());

            assert_eq!(r.unwrap_err().to_string(), "stack overflow");
        }

        #[test]
        fn errors_on_too_long_evaluation() {
            let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};

            let config = Config::default().with_max_task_instructions(Some(10));
            let (program, _, _) = compile_prog(code);
            let mut task: Task<5> = Task::new(Memory::default());
            let r = task.initialize_program(program, config, ObjectSpace::default());

            assert_eq!(
                r.unwrap_err().to_string(),
                "evaluation limit of `10` instructions has been reached."
            );
        }
    }
}
