use crate::{
    asm::{
        instruction::{Address, Instruction},
        register::Register,
    },
    errors::LpcError,
    interpreter::{
        efun::{EFUNS, EFUN_PROTOTYPES},
        lpc_ref::LpcRef,
        lpc_value::LpcValue,
        process::Process,
        program::Program,
        stack_frame::StackFrame,
    },
    semantic::function_symbol::FunctionSymbol,
    try_extract_value,
    util::config::Config,
    value_to_ref, LpcInt, Result,
};
use decorum::Total;
use refpool::{Pool, PoolRef};
use std::{cell::RefCell, collections::HashMap, fmt::Display, path::PathBuf, rc::Rc};
use crate::interpreter::function_type::{FunctionPtr, FunctionAddress, FunctionTarget, FunctionName, FunctionReceiver, LpcFunction};
use std::borrow::Cow;
use crate::codegen::codegen_walker::INIT_PROGRAM;

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

/// The initial size (in frames) of the call stack
const STACK_SIZE: usize = 2_000;

/// The initial size (in cells) of system memory
const MEMORY_SIZE: usize = 100_000;

/// A type to track where `catch` calls need to go if there is an error
#[derive(Debug, Clone)]
struct CatchPoint {
    /// The index of the stack frame that contains this `catch`
    frame_index: usize,

    /// The address to jump in the current function, if there is an error
    address: Address,

    /// The register to put the error in, within the above [`StackFrame`]
    register: Register,
}

/// An interpreter that executes instructions
///
/// # Examples
///
/// ```
/// use lpc_rs::interpreter::asm_interpreter::AsmInterpreter;
/// use lpc_rs::compiler::Compiler;
///
/// let prog = r#"int create() { dump("hello, world"); int b = 123; return b; }"#;
/// let compiler = Compiler::default();
/// // See also `compile_file` for compiling files from disk.
/// let program = compiler.compile_string("my_prog.c", prog).expect("Unable to compile.");
///
/// // Load the program and run it
/// let mut interpreter = AsmInterpreter::default();
/// interpreter.init_master(program);
/// ```
#[derive(Debug, Clone)]
pub struct AsmInterpreter {
    /// The configuration received from the running user
    pub config: Rc<Config>,

    /// The process to run
    pub process: Rc<Process>,

    /// Our object space
    pub processes: HashMap<String, Rc<Process>>,

    /// The call stack
    pub stack: Vec<StackFrame>,

    /// How many clones have been created so far?
    pub clone_count: usize,

    /// Our memory
    pub memory: Pool<RefCell<LpcValue>>,

    /// The most recently-popped stack frame. Used by `apply` to get return values.
    popped_frame: Option<StackFrame>,

    /// A spot to store a snapshot, used by the [`debug`]() efun
    pub snapshot: Option<Box<AsmInterpreter>>,

    /// How many instructions have run during the current [`Task`]?
    instruction_count: usize,

    /// stack of [`CatchPoint`]s
    catch_points: Vec<CatchPoint>,
}

/// Get a reference to the passed stack frame's registers
#[inline]
pub fn current_registers(stack: &[StackFrame]) -> Result<&Vec<LpcRef>> {
    match &stack.last() {
        Some(frame) => Ok(&frame.registers),
        None => Err(LpcError::new(
            "Trying to get the current registers with an empty stack.",
        )),
    }
}

/// Get a mutable reference to the passed stack frame's registers
#[inline]
fn current_registers_mut(frame: &mut StackFrame) -> Result<&mut Vec<LpcRef>> {
    Ok(&mut frame.registers)
    // if stack.is_empty() {
    //     return Err(LpcError::new(
    //         "Trying to get the current registers (mutable) with an empty stack.",
    //     ));
    // }
    //
    // Ok(&mut stack.last_mut().unwrap().registers)
}

// #[inline]
// fn current_registers_mut(stack: &mut Vec<StackFrame>) -> Result<&mut Vec<LpcRef>> {
//     if stack.is_empty() {
//         return Err(LpcError::new(
//             "Trying to get the current registers (mutable) with an empty stack.",
//         ));
//     }
//
//     Ok(&mut stack.last_mut().unwrap().registers)
// }
//
/// Return the current frame, if there is one
#[inline]
pub fn current_frame_mut(stack: &mut Vec<StackFrame>) -> Result<&mut StackFrame> {
    if let Some(x) = stack.last_mut() {
        return Ok(x);
    }

    Err(LpcError::new("Runtime Error: No current stack frame"))
}

/// A macro for pushing new frames while evaluating [`Instruction`]s.
macro_rules! try_push_frame {
    ( $frame:expr, $interpreter:expr ) => {
        // let max_stack = $interpreter.config.max_call_stack_size().unwrap_or(0);
        //
        // if max_stack > 0 && $interpreter.stack.len() >= max_stack {
        //     return Err($interpreter.runtime_error("Stack overflow"));
        // }

        $interpreter.stack.push($frame);

        // Do not replace the process here, because the borrow checker already knows we have a
        // reference to the current instruction.
    };
}

impl AsmInterpreter {
    /// Create a new [`AsmInterpreter`] with the passed [`Config`]
    pub fn new(config: Rc<Config>) -> Self {
        Self {
            config,
            ..Self::default()
        }
    }

    /// Load a program for evaluation
    /// `load_master` is intended for programs destined to be master objects
    ///
    /// # Arguments
    ///
    /// * `program` - The Program to load
    pub fn load_master(&mut self, program: Program) -> Rc<Process> {
        let r = self.insert_master(program);
        self.process = r.clone();
        r
    }

    /// Load a program for evaluation
    /// `load_master` is intended for programs destined to be master objects
    ///
    /// # Arguments
    ///
    /// * `program` - The Program to load. Assumed to already be wrapped in an [`Rc`]
    ///   from cloning from an existing process.
    pub fn load_clone(&mut self, program: Rc<Program>) -> Rc<Process> {
        let r = self.insert_clone(program);
        self.process = r.clone();
        r
    }

    /// Create a [`Process`] from a [`Program`], and add add it to the process table.
    /// If a new program with the same filename as an existing one is added,
    /// the new will overwrite the old in the table.
    /// Storage keys are the in-game filename
    pub fn insert_master(&mut self, program: Program) -> Rc<Process> {
        let process = Rc::new(Process::new(program));
        self.insert_process(process.clone());
        process
    }

    pub fn insert_clone(&mut self, program: Rc<Program>) -> Rc<Process> {
        let cnt = self.clone_count;
        self.clone_count += 1;
        let process = Rc::new(Process::new_clone(program, cnt));
        self.insert_process(process.clone());
        process
    }

    pub fn insert_process(&mut self, process: Rc<Process>) {
        let name = process.localized_filename(self.config.lib_dir());

        self.processes.insert(name, process);
    }

    /// Set up the stack frame for initializing the global vars, and calling `create`.
    /// No code is executed by this function - it merely sets up the stack frame.
    pub fn setup_program_globals_frame(&mut self) -> Result<()> {
        let process = self.process.clone();
        let sym = process.functions.get(INIT_PROGRAM).unwrap();
        let create = StackFrame::new(self.process.clone(), sym.clone());

        self.push_frame(create)?;

        Ok(())
    }

    /// Fully initialize the current [`Program`].
    /// This sets up all of the global variables and calls `create`.
    pub fn init(&mut self) -> Result<()> {
        self.setup_program_globals_frame()?;

        self.eval()
    }

    /// Convenience method to load and initialize a [`Program`]
    pub fn init_master(&mut self, program: Program) -> Result<Rc<Process>> {
        let r = self.load_master(program);
        self.init()?;

        Ok(r)
    }

    /// Convenience method to run a closure, while maintaining
    /// & resetting the current process and stack, and sharing the memory pool.
    pub fn with_clean_stack<F, T>(&mut self, closure: F) -> Result<T>
    where
        F: FnOnce(&mut AsmInterpreter) -> Result<T>,
    {
        let current_process = self.process.clone();
        let clean_stack = Vec::with_capacity(20);
        let current_stack = std::mem::replace(&mut self.stack, clean_stack);

        let result = closure(self);

        let _ = std::mem::replace(&mut self.stack, current_stack);
        let _ = std::mem::replace(&mut self.process, current_process);

        result
    }

    pub fn lookup_process<T>(&self, path: T) -> Result<&Rc<Process>>
    where
        T: AsRef<str>,
    {
        let s = path.as_ref();

        match self.processes.get(s) {
            Some(proc) => Ok(proc),
            None => {
                if !s.ends_with(".c") {
                    let mut owned = s.to_string();
                    owned.push_str(".c");
                    return self.lookup_process(owned);
                }

                Err(self.runtime_error(format!("Unable to find object `{}`", path.as_ref())))
            }
        }
    }

    pub fn increment_instruction_count(&mut self, amount: usize) -> Result<()> {
        self.instruction_count += amount;

        let max_instructions = self.config.max_task_instructions().unwrap_or(0);
        if max_instructions > 0 && self.instruction_count > max_instructions {
            return Err(self.runtime_error("Evaluation limit has been reached."));
        }

        Ok(())
    }

    /// Push a new stack frame onto the call stack
    pub fn push_frame(&mut self, frame: StackFrame) -> Result<()> {
        try_push_frame!(frame, self);
        self.process = self.stack.last().unwrap().process.clone();

        Ok(())
    }

    /// Pop the current stack frame off the call stack
    pub fn pop_frame(&mut self) -> Option<StackFrame> {
        let previous_frame = self.stack.pop();
        // println!("popped frame: {:?}", previous_frame);

        if !self.stack.is_empty() {
            self.process = self.stack.last().unwrap().process.clone();
        }

        previous_frame
    }

    /// Return the current frame, if there is one
    pub fn current_frame(&self) -> Result<&StackFrame> {
        self.stack.last().ok_or_else(|| self.runtime_error("No current stack frame"))
    }

    /// Get the in-game directory of the current process
    pub fn in_game_cwd(&self) -> Result<PathBuf> {
        match self.process.cwd().strip_prefix(self.config.lib_dir()) {
            // TODO: rewrite this to avoid converting this to an owned value
            // The problem is the self.process.cwd() call creates a value
            // owned by the current function.
            Ok(x) => Ok(x.to_path_buf()),
            Err(e) => Err(LpcError::new(e.to_string())),
        }
    }

    /// Resolve the passed index within the current stack frame's registers, down to an LpcRef
    /// # Arguments
    /// `index` - the register index to resolve
    ///
    /// # Panics
    /// Panics if the stack is empty, or if there is nothing in the registers at the requested index
    pub fn register_to_lpc_ref(&self, index: usize) -> LpcRef {
        self.current_frame().unwrap().resolve_lpc_ref(index)
    }

    /// Evaluate loaded instructions, starting from the current value of the PC
    fn eval(&mut self) -> Result<()> {
        let mut halted = false;

        self.instruction_count = 0;

        while !halted {
            halted = match self.eval_one_instruction() {
                Ok(x) => x,
                Err(e) => {
                    if !self.catch_points.is_empty() {
                        self.catch_error(e)?;
                        false
                    } else {
                        return Err(e);
                    }
                }
            };
        }

        Ok(())
    }

    /// Evaluate the instruction at the current value of the PC
    /// The boolean represents whether we are at the end of input (i.e. we should halt the machine)
    pub fn eval_one_instruction(&mut self) -> Result<bool> {
        self.increment_instruction_count(1)?;

        if self.stack.is_empty() {
            return Ok(true);
        }

        let mut frame = current_frame_mut(&mut self.stack)?;
        frame.inc_pc();
        let (instruction, registers) = frame.current_eval_context();
        // let mut registers = &mut frame.registers;

        let instruction = match instruction {
            Some(i) => i.clone(),
            None => return Ok(true),
        };
        let instruction = &instruction;

        // println!("evaling ({}) {}", self.process.filename, instruction);

        match instruction {
            Instruction::AConst(r, vec) => {
                // let registers = current_registers_mut(&mut frame)?;
                // let registers = &mut frame.registers;
                let vars = vec
                    .iter()
                    .map(|i| registers[i.index()].clone())
                    .collect::<Vec<_>>();
                let new_ref = value_to_ref!(LpcValue::from(vars), &self.memory);

                registers[r.index()] = new_ref;
            }
            Instruction::And(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x & y)?;
            }
            Instruction::Call {
                name,
                num_args,
                initial_arg,
            } => {
                let mut new_frame = if let Some(func) = self.process.functions.get(name) {
                    StackFrame::new(self.process.clone(), func.clone())
                } else if let Some(prototype) = EFUN_PROTOTYPES.get(name.as_str()) {
                    let sym = FunctionSymbol::new(name.clone(), prototype.num_args, 0);

                    StackFrame::new(self.process.clone(), Rc::new(sym))
                } else {
                    println!("proc {:#?}", self.process);
                    println!("functions {:#?}", self.process.functions);
                    let msg = format!("Call to unknown function `{}`", name);
                    return Err(self.runtime_error(msg));
                };

                // copy argument registers from old frame to new
                if *num_args > 0_usize {
                    let index = initial_arg.index();
                    // let current_frame = self.stack.last().unwrap();
                    new_frame.registers[1..=*num_args]
                        .clone_from_slice(&self.current_frame()?.registers[index..(index + num_args)]);
                }

                // println!("pushing frame in Call: {:?}", new_frame);
                try_push_frame!(new_frame, self);

                // if let Some(x) = self.process.functions.get(name) {
                //     frame.set_pc(x.address);
                // } else if let Some(efun) = EFUNS.get(name.as_str()) {
                if let Some(efun) = EFUNS.get(name.as_str()) {
                    // the efun is responsible for populating the return value in its own frame
                    efun(self)?;

                    if let Some(frame) = self.pop_frame() {
                        self.copy_call_result(&frame)?;
                        self.popped_frame = Some(frame);
                    }
                } else if !self.process.functions.contains_key(name) {
                    return Err(self.runtime_error(format!(
                        "Call to unknown function (that had a valid prototype?) `{}`",
                        name
                    )));
                }
            }
            Instruction::CallFp {
                location,
                num_args,
                initial_arg,
            } => {
                let func_ref = &frame.registers[location.index()];

                todo!();




                // let mut new_frame = if let Some(func) = self.process.functions.get(name) {
                //     StackFrame::new(self.process.clone(), func.clone(), self.process.pc())
                // } else if let Some(prototype) = EFUN_PROTOTYPES.get(name.as_str()) {
                //     let sym = FunctionSymbol {
                //         name: name.clone(),
                //         num_args: prototype.num_args,
                //         num_locals: 0,
                //         address: 0,
                //     };
                //
                //     StackFrame::new(self.process.clone(), Rc::new(sym), self.process.pc())
                // } else {
                //     println!("proc {:#?}", self.process);
                //     println!("functions {:#?}", self.process.functions);
                //     return Err(self.runtime_error(format!("Call to unknown function `{}`", name)));
                // };
                //
                // // copy argument registers from old frame to new
                // if *num_args > 0_usize {
                //     let index = initial_arg.index();
                //     let current_frame = self.stack.last().unwrap();
                //     new_frame.registers[1..=*num_args]
                //         .clone_from_slice(&current_frame.registers[index..(index + num_args)]);
                // }
                //
                // // println!("pushing frame in Call: {:?}", new_frame);
                // try_push_frame!(new_frame, self);
                //
                // if let Some(x) = self.process.functions.get(name) {
                //     self.process.set_pc(x.address);
                // } else if let Some(efun) = EFUNS.get(name.as_str()) {
                //     // the efun is responsible for populating the return value in its own frame
                //     efun(self)?;
                //
                //     if let Some(frame) = self.pop_frame() {
                //         self.copy_call_result(&frame)?;
                //         self.popped_frame = Some(frame);
                //     }
                // } else {
                //     return Err(self.runtime_error(format!(
                //         "Call to unknown function (that had a valid prototype?) `{}`",
                //         name
                //     )));
                // }
            }
            Instruction::CallOther {
                receiver,
                name,
                num_args,
                initial_arg,
            } => {
                // get receiver process and make it the current one
                let receiver_ref = &frame.registers[receiver.index()];

                // Figure out which function we're calling
                let name_ref = &frame.registers[name.index()];
                let pool_ref = if let LpcRef::String(r) = name_ref {
                    r
                } else {
                    let str = format!(
                        "Invalid name passed to `call_other`: {}",
                        name_ref
                    );
                    return Err(self.runtime_error(str));
                };
                let borrowed = pool_ref.borrow();
                let function_name = try_extract_value!(*borrowed, LpcValue::String);

                let num_args = *num_args;
                let initial_index = initial_arg.index();
                // let return_address = self.process.pc();

                let get_result = |value: LpcValue, interpreter: &mut AsmInterpreter| match value {
                    LpcValue::Object(receiver) => {
                        let args = interpreter.stack.last().unwrap().registers
                            [initial_index..(initial_index + num_args)]
                            .to_vec();
                        let closure = |inner: &mut AsmInterpreter| {
                            inner.call_other(receiver, function_name, args)
                        };

                        interpreter.with_clean_stack(closure)
                    }
                    _ => Ok(LpcRef::Int(0)),
                };

                let resolve_result = |receiver_ref, interpreter: &mut AsmInterpreter| {
                    let resolved =
                        interpreter.resolve_call_other_receiver(receiver_ref, function_name);

                    if let Some(pr) = resolved {
                        let value = LpcValue::from(pr);
                        Ok(get_result(value, interpreter).unwrap_or(LpcRef::Int(0)))
                    } else {
                        Err(interpreter.runtime_error("Unable to find the receiver."))
                    }
                };

                let result_ref = match &receiver_ref {
                    LpcRef::String(_) | LpcRef::Object(_) => resolve_result(&receiver_ref, self)?,
                    LpcRef::Array(r) => {
                        let b = r.borrow();
                        let array = try_extract_value!(*b, LpcValue::Array);

                        let array_value: LpcValue = array
                            .iter()
                            .map(|lpc_ref| resolve_result(lpc_ref, self).unwrap_or(LpcRef::Int(0)))
                            .collect::<Vec<_>>()
                            .into();
                        value_to_ref!(array_value, &self.memory)
                    }
                    LpcRef::Mapping(m) => {
                        let b = m.borrow();
                        let hashmap = try_extract_value!(*b, LpcValue::Mapping);

                        let with_results: LpcValue = hashmap
                            .iter()
                            .map(|(key_ref, value_ref)| {
                                (
                                    key_ref.clone(),
                                    resolve_result(value_ref, self).unwrap_or(LpcRef::Int(0)),
                                )
                            })
                            .collect::<HashMap<_, _>>()
                            .into();

                        value_to_ref!(with_results, &self.memory)
                    }
                    _ => {
                        return Err(self.runtime_error(format!(
                            "What are you trying to call `{}` on?",
                            function_name
                        )))
                    }
                };

                self.return_efun_result(result_ref)
            }
            Instruction::CatchEnd => {
                self.catch_points.pop();
            }
            Instruction::CatchStart(r, label) => {
                let address = match self.current_frame()?.lookup_label(label) {
                    Some(x) => *x,
                    None => {
                        return Err(
                            self.runtime_error(format!("Missing address for label `{}`", label))
                        )
                    }
                };

                let catch_point = CatchPoint {
                    frame_index: self.stack.len() - 1,
                    register: *r,
                    address,
                };

                self.catch_points.push(catch_point);
            }
            Instruction::EqEq(r1, r2, r3) => {
                let registers = current_registers_mut(&mut frame)?;
                let out = if registers[r1.index()] == registers[r2.index()] {
                    1
                } else {
                    0
                };

                let registers = current_registers_mut(&mut frame)?;
                registers[r3.index()] = LpcRef::Int(out);
            }
            Instruction::FConst(r, f) => {
                let registers = current_registers_mut(&mut frame)?;
                registers[r.index()] = LpcRef::Float(*f);
            }
            Instruction::FunctionPtrConst { location, target, applied_arguments } => {
                let address = match target {
                    FunctionTarget::Efun(func_name) => {
                        FunctionAddress::Efun(self.resolve_function_name(func_name)?.into_owned())
                    }
                    FunctionTarget::Local(func_name) => {
                        let s = self.resolve_function_name(func_name)?;
                        let sym = self.process.lookup_function(s);
                        if sym.is_none() {
                            return Err(self.runtime_error(format!("Unknown local target `{}`", s)));
                        }

                        FunctionAddress::Local(sym.unwrap().clone())
                    }
                    FunctionTarget::CallOther(func_name, func_receiver) => {
                        let proc = match func_receiver {
                            FunctionReceiver::Value(receiver_reg) => {
                                // let receiver_ref = self.register_to_lpc_ref(receiver_reg.index());
                                let receiver_ref = &frame.registers[receiver_reg.index()];
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
                            FunctionReceiver::Argument => todo!(),
                            FunctionReceiver::None => {
                                return Err(self.runtime_error("A None receiver for a CallOther target? Should be unreachable."));
                            }
                        };
                        let s = self.resolve_function_name(func_name)?;
                        let sym = self.process.lookup_function(s);
                        if sym.is_none() {
                            return Err(self.runtime_error(format!("Unknown local target `{}`", s)));
                        }

                        FunctionAddress::Remote(proc, sym.unwrap().clone())
                    }
                };

                let args: Vec<Option<LpcRef>> = applied_arguments.iter().map(|arg| {
                    match arg {
                        Some(register) => {
                            Some(frame.resolve_lpc_ref(register))
                        }
                        None => None,
                    }
                }).collect();

                let fp = FunctionPtr {
                    owner: Rc::new(Default::default()),
                    address,
                    args
                };

                let func = LpcFunction::FunctionPtr(fp);
                
                let new_ref = value_to_ref!(LpcValue::from(func), &self.memory);

                let registers = current_registers_mut(&mut frame)?;

                registers[location.index()] = new_ref;
            }
            Instruction::GLoad(r1, r2) => {
                // load from global r1, into local r2
                let global = self.process.globals[r1.index()].borrow().clone();
                let registers = current_registers_mut(&mut frame)?;
                registers[r2.index()] = global
            }
            Instruction::GStore(r1, r2) => {
                // store local r1 into global r2
                let registers = current_registers_mut(&mut frame)?;
                self.process.globals[r2.index()].replace(registers[r1.index()].clone());
            }
            Instruction::Gt(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_boolean_operation(n1, n2, n3, |x, y| x > y)?;
            }
            Instruction::Gte(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_boolean_operation(n1, n2, n3, |x, y| x >= y)?;
            }
            Instruction::IAdd(r1, r2, r3) => {
                let registers = current_registers_mut(&mut frame)?;
                match &registers[r1.index()] + &registers[r2.index()] {
                    Ok(result) => {
                        let out = value_to_ref!(result, self.memory);

                        registers[r3.index()] = out
                    }
                    Err(e) => {
                        return Err(e.with_span(self.current_frame()?.current_debug_span()));
                    }
                }
            }
            Instruction::IConst(r, i) => {
                let registers = current_registers_mut(&mut frame)?;
                registers[r.index()] = LpcRef::Int(*i);
            }
            Instruction::IConst0(r) => {
                let registers = current_registers_mut(&mut frame)?;
                registers[r.index()] = LpcRef::Int(0);
            }
            Instruction::IConst1(r) => {
                let registers = current_registers_mut(&mut frame)?;
                registers[r.index()] = LpcRef::Int(1);
            }
            Instruction::IDiv(r1, r2, r3) => {
                let registers = current_registers_mut(&mut frame)?;
                match &registers[r1.index()] / &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.current_frame()?.current_debug_span()));
                    }
                }
            }
            Instruction::IMod(r1, r2, r3) => {
                let registers = current_registers_mut(&mut frame)?;
                match &registers[r1.index()] % &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.current_frame()?.current_debug_span()));
                    }
                }
            }
            Instruction::IMul(r1, r2, r3) => {
                let registers = current_registers_mut(&mut frame)?;
                match &registers[r1.index()] * &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.current_frame()?.current_debug_span()));
                    }
                }
            }
            Instruction::ISub(r1, r2, r3) => {
                let registers = current_registers_mut(&mut frame)?;
                match &registers[r1.index()] - &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.current_frame()?.current_debug_span()));
                    }
                }
            }
            Instruction::Jmp(label) => {
                let address = self.lookup_address(label)?;
                self.current_frame()?.set_pc(address);
            }
            Instruction::Jnz(r1, label) => {
                let v = &current_registers_mut(&mut frame)?[r1.index()];

                if v != &LpcRef::Int(0) && v != &LpcRef::Float(Total::from(0.0)) {
                    let address = self.lookup_address(label)?;
                    self.current_frame()?.set_pc(address);
                }
            }
            Instruction::Jz(r1, label) => {
                let v = &current_registers_mut(&mut frame)?[r1.index()];

                if v == &LpcRef::Int(0) || v == &LpcRef::Float(Total::from(0.0)) {
                    let address = self.lookup_address(label)?;
                    self.current_frame()?.set_pc(address);
                }
            }
            Instruction::Load(r1, r2, r3) => {
                let container_ref = frame.resolve_lpc_ref(r1);

                match container_ref {
                    LpcRef::Array(vec_ref) => {
                        let value = vec_ref.borrow();
                        let vec = try_extract_value!(*value, LpcValue::Array);

                        let index = frame.resolve_lpc_ref(r2);
                        let registers = current_registers_mut(&mut frame)?;

                        if let LpcRef::Int(i) = index {
                            let idx = if i >= 0 { i } else { vec.len() as LpcInt + i };

                            if idx >= 0 {
                                if let Some(v) = vec.get(idx as usize) {
                                    registers[r3.index()] = v.clone();
                                } else {
                                    return Err(self.array_index_error(idx, vec.len()));
                                }
                            } else {
                                return Err(self.array_index_error(idx, vec.len()));
                            }
                        } else {
                            return Err(self.array_index_error(index, vec.len()));
                        }
                    }
                    LpcRef::String(string_ref) => {
                        let value = string_ref.borrow();
                        let string = try_extract_value!(*value, LpcValue::String);

                        let index = frame.resolve_lpc_ref(r2);
                        let registers = current_registers_mut(&mut frame)?;

                        if let LpcRef::Int(i) = index {
                            let idx = if i >= 0 {
                                i
                            } else {
                                string.len() as LpcInt + i
                            };

                            if idx >= 0 {
                                if let Some(v) = string.chars().nth(idx as usize) {
                                    registers[r3.index()] = LpcRef::Int(v as i64);
                                } else {
                                    registers[r3.index()] = LpcRef::Int(0);
                                }
                            } else {
                                registers[r3.index()] = LpcRef::Int(0);
                            }
                        } else {
                            return Err(self.runtime_error(format!(
                                "Attempting to access index {} in a string of length {}",
                                index,
                                string.len()
                            )));
                        }
                    }
                    LpcRef::Mapping(map_ref) => {
                        let index = frame.resolve_lpc_ref(r2);
                        let value = map_ref.borrow();
                        let map = try_extract_value!(*value, LpcValue::Mapping);

                        let var = if let Some(v) = map.get(&index) {
                            v.clone()
                        } else {
                            LpcRef::Int(0)
                        };

                        let registers = current_registers_mut(&mut frame)?;
                        registers[r3.index()] = var;
                    }
                    x => {
                        return Err(
                            self.runtime_error(format!("Invalid attempt to take index of `{}`", x))
                        );
                    }
                }
            }
            Instruction::Lt(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_boolean_operation(n1, n2, n3, |x, y| x < y)?;
            }
            Instruction::Lte(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_boolean_operation(n1, n2, n3, |x, y| x <= y)?;
            }
            Instruction::MapConst(r, map) => {
                let mut register_map = HashMap::new();
                for (key, value) in map {
                    let registers = current_registers_mut(&mut frame)?;
                    let r = registers[key.index()].clone();

                    register_map.insert(r, registers[value.index()].clone());
                }

                let new_ref = value_to_ref!(LpcValue::from(register_map), self.memory);
                let registers = current_registers_mut(&mut frame)?;

                registers[r.index()] = new_ref;
            }
            Instruction::MAdd(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x + y)?;
            }
            Instruction::MMul(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x * y)?;
            }
            Instruction::MSub(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x - y)?;
            }
            Instruction::Not(r1, r2) => {
                let registers = current_registers_mut(&mut frame)?;
                registers[r2.index()] = if matches!(registers[r1.index()], LpcRef::Int(0)) {
                    LpcRef::Int(1)
                } else {
                    LpcRef::Int(0)
                };
            }
            Instruction::Or(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x | y)?;
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

                let return_value = |value,
                                    memory: &mut Pool<RefCell<LpcValue>>,
                                    stack: &mut Vec<StackFrame>|
                 -> Result<()> {
                    let new_ref = value_to_ref!(value, memory);
                    let registers = &mut frame.registers;
                    registers[r4.index()] = new_ref;

                    Ok(())
                };

                let lpc_ref = frame.resolve_lpc_ref(r1);

                match lpc_ref {
                    LpcRef::Array(v_ref) => {
                        let value = v_ref.borrow();
                        let vec = try_extract_value!(*value, LpcValue::Array);

                        if vec.is_empty() {
                            return_value(
                                LpcValue::from(vec![]),
                                &mut self.memory,
                                &mut self.stack,
                            )?;
                        }

                        let index1 = frame.resolve_lpc_ref(r2);
                        let index2 = frame.resolve_lpc_ref(r3);

                        if let (LpcRef::Int(start), LpcRef::Int(end)) = (index1, index2) {
                            let (real_start, real_end) = resolve_range(start, end, vec.len());

                            if real_start <= real_end {
                                let slice = &vec[real_start..=real_end];
                                let mut new_vec = vec![LpcRef::Int(0); slice.len()];
                                new_vec.clone_from_slice(slice);
                                return_value(
                                    LpcValue::from(new_vec),
                                    &mut self.memory,
                                    &mut self.stack,
                                )?;
                            } else {
                                return_value(
                                    LpcValue::from(vec![]),
                                    &mut self.memory,
                                    &mut self.stack,
                                )?;
                            }
                        } else {
                            return Err(LpcError::new(
                                "Invalid code was generated for a Range instruction.",
                            )
                            .with_span(self.current_frame()?.current_debug_span()));
                        }
                    }
                    LpcRef::String(v_ref) => {
                        let value = v_ref.borrow();
                        let string = try_extract_value!(*value, LpcValue::String);

                        if string.is_empty() {
                            return_value(LpcValue::from(""), &mut self.memory, &mut self.stack)?;
                        }

                        let index1 = frame.resolve_lpc_ref(r2);
                        let index2 = frame.resolve_lpc_ref(r3);

                        if let (LpcRef::Int(start), LpcRef::Int(end)) = (index1, index2) {
                            let (real_start, real_end) = resolve_range(start, end, string.len());

                            if real_start <= real_end {
                                let len = real_end - real_start + 1;
                                let new_string: String =
                                    string.chars().skip(real_start).take(len).collect();
                                return_value(
                                    LpcValue::from(new_string),
                                    &mut self.memory,
                                    &mut self.stack,
                                )?;
                            } else {
                                return_value(
                                    LpcValue::from(""),
                                    &mut self.memory,
                                    &mut self.stack,
                                )?;
                            }
                        } else {
                            return Err(LpcError::new(
                                "Invalid code was generated for a Range instruction.",
                            )
                            .with_span(self.current_frame()?.current_debug_span()));
                        }
                    }
                    LpcRef::Float(_) | LpcRef::Int(_) | LpcRef::Mapping(_) | LpcRef::Object(_) | LpcRef::Function(_) => {
                        return Err(LpcError::new(
                            "Range's receiver isn't actually an array or string?",
                        )
                        .with_span(self.current_frame()?.current_debug_span()));
                    }
                }
            }
            Instruction::RegCopy(r1, r2) => {
                let registers = current_registers_mut(&mut frame)?;
                registers[r2.index()] = registers[r1.index()].clone()
            }
            Instruction::Ret => {
                if let Some(frame) = self.pop_frame() {
                    self.copy_call_result(&frame)?;

                    // if !self.stack.is_empty()
                    //     && Rc::ptr_eq(&frame.process, &self.stack.last().unwrap().process)
                    // {
                    //     self.current_frame()?.set_pc(frame.return_address);
                    // }

                    self.popped_frame = Some(frame);
                }

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
            }
            Instruction::Shl(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x << y)?;
            }
            Instruction::Shr(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x >> y)?;
            }
            Instruction::Store(r1, r2, r3) => {
                // r2[r3] = r1;

                let mut container = frame.resolve_lpc_ref(r2);
                let index = frame.resolve_lpc_ref(r3);
                let array_idx = if let LpcRef::Int(i) = index { i } else { 0 };

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
                            vec[idx as usize] = current_registers(&self.stack)?[r1.index()].clone();
                        } else {
                            return Err(self.array_index_error(idx, len));
                        }
                    }
                    LpcRef::Mapping(ref mut map_ref) => {
                        let mut r = map_ref.borrow_mut();
                        let map = match *r {
                            LpcValue::Mapping(ref mut m) => m,
                            _ => return Err(self.runtime_error(
                                "LpcRef with a non-Mapping reference as its value. This indicates a bug in the interpreter.")
                            )
                        };

                        map.insert(index, current_registers(&self.stack)?[r1.index()].clone());
                    }
                    x => {
                        return Err(
                            self.runtime_error(format!("Invalid attempt to take index of `{}`", x))
                        )
                    }
                }
            }
            Instruction::SConst(r, s) => {
                let registers = current_registers_mut(&mut frame)?;
                let new_ref = value_to_ref!(LpcValue::from(s), self.memory);

                registers[r.index()] = new_ref;
            }
            Instruction::Xor(r1, r2, r3) => {
                let (n1, n2, n3) = (*r1, *r2, *r3);
                self.binary_operation(n1, n2, n3, |x, y| x ^ y)?;
            }
        }

        Ok(false)
    }

    /// Set the state to handle a caught error.
    /// Panics if there aren't actually any catch points.
    fn catch_error(&mut self, error: LpcError) -> Result<()> {
        let catch_point = self.catch_points.last().unwrap();
        let result_index = catch_point.register.index();
        let frame_index = catch_point.frame_index;
        let new_pc = catch_point.address;

        // clear away stack frames that won't be executed any further, which lie between the
        // error and the catch point's stack frame.
        // Does nothing if you're already in the correct stack frame, or one away.
        self.stack.truncate(frame_index + 2);

        // If these aren't equal, we're already in the correct stack frame.
        if self.stack.len() == frame_index + 2 {
            // Pop the final frame via pop_frame(), to keep other state changes to a single
            // code path, (e.g. changing the current process)
            if let Some(frame) = self.pop_frame() {
                self.popped_frame = Some(frame);
            }
        }

        if self.stack.is_empty() {
            return Err(self.runtime_error("Stack is empty after popping to catch point?"));
        }

        // set up the catch point's return value
        let value = LpcValue::from(error.to_string());
        let lpc_ref = value_to_ref!(value, self.memory);
        self.stack.last_mut().unwrap().registers[result_index] = lpc_ref;

        // jump to the corresponding catchend instruction
        self.current_frame()?.set_pc(new_pc);

        Ok(())
    }

    fn resolve_call_other_receiver(
        &self,
        receiver_ref: &LpcRef,
        name: &str,
    ) -> Option<Rc<Process>> {
        let process = match receiver_ref {
            LpcRef::String(s) => {
                let r = s.borrow();
                let str = if let LpcValue::String(ref s) = *r {
                    s
                } else {
                    return None;
                };

                match self.lookup_process(str) {
                    Ok(proc) => proc.clone(),
                    Err(_) => return None,
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
        if process.functions.contains_key(name) {
            Some(process)
        } else {
            None
        }
    }

    fn resolve_function_name<'a>(&'a self, name: &'a FunctionName) -> Result<Cow<'a, str>> {
        match name {
            FunctionName::Var(reg) => {
                let name_ref = self.register_to_lpc_ref(reg.index());

                if let LpcRef::String(s) = name_ref {
                    let b = s.borrow();
                    let str = try_extract_value!(*b, LpcValue::String);
                    Ok(str.clone().into())
                } else {
                    Err(self.runtime_error(format!("Found function var that didn't resolve to a string?")))
                }
            }
            FunctionName::Literal(s) => Ok(s.into())
        }
    }

    /// Call a function in the specified receiver. This assumes the stack has been cleared ahead of time.
    fn call_other(
        &mut self,
        receiver: Rc<Process>,
        name: &str,
        // return_address: usize,
        mut args: Vec<LpcRef>,
    ) -> Result<LpcRef> {
        self.process = receiver;

        let sym = if let Some(fs) = self.process.functions.get(name) {
            fs
        } else {
            // if no function by that name, just return 0 immediately
            return Ok(LpcRef::Int(0));
        };

        let num_args = args.len();

        // Because call_other args aren't arity-checked at compile time, there might
        // be more passed than expected, so we might need to reserve more space
        let mut new_frame = StackFrame::with_minimum_arg_capacity(
            self.process.clone(),
            sym.clone(),
            // return_address,
            num_args + 1, // +1 for r0
        );

        // put the arguments into the new correct place in the new frame.
        if num_args > 0_usize {
            new_frame.registers[1..=num_args].swap_with_slice(&mut args);
        }

        // println!("pushing frame in CallOther: {:?}", new_frame);
        try_push_frame!(new_frame, self);
        
        // self.current_frame()?.set_pc(sym.address);

        self.eval()?;

        if let Some(frame) = &self.popped_frame {
            return Ok(frame.registers[0].clone());
        }

        Err(self.runtime_error("`call_other` was called but never pushed a frame?"))
    }

    fn binary_operation<F>(
        &mut self,
        r1: Register,
        r2: Register,
        r3: Register,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &LpcRef) -> Result<LpcValue>,
    {
        let ref1 = &self.register_to_lpc_ref(r1.index());
        let ref2 = &self.register_to_lpc_ref(r2.index());

        match operation(ref1, ref2) {
            Ok(result) => {
                let var = value_to_ref!(result, self.memory);

                let registers = &mut current_frame_mut(&mut self.stack)?.registers;
                registers[r3.index()] = var
            }
            Err(e) => {
                return Err(e.with_span(self.current_frame()?.current_debug_span()));
            }
        }

        Ok(())
    }

    /// Binary operations that return a boolean value
    fn binary_boolean_operation<F>(
        &mut self,
        r1: Register,
        r2: Register,
        r3: Register,
        operation: F,
    ) -> Result<()>
    where
        F: Fn(&LpcRef, &LpcRef) -> bool,
    {
        let ref1 = &self.register_to_lpc_ref(r1.index());
        let ref2 = &self.register_to_lpc_ref(r2.index());

        let out = if operation(ref1, ref2) { 1 } else { 0 };

        let registers = &mut current_frame_mut(&mut self.stack)?.registers;
        registers[r3.index()] = LpcRef::Int(out);

        Ok(())
    }

    /// Convenience helper for [`Efun`]s to get their result into the correct location.
    pub fn return_efun_result(&mut self, result: LpcRef) {
        self.stack.last_mut().unwrap().registers[0] = result;
    }

    /// Convenience helper to copy a return value from a given stack frame, back to the current one.
    fn copy_call_result(&mut self, from: &StackFrame) -> Result<()> {
        if !self.stack.is_empty() {
            current_frame_mut(&mut self.stack)?.registers[0] = from.registers[0].clone();
        }

        Ok(())
    }

    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        let span = match self.current_frame() {
            Ok(frame) => frame.current_debug_span(),
            Err(e) => None
        };
        LpcError::new(format!("Runtime Error: {}", msg.as_ref()))
            .with_span(span)
    }

    fn array_index_error<T: Display>(&self, index: T, length: usize) -> LpcError {
        self.runtime_error(format!(
            "Attempting to access index {} in an array of length {}",
            index, length
        ))
    }

    /// Call the specified function in the specified object.
    /// Note: `args` are *not* type-checked.
    ///
    /// # Arguments
    /// `object` - The object containing the function to call
    /// `func` - The name of the function to call
    /// `args` - The arguments being applied to
    pub fn apply<T>(&mut self, object: Rc<Process>, func: T, args: &[LpcRef]) -> Result<LpcRef>
    where
        T: AsRef<str>,
    {
        let f = match object.functions.get(func.as_ref()) {
            Some(sym) => sym,
            None => {
                return Err(self.runtime_error(format!(
                    "Applied function `{}` not found in `{}`",
                    func.as_ref(),
                    object.filename
                )));
            }
        };

        let sym = f.clone();

        // use a pristine stack for the apply
        let current_process = std::mem::replace(&mut self.process, object);
        let clean_stack = Vec::with_capacity(20);
        let current_stack = std::mem::replace(&mut self.stack, clean_stack);

        let mut frame =
            StackFrame::with_minimum_arg_capacity(self.process.clone(), sym, args.len());
        frame.registers[1..=args.len()].clone_from_slice(args);

        self.push_frame(frame)?;

        let result = self.eval();

        // Clean up
        let _ = std::mem::replace(&mut self.stack, current_stack);
        let _ = std::mem::replace(&mut self.process, current_process);

        if let Err(e) = result {
            return Err(e);
        }

        let return_val = match self.popped_frame {
            Some(ref mut frame) => std::mem::replace(&mut frame.registers[0], LpcRef::Int(0)),
            None => {
                return Err(self.runtime_error(format!(
                    "Expected a stack frame after apply of `{}`, but did not find one",
                    func.as_ref()
                )));
            }
        };

        Ok(return_val)
    }

    fn lookup_address(&self, label: &str) -> Result<Address> {
        match self.current_frame()?.lookup_label(label) {
            Some(a) => Ok(*a),
            None => Err(self.runtime_error(format!("Unable to find address for {}", label))),
        }
    }
}

impl Default for AsmInterpreter {
    fn default() -> Self {
        let programs = HashMap::with_capacity(OBJECT_SPACE_SIZE);
        let program = Rc::new(Process::new(Program::default()));

        Self {
            config: Rc::new(Config::default()),
            process: program,
            processes: programs,
            clone_count: 0,
            stack: Vec::with_capacity(STACK_SIZE),
            memory: Pool::new(MEMORY_SIZE),
            popped_frame: None,
            snapshot: None,
            instruction_count: 0,
            catch_points: Vec::new()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{compiler::Compiler, extract_value, LpcFloat};
    use indoc::indoc;
    use std::hash::{Hash, Hasher};

    /// TODO: share this
    fn compile_prog(code: &str) -> Program {
        let compiler = Compiler::default();
        compiler
            .compile_string("~/my_file.c", code)
            .expect("Failed to compile.")
    }

    fn run_prog(code: &str) -> AsmInterpreter {
        let mut interpreter = AsmInterpreter::default();

        let program = compile_prog(code);

        interpreter.init_master(program).expect("init failed?");

        interpreter
    }

    /// A type to make it easier to set up test expectations for register contents
    #[derive(Debug, PartialEq, Eq)]
    enum BareVal {
        String(String),
        Int(LpcInt),
        Float(LpcFloat),
        Array(Box<Vec<BareVal>>),
        Mapping(HashMap<BareVal, BareVal>),
        Object(HashMap<BareVal, BareVal>),
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
                    BareVal::Array(Box::new(array))
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
                LpcRef::Object(_x) => {
                    todo!()
                    // let o = extract_value!(&*x, LpcValue::Object);
                }
            }
        }
    }

    impl PartialEq<LpcRef> for BareVal {
        fn eq(&self, other: &LpcRef) -> bool {
            &BareVal::from(other) == self
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
            }
        }
    }

    mod test_instructions {
        use super::*;

        mod test_aconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed *a = ({ 12, 4.3, "hello", ({ 1, 2, 3 }) });
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(12),
                    BareVal::Float(LpcFloat::from(4.3)),
                    BareVal::String("hello".into()),
                    BareVal::Int(1),
                    BareVal::Int(2),
                    BareVal::Int(3),
                    BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
                    BareVal::Array(
                        vec![
                            BareVal::Int(12),
                            BareVal::Float(LpcFloat::from(4.3)),
                            BareVal::String("hello".into()),
                            BareVal::Array(
                                vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into(),
                            ),
                        ]
                        .into(),
                    ),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_and {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 & 27;
                    mixed b = 0 & a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(11), Int(0), Int(11), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_andand {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 && 333;
                    mixed b = 0;
                    mixed c = b && a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

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

        mod test_call {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = tacos();
                    int tacos() { return 666; }
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(666), BareVal::Int(666)];

                assert_eq!(&expected, &registers);
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(666), BareVal::Int(666), BareVal::Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_catch {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::{Int, String};

            #[test]
            fn stores_the_error_string() {
                let code = indoc! { r##"
                    void create() {
                        int j = 0;
                        catch(10 / j);

                        debug("in_memory_snapshot");
                    }
                "##};

                let interpreter = run_prog(code);
                let stack = interpreter.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    Int(0),
                    Int(0),
                    String("Runtime Error: Division by zero".into()),
                    Int(10),
                    Int(0),
                    String("in_memory_snapshot".into()),
                    Int(0),
                ];

                assert_eq!(&expected, registers);
            }

            #[test]
            fn stores_zero_when_no_error() {
                let code = indoc! { r##"
                    void create() {
                        int j = 5;
                        catch(10 / j);

                        debug("in_memory_snapshot");
                    }
                "##};

                let interpreter = run_prog(code);
                let stack = interpreter.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    Int(0),
                    Int(5),
                    Int(0),
                    Int(10),
                    Int(2),
                    String("in_memory_snapshot".into()),
                    Int(0),
                ];

                assert_eq!(&expected, registers);
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

                let interpreter = run_prog(code);

                assert!(interpreter.catch_points.is_empty());
            }
        }

        mod test_eq_eq {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 2 == 2;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(2),
                    BareVal::Int(2),
                    BareVal::Int(1),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_fconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 3.14;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(0), BareVal::Float(3.14.into())];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Float(3.14.into()),
                    BareVal::Float(3.14.into()),
                    BareVal::Float(1.1.into()),
                    BareVal::Float(4.24.into()),
                ];

                assert_eq!(&expected, &registers);

                let global_registers = interpreter
                    .process
                    .globals
                    .iter()
                    .map(|global| (*global.borrow()).clone())
                    .collect::<Vec<_>>();

                let global_expected = vec![
                    BareVal::Int(0), // "wasted" global r0
                    BareVal::Float(3.14.into()),
                    BareVal::Float(4.24.into()),
                ];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(0), BareVal::Float(3.14.into())];

                assert_eq!(&expected, &registers);

                let global_registers = interpreter
                    .process
                    .globals
                    .iter()
                    .map(|global| (*global.borrow()).clone())
                    .collect::<Vec<_>>();

                let global_expected = vec![
                    BareVal::Int(0), // "wasted" global r0
                    BareVal::Float(3.14.into()),
                ];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1200),
                    BareVal::Int(1199),
                    BareVal::Int(1),
                    BareVal::Int(1199),
                    BareVal::Int(1200),
                    BareVal::Int(0),
                    BareVal::Int(1200),
                    BareVal::Int(1200),
                    BareVal::Int(0),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1200),
                    BareVal::Int(1199),
                    BareVal::Int(1),
                    BareVal::Int(1199),
                    BareVal::Int(1200),
                    BareVal::Int(0),
                    BareVal::Int(1200),
                    BareVal::Int(1200),
                    BareVal::Int(1),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_iadd {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 + 34;
                    mixed r = 12 + -4;
                    mixed s = q + r;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    // the constant expressions are folded at parse time
                    BareVal::Int(50),
                    BareVal::Int(8),
                    BareVal::Int(50),
                    BareVal::Int(8),
                    BareVal::Int(58),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(0), BareVal::Int(666)];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(0), BareVal::Int(0)];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(0), BareVal::Int(1)];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    // the constant expressions are folded at parse time
                    BareVal::Int(8),
                    BareVal::Int(-3),
                    BareVal::Int(8),
                    BareVal::Int(-3),
                    BareVal::Int(-2),
                ];

                assert_eq!(&expected, &registers);
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    // the constant expressions are folded at parse time
                    BareVal::Int(2),
                    BareVal::Int(5),
                    BareVal::Int(2),
                    BareVal::Int(5),
                    BareVal::Int(2),
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

                let mut interpreter = AsmInterpreter::default();

                let program = compile_prog(code);

                let r = interpreter.init_master(program);

                assert_eq!(
                    r.unwrap_err().to_string(),
                    "Runtime Error: Division by zero"
                )
            }
        }

        mod test_imul {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 * 2;
                    mixed r = 12 * -4;
                    mixed s = q * r;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(32),
                    BareVal::Int(-48),
                    BareVal::Int(32),
                    BareVal::Int(-48),
                    BareVal::Int(-1536),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_isub {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed q = 16 - 2;
                    mixed r = 12 - -4;
                    mixed s = q - r;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(14),
                    BareVal::Int(16),
                    BareVal::Int(14),
                    BareVal::Int(16),
                    BareVal::Int(-2),
                ];

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
                        debug("in_memory_snapshot");
                    }
                "##};

                let interpreter = run_prog(code);
                let stack = interpreter.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(69),
                    BareVal::Int(12),
                    BareVal::Int(10),
                    BareVal::Int(1),
                    BareVal::Int(69),
                    BareVal::Int(0),
                    BareVal::String("in_memory_snapshot".into()),
                    BareVal::Int(0),
                ];

                assert_eq!(&expected, registers);
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
                        debug("in_memory_snapshot");
                    }
                "##};

                let interpreter = run_prog(code);
                let stack = interpreter.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(8),
                    BareVal::Int(1),
                    BareVal::Int(8),
                    BareVal::Int(8),
                    BareVal::Int(0),
                    BareVal::String("in_memory_snapshot".into()),
                    BareVal::Int(0),
                ];

                assert_eq!(&expected, registers);
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(12),
                    BareVal::Int(1000),
                    BareVal::Int(12),
                    BareVal::Int(12),
                    BareVal::Int(0),
                    BareVal::Int(0),
                    BareVal::Int(1000),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1),
                    BareVal::Int(2),
                    BareVal::Int(3),
                    BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
                    BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
                    BareVal::Int(1),
                    BareVal::Int(2),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1200),
                    BareVal::Int(1199),
                    BareVal::Int(0),
                    BareVal::Int(1199),
                    BareVal::Int(1200),
                    BareVal::Int(1),
                    BareVal::Int(1200),
                    BareVal::Int(1200),
                    BareVal::Int(0),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1200),
                    BareVal::Int(1199),
                    BareVal::Int(0),
                    BareVal::Int(1199),
                    BareVal::Int(1200),
                    BareVal::Int(1),
                    BareVal::Int(1200),
                    BareVal::Int(1200),
                    BareVal::Int(1),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let mut hashmap = HashMap::new();
                hashmap.insert(BareVal::String("asdf".into()), BareVal::Int(123));
                hashmap.insert(BareVal::Int(456), BareVal::Float(3.14.into()));

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(456),
                    BareVal::Float(3.14.into()),
                    BareVal::String("asdf".into()),
                    BareVal::Int(123),
                    BareVal::Mapping(hashmap),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::String("abc".into()),
                    BareVal::Int(123),
                    BareVal::String("abc".into()),
                    BareVal::Int(123),
                    BareVal::String("abc123".into()),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::String("abc".into()),
                    BareVal::Int(4),
                    BareVal::String("abc".into()),
                    BareVal::Int(4),
                    BareVal::String("abcabcabcabc".into()),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1),
                    BareVal::Int(1),
                    BareVal::Int(2),
                    BareVal::Int(3),
                    BareVal::Array(
                        vec![
                            BareVal::Int(1),
                            BareVal::Int(1),
                            BareVal::Int(2),
                            BareVal::Int(3),
                        ]
                        .into(),
                    ),
                    BareVal::Array(
                        vec![
                            BareVal::Int(1),
                            BareVal::Int(1),
                            BareVal::Int(2),
                            BareVal::Int(3),
                        ]
                        .into(),
                    ),
                    BareVal::Int(1),
                    BareVal::Array(vec![BareVal::Int(1)].into()),
                    BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)].into()),
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
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(2),
                    BareVal::Int(0),
                    BareVal::Int(4),
                    BareVal::Int(0),
                    BareVal::Int(1),
                ];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_or {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 | 27;
                    mixed b = 0 | a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(31), Int(0), Int(31), Int(31)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_oror {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 123 || 333;
                    mixed b = 0;
                    mixed c = b || a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

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

        mod test_range {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = ({ 1, 2, 3 })[1..];
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1),
                    BareVal::Int(2),
                    BareVal::Int(3),
                    BareVal::Array(vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(3)].into()),
                    BareVal::Int(1),
                    BareVal::Int(-1),
                    BareVal::Array(vec![BareVal::Int(2), BareVal::Int(3)].into()),
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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(4),
                    BareVal::Int(4),
                    BareVal::Int(4),
                ];

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

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![
                    BareVal::Int(666), // return value from create()
                    BareVal::Int(666), // The copy of the call return value into its own register
                ];

                assert_eq!(&expected, &registers);
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
                        debug("in_memory_snapshot");
                    }
                "##};

                let interpreter = run_prog(code);
                let stack = interpreter.snapshot.unwrap().stack;

                // The top of the stack in the snapshot is the object initialization frame,
                // which is not what we care about here, so we get the second-to-top frame instead.
                let registers = &stack[stack.len() - 2].registers;

                let expected = vec![
                    BareVal::Int(0),
                    BareVal::Int(1),
                    BareVal::Int(2),
                    BareVal::Int(3),
                    BareVal::Array(
                        vec![BareVal::Int(1), BareVal::Int(2), BareVal::Int(678)].into(),
                    ),
                    BareVal::Int(678),
                    BareVal::Int(2),
                    BareVal::String("in_memory_snapshot".into()),
                    BareVal::Int(0),
                ];

                assert_eq!(&expected, registers);
            }
        }

        mod test_sconst {
            use super::*;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    string foo = "lolwut";
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![BareVal::Int(0), BareVal::String("lolwut".into())];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_xor {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 15 ^ 27;
                    mixed b = 0 ^ a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(20), Int(0), Int(20), Int(20)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_shl {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 12345 << 6;
                    mixed b = 0 << a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(790080), Int(0), Int(790080), Int(0)];

                assert_eq!(&expected, &registers);
            }
        }

        mod test_shr {
            use super::*;
            use crate::interpreter::asm_interpreter::tests::BareVal::Int;

            #[test]
            fn stores_the_value() {
                let code = indoc! { r##"
                    mixed a = 12345 >> 6;
                    mixed b = 0 >> a;
                "##};

                let interpreter = run_prog(code);
                let registers = interpreter.popped_frame.unwrap().registers;

                let expected = vec![Int(0), Int(192), Int(0), Int(192), Int(0)];

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

            let config = Config::default().with_max_call_stack_size(Some(10));
            let mut interpreter = AsmInterpreter::new(config.into());
            let program = compile_prog(code);
            let r = interpreter.init_master(program);

            assert_eq!(r.unwrap_err().to_string(), "Runtime Error: Stack overflow");
        }

        #[test]
        fn errors_on_too_long_evaluation() {
            let code = indoc! { r##"
                void create() {
                    while(1) {}
                }
            "##};

            let config = Config::default().with_max_task_instructions(Some(10));
            let mut interpreter = AsmInterpreter::new(config.into());
            let program = compile_prog(code);
            let r = interpreter.init_master(program);

            assert_eq!(
                r.unwrap_err().to_string(),
                "Runtime Error: Evaluation limit has been reached."
            );
        }
    }
}
