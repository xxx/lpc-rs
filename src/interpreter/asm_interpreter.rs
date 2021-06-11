use crate::{
    asm::{instruction::Instruction, register::Register},
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
    try_extract_value, value_to_ref, LpcInt, Result,
};
use decorum::Total;
use refpool::{Pool, PoolRef};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

/// The initial size (in objects) of the object space
const OBJECT_SPACE_SIZE: usize = 100_000;

/// The initial size (in frames) of the call stack
const STACK_SIZE: usize = 2_000;

/// The initial size (in cells) of system memory
const MEMORY_SIZE: usize = 100_000;

/// An interpreter that executes instructions
///
/// # Examples
///
/// ```
/// use lpc_rs::interpreter::asm_interpreter::AsmInterpreter;
/// use lpc_rs::compiler::compile_string;
///
/// let prog = r#"int create() { dump("hello, world"); int b = 123; return b; }"#;
/// // See also `compile_file` for compiling files from disk.
/// let program = compile_string("my_prog.c", prog).expect("Unable to compile.");
///
/// // Load the program and run it
/// let mut interpreter = AsmInterpreter::new(program);
/// interpreter.init();
/// ```
#[derive(Debug)]
pub struct AsmInterpreter {
    /// The process to run
    pub process: Rc<Process>,

    /// Our object space
    pub processes: HashMap<String, Rc<Process>>,

    /// The call stack
    pub stack: Vec<StackFrame>,

    pub clone_count: usize,

    /// Our memory
    pub memory: Pool<RefCell<LpcValue>>,
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
fn current_registers_mut(stack: &mut Vec<StackFrame>) -> Result<&mut Vec<LpcRef>> {
    if stack.is_empty() {
        return Err(LpcError::new(
            "Trying to get the current registers (mutable) with an empty stack.",
        ));
    }

    Ok(&mut stack.last_mut().unwrap().registers)
}

impl AsmInterpreter {
    /// Create a new [`AsmInterpreter`]
    pub fn new(program: Program) -> Self {
        let mut s = Self::default();
        s.load(program);
        s
    }

    /// Load a program for evaluation
    ///
    /// # Arguments
    ///
    /// * `program` - The Program to load
    pub fn load(&mut self, program: Program) -> Rc<Process> {
        let r = Rc::new(Process::new(program));
        self.processes.insert(r.filename.clone(), r.clone());
        self.process = r.clone();
        r
    }

    /// Set up the stack frame for initializing the global vars, and calling `create`.
    /// No code is executed by this function - it merely sets up the stack frame.
    pub fn init_stack(&mut self) {
        let sym = FunctionSymbol {
            name: "_global-var-init".to_string(),
            num_args: 0,
            num_locals: self.process.num_globals,
            address: 0,
        };
        let create = StackFrame::new(self.process.clone(), sym, 0);
        self.push_frame(create);
        self.process.set_pc(0);
    }

    /// Fully initialize the current [`Program`].
    /// This sets up all of the global variables and calls `create`.
    pub fn init(&mut self) -> Result<()> {
        self.init_stack();

        self.eval()
    }

    /// Convenience method to load and initialize a [`Program`]
    pub fn init_program(&mut self, program: Program) -> Result<Rc<Process>> {
        let r = self.load(program);
        self.init()?;

        Ok(r)
    }

    fn lookup_process<T>(&self, path: T) -> Result<&Rc<Process>>
    where
        T: AsRef<str>,
    {
        match self.processes.get(path.as_ref()) {
            Some(proc) => Ok(proc),
            None => Err(
                LpcError::new(format!("Unable to find object `{}`", path.as_ref()))
                    .with_span(self.process.current_debug_span()),
            ),
        }
    }

    /// Push a new stack frame onto the call stack
    fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame);
        self.process = self.stack.last().unwrap().process.clone();
    }

    /// Pop the current stack frame off the stack
    fn pop_frame(&mut self) -> Option<StackFrame> {
        let previous_frame = self.stack.pop();

        if !self.stack.is_empty() {
            self.process = self.stack.last().unwrap().process.clone();
        }

        previous_frame
    }

    /// Resolve the passed index within the current stack frame's registers, down to an LpcRef
    /// # Arguments
    /// `index` - the register index to resolve
    ///
    /// # Panics
    /// Panics if the stack is empty, or if there is nothing in the registers at the requested index
    pub fn register_to_lpc_ref(&self, index: usize) -> LpcRef {
        let len = self.stack.len();
        let registers = &self.stack[len - 1].registers;

        registers.get(index).unwrap().clone()
    }

    fn eval(&mut self) -> Result<()> {
        let mut halted = false;

        while !halted {
            halted = self.eval_one_instruction()?;
        }

        Ok(())
    }

    /// Evaluate loaded instructions, starting from the current value of the PC
    /// The boolean represents whether we are at the end of input (i.e. we should halt the machine)
    pub fn eval_one_instruction(&mut self) -> Result<bool> {
        let instruction = match self.process.instruction() {
            Some(i) => i,
            None => return Ok(true),
        };

        self.process.inc_pc();

        match instruction {
            Instruction::AConst(r, vec) => {
                let registers = current_registers_mut(&mut self.stack)?;
                let vars = vec
                    .iter()
                    .map(|i| registers[i.index()].clone())
                    .collect::<Vec<_>>();
                let new_ref = value_to_ref!(LpcValue::from(vars), &self.memory);

                registers[r.index()] = new_ref;
            }
            Instruction::ARange(r1, r2, r3, r4) => {
                // r4 = r1[r2..r3]
                let return_array = |arr,
                                    memory: &mut Pool<RefCell<LpcValue>>,
                                    stack: &mut Vec<StackFrame>|
                 -> Result<()> {
                    let new_ref = value_to_ref!(LpcValue::from(arr), memory);
                    let registers = current_registers_mut(stack)?;
                    registers[r4.index()] = new_ref;

                    Ok(())
                };

                let lpc_ref = self.register_to_lpc_ref(r1.index());

                if let LpcRef::Array(v_ref) = lpc_ref {
                    let value = v_ref.borrow();
                    let vec = try_extract_value!(*value, LpcValue::Array);

                    if vec.is_empty() {
                        return_array(vec![], &mut self.memory, &mut self.stack)?;
                    }

                    let index1 = self.register_to_lpc_ref(r2.index());
                    let index2 = self.register_to_lpc_ref(r3.index());

                    if let (LpcRef::Int(start), LpcRef::Int(end)) = (index1, index2) {
                        let to_idx = |i: LpcInt| {
                            // We handle the potential overflow just below.
                            if i >= 0 {
                                i as usize
                            } else {
                                (vec.len() as LpcInt + i) as usize
                            }
                        };
                        let real_start = to_idx(start);
                        let mut real_end = to_idx(end);

                        if real_end >= vec.len() {
                            real_end = vec.len() - 1;
                        }

                        if real_start <= real_end {
                            let slice = &vec[real_start..=real_end];
                            let mut new_vec = vec![LpcRef::Int(0); slice.len()];
                            new_vec.clone_from_slice(slice);
                            return_array(new_vec, &mut self.memory, &mut self.stack)?;
                        } else {
                            return_array(vec![], &mut self.memory, &mut self.stack)?;
                        }
                    } else {
                        return Err(LpcError::new(
                            "Invalid code was generated for an ARange instruction.",
                        )
                        .with_span(self.process.current_debug_span()));
                    }
                } else {
                    return Err(LpcError::new("ARange's array isn't actually an array?")
                        .with_span(self.process.current_debug_span()));
                }
            }
            Instruction::EqEq(r1, r2, r3) => {
                let registers = current_registers_mut(&mut self.stack)?;
                let out = if registers[r1.index()] == registers[r2.index()] {
                    1
                } else {
                    0
                };

                let registers = current_registers_mut(&mut self.stack)?;
                registers[r3.index()] = LpcRef::Int(out);
            }
            Instruction::Call {
                name,
                num_args,
                initial_arg,
            } => {
                let mut new_frame = if let Some(func) = self.process.functions.get(name) {
                    StackFrame::new(self.process.clone(), func.clone(), self.process.pc())
                } else if let Some(prototype) = EFUN_PROTOTYPES.get(name.as_str()) {
                    let sym = FunctionSymbol {
                        name: name.clone(),
                        num_args: prototype.num_args,
                        num_locals: 0,
                        address: 0,
                    };

                    StackFrame::new(self.process.clone(), sym, self.process.pc())
                } else {
                    return Err(self.runtime_error(format!("Call to unknown function `{}`", name)));
                };

                // copy argument registers from old frame to new
                if *num_args > 0_usize {
                    let index = initial_arg.index();
                    let current_frame = self.stack.last().unwrap();
                    new_frame.registers[1..=*num_args]
                        .clone_from_slice(&current_frame.registers[index..(index + num_args)]);
                }

                self.stack.push(new_frame);

                if let Some(FunctionSymbol { address, .. }) = self.process.functions.get(name) {
                    self.process.set_pc(*address);
                } else if let Some(efun) = EFUNS.get(name.as_str()) {
                    // the efun is responsible for populating the return value
                    efun(self)?;
                    if let Some(frame) = self.pop_frame() {
                        self.copy_call_result(&frame)?;
                    }
                } else {
                    return Err(self.runtime_error(format!(
                        "Call to unknown function (that had a valid prototype?) `{}`",
                        name
                    )));
                }
            }
            Instruction::CallOther {
                receiver,
                name,
                num_args,
                initial_arg,
            } => {
                // get receiver process and make it the current one
                let receiver_ref = self.register_to_lpc_ref(receiver.index());
                let nc = name.clone();
                let num_args = *num_args;
                let initial_index = initial_arg.index();
                let return_address = self.process.pc();

                match receiver_ref {
                    LpcRef::String(s) => {
                        let r = s.borrow();
                        let str = try_extract_value!(*r, LpcValue::String);

                        let pr = self.lookup_process(str)?;
                        // Only switch the process if there's actually a function to
                        // call by this name on the other side.
                        if pr.functions.contains_key(&nc) {
                            self.process = pr.clone();
                        }
                    }
                    _ => {
                        return Err(LpcError::new(format!(
                            "What are you trying to call `{}` on?",
                            name
                        ))
                        .with_span(self.process.current_debug_span()))
                    }
                }

                let sym = if let Some(fs) = self.process.functions.get(&nc) {
                    fs
                } else {
                    return if let Some(frame) = self.stack.last_mut() {
                        frame.registers[0] = LpcRef::Int(0);
                        self.process.inc_pc();

                        Ok(false)
                    } else {
                        Err(self.runtime_error(format!("Call to unknown function `{}`", nc)))
                    };
                };

                // Because call_other args aren't arity-checked, there might be more
                // passed than expected, so we might need to reserve more space
                let mut new_frame = StackFrame::with_minimum_arg_capacity(
                    self.process.clone(),
                    sym.clone(),
                    return_address,
                    num_args + 1, // +1 for r0
                );

                // copy argument registers from old frame to new
                if num_args > 0_usize {
                    let current_frame = self.stack.last().unwrap();
                    new_frame.registers[1..=num_args].clone_from_slice(
                        &current_frame.registers[initial_index..(initial_index + num_args)],
                    );
                }

                self.stack.push(new_frame);

                self.process.set_pc(sym.address);
            }
            Instruction::FConst(r, f) => {
                let registers = current_registers_mut(&mut self.stack)?;
                registers[r.index()] = LpcRef::Float(*f);
            }
            Instruction::GLoad(r1, r2) => {
                // load from global r1, into local r2
                let global = self.process.globals[r1.index()].borrow().clone();
                let registers = current_registers_mut(&mut self.stack)?;
                registers[r2.index()] = global
            }
            Instruction::GStore(r1, r2) => {
                // store local r1 into global r2
                let registers = current_registers_mut(&mut self.stack)?;
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
                let registers = current_registers_mut(&mut self.stack)?;
                match &registers[r1.index()] + &registers[r2.index()] {
                    Ok(result) => {
                        let out = value_to_ref!(result, self.memory);

                        registers[r3.index()] = out
                    }
                    Err(e) => {
                        return Err(e.with_span(self.process.current_debug_span()));
                    }
                }
            }
            Instruction::IConst(r, i) => {
                let registers = current_registers_mut(&mut self.stack)?;
                registers[r.index()] = LpcRef::Int(*i);
            }
            Instruction::IConst0(r) => {
                let registers = current_registers_mut(&mut self.stack)?;
                registers[r.index()] = LpcRef::Int(0);
            }
            Instruction::IConst1(r) => {
                let registers = current_registers_mut(&mut self.stack)?;
                registers[r.index()] = LpcRef::Int(1);
            }
            Instruction::IDiv(r1, r2, r3) => {
                let registers = current_registers_mut(&mut self.stack)?;
                match &registers[r1.index()] / &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.process.current_debug_span()));
                    }
                }
            }
            Instruction::IMul(r1, r2, r3) => {
                let registers = current_registers_mut(&mut self.stack)?;
                match &registers[r1.index()] * &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.process.current_debug_span()));
                    }
                }
            }
            Instruction::ISub(r1, r2, r3) => {
                let registers = current_registers_mut(&mut self.stack)?;
                match &registers[r1.index()] - &registers[r2.index()] {
                    Ok(result) => registers[r3.index()] = value_to_ref!(result, self.memory),
                    Err(e) => {
                        return Err(e.with_span(self.process.current_debug_span()));
                    }
                }
            }
            Instruction::Jmp(address) => {
                self.process.set_pc(*address);
            }
            Instruction::Jnz(r1, address) => {
                let v = &current_registers_mut(&mut self.stack)?[r1.index()];

                if v != &LpcRef::Int(0) && v != &LpcRef::Float(Total::from(0.0)) {
                    self.process.set_pc(*address);
                }
            }
            Instruction::Jz(r1, address) => {
                let v = &current_registers_mut(&mut self.stack)?[r1.index()];

                if v == &LpcRef::Int(0) || v == &LpcRef::Float(Total::from(0.0)) {
                    self.process.set_pc(*address);
                }
            }
            Instruction::Load(r1, r2, r3) => {
                let container_ref = self.register_to_lpc_ref(r1.index());

                match container_ref {
                    LpcRef::Array(vec_ref) => {
                        let value = vec_ref.borrow();
                        let vec = try_extract_value!(*value, LpcValue::Array);

                        let index = self.register_to_lpc_ref(r2.index());
                        let registers = current_registers_mut(&mut self.stack)?;

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
                    LpcRef::Mapping(map_ref) => {
                        let index = self.register_to_lpc_ref(r2.index());
                        let value = map_ref.borrow();
                        let map = try_extract_value!(*value, LpcValue::Mapping);

                        let var = if let Some(v) = map.get(&index) {
                            v.clone()
                        } else {
                            LpcRef::Int(0)
                        };

                        let registers = current_registers_mut(&mut self.stack)?;
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
                    let registers = current_registers_mut(&mut self.stack)?;
                    let r = registers[key.index()].clone();

                    register_map.insert(r, registers[value.index()].clone());
                }

                let new_ref = value_to_ref!(LpcValue::from(register_map), self.memory);
                let registers = current_registers_mut(&mut self.stack)?;

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
            Instruction::RegCopy(r1, r2) => {
                let registers = current_registers_mut(&mut self.stack)?;
                registers[r2.index()] = registers[r1.index()].clone()
            }
            Instruction::Ret => {
                if let Some(frame) = self.pop_frame() {
                    self.copy_call_result(&frame)?;

                    if !self.stack.is_empty()
                        && Rc::ptr_eq(&frame.process, &self.stack.last().unwrap().process)
                    {
                        self.process.set_pc(frame.return_address);
                    }
                }

                // halt at the end of all input
                if self.stack.is_empty() {
                    return Ok(true);
                }
            }
            Instruction::Store(r1, r2, r3) => {
                // r2[r3] = r1;

                let mut container = self.register_to_lpc_ref(r2.index());
                let index = self.register_to_lpc_ref(r3.index());
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
                let registers = current_registers_mut(&mut self.stack)?;
                let new_ref = value_to_ref!(LpcValue::from(s), self.memory);

                registers[r.index()] = new_ref;
            }
        }

        Ok(false)
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

                let registers = current_registers_mut(&mut self.stack)?;
                registers[r3.index()] = var
            }
            Err(e) => {
                return Err(e.with_span(self.process.current_debug_span()));
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

        let registers = current_registers_mut(&mut self.stack)?;
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
            current_registers_mut(&mut self.stack)?[0] = from.registers[0].clone();
        }

        Ok(())
    }

    fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::new(format!("Runtime Error: {}", msg.as_ref()))
            .with_span(self.process.current_debug_span())
    }

    fn array_index_error<T: Display>(&self, index: T, length: usize) -> LpcError {
        self.runtime_error(format!(
            "Attempting to access index {} in an array of length {}",
            index, length
        ))
    }
}

impl Default for AsmInterpreter {
    fn default() -> Self {
        let programs = HashMap::with_capacity(OBJECT_SPACE_SIZE);
        let program = Rc::new(Process::new(Program::default()));

        Self {
            process: program,
            processes: programs,
            clone_count: 0,
            stack: Vec::with_capacity(STACK_SIZE),
            memory: Pool::new(MEMORY_SIZE),
        }
    }
}
