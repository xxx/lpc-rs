use crate::{
    asm::instruction::Instruction,
    errors::runtime_error::{index_error::IndexError, RuntimeError},
    interpreter::{
        efun::EFUNS, lpc_value::LPCValue, lpc_var::LPCVar, program::Program,
        stack_frame::StackFrame,
    },
    semantic::function_symbol::FunctionSymbol,
};
use std::borrow::BorrowMut;

/// The initial size (in frames) of the call stack
const STACK_SIZE: usize = 1000;

/// The initial size (in cells) of system memory
const MEMORY_SIZE: usize = 100000;

/// The initial size of the globals vector
const GLOBALS_SIZE: usize = 100;

/// Convenience helper for registers
macro_rules! int {
    ($x:expr) => {
        LPCVar::Int($x)
    };
}

/// Convenience helper for registers
macro_rules! array {
    ($x:expr) => {
        LPCVar::Array($x)
    };
}

/// Convenience helper for registers
macro_rules! string_constant {
    ($x:expr) => {
        LPCVar::StringConstant($x)
    };
}

/// An interpreter that executes instructions
///
/// # Examples
///
/// ```
/// use std::borrow::BorrowMut;
/// use lpc_rs::lpc_parser;
/// use lpc_rs::codegen::tree_walker::TreeWalker;
/// use lpc_rs::codegen::asm_tree_walker::AsmTreeWalker;
/// use lpc_rs::codegen::scope_walker::ScopeWalker;
/// use lpc_rs::interpreter::asm_interpreter::AsmInterpreter;
/// use lpc_rs::semantic::scope_tree::ScopeTree;
/// use lpc_rs::codegen::default_params_walker::DefaultParamsWalker;
///
/// let prog = r#"int main() { dump("hello, world"); int b = 123; return b; }"#;
/// let mut program_node = lpc_parser::ProgramParser::new().parse(prog).unwrap();
/// let filepath = "path/to/myfile.c";
///
/// // Populate the symbol tables
/// let mut scope_walker = ScopeWalker::new(filepath);
/// let scope_result = scope_walker.visit_program(program_node.borrow_mut());
///
/// // Gather information about function default params
/// let mut default_params_walker = DefaultParamsWalker::new();
/// let params_result = default_params_walker.visit_program(program_node.borrow_mut());
///
/// // Generate machine instructions
/// let mut walker = AsmTreeWalker::new(
///     ScopeTree::from(scope_walker),
///     default_params_walker.into_functions()
/// );
/// walker.visit_program(program_node.borrow_mut());
/// let mut program = walker.to_program(filepath);
///
/// // Load the program and run it
/// let mut interpreter = AsmInterpreter::new(program);
/// interpreter.exec();
/// ```
#[derive(Debug)]
pub struct AsmInterpreter {
    /// The program to run
    program: Program,

    /// The call stack
    stack: Vec<StackFrame>,

    /// Our memory
    memory: Vec<LPCValue>,

    /// Registers that hold global variables
    globals: Vec<LPCVar>,

    /// program counter
    pc: usize,

    /// Is the machine halted?
    is_halted: bool,
}

/// Get a reference to the passed stack frame's registers
#[inline]
fn current_registers(stack: &Vec<StackFrame>) -> &Vec<LPCVar> {
    &stack.last().unwrap().registers
}

/// Get a mutable reference to the passed stack frame's registers
#[inline]
fn current_registers_mut(stack: &mut Vec<StackFrame>) -> &mut Vec<LPCVar> {
    stack.last_mut().unwrap().registers.as_mut()
}

/// Get a mutable reference to an array in memory, so writes can be made to it.
/// # Arguments
/// `index` - the register index to resolve
fn resolve_array_reference_mut<'a>(
    index: usize,
    stack: &Vec<StackFrame>,
    memory: &'a mut Vec<LPCValue>,
) -> &'a mut Vec<LPCVar> {
    let len = stack.len();
    let registers = &stack[len - 1].registers;

    // println!("regs {:?}", registers);
    let var = registers.get(index).unwrap();

    match var {
        LPCVar::Array(i) => {
            // not recursive
            if let LPCValue::Array(ref mut vec) = memory.get_mut(*i).unwrap() {
                vec
            } else {
                unimplemented!()
            }
        }
        _ => unimplemented!(),
    }
}

impl AsmInterpreter {
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
    pub fn load(&mut self, program: Program) {
        self.globals = vec![LPCVar::Int(0); program.num_globals];
        self.program = program;
    }

    /// Dummy starter for the interpreter, to get the "main" stack frame setup
    pub fn exec(&mut self) -> Result<(), RuntimeError> {
        let main = StackFrame::new(
            FunctionSymbol {
                name: "main".to_string(),
                num_args: 0,
                num_locals: 200,
                address: 0,
            },
            0,
        );
        self.push_frame(main);

        self.is_halted = false;

        self.eval()
    }

    /// Push a new stack frame onto the call stack
    fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame);
    }

    /// Pop the current stack frame off the stack
    fn pop_frame(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    /// Resolve an LPCVar into an LPCValue. This clones the value, so writes will not do anything.
    pub fn resolve_var(&self, var: &LPCVar) -> LPCValue {
        match var {
            LPCVar::Int(v) => LPCValue::Int(*v),
            LPCVar::String(i) => self.memory.get(*i).unwrap().clone(),
            LPCVar::Array(i) => {
                // not recursive
                self.memory.get(*i).unwrap().clone()
            }
            LPCVar::StringConstant(i) => self.program.constants.get(*i).unwrap().clone(),
        }
    }

    /// Resolve the passed index within the current stack frame's registers
    /// # Arguments
    /// `index` - the register index to resolve
    pub fn resolve_register(&self, index: usize) -> LPCValue {
        let len = self.stack.len();
        let registers = &self.stack[len - 1].registers;

        // println!("regs {:?}", registers);

        self.resolve_var(registers.get(index).unwrap())
    }

    /// Evaluate loaded instructions, starting from the current value of the PC
    fn eval(&mut self) -> Result<(), RuntimeError> {
        let instructions = self.program.instructions.clone();

        while let Some(instruction) = instructions.get(self.pc) {
            if self.is_halted {
                break;
            }

            // println!("instruction: {:?}", instruction);
            // println!("registers: {:?}", self.current_registers());

            match instruction {
                Instruction::AAppend(_r1, _r2) => {
                    todo!()
                }
                Instruction::AConst(r, vec) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    let vars = vec.iter().map(|i| registers[i.index()]).collect::<Vec<_>>();
                    let index = self.memory.len();
                    self.memory.push(LPCValue::from(vars));
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r.index()] = array!(index);
                }
                Instruction::ALoad(r1, r2, r3) => {
                    let arr = self.resolve_register(r1.index());
                    let index = self.resolve_register(r2.index());
                    let registers = current_registers_mut(self.stack.borrow_mut());

                    if let (LPCValue::Array(vec), LPCValue::Int(i)) = (arr, index) {
                        let idx = if i >= 0 { i } else { vec.len() as i64 + i };

                        if idx >= 0 {
                            if let Some(v) = vec.get(idx as usize) {
                                registers[r3.index()] = *v;
                            } else {
                                return Err(self.make_index_error(idx, vec.len()));
                            }
                        } else {
                            return Err(self.make_index_error(idx, vec.len()));
                        }
                    } else {
                        panic!("This shouldn't have passed type checks.")
                    }
                }
                Instruction::ARange(r1, r2, r3, r4) => {
                    // r4 = r1[r2..r3]
                    let return_array = |arr, memory: &mut Vec<LPCValue>, stack: &mut Vec<StackFrame>| {
                        let index = memory.len();
                        memory.push(LPCValue::from(arr));
                        let registers = current_registers_mut(stack);
                        registers[r4.index()] = array!(index);
                    };

                    let value = self.resolve_register(r1.index());
                    if let LPCValue::Array(vec) = value {
                        if vec.is_empty() {
                            return_array(vec![], &mut self.memory, &mut self.stack);
                        }

                        let index1 = self.resolve_register(r2.index());
                        let index2 = self.resolve_register(r3.index());

                        if let (LPCValue::Int(start), LPCValue::Int(end)) = (index1, index2) {
                            let to_idx = |i: i64| {
                                // We handle the potential overflow just below.
                                if i >= 0 { i as usize } else { (vec.len() as i64 + i) as usize }
                            };
                            let real_start = to_idx(start);
                            let mut real_end = to_idx(end);

                            if real_end >= vec.len() {
                                real_end = vec.len() - 1;
                            }

                            if real_start <= real_end {
                                let slice = &vec[real_start..=real_end];
                                let mut new_vec = vec![LPCVar::Int(0); slice.len()];
                                new_vec.copy_from_slice(slice);
                                return_array(new_vec, &mut self.memory, &mut self.stack);
                            } else {
                                return_array(vec![], &mut self.memory, &mut self.stack);
                            }
                        } else {
                            panic!("Invalid code was generated for an ARange instruction.")
                        }

                    } else {
                        panic!("ARange's array isn't actually an array?");
                    }
                }
                Instruction::AStore(r1, r2, r3) => {
                    // r2[r3] = r1;

                    let index = self.resolve_register(r3.index());

                    if let LPCValue::Int(i) = index {
                        // update the vector in memory directly
                        let vec = resolve_array_reference_mut(
                            r2.index(),
                            &self.stack,
                            &mut self.memory,
                        );
                        let len = vec.len();
                        // handle negative indices
                        let idx = if i >= 0 { i } else { len as i64 + i };

                        if (0..len).contains(&(idx as usize)) {
                            vec[idx as usize] = current_registers(&self.stack)[r1.index()];
                        } else {
                            return Err(self.make_index_error(idx, len));
                        }
                    } else {
                        panic!("This shouldn't have passed type checks.")
                    }
                }
                Instruction::Call {
                    name,
                    num_args,
                    initial_arg,
                } => {
                    let mut new_frame = if let Some(func) = self.program.functions.get(name) {
                        StackFrame::new(func.clone(), self.pc + 1)
                    } else if EFUNS.contains_key(name.as_str()) {
                        // TODO: memoize this symbol
                        let sym = FunctionSymbol {
                            name: name.clone(),
                            num_args: *num_args, // TODO: look this up server-side
                            num_locals: 0,
                            address: 0,
                        };

                        StackFrame::new(sym, self.pc + 1)
                    } else {
                        panic!("Unable to find function: {}", name);
                    };

                    // copy argument registers from old frame to new
                    if *num_args > 0 as usize {
                        let index = initial_arg.index();
                        let current_frame = self.stack.last().unwrap();
                        new_frame.registers[1..=*num_args]
                            .copy_from_slice(&current_frame.registers[index..(index + num_args)]);
                    }

                    self.stack.push(new_frame);

                    if let Some(FunctionSymbol { address, .. }) = self.program.functions.get(name) {
                        self.pc = *address;
                        continue;
                    } else if let Some(efun) = EFUNS.get(name.as_str()) {
                        // the efun is responsible for populating the return value
                        efun(&self.stack.last().unwrap(), &self);
                        if let Some(frame) = self.pop_frame() {
                            self.copy_call_result(&frame);
                        }
                    } else {
                        unimplemented!()
                    }
                }
                Instruction::GLoad(r1, r2) => {
                    let global = self.globals[r1.index()];
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r2.index()] = global
                }
                Instruction::GStore(r1, r2) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    self.globals[r2.index()] = registers[r1.index()]
                }
                Instruction::IAdd(r1, r2, r3) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    match registers[r1.index()] + registers[r2.index()] {
                        Ok(result) => registers[r3.index()] = result,
                        Err(mut e) => {
                            self.populate_error_span(&mut e);

                            return Err(e);
                        }
                    }
                }
                Instruction::IConst(r, i) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r.index()] = int!(*i);
                }
                Instruction::IConst0(r) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r.index()] = int!(0);
                }
                Instruction::IConst1(r) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r.index()] = int!(1);
                }
                Instruction::SConst(r, s) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r.index()] = string_constant!(*s);
                }
                Instruction::IDiv(r1, r2, r3) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    match registers[r1.index()] / registers[r2.index()] {
                        Ok(result) => registers[r3.index()] = result,
                        Err(mut e) => {
                            self.populate_error_span(&mut e);

                            return Err(e);
                        }
                    }
                }
                Instruction::IMul(r1, r2, r3) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    match registers[r1.index()] * registers[r2.index()] {
                        Ok(result) => registers[r3.index()] = result,
                        Err(mut e) => {
                            self.populate_error_span(&mut e);

                            return Err(e);
                        }
                    }
                }
                Instruction::ISub(r1, r2, r3) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    match registers[r1.index()] - registers[r2.index()] {
                        Ok(result) => registers[r3.index()] = result,
                        Err(mut e) => {
                            self.populate_error_span(&mut e);

                            return Err(e);
                        }
                    }
                }
                Instruction::MAdd(r1, r2, r3) => {
                    // look up vals, add, store result.
                    let val1 = &self.resolve_register(r1.index());
                    let val2 = &self.resolve_register(r2.index());

                    match val1 + val2 {
                        Ok(result) => {
                            let index = self.memory.len();
                            let var = match result {
                                LPCValue::String(_) => LPCVar::String(index),
                                LPCValue::Array(_) => LPCVar::Array(index),
                                LPCValue::Int(_) => unimplemented!(),
                            };

                            self.memory.push(result);
                            let registers = current_registers_mut(self.stack.borrow_mut());
                            registers[r3.index()] = var
                        }
                        Err(mut e) => {
                            self.populate_error_span(&mut e);

                            return Err(e);
                        }
                    }
                }
                Instruction::MDiv(r1, r2, r3) => {
                    // look up vals, divide, store result.
                    let val1 = &self.resolve_register(r1.index());
                    let val2 = &self.resolve_register(r2.index());
                    match val1 / val2 {
                        Ok(result) => {
                            let index = self.memory.len();
                            self.memory.push(result);

                            let var = LPCVar::String(index);
                            let registers = current_registers_mut(&mut self.stack);
                            registers[r3.index()] = var
                        }
                        Err(mut e) => {
                            self.populate_error_span(&mut e);

                            return Err(e);
                        }
                    }
                }
                Instruction::MMul(r1, r2, r3) => {
                    // look up vals, multiply, store result.
                    let val1 = &self.resolve_register(r1.index());
                    let val2 = &self.resolve_register(r2.index());
                    match val1 * val2 {
                        Ok(result) => {
                            let index = self.memory.len();
                            self.memory.push(result);

                            let var = LPCVar::String(index);
                            let registers = current_registers_mut(self.stack.borrow_mut());
                            registers[r3.index()] = var
                        }
                        Err(mut e) => {
                            self.populate_error_span(e.borrow_mut());

                            return Err(e);
                        }
                    }
                }
                Instruction::MSub(r1, r2, r3) => {
                    // look up vals, subtract, store result.
                    let val1 = &self.resolve_register(r1.index());
                    let val2 = &self.resolve_register(r2.index());
                    match val1 - val2 {
                        Ok(result) => {
                            let index = self.memory.len();
                            self.memory.push(result);

                            let var = LPCVar::String(index);
                            let registers = current_registers_mut(self.stack.borrow_mut());
                            registers[r3.index()] = var
                        }
                        Err(mut e) => {
                            self.populate_error_span(e.borrow_mut());

                            return Err(e);
                        }
                    }
                }
                Instruction::RegCopy(r1, r2) => {
                    let registers = current_registers_mut(self.stack.borrow_mut());
                    registers[r2.index()] = registers[r1.index()]
                }
                Instruction::Ret => {
                    if let Some(frame) = self.pop_frame() {
                        self.copy_call_result(&frame);
                        self.pc = frame.return_address;
                    }

                    // halt at the end of all input
                    if self.stack.is_empty() {
                        self.halt();
                    }

                    continue;
                }
            }

            self.pc += 1;
        }

        Ok(())
    }

    /// Flag the machine to halt after it finishes executing its next instruction.
    fn halt(&mut self) {
        self.is_halted = true;
    }

    /// Convenience helper to copy a return value from a given stack frame, back to the current one.
    fn copy_call_result(&mut self, from: &StackFrame) {
        if !self.stack.is_empty() {
            current_registers_mut(self.stack.borrow_mut())[0] = from.registers[0];
        }
    }

    #[doc(hidden)]
    fn populate_error_span(&self, error: &mut RuntimeError) {
        match error {
            RuntimeError::BinaryOperationError(err) => {
                err.span = *self.program.debug_spans.get(self.pc).unwrap()
            }
            RuntimeError::DivisionByZeroError(err) => {
                err.span = *self.program.debug_spans.get(self.pc).unwrap()
            }
            RuntimeError::IndexError(err) => {
                err.span = *self.program.debug_spans.get(self.pc).unwrap()
            }
            RuntimeError::UnknownError(_) => unimplemented!(),
        }
    }

    #[doc(hidden)]
    fn make_index_error(&self, index: i64, length: usize) -> RuntimeError {
        let e = IndexError {
            index,
            length,
            span: None,
        };

        let mut e = RuntimeError::IndexError(e);

        self.populate_error_span(e.borrow_mut());

        e
    }
}

impl Default for AsmInterpreter {
    fn default() -> Self {
        Self {
            program: Program::default(),
            stack: Vec::with_capacity(STACK_SIZE),
            memory: Vec::with_capacity(MEMORY_SIZE),
            globals: Vec::with_capacity(GLOBALS_SIZE),
            is_halted: true,
            pc: 0,
        }
    }
}
