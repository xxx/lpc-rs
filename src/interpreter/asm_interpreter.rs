use std::collections::HashMap;
use std::convert::TryInto;
use crate::asm::instruction::Instruction;
use crate::interpreter::efun::EFUNS;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::function_symbol::FunctionSymbol;
use crate::interpreter::lpc_var::LPCVar;
use crate::interpreter::constant_pool::ConstantPool;
use crate::interpreter::lpc_value::LPCValue;

/// The max size (in frames) of the call stack
const MAX_STACK: usize = 1000;

/// Convenience helper for registers
macro_rules! int {
    ($x:expr) => {
        LPCVar::Int($x)
    };
}

/// Convenience helper for registers
macro_rules! string {
    ($x:expr) => {
        LPCVar::String($x)
    };
}

/// An interpreter that executes instructions
///
/// # Examples
///
/// ```
/// use mathstack::mathstack_parser;
/// use mathstack::codegen::tree_walker::TreeWalker;
/// use mathstack::codegen::asm_tree_walker::AsmTreeWalker;
/// use mathstack::interpreter::asm_interpreter::AsmInterpreter;
///
/// let prog = "int main() { int b = 123; return b; }";
/// let program_node = mathstack_parser::ProgramParser::new().parse(prog).unwrap();
/// let mut walker = AsmTreeWalker::default();
/// let mut interpreter = AsmInterpreter::default();
///
/// walker.visit_program(&program_node);
///
/// interpreter.load(
///     &walker.instructions,
///     &walker.combined_labels(),
///     &walker.function_map()
/// );
///
/// interpreter.exec();
/// ```
#[derive(Debug)]
pub struct AsmInterpreter {
    /// The actual program to execute
    instructions: Vec<Instruction>,

    /// The call stack
    stack: Vec<StackFrame>,

    /// jump labels
    labels: HashMap<String, usize>,

    /// function mapping of name to Symbol
    functions: HashMap<String, FunctionSymbol>,

    /// All non-int values are stored in the pool.
    constants: ConstantPool,

    /// program counter
    pc: usize,

    /// Is the machine halted?
    is_halted: bool
}

impl AsmInterpreter {
    /// Load instructions and associated data for evaluation
    ///
    /// # Arguments
    ///
    /// * `instructions` - The instructions to be executed.
    /// * `labels` - The map of label names to their corresponding address.
    /// * `functions` - The map of function names to their corresponding Symbols
    pub fn load(&mut self,
                instructions: &[Instruction],
                labels: &HashMap<String, usize>,
                functions: &HashMap<String, FunctionSymbol>) {
        self.instructions = instructions.to_vec();
        self.labels = labels.clone();
        self.functions = functions.clone();
    }

    /// Dummy starter for the interpreter, to get the "main" stack frame setup
    pub fn exec(&mut self) {
        let main = StackFrame::new(
            FunctionSymbol {
                name: "main".to_string(),
                num_args: 0,
                num_locals: 200,
                address: 0
            },
            0
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

    /// Get a mutable reference to the current stack frame's registers
    fn current_registers(&mut self) -> &mut Vec<LPCVar> {
        self.stack.last_mut().unwrap().registers.as_mut()
    }

    /// Resolve the passed index within the current stack frame's registers
    pub fn resolve_register(&self, index: usize) -> LPCValue {
        let len = self.stack.len();
        let registers = &self.stack[len - 1].registers;

        match registers.get(index).unwrap() {
            LPCVar::Int(v) => LPCValue::Int(*v),
            LPCVar::String(i) => {
                self.constants.get(*i).unwrap().clone()
            }
        }
    }

    /// Evaluate loaded instructions, starting from the current value of the PC
    fn eval(&mut self) {
        let instructions = self.instructions.clone();
        while let Some(instruction) = instructions.get(self.pc) {
            if self.is_halted {
                break;
            }

            // println!("{:?}", instruction);

            match instruction {
                Instruction::Call { name, num_args, initial_arg } => {
                    let mut new_frame = if let Some(func) = self.functions.get(name) {
                        StackFrame::new(
                            func.clone(),
                            self.pc + 1
                        )
                    } else if EFUNS.contains_key(name) {
                        let sym = FunctionSymbol {
                            name: name.clone(),
                            num_args: *num_args, // TODO: look this up server-side
                            num_locals: 0,
                            address: 0
                        };

                        StackFrame::new(
                            sym,
                            self.pc + 1
                        )
                    } else {
                        panic!("Unable to find function: {}", name);
                    };

                    // copy argument registers from old frame to new
                    if *num_args > 0 as usize {
                        let index = initial_arg.index();
                        let current_frame = self.stack.last().unwrap();
                        new_frame.registers[1..=*num_args].copy_from_slice(
                            &current_frame.registers[index..(index + num_args)]
                        );
                    }

                    self.stack.push(new_frame);

                    if let Some(address) = self.labels.get(name) {
                        self.pc = *address;
                        continue;
                    } else if let Some(efun) = EFUNS.get(name) {
                        // the efun is responsible for populating the return value
                        efun(&self.stack[self.stack.len() - 1], &self);
                        if let Some(frame) = self.pop_frame() {
                            self.copy_call_result(&frame);
                        }
                    } else {
                        unimplemented!()
                    }
                },
                Instruction::IAdd(r1, r2, r3) => {
                    let registers = self.current_registers();
                    registers[r3.index()] =
                        registers[r1.index()] + registers[r2.index()]
                },
                Instruction::IConst(r, i) => {
                    let registers = self.current_registers();
                    registers[r.index()] = int!(*i);
                },
                Instruction::IConst0(r) => {
                    let registers = self.current_registers();
                    registers[r.index()] = int!(0);
                },
                Instruction::IConst1(r) => {
                    let registers = self.current_registers();
                    registers[r.index()] = int!(1);
                },
                Instruction::SConst(r, s) => {
                    let index = self.constants.insert(LPCValue::from(s));
                    let registers = self.current_registers();
                    registers[r.index()] = string!(index.try_into().unwrap());
                },
                Instruction::IDiv(r1, r2, r3) => {
                    let registers = self.current_registers();
                    registers[r3.index()] =
                        registers[r1.index()] / registers[r2.index()]
                },
                Instruction::IMul(r1, r2, r3) => {
                    let registers = self.current_registers();
                    registers[r3.index()] =
                        registers[r1.index()] * registers[r2.index()]
                },
                Instruction::ISub(r1, r2, r3) => {
                    let registers = self.current_registers();
                    registers[r3.index()] =
                        registers[r1.index()] - registers[r2.index()]
                },
                Instruction::RegCopy(r1, r2) => {
                    let registers = self.current_registers();
                    registers[r2.index()] = registers[r1.index()]
                },
                Instruction::SAdd(r1, r2, r3) => {
                    // look up strings, concat, add to constant pool,
                    let string1 = &self.resolve_register(r1.index());
                    let string2 = &self.resolve_register(r2.index());
                    let result = string1 + string2;
                    let index = self.constants.insert(result);

                    // set r3.index to the new constant index
                    let var = LPCVar::String(index);
                    let registers = self.current_registers();
                    registers[r3.index()] = var
                },
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
    }

    /// Flag the machine to halt after it finishes executing its next instruction.
    fn halt(&mut self) {
        self.is_halted = true;
    }

    /// Convenience helper to copy a return value from a given stack frame, back to the current one.
    fn copy_call_result(&mut self, from: &StackFrame) {
        if !self.stack.is_empty() {
            self.current_registers()[0] = from.registers[0];
        }
    }
}

impl Default for AsmInterpreter {
    fn default() -> Self {
        Self {
            instructions: vec![],
            labels: HashMap::new(),
            functions: HashMap::new(),
            stack: Vec::with_capacity(MAX_STACK),
            constants: ConstantPool::default(),
            is_halted: true,
            pc: 0
        }
    }
}