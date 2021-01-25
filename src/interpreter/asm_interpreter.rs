use std::collections::HashMap;
use std::convert::TryInto;
use crate::asm::instruction::Instruction;
use crate::interpreter::efun::EFUNS;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::function_symbol::FunctionSymbol;
use crate::interpreter::lpc_var::LPCVar;
use crate::interpreter::constant_pool::ConstantPool;
use crate::interpreter::lpc_constant::LPCConstant;

const MAX_STACK: usize = 1000;

macro_rules! int {
    ($x:expr) => {
        LPCVar::Int($x)
    };
}

macro_rules! string {
    ($x:expr) => {
        LPCVar::String($x)
    };
}

#[derive(Debug)]
pub struct AsmInterpreter {
    /// Out program to execute
    instructions: Vec<Instruction>,

    /// The call stack
    stack: Vec<StackFrame>,

    /// jump labels
    labels: HashMap<String, usize>,

    /// function data
    functions: HashMap<String, FunctionSymbol>,

    constants: ConstantPool,

    /// program counter
    pc: usize,

    is_halted: bool
}

impl AsmInterpreter {
    /// load instructions for evaluation
    pub fn load(&mut self, instructions: &[Instruction],
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
        let len = self.stack.len();
        self.stack[len - 1].registers.as_mut()
    }

    /// Resolve the passed index within the current stack frame's registers
    pub fn resolve_register(&self, index: usize) -> LPCConstant {
        let len = self.stack.len();
        let registers = &self.stack[len - 1].registers;

        match registers.get(index).unwrap() {
            LPCVar::Int(v) => LPCConstant::Int(*v),
            LPCVar::String(i) => {
                self.constants.get((*i).try_into().unwrap()).unwrap().clone()
            }
        }
    }

    /// evaluate loaded instructions, starting from the current value of the PC
    fn eval(&mut self) {
        let instructions = self.instructions.clone();
        while let Some(instruction) = instructions.get(self.pc) {
            if self.is_halted {
                break;
            }

            // println!("{:?}", instruction);

            let len = self.stack.len();
            let registers: &mut Vec<LPCVar> = self.stack[len - 1].registers.as_mut();

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
                        let current_frame = &self.stack[self.stack.len() - 1];
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
                    registers[r3.index()] =
                        registers[r1.index()] + registers[r2.index()]
                },
                Instruction::IConst(r, i) => {
                    registers[r.index()] = int!(*i);
                },
                Instruction::IConst0(r) => {
                    registers[r.index()] = int!(0);
                },
                Instruction::IConst1(r) => {
                    registers[r.index()] = int!(1);
                },
                Instruction::SConst(r, s) => {
                    let index = self.constants.insert(LPCConstant::from(s));
                    registers[r.index()] = string!(index.try_into().unwrap());
                },
                Instruction::IDiv(r1, r2, r3) => {
                    registers[r3.index()] =
                        registers[r1.index()] / registers[r2.index()]
                },
                Instruction::IMul(r1, r2, r3) => {
                    registers[r3.index()] =
                        registers[r1.index()] * registers[r2.index()]
                },
                Instruction::ISub(r1, r2, r3) => {
                    registers[r3.index()] =
                        registers[r1.index()] - registers[r2.index()]
                },
                Instruction::RegCopy(r1, r2) => {
                    registers[r2.index()] = registers[r1.index()]
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

    fn halt(&mut self) {
        self.is_halted = true;
    }

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