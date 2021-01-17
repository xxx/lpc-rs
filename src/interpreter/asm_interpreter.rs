use crate::asm::instruction::Instruction;
use crate::interpreter::efun::EFUNS;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::function_symbol::FunctionSymbol;
use std::collections::HashMap;

const MAX_STACK: usize = 1000;

#[derive(Debug)]
pub struct AsmInterpreter {
    instructions: Vec<Instruction>,
    stack: Vec<StackFrame>,
    labels: HashMap<String, usize>,
    functions: HashMap<String, FunctionSymbol>,
    fp: usize,
    pc: usize
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

        self.eval()
    }

    fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame);
    }

    fn pop_frame(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    fn current_registers(&mut self) -> &mut Vec<i64> {
        let len = self.stack.len();
        self.stack[len - 1].registers.as_mut()
    }

    /// evaluate loaded instructions, starting from the current value of the PC
    pub fn eval(&mut self) {
        let instructions = self.instructions.clone();
        while let Some(instruction) = instructions.get(self.pc) {
            // println!("{:?}", instruction);
            let registers = self.current_registers();

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
                        efun(&self.stack[self.stack.len() - 1]);
                        // TODO: store return value in current frame
                        self.pop_frame();
                    } else {
                        unimplemented!()
                    }
                },
                Instruction::IAdd(r1, r2, r3) => {
                    registers[r3.index()] =
                        registers[r1.index()] + registers[r2.index()]
                },
                Instruction::IConst(r, i) => {
                    registers[r.index()] = *i;
                },
                Instruction::IConst0(r) => {
                    registers[r.index()] = 0;
                },
                Instruction::IConst1(r) => {
                    registers[r.index()] = 1;
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
                        // TODO: store return value in current frame
                        self.pc = frame.return_address;
                    }

                    // halt at the end of all input
                    if self.stack.is_empty() {
                        break;
                    }

                    continue;
                }
            }

            self.pc += 1;
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
            fp: 0,
            pc: 0
        }
    }
}