use crate::asm::instruction::Instruction;
use crate::interpreter::efun::EFUNS;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::function_symbol::FunctionSymbol;

const MAX_STACK: usize = 1000;

#[derive(Debug)]
pub struct AsmInterpreter {
    instructions: Vec<Instruction>,
    stack: Vec<StackFrame>,
    fp: usize,
    pc: usize
}

impl AsmInterpreter {
    /// load instructions for evaluation
    pub fn load(&mut self, instructions: &[Instruction]) {
        let mut cloned = instructions.to_vec();

        if self.stack.is_empty() {
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
        };

        self.instructions.append(&mut cloned);
    }

    fn push_frame(&mut self, frame: StackFrame) {
        self.stack.push(frame);
    }

    fn pop_frame(&mut self) -> Option<StackFrame> {
        self.stack.pop()
    }

    /// this has undefined behavior if called with an empty stack
    fn current_registers(&mut self) -> &mut Vec<i64> {
        let len = self.stack.len();
        self.stack[len - 1].registers.as_mut()
    }

    /// evaluate loaded instructions, starting from the current value of the PC
    pub fn eval(&mut self) {
        let instructions = self.instructions.clone();
        while let Some(instruction) = instructions.get(self.pc) {
            println!("{:?}", instruction);
            let registers = self.current_registers();

            match instruction {
                Instruction::Call { name, num_args: _, initial_arg } => {
                    // TODO: do this correctly
                    match EFUNS.get(name) {
                        Some(efun) => {
                            efun(&self.stack[self.stack.len() - 1], initial_arg);
                        },
                        None => unimplemented!()
                    }
                },
                Instruction::IAdd(r1, r2, r3) => {
                    registers[r3.value()] =
                        registers[r1.value()] + registers[r2.value()]
                },
                Instruction::IConst(r, i) => {
                    registers[r.value()] = *i;
                },
                Instruction::IConst0(r) => {
                    registers[r.value()] = 0;
                },
                Instruction::IConst1(r) => {
                    registers[r.value()] = 1;
                },
                Instruction::IDiv(r1, r2, r3) => {
                    registers[r3.value()] =
                        registers[r1.value()] / registers[r2.value()]
                },
                Instruction::IMul(r1, r2, r3) => {
                    registers[r3.value()] =
                        registers[r1.value()] * registers[r2.value()]
                },
                Instruction::ISub(r1, r2, r3) => {
                    registers[r3.value()] =
                        registers[r1.value()] - registers[r2.value()]
                },
                Instruction::RegCopy(r1, r2) => {
                    registers[r2.value()] = registers[r1.value()]
                },
                Instruction::Ret => {
                    if let Some(frame) = self.pop_frame() {
                        self.pc = frame.return_address;
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
            stack: Vec::with_capacity(MAX_STACK),
            fp: 0,
            pc: 0
        }
    }
}