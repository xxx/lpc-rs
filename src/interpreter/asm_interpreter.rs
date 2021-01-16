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
    pub fn load(&mut self, instructions: &Vec<Instruction>) {
        let mut cloned = instructions.to_vec();

        if self.stack.is_empty() {
            let main = StackFrame::new(
               FunctionSymbol {
                    name: "main".to_string(),
                    num_args: 0,
                    num_locals: 0,
                    address: 0
                },
                0
            );
            self.stack.push(main);
            self.fp = 0;
        };

        self.instructions.append(&mut cloned);
    }

    fn current_registers(&mut self) -> &mut Vec<i64> {
        self.stack[self.fp].registers.as_mut()
    }

    /// evaluate loaded instructions, starting from the current value of the PC
    pub fn eval(&mut self) {
        let instructions = self.instructions.clone();
        while let Some(instruction) = instructions.get(self.pc) {
            let registers = self.current_registers();

            match instruction {
                Instruction::Call { name, num_args: _, initial_arg } => {
                    // TODO: do this correctly
                    match EFUNS.get(name) {
                        Some(efun) => {
                            efun(&self.stack[self.fp], initial_arg);
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
                    // pop stack frame, jump to return address
                    let frame = &self.stack[self.fp];
                    self.fp -= 1;
                    self.pc = frame.return_address;
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