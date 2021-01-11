use crate::asm::instruction::Instruction;
use crate::interpreter::efun::EFUNS;
use crate::interpreter::stack_frame::StackFrame;
use crate::interpreter::function_symbol::FunctionSymbol;

const MAX_STACK: usize = 1000;

#[derive(Debug)]
pub struct AsmInterpreter {
    instructions: Vec<Instruction>,
    stack: Vec<StackFrame>,
    sp: usize,
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
            self.sp = 0;
        };

        self.instructions.append(&mut cloned);
    }

    fn current_registers(&mut self) -> &mut Vec<i64> {
        self.stack[self.sp].registers.as_mut()
    }

    /// evaluate loaded instructions, starting from the current value of the PC
    pub fn eval(&mut self) {
        let instructions = self.instructions.clone();
        while let Some(instruction) = instructions.get(self.pc) {
            let registers = self.current_registers();

            match instruction {
                Instruction::Call(i) => {
                    // TODO: do this correctly
                    match EFUNS.get(&i.name) {
                        Some(efun) => {
                            efun(&self.stack[self.sp], i);
                        },
                        None => unimplemented!()
                    }
                },
                Instruction::IAdd(i) => {
                    registers[i.2.value()] =
                        registers[i.0.value()] + registers[i.1.value()]
                },
                Instruction::IConst(i) => {
                    registers[i.0.value()] = i.1;
                },
                Instruction::IConst0(i) => {
                    registers[i.0.value()] = 0;
                },
                Instruction::IConst1(i) => {
                    registers[i.0.value()] = 1;
                },
                Instruction::IDiv(i) => {
                    registers[i.2.value()] =
                        registers[i.0.value()] / registers[i.1.value()]
                },
                Instruction::ILoad(i) => println!("{}", i),
                Instruction::IMul(i) => {
                    registers[i.2.value()] =
                        registers[i.0.value()] * registers[i.1.value()]
                },
                Instruction::IStore(i) => println!("{}", i),
                Instruction::ISub(i) => {
                    registers[i.2.value()] =
                        registers[i.0.value()] - registers[i.1.value()]
                },
                Instruction::RegCopy(i) => {
                    registers[i.1.value()] = registers[i.0.value()]
                },
                Instruction::Ret(_) => {
                    // pop stack frame, jump to return address
                    let frame = &self.stack[self.sp];
                    self.sp -= 1;
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
            sp: 0,
            pc: 0
        }
    }
}