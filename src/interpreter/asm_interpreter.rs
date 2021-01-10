use crate::asm::instruction::Instruction;
use crate::interpreter::efun::EFUNS;

const MAX_REGISTERS: usize = 1000;

pub struct AsmInterpreter {
    instructions: Vec<Instruction>,
    pub registers: Vec<i64>,
    pc: usize
}

impl AsmInterpreter {
    /// load instructions for evaluation
    pub fn load(&mut self, instructions: &Vec<Instruction>) {
        let mut cloned = instructions.to_vec();
        self.instructions.append(&mut cloned);
    }

    /// evaluate loaded instructions, starting from the current value of the PC
    pub fn eval(&mut self) {
        while let Some(instruction) = self.instructions.get(self.pc) {
            match instruction {
                Instruction::Call(i) => {
                    // TODO: do this correctly
                    match EFUNS.get(&i.name) {
                        Some(efun) => {
                            efun(self, i);
                        },
                        None => unimplemented!()
                    }
                },
                Instruction::IAdd(i) => {
                    self.registers[i.2.value()] =
                        self.registers[i.0.value()] + self.registers[i.1.value()]
                },
                Instruction::IConst(i) => {
                    self.registers[i.0.value()] = i.1;
                },
                Instruction::IConst0(i) => {
                    self.registers[i.0.value()] = 0;
                },
                Instruction::IConst1(i) => {
                    self.registers[i.0.value()] = 1;
                },
                Instruction::IDiv(i) => {
                    self.registers[i.2.value()] =
                        self.registers[i.0.value()] / self.registers[i.1.value()]
                },
                Instruction::ILoad(i) => println!("{}", i),
                Instruction::IMul(i) => {
                    self.registers[i.2.value()] =
                        self.registers[i.0.value()] * self.registers[i.1.value()]
                },
                Instruction::IStore(i) => println!("{}", i),
                Instruction::ISub(i) => {
                    self.registers[i.2.value()] =
                        self.registers[i.0.value()] - self.registers[i.1.value()]
                },
                Instruction::RegCopy(i) => {
                    self.registers[i.1.value()] = self.registers[i.0.value()]
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
            registers: vec![0; MAX_REGISTERS],
            pc: 0
        }
    }
}