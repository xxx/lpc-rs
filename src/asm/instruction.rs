use std::fmt::{Formatter,Display};
use std::fmt;
use crate::asm::register::Register;

pub trait InstructionTrait: Display {}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Instruction {
    Call {
        name: String,
        num_args: usize,
        initial_arg: Register
    },
    IAdd(Register, Register, Register),
    IConst(Register, i64),
    IConst0(Register),
    IConst1(Register),
    SConst(Register, String),
    IDiv(Register, Register, Register),
    IMul(Register, Register, Register),
    ISub(Register, Register, Register),
    RegCopy(Register, Register),
    Ret,
    SAdd(Register, Register, Register),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::Call { name, num_args, initial_arg } => {
                write!(f, "call {}, {}, {}", name, num_args, initial_arg)
            },
            Instruction::IAdd(r1, r2, r3) => {
                write!(f, "iadd {}, {}, {}", r1, r2, r3)
            },
            Instruction::IConst(r, i) => {
                write!(f, "iconst {}, {}", r, i)
            },
            Instruction::IConst0(r) => {
                write!(f, "iconst0 {}", r)
            },
            Instruction::SConst(r, s) => {
                write!(f, "sconst {}, \"{}\"", r, s)
            },
            Instruction::IConst1(r) => {
                write!(f, "iconst1 {}", r)
            },
            Instruction::IDiv(r1, r2, r3) => {
                write!(f, "idiv {}, {}, {}", r1, r2, r3)
            },
            Instruction::IMul(r1, r2, r3) => {
                write!(f, "imul {}, {}, {}", r1, r2, r3)
            },
            Instruction::ISub(r1, r2, r3) => {
                write!(f, "isub {}, {}, {}", r1, r2, r3)
            },
            Instruction::RegCopy(r1, r2) => {
                write!(f, "regcopy {}, {}", r1, r2)
            },
            Instruction::Ret => {
                write!(f, "ret")
            },
            Instruction::SAdd(r1, r2, r3) => {
                write!(f, "sadd {}, {}, {}", r1, r2, r3)
            },
        }
    }
}
