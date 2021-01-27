use std::fmt::{Formatter,Display};
use std::fmt;
use crate::asm::register::Register;

pub trait InstructionTrait: Display {}

/// Representation of an assembly language instruction.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Instruction {
    /// Function calls
    Call {
        name: String,
        num_args: usize,
        initial_arg: Register
    },
    /// Integer addition - x.2 = x.0 + x.1
    IAdd(Register, Register, Register),
    /// Integer constant
    IConst(Register, i64),
    /// Integer constant 0
    IConst0(Register),
    /// Integer constant 1
    IConst1(Register),
    /// String constant
    SConst(Register, String),
    /// Integer division - x.2 = x.0 / x.1
    IDiv(Register, Register, Register),
    /// Integer division - x.2 = x.0 * x.1
    IMul(Register, Register, Register),
    /// Integer division - x.2 = x.0 - x.1
    ISub(Register, Register, Register),
    /// Copy x.0 to x.1
    RegCopy(Register, Register),
    /// Return from current function
    Ret,
    /// String addition (concatenation) - x.2 = x.0 + x.1
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
