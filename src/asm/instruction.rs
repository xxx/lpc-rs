use crate::{asm::register::Register, LpcFloat, LpcInt};
use std::{
    collections::HashMap,
    fmt,
    fmt::{Display, Formatter},
};

/// Representation of an assembly language instruction.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Instruction {
    /// Create an array with values from the vector
    AConst(Register, Vec<Register>),

    /// Create an array from some range of another array
    /// x.4 = x.1[x.2 .. x.3]
    ARange(Register, Register, Register, Register),

    /// Call a function
    Call {
        name: String,
        num_args: usize,
        initial_arg: Register,
    },

    /// Float Constant
    FConst(Register, LpcFloat),

    /// Copy a global from the global registers, into the current stack frame.
    /// Copies *global* register x.0 to *local* register x.1.
    GLoad(Register, Register),

    /// Copy a variable from the current stack frame, to the global registers.
    /// Copies a variable from *local* register x.0, into the *global* register x.1.
    GStore(Register, Register),

    /// Integer addition - x.2 = x.0 + x.1
    IAdd(Register, Register, Register),

    /// Integer constant
    IConst(Register, LpcInt),

    /// Integer constant 0
    IConst0(Register),

    /// Integer constant 1
    IConst1(Register),

    /// Load a single item from an array or mapping into a register
    /// x.2 = x.0[x.1]
    Load(Register, Register, Register),

    /// Create a mapping from the keys and values in the hashmap
    MapConst(Register, HashMap<Register, Register>),

    /// String constant.
    /// Store an index into the program's ConstantPool in the passed register
    SConst(Register, usize),

    /// Integer division - x.2 = x.0 / x.1
    IDiv(Register, Register, Register),

    /// Integer division - x.2 = x.0 * x.1
    IMul(Register, Register, Register),

    /// Integer division - x.2 = x.0 - x.1
    ISub(Register, Register, Register),

    /// Addition where at least one side is a reference type, so check at runtime.
    MAdd(Register, Register, Register),

    /// Division where at least one side is a reference type, so check at runtime.
    MDiv(Register, Register, Register),

    /// Multiplication where at least one side is a reference type, so check at runtime.
    MMul(Register, Register, Register),

    /// Subtraction where at least one side is a reference type, so check at runtime.
    MSub(Register, Register, Register),

    /// Copy x.0 to x.1
    RegCopy(Register, Register),

    /// Return from current function
    Ret,

    /// Store a single item into an array or mapping
    /// x.1[x.2] = x.0
    Store(Register, Register, Register),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::AConst(r1, vec) => {
                let s = vec
                    .iter()
                    .map(|i| format!("{}", i))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "aconst {}, {}", r1, s)
            }
            Instruction::ARange(r1, r2, r3, r4) => {
                write!(f, "arange {}, {}, {}, {}", r1, r2, r3, r4)
            }
            Instruction::GLoad(r1, r2) => {
                write!(f, "gload {}, {}", r1, r2)
            }
            Instruction::GStore(r1, r2) => {
                write!(f, "gstore {}, {}", r1, r2)
            }
            Instruction::Call {
                name,
                num_args,
                initial_arg,
            } => {
                write!(f, "call {}, {}, {}", name, num_args, initial_arg)
            }
            Instruction::FConst(r, fl) => {
                write!(f, "fconst {}, {}", r, fl)
            }
            Instruction::IAdd(r1, r2, r3) => {
                write!(f, "iadd {}, {}, {}", r1, r2, r3)
            }
            Instruction::IConst(r, i) => {
                write!(f, "iconst {}, {}", r, i)
            }
            Instruction::IConst0(r) => {
                write!(f, "iconst0 {}", r)
            }
            Instruction::IConst1(r) => {
                write!(f, "iconst1 {}", r)
            }
            Instruction::Load(r1, r2, r3) => {
                write!(f, "load {}, {}, {}", r1, r2, r3)
            }
            Instruction::SConst(r, i) => {
                write!(f, "sconst {}, {}", r, i)
            }
            Instruction::MapConst(r, i) => {
                let str = i
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "mapconst {}, {}", r, str)
            }
            Instruction::IDiv(r1, r2, r3) => {
                write!(f, "idiv {}, {}, {}", r1, r2, r3)
            }
            Instruction::IMul(r1, r2, r3) => {
                write!(f, "imul {}, {}, {}", r1, r2, r3)
            }
            Instruction::ISub(r1, r2, r3) => {
                write!(f, "isub {}, {}, {}", r1, r2, r3)
            }
            Instruction::MAdd(r1, r2, r3) => {
                write!(f, "madd {}, {}, {}", r1, r2, r3)
            }
            Instruction::MDiv(r1, r2, r3) => {
                write!(f, "mdiv {}, {}, {}", r1, r2, r3)
            }
            Instruction::MMul(r1, r2, r3) => {
                write!(f, "mmul {}, {}, {}", r1, r2, r3)
            }
            Instruction::MSub(r1, r2, r3) => {
                write!(f, "msub {}, {}, {}", r1, r2, r3)
            }
            Instruction::RegCopy(r1, r2) => {
                write!(f, "regcopy {}, {}", r1, r2)
            }
            Instruction::Ret => {
                write!(f, "ret")
            },
            Instruction::Store(r1, r2, r3) => {
                write!(f, "store {}, {}, {}", r1, r2, r3)
            }
        }
    }
}
