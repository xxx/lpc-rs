use std::{
    fmt,
    fmt::{Display, Formatter},
};

use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_core::{
    call_namespace::CallNamespace, function::FunctionTarget, function_arity::FunctionArity,
    register::RegisterVariant, LpcFloat, LpcInt,
};
use serde::{Deserialize, Serialize};

/// Really just a `pc` index in the vm.
pub type Address = usize;

pub type Label = String;

/// Representation of an assembly language instruction.
/// In general, they are structured as `name(arg1, ...argn, destination)`, a la
/// the AT&T syntax
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Instruction {
    /// Create an array with values from the vector
    AConst(RegisterVariant, Vec<RegisterVariant>),

    /// bitwise-and combination.
    /// x.2 = x.0 & x.1
    And(RegisterVariant, RegisterVariant, RegisterVariant),

    /// x.1 = ~x.0
    BitwiseNot(RegisterVariant, RegisterVariant),

    /// Call a function
    Call {
        name: String,
        namespace: CallNamespace,
        num_args: usize,
        initial_arg: RegisterVariant,
    },

    /// Call a function pointer, located in `location`
    CallFp {
        location: RegisterVariant,
        num_args: usize,
        initial_arg: RegisterVariant,
    },

    /// Call a function in another object
    CallOther {
        receiver: RegisterVariant,
        name: RegisterVariant,
        num_args: usize,
        initial_arg: RegisterVariant,
    },

    /// Finish a block of instructions that can catch errors and continue
    /// execution. Store the error in x.0, and jump to x.1 to continue
    /// execution
    CatchEnd,

    /// Start a block of instructions that can catch errors and continue
    /// execution. Store the error in x.0, and jump to x.1 to continue
    /// execution
    CatchStart(RegisterVariant, Label),

    /// Decrement the value in x.0 by 1
    Dec(RegisterVariant),

    /// == comparison
    /// x.2 = x.0 == x.1
    EqEq(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Float Constant
    FConst(RegisterVariant, LpcFloat),

    /// A function pointer constant
    FunctionPtrConst {
        location: RegisterVariant,
        target: FunctionTarget,
        arity: FunctionArity,
        applied_arguments: Vec<Option<RegisterVariant>>, // TODO: should this be RegisterVariant?
    },

    /// Greater than
    /// x.2 = x.0 > x.1
    Gt(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Greater than or equal to
    /// x.2 = x.0 >= x.1
    Gte(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Integer addition - x.2 = x.0 + x.1
    IAdd(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Integer constant
    IConst(RegisterVariant, LpcInt),

    /// Integer constant 0
    IConst0(RegisterVariant),

    /// Integer constant 1
    IConst1(RegisterVariant),

    /// Integer division - x.2 = x.0 / x.1
    IDiv(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Integer modulo division - x.2 = x.0 % x.1
    IMod(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Increment the value in x.0 by 1
    Inc(RegisterVariant),

    /// Integer division - x.2 = x.0 * x.1
    IMul(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Integer division - x.2 = x.0 - x.1
    ISub(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Unconditional jump
    Jmp(Label),

    /// Jump if the value in the register is not zero (Int or Float)
    Jnz(RegisterVariant, Label),

    /// Jump if the value in the register is zero (Int or Float)
    Jz(RegisterVariant, Label),

    /// Load a single item from an array or mapping into a register
    /// x.2 = x.0[x.1]
    Load(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Load the value of a key from a mapping into a register
    /// x.2 = x.0[x.1]
    LoadMappingKey(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Less than
    /// x.2 = x.0 < x.1
    Lt(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Less than or equal to
    /// x.2 = x.0 <= x.1
    Lte(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Create a mapping from the keys and values in the hashmap
    MapConst(RegisterVariant, IndexMap<RegisterVariant, RegisterVariant>),

    /// Addition where at least one side is a reference type, so check at
    /// runtime.
    MAdd(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Multiplication where at least one side is a reference type, so check at
    /// runtime.
    MMul(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Subtraction where at least one side is a reference type, so check at
    /// runtime.
    MSub(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Check if x.0 is equal to 0
    Not(RegisterVariant, RegisterVariant),

    /// bitwise | comparison.
    /// x.2 = x.0 | x.1
    Or(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Special case instruction to dynamically populate the `argv` variable
    ///   that is created for ellipsis functions.
    /// `Register` is the location of `argv`.
    /// The first `usize` is the number of formal parameters to the function
    ///   (whether they have default values or not, basically just the count
    ///   of non-ellipsis params).
    /// The second `usize` is the number of local variables used by the
    /// function. We know both of these numbers at compile time, and any
    /// other register   present in the frame is an ellipsis argument, so
    /// those are the ones we   populate.
    PopulateArgv(RegisterVariant, usize, usize),

    /// Special case instruction to handle calls to functions that have default
    /// argument values.
    /// The first `usize` is the number of formal parameters to the function
    ///   (whether they have default values or not).
    /// The vector is the list of addresses to jump to, to initialize the
    /// parameters   that have default values.
    PopulateDefaults(Vec<Address>),

    /// Create a new value from some range of another value
    /// x.4 = x.1[x.2 .. x.3]
    Range(
        RegisterVariant,
        RegisterVariant,
        RegisterVariant,
        RegisterVariant,
    ),

    /// Copy x.0 to x.1
    RegCopy(RegisterVariant, RegisterVariant),

    /// Return from current function
    Ret,

    /// left shift
    /// x.2 = x.0 << x.1
    Shl(RegisterVariant, RegisterVariant, RegisterVariant),

    /// right shift
    /// x.1 = x.1 >> x.1
    Shr(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Get the size of arrays or mappings
    /// x.1 = sizeof(x.0)
    Sizeof(RegisterVariant, RegisterVariant),

    /// Store a single item into an array or mapping
    /// x.1[x.2] = x.0
    Store(RegisterVariant, RegisterVariant, RegisterVariant),

    /// String constant.
    /// Store an index into the program's ConstantPool in the passed register
    SConst(RegisterVariant, String),

    /// bitwise ^ comparison.
    /// x.2 = x.0 ^ x.1
    Xor(RegisterVariant, RegisterVariant, RegisterVariant),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::AConst(r1, vec) => {
                let s = vec
                    .iter()
                    .map(|i| format!("{i}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "aconst {r1}, {s}")
            }
            Instruction::And(r1, r2, r3) => {
                write!(f, "and {r1}, {r2}, {r3}")
            }
            Instruction::BitwiseNot(r1, r2) => {
                write!(f, "bitwisenot {r1}, {r2}")
            }
            Instruction::CatchEnd => {
                write!(f, "catchend")
            }
            Instruction::CatchStart(r1, label) => {
                write!(f, "catchstart {r1}, {label}")
            }
            Instruction::Call {
                name,
                namespace,
                num_args,
                initial_arg,
            } => {
                write!(
                    f,
                    "call {name}, {namespace}, {num_args}, {initial_arg}"
                )
            }
            Instruction::CallFp {
                location,
                num_args,
                initial_arg,
            } => {
                write!(f, "callfp {location}, {num_args}, {initial_arg}")
            }
            Instruction::CallOther {
                receiver,
                name,
                num_args,
                initial_arg,
            } => {
                write!(
                    f,
                    "callother {receiver}, {name}, {num_args}, {initial_arg}"
                )
            }
            Instruction::Dec(r) => {
                write!(f, "dec {r}")
            }
            Instruction::EqEq(r1, r2, r3) => {
                write!(f, "eqeq {r1}, {r2}, {r3}")
            }
            Instruction::FConst(r, fl) => {
                write!(f, "fconst {r}, {fl}")
            }
            Instruction::FunctionPtrConst {
                location,
                target,
                applied_arguments,
                arity: _arity,
            } => {
                let args = applied_arguments
                    .iter()
                    .map(|i| {
                        if let Some(reg) = i {
                            format!("{reg}")
                        } else {
                            "None".into()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "functionptrconst {location}, {target}, {args}")
            }
            Instruction::Gt(r1, r2, r3) => {
                write!(f, "gt {r1}, {r2}, {r3}")
            }
            Instruction::Gte(r1, r2, r3) => {
                write!(f, "gte {r1}, {r2}, {r3}")
            }
            Instruction::IAdd(r1, r2, r3) => {
                write!(f, "iadd {r1}, {r2}, {r3}")
            }
            Instruction::IConst(r, i) => {
                write!(f, "iconst {r}, {i}")
            }
            Instruction::IConst0(r) => {
                write!(f, "iconst0 {r}")
            }
            Instruction::IConst1(r) => {
                write!(f, "iconst1 {r}")
            }
            Instruction::IDiv(r1, r2, r3) => {
                write!(f, "idiv {r1}, {r2}, {r3}")
            }
            Instruction::IMod(r1, r2, r3) => {
                write!(f, "imod {r1}, {r2}, {r3}")
            }
            Instruction::Inc(r) => {
                write!(f, "inc {r}")
            }
            Instruction::IMul(r1, r2, r3) => {
                write!(f, "imul {r1}, {r2}, {r3}")
            }
            Instruction::ISub(r1, r2, r3) => {
                write!(f, "isub {r1}, {r2}, {r3}")
            }
            Instruction::Jmp(address) => {
                write!(f, "jmp {address}")
            }
            Instruction::Jnz(r1, address) => {
                write!(f, "jnz {r1}, {address}")
            }
            Instruction::Jz(r1, address) => {
                write!(f, "jz {r1}, {address}")
            }
            Instruction::Load(r1, r2, r3) => {
                write!(f, "load {r1}, {r2}, {r3}")
            }
            Instruction::LoadMappingKey(r1, r2, r3) => {
                write!(f, "loadmappingkey {r1}, {r2}, {r3}")
            }
            Instruction::Lt(r1, r2, r3) => {
                write!(f, "lt {r1}, {r2}, {r3}")
            }
            Instruction::Lte(r1, r2, r3) => {
                write!(f, "lte {r1}, {r2}, {r3}")
            }
            Instruction::MapConst(r, i) => {
                let str = i
                    .iter()
                    .map(|(key, value)| format!("{key}: {value}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "mapconst {r}, {str}")
            }
            Instruction::MAdd(r1, r2, r3) => {
                write!(f, "madd {r1}, {r2}, {r3}")
            }
            Instruction::MMul(r1, r2, r3) => {
                write!(f, "mmul {r1}, {r2}, {r3}")
            }
            Instruction::MSub(r1, r2, r3) => {
                write!(f, "msub {r1}, {r2}, {r3}")
            }
            Instruction::Not(r1, r2) => {
                write!(f, "not {r1}, {r2}")
            }
            Instruction::Or(r1, r2, r3) => {
                write!(f, "or {r1}, {r2}, {r3}")
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                write!(f, "populateargv {r}, {num_args}, {num_locals}")
            }
            Instruction::PopulateDefaults(default_inits) => {
                let s = default_inits.iter().map(|i| format!("{i}")).join(", ");

                write!(f, "populatedefaults {s}")
            }
            Instruction::Range(r1, r2, r3, r4) => {
                write!(f, "range {r1}, {r2}, {r3}, {r4}")
            }
            Instruction::RegCopy(r1, r2) => {
                write!(f, "regcopy {r1}, {r2}")
            }
            Instruction::Ret => {
                write!(f, "ret")
            }
            Instruction::Shl(r1, r2, r3) => {
                write!(f, "shl {r1}, {r2}, {r3}")
            }
            Instruction::Shr(r1, r2, r3) => {
                write!(f, "shr {r1}, {r2}, {r3}")
            }
            Instruction::Sizeof(r1, r2) => {
                write!(f, "sizeof {r1}, {r2}")
            }
            Instruction::Store(r1, r2, r3) => {
                write!(f, "store {r1}, {r2}, {r3}")
            }
            Instruction::SConst(r, s) => {
                write!(f, "sconst {r}, \"{s}\"")
            }
            Instruction::Xor(r1, r2, r3) => {
                write!(f, "xor {r1}, {r2}, {r3}")
            }
        }
    }
}
