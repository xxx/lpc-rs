use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_core::{
    call_namespace::CallNamespace, function::FunctionTarget, function_arity::FunctionArity,
    register::Register, LpcFloat, LpcInt,
};
use serde::{Deserialize, Serialize};
use std::{
    fmt,
    fmt::{Display, Formatter},
};

/// Really just a `pc` index in the vm.
pub type Address = usize;

pub type Label = String;

/// Representation of an assembly language instruction.
/// In general, they are structured as `name(arg1, ...argn, destination)`, a la the AT&T syntax
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Instruction {
    /// Create an array with values from the vector
    AConst(Register, Vec<Register>),

    /// bitwise-and combination.
    /// x.2 = x.0 & x.1
    And(Register, Register, Register),

    /// x.1 = ~x.0
    BitwiseNot(Register, Register),

    /// Call a function
    Call {
        name: String,
        namespace: CallNamespace,
        num_args: usize,
        initial_arg: Register,
    },

    /// Call a function pointer, located in `location`
    CallFp {
        location: Register,
        num_args: usize,
        initial_arg: Register,
    },

    /// Call a function in another object
    CallOther {
        receiver: Register,
        name: Register,
        num_args: usize,
        initial_arg: Register,
    },

    /// Finish a block of instructions that can catch errors and continue execution.
    /// Store the error in x.0, and jump to x.1 to continue execution
    CatchEnd,

    /// Start a block of instructions that can catch errors and continue execution.
    /// Store the error in x.0, and jump to x.1 to continue execution
    CatchStart(Register, Label),

    /// Decrement the value in x.0 by 1
    Dec(Register),

    /// == comparison
    /// x.2 = x.0 == x.1
    EqEq(Register, Register, Register),

    /// Float Constant
    FConst(Register, LpcFloat),

    /// A function pointer constant
    FunctionPtrConst {
        location: Register,
        target: FunctionTarget,
        arity: FunctionArity,
        applied_arguments: Vec<Option<Register>>,
    },

    /// Copy a global from the global registers, into the current stack frame.
    /// Copies *global* register x.0 to *local* register x.1.
    GLoad(Register, Register),

    /// Copy a variable from the current stack frame, to the global registers.
    /// Copies a variable from *local* register x.0, into the *global* register x.1.
    GStore(Register, Register),

    /// Greater than
    /// x.2 = x.0 > x.1
    Gt(Register, Register, Register),

    /// Greater than or equal to
    /// x.2 = x.0 >= x.1
    Gte(Register, Register, Register),

    /// Integer addition - x.2 = x.0 + x.1
    IAdd(Register, Register, Register),

    /// Integer constant
    IConst(Register, LpcInt),

    /// Integer constant 0
    IConst0(Register),

    /// Integer constant 1
    IConst1(Register),

    /// Integer division - x.2 = x.0 / x.1
    IDiv(Register, Register, Register),

    /// Integer modulo division - x.2 = x.0 % x.1
    IMod(Register, Register, Register),

    /// Increment the value in x.0 by 1
    Inc(Register),

    /// Integer division - x.2 = x.0 * x.1
    IMul(Register, Register, Register),

    /// Integer division - x.2 = x.0 - x.1
    ISub(Register, Register, Register),

    /// Unconditional jump
    Jmp(Label),

    /// Jump if the value in the register is not zero (Int or Float)
    Jnz(Register, Label),

    /// Jump if the value in the register is zero (Int or Float)
    Jz(Register, Label),

    /// Load a single item from an array or mapping into a register
    /// x.2 = x.0[x.1]
    Load(Register, Register, Register),

    /// Load the value of a key from a mapping into a register
    /// x.2 = x.0[x.1]
    LoadMappingKey(Register, Register, Register),

    /// Less than
    /// x.2 = x.0 < x.1
    Lt(Register, Register, Register),

    /// Less than or equal to
    /// x.2 = x.0 <= x.1
    Lte(Register, Register, Register),

    /// Create a mapping from the keys and values in the hashmap
    MapConst(Register, IndexMap<Register, Register>),

    /// Addition where at least one side is a reference type, so check at runtime.
    MAdd(Register, Register, Register),

    /// Multiplication where at least one side is a reference type, so check at runtime.
    MMul(Register, Register, Register),

    /// Subtraction where at least one side is a reference type, so check at runtime.
    MSub(Register, Register, Register),

    /// Check if x.0 is equal to 0
    Not(Register, Register),

    /// bitwise | comparison.
    /// x.2 = x.0 | x.1
    Or(Register, Register, Register),

    /// Special case instruction to dynamically populate the `argv` variable
    ///   that is created for ellipsis functions.
    /// `Register` is the location of `argv`.
    /// The first `usize` is the number of formal parameters to the function
    ///   (whether they have default values or not, basically just the count
    ///   of non-ellipsis params).
    /// The second `usize` is the number of local variables used by the function.
    /// We know both of these numbers at compile time, and any other register
    ///   present in the frame is an ellipsis argument, so those are the ones we
    ///   populate.
    PopulateArgv(Register, usize, usize),

    /// Special case instruction to handle calls to functions that have default
    /// argument values.
    /// The first `usize` is the number of formal parameters to the function
    ///   (whether they have default values or not).
    /// The vector is the list of addresses to jump to, to initialize the parameters
    ///   that have default values.
    PopulateDefaults(Vec<Address>),

    /// Create a new value from some range of another value
    /// x.4 = x.1[x.2 .. x.3]
    Range(Register, Register, Register, Register),

    /// Copy x.0 to x.1
    RegCopy(Register, Register),

    /// Return from current function
    Ret,

    /// left shift
    /// x.2 = x.0 << x.1
    Shl(Register, Register, Register),

    /// right shift
    /// x.1 = x.1 >> x.1
    Shr(Register, Register, Register),

    /// Get the size of arrays or mappings
    /// x.1 = sizeof(x.0)
    Sizeof(Register, Register),

    /// Store a single item into an array or mapping
    /// x.1[x.2] = x.0
    Store(Register, Register, Register),

    /// String constant.
    /// Store an index into the program's ConstantPool in the passed register
    SConst(Register, String),

    /// bitwise ^ comparison.
    /// x.2 = x.0 ^ x.1
    Xor(Register, Register, Register),
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
            Instruction::And(r1, r2, r3) => {
                write!(f, "and {}, {}, {}", r1, r2, r3)
            }
            Instruction::BitwiseNot(r1, r2) => {
                write!(f, "bitwisenot {}, {}", r1, r2)
            }
            Instruction::CatchEnd => {
                write!(f, "catchend")
            }
            Instruction::CatchStart(r1, label) => {
                write!(f, "catchstart {}, {}", r1, label)
            }
            Instruction::Call {
                name,
                namespace,
                num_args,
                initial_arg,
            } => {
                write!(
                    f,
                    "call {}, {}, {}, {}",
                    name, namespace, num_args, initial_arg
                )
            }
            Instruction::CallFp {
                location,
                num_args,
                initial_arg,
            } => {
                write!(f, "callfp {}, {}, {}", location, num_args, initial_arg)
            }
            Instruction::CallOther {
                receiver,
                name,
                num_args,
                initial_arg,
            } => {
                write!(
                    f,
                    "callother {}, {}, {}, {}",
                    receiver, name, num_args, initial_arg
                )
            }
            Instruction::Dec(r) => {
                write!(f, "dec {}", r)
            }
            Instruction::EqEq(r1, r2, r3) => {
                write!(f, "eqeq {}, {}, {}", r1, r2, r3)
            }
            Instruction::FConst(r, fl) => {
                write!(f, "fconst {}, {}", r, fl)
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
                            format!("{}", reg)
                        } else {
                            "None".into()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "functionptrconst {}, {}, {}", location, target, args)
            }
            Instruction::GLoad(r1, r2) => {
                write!(f, "gload {}, {}", r1, r2)
            }
            Instruction::GStore(r1, r2) => {
                write!(f, "gstore {}, {}", r1, r2)
            }
            Instruction::Gt(r1, r2, r3) => {
                write!(f, "gt {}, {}, {}", r1, r2, r3)
            }
            Instruction::Gte(r1, r2, r3) => {
                write!(f, "gte {}, {}, {}", r1, r2, r3)
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
            Instruction::IDiv(r1, r2, r3) => {
                write!(f, "idiv {}, {}, {}", r1, r2, r3)
            }
            Instruction::IMod(r1, r2, r3) => {
                write!(f, "imod {}, {}, {}", r1, r2, r3)
            }
            Instruction::Inc(r) => {
                write!(f, "inc {}", r)
            }
            Instruction::IMul(r1, r2, r3) => {
                write!(f, "imul {}, {}, {}", r1, r2, r3)
            }
            Instruction::ISub(r1, r2, r3) => {
                write!(f, "isub {}, {}, {}", r1, r2, r3)
            }
            Instruction::Jmp(address) => {
                write!(f, "jmp {}", address)
            }
            Instruction::Jnz(r1, address) => {
                write!(f, "jnz {}, {}", r1, address)
            }
            Instruction::Jz(r1, address) => {
                write!(f, "jz {}, {}", r1, address)
            }
            Instruction::Load(r1, r2, r3) => {
                write!(f, "load {}, {}, {}", r1, r2, r3)
            }
            Instruction::LoadMappingKey(r1, r2, r3) => {
                write!(f, "loadmappingkey {}, {}, {}", r1, r2, r3)
            }
            Instruction::Lt(r1, r2, r3) => {
                write!(f, "lt {}, {}, {}", r1, r2, r3)
            }
            Instruction::Lte(r1, r2, r3) => {
                write!(f, "lte {}, {}, {}", r1, r2, r3)
            }
            Instruction::MapConst(r, i) => {
                let str = i
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "mapconst {}, {}", r, str)
            }
            Instruction::MAdd(r1, r2, r3) => {
                write!(f, "madd {}, {}, {}", r1, r2, r3)
            }
            Instruction::MMul(r1, r2, r3) => {
                write!(f, "mmul {}, {}, {}", r1, r2, r3)
            }
            Instruction::MSub(r1, r2, r3) => {
                write!(f, "msub {}, {}, {}", r1, r2, r3)
            }
            Instruction::Not(r1, r2) => {
                write!(f, "not {}, {}", r1, r2)
            }
            Instruction::Or(r1, r2, r3) => {
                write!(f, "or {}, {}, {}", r1, r2, r3)
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                write!(f, "populateargv {}, {}, {}", r, num_args, num_locals)
            }
            Instruction::PopulateDefaults(default_inits) => {
                let s = default_inits.iter().map(|i| format!("{}", i)).join(", ");

                write!(f, "populatedefaults {}", s)
            }
            Instruction::Range(r1, r2, r3, r4) => {
                write!(f, "range {}, {}, {}, {}", r1, r2, r3, r4)
            }
            Instruction::RegCopy(r1, r2) => {
                write!(f, "regcopy {}, {}", r1, r2)
            }
            Instruction::Ret => {
                write!(f, "ret")
            }
            Instruction::Shl(r1, r2, r3) => {
                write!(f, "shl {}, {}, {}", r1, r2, r3)
            }
            Instruction::Shr(r1, r2, r3) => {
                write!(f, "shr {}, {}, {}", r1, r2, r3)
            }
            Instruction::Sizeof(r1, r2) => {
                write!(f, "sizeof {}, {}", r1, r2)
            }
            Instruction::Store(r1, r2, r3) => {
                write!(f, "store {}, {}, {}", r1, r2, r3)
            }
            Instruction::SConst(r, s) => {
                write!(f, "sconst {}, \"{}\"", r, s)
            }
            Instruction::Xor(r1, r2, r3) => {
                write!(f, "xor {}, {}, {}", r1, r2, r3)
            }
        }
    }
}