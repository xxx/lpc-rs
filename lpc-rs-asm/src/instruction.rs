use std::{
    fmt,
    fmt::{Display, Formatter},
};

use indexmap::IndexMap;
use itertools::Itertools;
use lpc_rs_core::{
    function::FunctionTarget, function_arity::FunctionArity, LpcFloat, LpcInt,
    register::RegisterVariant,
};
use serde::{Deserialize, Serialize};
use crate::jump_location::{Address, JumpLocation, Label};

/// Representation of an assembly language instruction.
/// In general, they are structured as `name(arg1, ...argn, destination)`, a la
/// the AT&T syntax
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Instruction {
    /// Create an array with values from the vector
    AConst(RegisterVariant),

    /// bitwise-and combination.
    /// x.2 = x.0 & x.1
    And(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Push the location into the Task's `args` vector
    Arg(RegisterVariant),

    /// x.1 = ~x.0
    BitwiseNot(RegisterVariant, RegisterVariant),

    /// Call a function in the current object.
    /// The `usize` is an index into the object's `strings` table.
    Call(usize),

    /// Call an Efun. The `usize` is the index into the object's `strings` table.
    // TODO: maybe index directly into a collection of efuns, which saves 1 lookup.
    //       This can potentially break programs that have been pre-compiled and cached to disk.
    CallEfun(usize),

    /// Call a function pointer, located in `location`
    CallFp { location: RegisterVariant },

    /// Call a function in another object
    CallOther {
        receiver: RegisterVariant,
        name: RegisterVariant,
    },

    /// Finish a block of instructions that can catch errors and continue
    /// execution.
    CatchEnd,

    /// Start a block of instructions that can catch errors and continue
    /// execution. Store the error in x.0, and jump to x.1 to continue
    /// execution
    CatchStart(RegisterVariant, Label),

    /// Clear the [`Task`]'s `args` vector, in preparation for a new call
    ClearArgs,

    /// Clear the [`Task`]'s `array_items` vector, in preparation for a new array constant
    ClearArrayItems,

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
        applied_arguments: Vec<Option<RegisterVariant>>,
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
    Jmp(JumpLocation),

    /// Jump if the value in the register is not zero (Int or Float)
    Jnz(RegisterVariant, JumpLocation),

    /// Jump if the value in the register is zero (Int or Float)
    Jz(RegisterVariant, JumpLocation),

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
    /// `RegisterVariant` is the location of `argv`.
    /// The first `usize` is the number of formal parameters to the function
    ///   (whether they have default values or not, basically just the count
    ///   of non-ellipsis params).
    /// The second `usize` is the number of local variables used by the
    /// function. We know both of these numbers at compile time, and any
    /// other register present in the frame is an ellipsis argument, so
    /// those are the ones we populate.
    PopulateArgv(RegisterVariant, usize, usize),

    /// Special case instruction to handle calls to functions that have default
    /// argument values.
    /// The first `usize` is the number of formal parameters to the function
    ///   (whether they have default values or not).
    /// The vector is the list of addresses to jump to, to initialize the
    /// parameters that have default values.
    PopulateDefaults(Vec<Address>),

    /// Push a location onto the [`Task`]'s `array_items` vector, used for creating
    /// array literals
    PushArrayItem(RegisterVariant),

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
    /// Store an index into the [`Program`]'s `strings` vector.
    SConst(RegisterVariant, usize),

    /// bitwise ^ comparison.
    /// x.2 = x.0 ^ x.1
    Xor(RegisterVariant, RegisterVariant, RegisterVariant),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::AConst(r1) => {
                write!(f, "aconst {r1}")
            }
            Instruction::And(r1, r2, r3) => {
                write!(f, "and {r1}, {r2}, {r3}")
            }
            Instruction::Arg(r) => {
                write!(f, "arg {r}")
            }
            Instruction::BitwiseNot(r1, r2) => {
                write!(f, "bitwise_not {r1}, {r2}")
            }
            Instruction::CatchEnd => {
                write!(f, "catch_end")
            }
            Instruction::CatchStart(r1, label) => {
                write!(f, "catch_start {r1}, {label}")
            }
            Instruction::Call(name) => {
                write!(f, "call {name}")
            }
            Instruction::CallEfun(name) => {
                write!(f, "call_efun {name}")
            }
            Instruction::CallFp { location } => {
                write!(f, "call_fp {location}")
            }
            Instruction::CallOther { receiver, name } => {
                write!(f, "call_other {receiver}, {name}")
            }
            Instruction::ClearArgs => {
                write!(f, "clear_args")
            }
            Instruction::ClearArrayItems => {
                write!(f, "clear_array_items")
            }
            Instruction::Dec(r) => {
                write!(f, "dec {r}")
            }
            Instruction::EqEq(r1, r2, r3) => {
                write!(f, "eq_eq {r1}, {r2}, {r3}")
            }
            Instruction::FConst(r, fl) => {
                write!(f, "f_const {r}, {fl}")
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
                write!(f, "function_ptr_const {location}, {target}, {args}")
            }
            Instruction::Gt(r1, r2, r3) => {
                write!(f, "gt {r1}, {r2}, {r3}")
            }
            Instruction::Gte(r1, r2, r3) => {
                write!(f, "gte {r1}, {r2}, {r3}")
            }
            Instruction::IAdd(r1, r2, r3) => {
                write!(f, "i_add {r1}, {r2}, {r3}")
            }
            Instruction::IConst(r, i) => {
                write!(f, "i_const {r}, {i}")
            }
            Instruction::IConst0(r) => {
                write!(f, "i_const0 {r}")
            }
            Instruction::IConst1(r) => {
                write!(f, "i_const1 {r}")
            }
            Instruction::IDiv(r1, r2, r3) => {
                write!(f, "i_div {r1}, {r2}, {r3}")
            }
            Instruction::IMod(r1, r2, r3) => {
                write!(f, "i_mod {r1}, {r2}, {r3}")
            }
            Instruction::Inc(r) => {
                write!(f, "inc {r}")
            }
            Instruction::IMul(r1, r2, r3) => {
                write!(f, "i_mul {r1}, {r2}, {r3}")
            }
            Instruction::ISub(r1, r2, r3) => {
                write!(f, "i_sub {r1}, {r2}, {r3}")
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
                write!(f, "load_mapping_key {r1}, {r2}, {r3}")
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
                write!(f, "map_const {r}, {str}")
            }
            Instruction::MAdd(r1, r2, r3) => {
                write!(f, "m_add {r1}, {r2}, {r3}")
            }
            Instruction::MMul(r1, r2, r3) => {
                write!(f, "m_mul {r1}, {r2}, {r3}")
            }
            Instruction::MSub(r1, r2, r3) => {
                write!(f, "m_sub {r1}, {r2}, {r3}")
            }
            Instruction::Not(r1, r2) => {
                write!(f, "not {r1}, {r2}")
            }
            Instruction::Or(r1, r2, r3) => {
                write!(f, "or {r1}, {r2}, {r3}")
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                write!(f, "populate_argv {r}, {num_args}, {num_locals}")
            }
            Instruction::PopulateDefaults(default_inits) => {
                let s = default_inits.iter().map(|i| format!("{i}")).join(", ");

                write!(f, "populate_defaults {s}")
            }
            Instruction::PushArrayItem(r1) => {
                write!(f, "push_array_item {r1}")
            }
            Instruction::Range(r1, r2, r3, r4) => {
                write!(f, "range {r1}, {r2}, {r3}, {r4}")
            }
            Instruction::RegCopy(r1, r2) => {
                write!(f, "reg_copy {r1}, {r2}")
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
                write!(f, "s_const {r}, {s}")
            }
            Instruction::Xor(r1, r2, r3) => {
                write!(f, "xor {r1}, {r2}, {r3}")
            }
        }
    }
}
