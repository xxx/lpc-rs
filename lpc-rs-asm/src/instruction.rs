use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_core::{RegisterSize, function_receiver::FunctionReceiver, register::RegisterVariant};
use lpc_rs_errors::{Result, lpc_bug};
use ustr::Ustr;

use crate::address::Address;

/// Representation of an assembly language instruction.
/// In general, they are structured as `name(arg1, ...argn, destination)`, a la
/// the AT&T syntax
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    /// Create an array with values from the vector
    AConst(RegisterVariant),

    /// x.2 = x.0 + x.1
    Add(RegisterVariant, RegisterVariant, RegisterVariant),

    /// bitwise-and combination.
    /// x.2 = x.0 & x.1
    And(RegisterVariant, RegisterVariant, RegisterVariant),

    /// x.1 = ~x.0
    BitwiseNot(RegisterVariant, RegisterVariant),

    /// Call a function in the current object, by mangled name.
    Call(Ustr),

    /// Call an Efun. x.0 is the index into the `EFUN_PROTOTYPES` map.
    CallEfun(u8),

    /// Call a simulated efun, by name.
    CallSimulEfun(Ustr),

    /// Call a function pointer, located in x.0.
    CallFp(RegisterVariant),

    /// Call a function in another object.
    /// x.0 is the receiver, x.1 is the function name
    CallOther(RegisterVariant, RegisterVariant),

    /// Finish a block of instructions that can catch errors and continue
    /// execution.
    CatchEnd,

    /// Start a block of instructions that can catch errors and continue
    /// execution. Store the error in x.0, and jump to x.1 to continue
    /// execution. Jumping to x.1 may include removing call frames to
    /// get back to the correct location.
    CatchStart(RegisterVariant, Address),

    /// Copy x.0 to x.1
    Copy(RegisterVariant, RegisterVariant),

    /// Decrement the value in x.0 by 1
    Dec(RegisterVariant),

    /// x.2 = x.0 / x.1
    Div(RegisterVariant, RegisterVariant, RegisterVariant),

    /// `==` comparison
    /// x.2 = x.0 == x.1
    EqEq(RegisterVariant, RegisterVariant, RegisterVariant),

    /// A function pointer constant. Closures are stored as function pointers as well.
    /// `location` is where the pointer will be stored
    FunctionPtrConst {
        location: RegisterVariant,
        receiver: FunctionReceiver,
        name: Ustr,
    },

    /// Greater than
    /// x.2 = x.0 > x.1
    Gt(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Greater than or equal to
    /// x.2 = x.0 >= x.1
    Gte(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Increment the value in x.0 by 1
    Inc(RegisterVariant),

    /// Unconditional jump
    Jmp(Address),

    /// Jump if the value in the register is not zero (Int or Float)
    Jnz(RegisterVariant, Address),

    /// Jump if the value in the register is zero (Int or Float)
    Jz(RegisterVariant, Address),

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
    MapConst(RegisterVariant),

    /// x.2 = x.0 % x.1
    Mod(RegisterVariant, RegisterVariant, RegisterVariant),

    /// x.2 = x.0 * x.1
    Mul(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Check if x.0 is equal to 0
    Not(RegisterVariant, RegisterVariant),

    /// `!=` comparison
    /// x.2 = x.0 != x.1
    NotEq(RegisterVariant, RegisterVariant, RegisterVariant),

    /// bitwise | comparison.
    /// Give the captured variable at x.0 a fresh cell: a declaration that runs
    /// again (a loop body) binds a new cell, and closures made earlier keep the old one.
    NewUpvalue(RegisterVariant),

    /// x.2 = x.0 | x.1
    Or(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Special case instruction to dynamically populate the `argv` variable
    ///   that is created for ellipsis functions.
    /// `RegisterVariant` is the location of `argv`.
    /// The first `u16` is the number of formal parameters to the function
    ///   (whether they have default values or not, basically just the count
    ///   of non-ellipsis params).
    /// The second `u16` is the number of local variables used by the
    /// function. We know both of these numbers at compile time, and any
    /// other register present in the frame is an ellipsis argument, so
    /// those are the ones we populate.
    PopulateArgv(RegisterVariant, RegisterSize, RegisterSize),

    /// Jump into the `Jmp` table that follows, one entry per default
    /// parameter, at the entry of the first parameter the call left
    /// unfilled; the eval loop reaches a slot by offset, so the table
    /// never moves or shrinks.
    PopulateDefaults,

    /// Push the location onto the `Task`'s `args` staging; the call
    /// instruction that consumes it leaves it empty, success or not.
    PushArg(RegisterVariant),

    /// Push a location onto the `Task`'s `array_items` staging for the next
    /// `AConst` or `MapConst`, which leaves it empty.
    PushArrayItem(RegisterVariant),

    /// Push a location onto the `Task`'s `partial_args` staging for the next
    /// `FunctionPtrConst`, which leaves it empty.
    PushPartialArg(Option<RegisterVariant>),

    /// Push the location as a by-reference argument: the callee aliases the
    /// cell behind it instead of copying its value.
    PushRef(RegisterVariant),

    /// Create a new value from some range of another value
    /// x.4 = x.1[x.2 .. x.3]
    Range(
        RegisterVariant,
        RegisterVariant,
        RegisterVariant,
        RegisterVariant,
    ),

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

    /// x.2 = x.0 - x.1
    Sub(RegisterVariant, RegisterVariant, RegisterVariant),

    /// bitwise ^ comparison.
    /// x.2 = x.0 ^ x.1
    Xor(RegisterVariant, RegisterVariant, RegisterVariant),
}

impl Instruction {
    /// The register this instruction writes its value into, when that dest
    /// is retargetable. `None` for everything else: calls write r0
    /// implicitly, `Inc`/`Dec` modify in place, `CatchStart`'s register is
    /// rebuilt as a Local at runtime, `PopulateArgv`'s is argv's home, and
    /// `NewUpvalue` binds a cell rather than writing a value.
    pub fn dest_register(&self) -> Option<RegisterVariant> {
        match *self {
            Self::AConst(d)
            | Self::BitwiseNot(_, d)
            | Self::Copy(_, d)
            | Self::FunctionPtrConst { location: d, .. }
            | Self::Load(_, _, d)
            | Self::LoadMappingKey(_, _, d)
            | Self::MapConst(d)
            | Self::Not(_, d)
            | Self::Range(_, _, _, d)
            | Self::Sizeof(_, d) => Some(d),
            Self::Add(_, _, d)
            | Self::And(_, _, d)
            | Self::Div(_, _, d)
            | Self::EqEq(_, _, d)
            | Self::Gt(_, _, d)
            | Self::Gte(_, _, d)
            | Self::Lt(_, _, d)
            | Self::Lte(_, _, d)
            | Self::Mod(_, _, d)
            | Self::Mul(_, _, d)
            | Self::NotEq(_, _, d)
            | Self::Or(_, _, d)
            | Self::Shl(_, _, d)
            | Self::Shr(_, _, d)
            | Self::Sub(_, _, d)
            | Self::Xor(_, _, d) => Some(d),
            Self::Call(_)
            | Self::CallEfun(_)
            | Self::CallSimulEfun(_)
            | Self::CallFp(_)
            | Self::CallOther(_, _)
            | Self::CatchEnd
            | Self::CatchStart(_, _)
            | Self::Dec(_)
            | Self::Inc(_)
            | Self::Jmp(_)
            | Self::Jnz(_, _)
            | Self::Jz(_, _)
            | Self::NewUpvalue(_)
            | Self::PopulateArgv(_, _, _)
            | Self::PopulateDefaults
            | Self::PushArg(_)
            | Self::PushArrayItem(_)
            | Self::PushPartialArg(_)
            | Self::PushRef(_)
            | Self::Ret
            | Self::Store(_, _, _) => None,
        }
    }

    /// This instruction with `f` applied to every register operand.
    pub fn map_registers<F>(self, f: F) -> Self
    where
        F: Fn(RegisterVariant) -> RegisterVariant,
    {
        match self {
            Self::AConst(a0) => Self::AConst(f(a0)),
            Self::Add(a0, a1, a2) => Self::Add(f(a0), f(a1), f(a2)),
            Self::And(a0, a1, a2) => Self::And(f(a0), f(a1), f(a2)),
            Self::BitwiseNot(a0, a1) => Self::BitwiseNot(f(a0), f(a1)),
            Self::Call(a0) => Self::Call(a0),
            Self::CallEfun(a0) => Self::CallEfun(a0),
            Self::CallSimulEfun(a0) => Self::CallSimulEfun(a0),
            Self::CallFp(a0) => Self::CallFp(f(a0)),
            Self::CallOther(a0, a1) => Self::CallOther(f(a0), f(a1)),
            Self::CatchEnd => Self::CatchEnd,
            Self::CatchStart(a0, a1) => Self::CatchStart(f(a0), a1),
            Self::Copy(a0, a1) => Self::Copy(f(a0), f(a1)),
            Self::Dec(a0) => Self::Dec(f(a0)),
            Self::Div(a0, a1, a2) => Self::Div(f(a0), f(a1), f(a2)),
            Self::EqEq(a0, a1, a2) => Self::EqEq(f(a0), f(a1), f(a2)),
            Self::FunctionPtrConst {
                location,
                receiver,
                name,
            } => Self::FunctionPtrConst {
                location: f(location),
                // A `Var` receiver names a register too; leaving it unmapped
                // loses the receiver to register rewrites.
                receiver: match receiver {
                    FunctionReceiver::Var(r) => FunctionReceiver::Var(f(r)),
                    other => other,
                },
                name,
            },
            Self::Gt(a0, a1, a2) => Self::Gt(f(a0), f(a1), f(a2)),
            Self::Gte(a0, a1, a2) => Self::Gte(f(a0), f(a1), f(a2)),
            Self::Inc(a0) => Self::Inc(f(a0)),
            Self::Jmp(a0) => Self::Jmp(a0),
            Self::Jnz(a0, a1) => Self::Jnz(f(a0), a1),
            Self::Jz(a0, a1) => Self::Jz(f(a0), a1),
            Self::Load(a0, a1, a2) => Self::Load(f(a0), f(a1), f(a2)),
            Self::LoadMappingKey(a0, a1, a2) => Self::LoadMappingKey(f(a0), f(a1), f(a2)),
            Self::Lt(a0, a1, a2) => Self::Lt(f(a0), f(a1), f(a2)),
            Self::Lte(a0, a1, a2) => Self::Lte(f(a0), f(a1), f(a2)),
            Self::MapConst(a0) => Self::MapConst(f(a0)),
            Self::Mod(a0, a1, a2) => Self::Mod(f(a0), f(a1), f(a2)),
            Self::Mul(a0, a1, a2) => Self::Mul(f(a0), f(a1), f(a2)),
            Self::Not(a0, a1) => Self::Not(f(a0), f(a1)),
            Self::NewUpvalue(a0) => Self::NewUpvalue(f(a0)),
            Self::NotEq(a0, a1, a2) => Self::NotEq(f(a0), f(a1), f(a2)),
            Self::Or(a0, a1, a2) => Self::Or(f(a0), f(a1), f(a2)),
            Self::PopulateArgv(a0, a1, a2) => Self::PopulateArgv(f(a0), a1, a2),
            Self::PopulateDefaults => Self::PopulateDefaults,
            Self::PushArg(a0) => Self::PushArg(f(a0)),
            Self::PushArrayItem(a0) => Self::PushArrayItem(f(a0)),
            Self::PushPartialArg(a0) => Self::PushPartialArg(a0.map(&f)),
            Self::PushRef(a0) => Self::PushRef(f(a0)),
            Self::Range(a0, a1, a2, a3) => Self::Range(f(a0), f(a1), f(a2), f(a3)),
            Self::Ret => Self::Ret,
            Self::Shl(a0, a1, a2) => Self::Shl(f(a0), f(a1), f(a2)),
            Self::Shr(a0, a1, a2) => Self::Shr(f(a0), f(a1), f(a2)),
            Self::Sizeof(a0, a1) => Self::Sizeof(f(a0), f(a1)),
            Self::Store(a0, a1, a2) => Self::Store(f(a0), f(a1), f(a2)),
            Self::Sub(a0, a1, a2) => Self::Sub(f(a0), f(a1), f(a2)),
            Self::Xor(a0, a1, a2) => Self::Xor(f(a0), f(a1), f(a2)),
        }
    }

    /// Backpatch an instruction with a new address.
    /// This is used to fix up jumps after the code has been generated.
    /// Returns an error if the instruction cannot be backpatched.
    pub fn backpatch<A>(&mut self, address: A) -> Result<()>
    where
        A: Into<Address>,
    {
        match self {
            Instruction::Jmp(_) => {
                *self = Instruction::Jmp(address.into());
            }
            Instruction::Jnz(r, _) => {
                *self = Instruction::Jnz(*r, address.into());
            }
            Instruction::Jz(r, _) => {
                *self = Instruction::Jz(*r, address.into());
            }
            _ => {
                return Err(lpc_bug!("Cannot backpatch instruction {:?}", self));
            }
        }

        Ok(())
    }
}

impl Instruction {
    /// How many instruction variants exist.
    pub const COUNT: usize = 47;

    /// Every mnemonic, ordered by [`Instruction::index`].
    pub const MNEMONICS: [&'static str; Self::COUNT] = [
        "aconst",
        "add",
        "and",
        "bitwise_not",
        "call",
        "call_efun",
        "call_simul_efun",
        "call_fp",
        "call_other",
        "catch_end",
        "catch_start",
        "copy",
        "dec",
        "div",
        "eq_eq",
        "function_ptr_const",
        "gt",
        "gte",
        "inc",
        "jmp",
        "jnz",
        "jz",
        "load",
        "load_mapping_key",
        "lt",
        "lte",
        "map_const",
        "mod",
        "mul",
        "not",
        "not_eq",
        "new_upvalue",
        "or",
        "populate_argv",
        "populate_defaults",
        "push_arg",
        "push_array_item",
        "push_partial_arg",
        "push_ref",
        "range",
        "ret",
        "shl",
        "shr",
        "sizeof",
        "store",
        "sub",
        "xor",
    ];

    /// This variant's dense index, `0..Self::COUNT`, in declaration order.
    pub const fn index(&self) -> u8 {
        match self {
            Self::AConst(..) => 0,
            Self::Add(..) => 1,
            Self::And(..) => 2,
            Self::BitwiseNot(..) => 3,
            Self::Call(..) => 4,
            Self::CallEfun(..) => 5,
            Self::CallSimulEfun(..) => 6,
            Self::CallFp(..) => 7,
            Self::CallOther(..) => 8,
            Self::CatchEnd => 9,
            Self::CatchStart(..) => 10,
            Self::Copy(..) => 11,
            Self::Dec(..) => 12,
            Self::Div(..) => 13,
            Self::EqEq(..) => 14,
            Self::FunctionPtrConst { .. } => 15,
            Self::Gt(..) => 16,
            Self::Gte(..) => 17,
            Self::Inc(..) => 18,
            Self::Jmp(..) => 19,
            Self::Jnz(..) => 20,
            Self::Jz(..) => 21,
            Self::Load(..) => 22,
            Self::LoadMappingKey(..) => 23,
            Self::Lt(..) => 24,
            Self::Lte(..) => 25,
            Self::MapConst(..) => 26,
            Self::Mod(..) => 27,
            Self::Mul(..) => 28,
            Self::Not(..) => 29,
            Self::NotEq(..) => 30,
            Self::NewUpvalue(..) => 31,
            Self::Or(..) => 32,
            Self::PopulateArgv(..) => 33,
            Self::PopulateDefaults => 34,
            Self::PushArg(..) => 35,
            Self::PushArrayItem(..) => 36,
            Self::PushPartialArg(..) => 37,
            Self::PushRef(..) => 38,
            Self::Range(..) => 39,
            Self::Ret => 40,
            Self::Shl(..) => 41,
            Self::Shr(..) => 42,
            Self::Sizeof(..) => 43,
            Self::Store(..) => 44,
            Self::Sub(..) => 45,
            Self::Xor(..) => 46,
        }
    }

    /// The instruction's name without operands.
    pub fn mnemonic(&self) -> &'static str {
        Self::MNEMONICS[self.index() as usize]
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::AConst(r1) => {
                write!(f, "{} {r1}", self.mnemonic())
            }
            Instruction::Add(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::And(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::BitwiseNot(r1, r2) => {
                write!(f, "{} {r1}, {r2}", self.mnemonic())
            }
            Instruction::CatchEnd => f.write_str(self.mnemonic()),
            Instruction::CatchStart(r1, label) => {
                write!(f, "{} {r1}, {label}", self.mnemonic())
            }
            Instruction::Call(name) => {
                write!(f, "{} {name}", self.mnemonic())
            }
            Instruction::CallEfun(name_index) => {
                write!(f, "{} {name_index}", self.mnemonic())
            }
            Instruction::CallFp(location) => {
                write!(f, "{} {location}", self.mnemonic())
            }
            Instruction::CallOther(receiver, name) => {
                write!(f, "{} {receiver}, {name}", self.mnemonic())
            }
            Instruction::CallSimulEfun(name) => {
                write!(f, "{} {name}", self.mnemonic())
            }
            Instruction::Copy(r1, r2) => {
                write!(f, "{} {r1}, {r2}", self.mnemonic())
            }
            Instruction::Dec(r) => {
                write!(f, "{} {r}", self.mnemonic())
            }
            Instruction::Div(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::EqEq(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::FunctionPtrConst {
                location,
                receiver,
                name,
            } => {
                write!(f, "{} {location}, {receiver}, {name}", self.mnemonic())
            }
            Instruction::Gt(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Gte(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Inc(r) => {
                write!(f, "{} {r}", self.mnemonic())
            }
            Instruction::Jmp(address) => {
                write!(f, "{} {address}", self.mnemonic())
            }
            Instruction::Jnz(r1, address) => {
                write!(f, "{} {r1}, {address}", self.mnemonic())
            }
            Instruction::Jz(r1, address) => {
                write!(f, "{} {r1}, {address}", self.mnemonic())
            }
            Instruction::Load(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::LoadMappingKey(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Lt(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Lte(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::MapConst(r) => {
                write!(f, "{} {r}", self.mnemonic())
            }
            Instruction::Mod(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Mul(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::NewUpvalue(r) => {
                write!(f, "{} {r}", self.mnemonic())
            }
            Instruction::Not(r1, r2) => {
                write!(f, "{} {r1}, {r2}", self.mnemonic())
            }
            Instruction::NotEq(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Or(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                write!(f, "{} {r}, {num_args}, {num_locals}", self.mnemonic())
            }
            Instruction::PopulateDefaults => f.write_str(self.mnemonic()),
            Instruction::PushArg(r) => {
                write!(f, "{} {r}", self.mnemonic())
            }
            Instruction::PushArrayItem(r1) => {
                write!(f, "{} {r1}", self.mnemonic())
            }
            Instruction::PushPartialArg(r) => {
                let s = r.map(|r| r.to_string()).unwrap_or_default();
                write!(f, "{} {s}", self.mnemonic())
            }
            Instruction::PushRef(r) => write!(f, "{} {r}", self.mnemonic()),
            Instruction::Range(r1, r2, r3, r4) => {
                write!(f, "{} {r1}, {r2}, {r3}, {r4}", self.mnemonic())
            }
            Instruction::Ret => f.write_str(self.mnemonic()),
            Instruction::Shl(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Shr(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Sizeof(r1, r2) => {
                write!(f, "{} {r1}, {r2}", self.mnemonic())
            }
            Instruction::Store(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Sub(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Xor(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
        }
    }
}

// This type is used a lot. Make sure it doesn't unintentionally get bigger.
// Note that if `RegisterSize` is changed, this will need to change as well.
#[cfg(target_arch = "x86_64")]
static_assertions::assert_eq_size!(Instruction, [u8; 24]);

#[cfg(test)]
mod tests {
    use lpc_rs_core::register::Register;
    use ustr::ustr;

    use super::*;

    #[test]
    fn string_operands_display_as_names() {
        let r1 = Register(1).as_local();
        assert_eq!(
            Instruction::Call(ustr("foo__v__/a.c__pb__")).to_string(),
            "call foo__v__/a.c__pb__"
        );
        assert_eq!(
            Instruction::CallSimulEfun(ustr("bar")).to_string(),
            "call_simul_efun bar"
        );
        assert_eq!(
            Instruction::FunctionPtrConst {
                location: r1,
                receiver: FunctionReceiver::Local,
                name: ustr("baz"),
            }
            .to_string(),
            "function_ptr_const r1, local, baz"
        );
    }

    fn r() -> RegisterVariant {
        RegisterVariant::Local(Register(0))
    }

    /// One instance of every variant, in declaration order.
    fn every_variant() -> Vec<Instruction> {
        use Instruction::*;
        vec![
            AConst(r()),
            Add(r(), r(), r()),
            And(r(), r(), r()),
            BitwiseNot(r(), r()),
            Call(ustr("f")),
            CallEfun(0),
            CallSimulEfun(ustr("f")),
            CallFp(r()),
            CallOther(r(), r()),
            CatchEnd,
            CatchStart(r(), Address(0)),
            Copy(r(), r()),
            Dec(r()),
            Div(r(), r(), r()),
            EqEq(r(), r(), r()),
            FunctionPtrConst {
                location: r(),
                receiver: FunctionReceiver::Local,
                name: ustr("f"),
            },
            Gt(r(), r(), r()),
            Gte(r(), r(), r()),
            Inc(r()),
            Jmp(Address(0)),
            Jnz(r(), Address(0)),
            Jz(r(), Address(0)),
            Load(r(), r(), r()),
            LoadMappingKey(r(), r(), r()),
            Lt(r(), r(), r()),
            Lte(r(), r(), r()),
            MapConst(r()),
            Mod(r(), r(), r()),
            Mul(r(), r(), r()),
            Not(r(), r()),
            NotEq(r(), r(), r()),
            NewUpvalue(r()),
            Or(r(), r(), r()),
            PopulateArgv(r(), 0, 0),
            PopulateDefaults,
            PushArg(r()),
            PushArrayItem(r()),
            PushPartialArg(Some(r())),
            PushRef(r()),
            Range(r(), r(), r(), r()),
            Ret,
            Shl(r(), r(), r()),
            Shr(r(), r(), r()),
            Sizeof(r(), r()),
            Store(r(), r(), r()),
            Sub(r(), r(), r()),
            Xor(r(), r(), r()),
        ]
    }

    #[test]
    fn map_registers_reaches_a_var_receiver() {
        let mapped = Instruction::FunctionPtrConst {
            location: RegisterVariant::Local(Register(1)),
            receiver: FunctionReceiver::Var(RegisterVariant::Local(Register(2))),
            name: ustr("f"),
        }
        .map_registers(|r| match r {
            RegisterVariant::Local(Register(2)) => RegisterVariant::Local(Register(9)),
            other => other,
        });

        assert_eq!(
            mapped,
            Instruction::FunctionPtrConst {
                location: RegisterVariant::Local(Register(1)),
                receiver: FunctionReceiver::Var(RegisterVariant::Local(Register(9))),
                name: ustr("f"),
            }
        );
    }

    #[test]
    fn dest_register_names_the_written_operand_or_nothing() {
        use Instruction::*;
        let d = RegisterVariant::Local(Register(7));

        assert_eq!(AConst(d).dest_register(), Some(d));
        assert_eq!(Add(r(), r(), d).dest_register(), Some(d));
        assert_eq!(BitwiseNot(r(), d).dest_register(), Some(d));
        assert_eq!(Copy(r(), d).dest_register(), Some(d));
        assert_eq!(Load(r(), r(), d).dest_register(), Some(d));
        assert_eq!(Range(r(), r(), r(), d).dest_register(), Some(d));
        assert_eq!(Sizeof(r(), d).dest_register(), Some(d));
        assert_eq!(
            FunctionPtrConst {
                location: d,
                receiver: FunctionReceiver::Local,
                name: ustr("f"),
            }
            .dest_register(),
            Some(d)
        );

        let none_count = every_variant()
            .iter()
            .filter(|i| i.dest_register().is_none())
            .count();
        assert_eq!(none_count, 21);
        for i in [
            Call(ustr("f")),
            CallEfun(0),
            CatchStart(r(), Address(0)),
            Inc(r()),
            Dec(r()),
            Store(r(), r(), r()),
            PopulateArgv(r(), 0, 0),
            NewUpvalue(r()),
            Ret,
        ] {
            assert_eq!(i.dest_register(), None, "{i}");
        }
    }

    #[test]
    fn push_ref_is_an_operand_without_a_dest() {
        let i = Instruction::PushRef(RegisterVariant::Upvalue(Register(3)));
        assert_eq!(i.dest_register(), None);
        assert_eq!(
            i.map_registers(|_| RegisterVariant::Global(Register(9))),
            Instruction::PushRef(RegisterVariant::Global(Register(9)))
        );
        assert_eq!(i.mnemonic(), "push_ref");
        assert_eq!(i.to_string(), "push_ref u3");
    }

    #[test]
    fn every_variant_has_a_dense_index_and_a_display_matching_mnemonic() {
        let samples = every_variant();
        assert_eq!(samples.len(), Instruction::COUNT);

        let mut seen = [false; Instruction::COUNT];
        for instruction in &samples {
            let index = instruction.index() as usize;
            assert!(index < Instruction::COUNT, "{instruction}");
            assert!(!seen[index], "duplicate index {index}");
            seen[index] = true;

            let rendered = instruction.to_string();
            let mnemonic = instruction.mnemonic();
            assert!(
                rendered == mnemonic || rendered.starts_with(&format!("{mnemonic} ")),
                "display {rendered:?} does not open with mnemonic {mnemonic:?}"
            );
        }
        assert!(seen.iter().all(|s| *s), "an index was never produced");
    }
}
