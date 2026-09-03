use std::{
    fmt,
    fmt::{Display, Formatter},
};

use lpc_rs_core::{RegisterSize, function_receiver::FunctionReceiver, register::RegisterVariant};
use lpc_rs_errors::{Result, lpc_bug};
use ustr::Ustr;

use crate::address::Address;

/// One argument of a call: a value copied into the callee, or a cell the
/// callee aliases in place of minting its own.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Arg {
    /// A value, read from the register at call time.
    Value(RegisterVariant),
    /// A `ref` argument's cell.
    Ref(RegisterVariant),
}

impl Arg {
    /// The register this argument reads.
    pub fn register(self) -> RegisterVariant {
        match self {
            Self::Value(r) | Self::Ref(r) => r,
        }
    }

    /// This argument with its register passed through `f`.
    pub fn map_register<F>(self, f: F) -> Self
    where
        F: FnOnce(RegisterVariant) -> RegisterVariant,
    {
        match self {
            Self::Value(r) => Self::Value(f(r)),
            Self::Ref(r) => Self::Ref(f(r)),
        }
    }
}

impl Display for Arg {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(r) => write!(f, "{r}"),
            Self::Ref(r) => write!(f, "ref {r}"),
        }
    }
}

/// The index of a call's argument list in its function's `arg_lists`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ArgList(pub u16);

impl Display for ArgList {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "a{}", self.0)
    }
}

/// The kind of comparison a `Cmp`, `Jcmp`, or `Jncmp` makes between its two operands.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Comparison {
    /// `<`
    Lt,
    /// `<=`
    Lte,
    /// `>`
    Gt,
    /// `>=`
    Gte,
    /// `==`
    Eq,
    /// `!=`
    Ne,
}

impl Comparison {
    /// Every kind, in declaration order.
    pub const ALL: [Comparison; 6] = [Self::Lt, Self::Lte, Self::Gt, Self::Gte, Self::Eq, Self::Ne];

    /// Whether `x self y` holds.
    #[inline(always)]
    pub fn holds<T: PartialOrd>(self, x: T, y: T) -> bool {
        match self {
            Self::Lt => x < y,
            Self::Lte => x <= y,
            Self::Gt => x > y,
            Self::Gte => x >= y,
            Self::Eq => x == y,
            Self::Ne => x != y,
        }
    }
}

impl Display for Comparison {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Lt => "lt",
            Self::Lte => "lte",
            Self::Gt => "gt",
            Self::Gte => "gte",
            Self::Eq => "eq",
            Self::Ne => "ne",
        };
        write!(f, "{s}")
    }
}

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
    Call(Ustr, ArgList),

    /// Call an Efun. x.0 is the index into the `EFUN_PROTOTYPES` map.
    CallEfun(u8, ArgList),

    /// Call a simulated efun, by name.
    CallSimulEfun(Ustr, ArgList),

    /// Call a function pointer, located in x.0.
    CallFp(RegisterVariant, ArgList),

    /// Call a function in another object.
    /// x.0 is the receiver, x.1 is the function name
    CallOther(RegisterVariant, RegisterVariant, ArgList),

    /// Finish a block of instructions that can catch errors and continue
    /// execution.
    CatchEnd,

    /// Start a block of instructions that can catch errors and continue
    /// execution. Store the error in x.0, and jump to x.1 to continue
    /// execution. Jumping to x.1 may include removing call frames to
    /// get back to the correct location.
    CatchStart(RegisterVariant, Address),

    /// x.3 = x.1 `kind` x.2, as 1 or 0
    Cmp(
        Comparison,
        RegisterVariant,
        RegisterVariant,
        RegisterVariant,
    ),

    /// Copy x.0 to x.1
    Copy(RegisterVariant, RegisterVariant),

    /// Decrement the value in x.0 by 1
    Dec(RegisterVariant),

    /// x.2 = x.0 / x.1
    Div(RegisterVariant, RegisterVariant, RegisterVariant),

    /// A function pointer constant. Closures are stored as function pointers as well.
    /// `location` is where the pointer will be stored
    FunctionPtrConst {
        location: RegisterVariant,
        receiver: FunctionReceiver,
        name: Ustr,
    },

    /// Increment the value in x.0 by 1
    Inc(RegisterVariant),

    /// Jump to x.3 when x.1 `kind` x.2 holds
    Jcmp(Comparison, RegisterVariant, RegisterVariant, Address),

    /// Unconditional jump
    Jmp(Address),

    /// Jump to x.3 when x.1 `kind` x.2 does not hold
    Jncmp(Comparison, RegisterVariant, RegisterVariant, Address),

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

    /// Create a mapping from the keys and values in the hashmap
    MapConst(RegisterVariant),

    /// x.2 = x.0 % x.1
    Mod(RegisterVariant, RegisterVariant, RegisterVariant),

    /// x.2 = x.0 * x.1
    Mul(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Check if x.0 is equal to 0
    Not(RegisterVariant, RegisterVariant),

    /// bitwise | comparison.
    /// Give the captured variable at x.0 a fresh cell: a declaration that runs
    /// again (a loop body) binds a new cell, and closures made earlier keep the old one.
    NewUpvalue(RegisterVariant),

    /// x.2 = x.0 | x.1
    Or(RegisterVariant, RegisterVariant, RegisterVariant),

    /// Special case instruction to dynamically populate the `argv` variable
    ///   that is created for ellipsis functions.
    /// `RegisterVariant` is the location of `argv`.
    /// The first `u16` is the function's parameter count (default or not,
    /// the ellipsis excluded), the second its locals count; `argv` is the
    /// registers after `params + locals + 1`, one per argument passed
    /// beyond the parameters.
    PopulateArgv(RegisterVariant, RegisterSize, RegisterSize),

    /// Jump into the `Jmp` table that follows, one entry per default
    /// parameter, at the entry of the first parameter the call left
    /// unfilled; the eval loop reaches a slot by offset, so the table
    /// never moves or shrinks.
    PopulateDefaults,

    /// Push a location onto the `Task`'s `array_items` staging for the next
    /// `AConst` or `MapConst`, which leaves it empty.
    PushArrayItem(RegisterVariant),

    /// Push a location onto the `Task`'s `partial_args` staging for the next
    /// `FunctionPtrConst`, which leaves it empty.
    PushPartialArg(Option<RegisterVariant>),

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
            | Self::Cmp(_, _, _, d)
            | Self::Div(_, _, d)
            | Self::Mod(_, _, d)
            | Self::Mul(_, _, d)
            | Self::Or(_, _, d)
            | Self::Shl(_, _, d)
            | Self::Shr(_, _, d)
            | Self::Sub(_, _, d)
            | Self::Xor(_, _, d) => Some(d),
            Self::Call(_, _)
            | Self::CallEfun(_, _)
            | Self::CallSimulEfun(_, _)
            | Self::CallFp(_, _)
            | Self::CallOther(_, _, _)
            | Self::CatchEnd
            | Self::CatchStart(_, _)
            | Self::Dec(_)
            | Self::Inc(_)
            | Self::Jcmp(_, _, _, _)
            | Self::Jmp(_)
            | Self::Jncmp(_, _, _, _)
            | Self::Jnz(_, _)
            | Self::Jz(_, _)
            | Self::NewUpvalue(_)
            | Self::PopulateArgv(_, _, _)
            | Self::PopulateDefaults
            | Self::PushArrayItem(_)
            | Self::PushPartialArg(_)
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
            Self::Call(a0, a1) => Self::Call(a0, a1),
            Self::CallEfun(a0, a1) => Self::CallEfun(a0, a1),
            Self::CallSimulEfun(a0, a1) => Self::CallSimulEfun(a0, a1),
            Self::CallFp(a0, a1) => Self::CallFp(f(a0), a1),
            Self::CallOther(a0, a1, a2) => Self::CallOther(f(a0), f(a1), a2),
            Self::CatchEnd => Self::CatchEnd,
            Self::CatchStart(a0, a1) => Self::CatchStart(f(a0), a1),
            Self::Cmp(kind, a0, a1, a2) => Self::Cmp(kind, f(a0), f(a1), f(a2)),
            Self::Copy(a0, a1) => Self::Copy(f(a0), f(a1)),
            Self::Dec(a0) => Self::Dec(f(a0)),
            Self::Div(a0, a1, a2) => Self::Div(f(a0), f(a1), f(a2)),
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
            Self::Inc(a0) => Self::Inc(f(a0)),
            Self::Jcmp(kind, a0, a1, a2) => Self::Jcmp(kind, f(a0), f(a1), a2),
            Self::Jmp(a0) => Self::Jmp(a0),
            Self::Jncmp(kind, a0, a1, a2) => Self::Jncmp(kind, f(a0), f(a1), a2),
            Self::Jnz(a0, a1) => Self::Jnz(f(a0), a1),
            Self::Jz(a0, a1) => Self::Jz(f(a0), a1),
            Self::Load(a0, a1, a2) => Self::Load(f(a0), f(a1), f(a2)),
            Self::LoadMappingKey(a0, a1, a2) => Self::LoadMappingKey(f(a0), f(a1), f(a2)),
            Self::MapConst(a0) => Self::MapConst(f(a0)),
            Self::Mod(a0, a1, a2) => Self::Mod(f(a0), f(a1), f(a2)),
            Self::Mul(a0, a1, a2) => Self::Mul(f(a0), f(a1), f(a2)),
            Self::Not(a0, a1) => Self::Not(f(a0), f(a1)),
            Self::NewUpvalue(a0) => Self::NewUpvalue(f(a0)),
            Self::Or(a0, a1, a2) => Self::Or(f(a0), f(a1), f(a2)),
            Self::PopulateArgv(a0, a1, a2) => Self::PopulateArgv(f(a0), a1, a2),
            Self::PopulateDefaults => Self::PopulateDefaults,
            Self::PushArrayItem(a0) => Self::PushArrayItem(f(a0)),
            Self::PushPartialArg(a0) => Self::PushPartialArg(a0.map(&f)),
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

    /// This instruction with its address, if it carries one, passed
    /// through `f`: a jump's target or a catch's end.
    pub fn map_address<F>(self, f: F) -> Self
    where
        F: FnOnce(Address) -> Address,
    {
        match self {
            Self::CatchStart(r, a) => Self::CatchStart(r, f(a)),
            Self::Jcmp(kind, x, y, a) => Self::Jcmp(kind, x, y, f(a)),
            Self::Jmp(a) => Self::Jmp(f(a)),
            Self::Jncmp(kind, x, y, a) => Self::Jncmp(kind, x, y, f(a)),
            Self::Jnz(r, a) => Self::Jnz(r, f(a)),
            Self::Jz(r, a) => Self::Jz(r, f(a)),
            other => other,
        }
    }

    /// The address this instruction can transfer control to, if any.
    pub fn address(&self) -> Option<Address> {
        let mut found = None;
        self.map_address(|a| {
            found = Some(a);
            a
        });
        found
    }

    /// The argument list this instruction calls with, for the call family.
    pub fn arg_list(&self) -> Option<ArgList> {
        match *self {
            Self::Call(_, list)
            | Self::CallEfun(_, list)
            | Self::CallSimulEfun(_, list)
            | Self::CallFp(_, list)
            | Self::CallOther(_, _, list) => Some(list),
            _ => None,
        }
    }

    /// Backpatch an instruction with a new address.
    /// This is used to fix up jumps after the code has been generated.
    /// Returns an error if the instruction cannot be backpatched.
    pub fn backpatch<A>(&mut self, address: A) -> Result<()>
    where
        A: Into<Address>,
    {
        if self.address().is_none() {
            return Err(lpc_bug!("Cannot backpatch instruction {:?}", self));
        }
        *self = self.map_address(|_| address.into());

        Ok(())
    }
}

impl Instruction {
    /// How many instruction variants exist.
    pub const COUNT: usize = 42;

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
        "cmp",
        "copy",
        "dec",
        "div",
        "function_ptr_const",
        "inc",
        "jcmp",
        "jmp",
        "jncmp",
        "jnz",
        "jz",
        "load",
        "load_mapping_key",
        "map_const",
        "mod",
        "mul",
        "not",
        "new_upvalue",
        "or",
        "populate_argv",
        "populate_defaults",
        "push_array_item",
        "push_partial_arg",
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
            Self::Cmp(..) => 11,
            Self::Copy(..) => 12,
            Self::Dec(..) => 13,
            Self::Div(..) => 14,
            Self::FunctionPtrConst { .. } => 15,
            Self::Inc(..) => 16,
            Self::Jcmp(..) => 17,
            Self::Jmp(..) => 18,
            Self::Jncmp(..) => 19,
            Self::Jnz(..) => 20,
            Self::Jz(..) => 21,
            Self::Load(..) => 22,
            Self::LoadMappingKey(..) => 23,
            Self::MapConst(..) => 24,
            Self::Mod(..) => 25,
            Self::Mul(..) => 26,
            Self::Not(..) => 27,
            Self::NewUpvalue(..) => 28,
            Self::Or(..) => 29,
            Self::PopulateArgv(..) => 30,
            Self::PopulateDefaults => 31,
            Self::PushArrayItem(..) => 32,
            Self::PushPartialArg(..) => 33,
            Self::Range(..) => 34,
            Self::Ret => 35,
            Self::Shl(..) => 36,
            Self::Shr(..) => 37,
            Self::Sizeof(..) => 38,
            Self::Store(..) => 39,
            Self::Sub(..) => 40,
            Self::Xor(..) => 41,
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
            Instruction::Cmp(kind, r1, r2, r3) => {
                write!(f, "{} {kind} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::Call(name, list) => {
                write!(f, "{} {name}, {list}", self.mnemonic())
            }
            Instruction::CallEfun(name_index, list) => {
                write!(f, "{} {name_index}, {list}", self.mnemonic())
            }
            Instruction::CallFp(location, list) => {
                write!(f, "{} {location}, {list}", self.mnemonic())
            }
            Instruction::CallOther(receiver, name, list) => {
                write!(f, "{} {receiver}, {name}, {list}", self.mnemonic())
            }
            Instruction::CallSimulEfun(name, list) => {
                write!(f, "{} {name}, {list}", self.mnemonic())
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
            Instruction::FunctionPtrConst {
                location,
                receiver,
                name,
            } => {
                write!(f, "{} {location}, {receiver}, {name}", self.mnemonic())
            }
            Instruction::Inc(r) => {
                write!(f, "{} {r}", self.mnemonic())
            }
            Instruction::Jcmp(kind, r1, r2, address) => {
                write!(f, "{} {kind} {r1}, {r2}, {address}", self.mnemonic())
            }
            Instruction::Jmp(address) => {
                write!(f, "{} {address}", self.mnemonic())
            }
            Instruction::Jncmp(kind, r1, r2, address) => {
                write!(f, "{} {kind} {r1}, {r2}, {address}", self.mnemonic())
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
            Instruction::Or(r1, r2, r3) => {
                write!(f, "{} {r1}, {r2}, {r3}", self.mnemonic())
            }
            Instruction::PopulateArgv(r, num_args, num_locals) => {
                write!(f, "{} {r}, {num_args}, {num_locals}", self.mnemonic())
            }
            Instruction::PopulateDefaults => f.write_str(self.mnemonic()),
            Instruction::PushArrayItem(r1) => {
                write!(f, "{} {r1}", self.mnemonic())
            }
            Instruction::PushPartialArg(r) => {
                let s = r.map(|r| r.to_string()).unwrap_or_default();
                write!(f, "{} {s}", self.mnemonic())
            }
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
    fn a_comparison_holds_between_ordered_values() {
        let answers: Vec<bool> = Comparison::ALL.iter().map(|&k| k.holds(1, 2)).collect();
        assert_eq!(answers, [true, true, false, false, false, true]);
    }

    #[test]
    fn string_operands_display_as_names() {
        let r1 = Register(1).as_local();
        assert_eq!(
            Instruction::Call(ustr("foo__v__/a.c__pb__"), ArgList(0)).to_string(),
            "call foo__v__/a.c__pb__, a0"
        );
        assert_eq!(
            Instruction::CallSimulEfun(ustr("bar"), ArgList(1)).to_string(),
            "call_simul_efun bar, a1"
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

    #[test]
    fn a_compare_displays_its_kind_before_its_operands() {
        let r1 = Register(1).as_local();
        let r2 = Register(2).as_local();
        let r3 = Register(3).as_local();
        assert_eq!(
            Instruction::Cmp(Comparison::Lte, r1, r2, r3).to_string(),
            "cmp lte r1, r2, r3"
        );
    }

    #[test]
    fn a_fused_branch_displays_its_kind_operands_and_target() {
        let r1 = Register(1).as_local();
        let k0 = RegisterVariant::Constant(Register(0));
        assert_eq!(
            Instruction::Jcmp(Comparison::Lt, r1, k0, Address(4)).to_string(),
            "jcmp lt r1, k0, 0004"
        );
        assert_eq!(
            Instruction::Jncmp(Comparison::Eq, r1, k0, Address(4)).to_string(),
            "jncmp eq r1, k0, 0004"
        );
    }

    #[test]
    fn a_fused_branch_writes_no_register() {
        let r = Register(1).as_local();
        assert_eq!(
            Instruction::Jcmp(Comparison::Lt, r, r, Address(0)).dest_register(),
            None
        );
    }

    #[test]
    fn six_variants_carry_an_address() {
        let carriers = every_variant()
            .iter()
            .filter(|i| i.address().is_some())
            .count();
        assert_eq!(carriers, 6);
    }

    #[test]
    fn a_fused_branch_reports_its_target() {
        let r = Register(1).as_local();
        assert_eq!(
            Instruction::Jncmp(Comparison::Ne, r, r, Address(7)).address(),
            Some(Address(7))
        );
    }

    #[test]
    fn map_address_rewrites_a_carrier_and_leaves_the_rest() {
        let r = Register(1).as_local();
        let bump = |a: Address| Address(a.0 + 1);
        assert_eq!(
            Instruction::Jcmp(Comparison::Gt, r, r, Address(3)).map_address(bump),
            Instruction::Jcmp(Comparison::Gt, r, r, Address(4))
        );
        assert_eq!(Instruction::Ret.map_address(bump), Instruction::Ret);
    }

    #[test]
    fn a_call_displays_its_argument_list_last() {
        let r1 = Register(1).as_local();
        assert_eq!(
            Instruction::Call(ustr("foo__v__/a.c__pb__"), ArgList(0)).to_string(),
            "call foo__v__/a.c__pb__, a0"
        );
        assert_eq!(
            Instruction::CallOther(r1, r1, ArgList(2)).to_string(),
            "call_other r1, r1, a2"
        );
    }

    #[test]
    fn an_argument_displays_bare_and_a_ref_argument_with_ref() {
        let r1 = Register(1).as_local();
        assert_eq!(
            (Arg::Value(r1).to_string(), Arg::Ref(r1).to_string()),
            ("r1".to_string(), "ref r1".to_string())
        );
    }

    #[test]
    fn five_variants_carry_an_argument_list() {
        let carriers = every_variant()
            .iter()
            .filter(|i| i.arg_list().is_some())
            .count();
        assert_eq!(carriers, 5);
    }

    #[test]
    fn every_comparison_kind_has_a_display() {
        let shown: Vec<String> = Comparison::ALL.iter().map(ToString::to_string).collect();
        assert_eq!(shown, ["lt", "lte", "gt", "gte", "eq", "ne"]);
    }

    /// One instance of every variant, in declaration order.
    fn every_variant() -> Vec<Instruction> {
        use Instruction::*;
        vec![
            AConst(r()),
            Add(r(), r(), r()),
            And(r(), r(), r()),
            BitwiseNot(r(), r()),
            Call(ustr("f"), ArgList(0)),
            CallEfun(0, ArgList(0)),
            CallSimulEfun(ustr("f"), ArgList(0)),
            CallFp(r(), ArgList(0)),
            CallOther(r(), r(), ArgList(0)),
            CatchEnd,
            CatchStart(r(), Address(0)),
            Cmp(Comparison::Lt, r(), r(), r()),
            Copy(r(), r()),
            Dec(r()),
            Div(r(), r(), r()),
            FunctionPtrConst {
                location: r(),
                receiver: FunctionReceiver::Local,
                name: ustr("f"),
            },
            Inc(r()),
            Jcmp(Comparison::Lt, r(), r(), Address(0)),
            Jmp(Address(0)),
            Jncmp(Comparison::Lt, r(), r(), Address(0)),
            Jnz(r(), Address(0)),
            Jz(r(), Address(0)),
            Load(r(), r(), r()),
            LoadMappingKey(r(), r(), r()),
            MapConst(r()),
            Mod(r(), r(), r()),
            Mul(r(), r(), r()),
            Not(r(), r()),
            NewUpvalue(r()),
            Or(r(), r(), r()),
            PopulateArgv(r(), 0, 0),
            PopulateDefaults,
            PushArrayItem(r()),
            PushPartialArg(Some(r())),
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
            Call(ustr("f"), ArgList(0)),
            CallEfun(0, ArgList(0)),
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
    fn a_ref_argument_keeps_its_kind_through_a_register_map() {
        let arg = Arg::Ref(RegisterVariant::Upvalue(Register(3)));
        assert_eq!(arg.register(), RegisterVariant::Upvalue(Register(3)));
        assert_eq!(
            arg.map_register(|_| RegisterVariant::Global(Register(9))),
            Arg::Ref(RegisterVariant::Global(Register(9)))
        );
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
