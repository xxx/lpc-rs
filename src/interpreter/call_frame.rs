use std::{
    borrow::Cow,
    fmt,
    fmt::{Display, Formatter},
    sync::{Arc, LazyLock},
};

use derive_builder::Builder;
use educe::Educe;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::LpcIntInner;
use lpc_rs_core::{
    RegisterSize,
    lpc_path::LpcPath,
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_function_support::{
    constant::LpcConstant, function_prototype::FunctionPrototypeBuilder,
    program_function::ProgramFunction,
};
use thin_vec::ThinVec;

use crate::interpreter::{
    bank::RefBank,
    lpc_int::LpcInt,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::{MergeOp, TxnHandle, VarId},
};

/// A collection `->` in flight: one receiver's frame on the stack at a time,
/// driven by `Task::advance_collection_call` from the frame that issued it.
#[derive(Debug, Clone)]
pub struct CollectionCall {
    /// The function every receiver is called with.
    pub name: String,
    /// The argument values, captured when the instruction ran.
    pub args: Vec<LpcRef>,
    /// Receivers not yet called, last first (`pop` yields the next).
    pub remaining: Vec<LpcRef>,
    /// The mapping's keys in results order; `None` for an array.
    pub keys: Option<Vec<LpcRef>>,
    /// One result per receiver called so far.
    pub results: Vec<LpcRef>,
    /// The last receiver's frame is on the stack; its `r0` is owed.
    pub owed: bool,
}

/// Where a [`RegisterVariant`] resolves in a frame: a local register, or the
/// world cell behind a global or an upvalue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Slot {
    Register(Register),
    Cell(VarId),
    Constant(Register),
}

/// A representation of a local variable name and value.
/// This exists only so we can stick a `Display` impl on it for
/// testing and debugging.
#[derive(Debug, Clone)]
#[cfg(test)]
pub struct LocalVariable {
    pub name: String,
    pub value: LpcRef,
}

#[cfg(test)]
impl LocalVariable {
    fn new(name: String, value: LpcRef) -> Self {
        Self { name, value }
    }
}

/// A representation of a function call's context.
#[derive(Educe, Clone, Builder)]
#[educe(Debug)]
#[builder(build_fn(error = "lpc_rs_errors::LpcError"))]
pub struct CallFrame {
    /// A pointer to the process that owns the function being called
    #[builder(setter(into))]
    pub process: Arc<Process>,

    /// The function that this frame is a call to.
    #[builder(setter(into))]
    pub function: Arc<ProgramFunction>,

    /// Our registers. By convention, `registers[0]` is for the return value of
    /// the call, and is not otherwise used for storage of locals.
    #[builder(default)]
    pub registers: RefBank,

    /// Track where the program counter is pointing in this frame's function's instructions.
    #[builder(default, setter(into))]
    pc: usize,

    /// How many explicit arguments were passed to the call that created this
    /// frame? This will include partially-applied arguments in the case
    /// that the CallFrame is for a call to a function pointer.
    #[builder(default)]
    pub called_with_num_args: RegisterSize,

    /// The captured cells this call can reach: its creators' first, then its own.
    #[builder(default, setter(into))]
    pub upvalue_ptrs: ThinVec<VarId>,

    /// The collection `->` this frame issued and has not finished.
    #[builder(default)]
    pub pending: Option<Box<CollectionCall>>,

    /// For an efun frame fired through a pointer: the file that wrote the
    /// pointer, which is the code the efun acts for.
    #[builder(default)]
    pub origin: Option<Arc<LpcPath>>,

    /// Entered through a door — `->`, a pointer call, a simul efun — rather
    /// than a local call: the frame below is what `previous_object` names.
    #[builder(default)]
    pub external: bool,
}

/// The function an efun fired through a pointer runs in: one `Ret`, so the
/// efun's result in `r0` reaches the frame that called the pointer, or the
/// task when there is none.
pub static ENTRY: LazyLock<Arc<ProgramFunction>> = LazyLock::new(|| {
    let prototype = FunctionPrototypeBuilder::default()
        .name("<entry>")
        .filename(Arc::new(LpcPath::InGame("/<entry>".into())))
        .return_type(LpcType::Mixed(false))
        .build()
        .expect("the entry prototype has every field");
    let mut function = ProgramFunction::new(prototype, 0);
    function.instructions = vec![Instruction::Ret];
    function.debug_spans = vec![None];
    Arc::new(function)
});

impl CallFrame {
    /// Create a new [`CallFrame`] instance
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    /// * `called_with_num_args` - how many arguments were explicitly passed in
    ///   the call to this function?
    /// * `upvalue_ptrs` - The captured cells inherited from the creator
    pub(crate) fn new<P, V>(
        process: P,
        function: Arc<ProgramFunction>,
        called_with_num_args: RegisterSize,
        upvalue_ptrs: Option<V>,
    ) -> Self
    where
        P: Into<Arc<Process>>,
        V: Into<ThinVec<VarId>>,
    {
        Self::with_minimum_arg_capacity(
            process,
            function,
            called_with_num_args,
            called_with_num_args,
            upvalue_ptrs,
        )
    }

    /// The frame an efun fired through a pointer runs in, as `process`.
    pub(crate) fn entry(process: Arc<Process>) -> Self {
        Self::new(process, ENTRY.clone(), 0, None::<&[VarId]>)
    }

    /// Whether this frame is an entry frame: the frame below it, if any, is
    /// the one that called the pointer.
    #[inline]
    pub fn is_entry(&self) -> bool {
        Arc::ptr_eq(&self.function, &ENTRY)
    }

    /// Create a new [`CallFrame`] instance with space for at least
    /// `arg_capacity` registers.
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    /// * `called_with_num_args` - how many arguments were explicitly passed in
    ///   the call to this function?
    /// * `arg_capacity` - Reserve space for at least this many registers (this
    ///   is used for ellipsis args and `call_other`)
    /// * `upvalue_ptrs` - The captured cells inherited from the creator
    ///
    /// The frame is the tail expression so a caller's slot receives it
    /// directly; a `&mut` step before the move copied it wide.
    #[inline]
    pub(crate) fn with_minimum_arg_capacity<P, V>(
        process: P,
        function: Arc<ProgramFunction>,
        called_with_num_args: RegisterSize,
        arg_capacity: RegisterSize,
        upvalue_ptrs: Option<V>,
    ) -> Self
    where
        P: Into<Arc<Process>>,
        V: Into<ThinVec<VarId>>,
    {
        let mut upvalue_ptrs: ThinVec<VarId> = upvalue_ptrs.map(Into::into).unwrap_or_default();
        // This call's own captured cells come after the inherited ones. A cell
        // is an identity only; its value lives in the committer's world once written.
        if function.num_upvalues > 0 {
            upvalue_ptrs.extend((0..function.num_upvalues).map(|_| VarId::new()));
        }

        Self {
            registers: RefBank::initialized_for_function(&function, arg_capacity),
            process: process.into(),
            function,
            pc: 0,
            called_with_num_args,
            upvalue_ptrs,
            pending: None,
            origin: None,
            external: false,
        }
    }

    /// Bind the captured variable at `location` to a fresh cell; pointers
    /// that copied the old cell keep it.
    pub(crate) fn new_upvalue(&mut self, location: RegisterVariant) -> Result<()> {
        let RegisterVariant::Upvalue(reg) = location else {
            return Err(self.runtime_bug(format!("new_upvalue on a non-upvalue {location}")));
        };
        let Some(cell) = self.upvalue_ptrs.get_mut(reg.index() as usize) else {
            return Err(self.bad_upvalue(reg));
        };
        *cell = VarId::new();
        Ok(())
    }

    /// Store argument `i` where the function declares it: at `i + 1` with
    /// no declared location, and past the locals beyond the declared list,
    /// where the bank has reserved room for it and `PopulateArgv` reads it
    /// back.
    fn store_arg(&mut self, txn: &TxnHandle, i: usize, value: LpcRef) -> Result<()> {
        let target = match self.function.arg_locations.get(i) {
            Some(&location) => location,
            None => {
                let Ok(i) = RegisterSize::try_from(i) else {
                    return Err(self.runtime_bug(format!("argument {i} does not fit a register")));
                };
                let num_args = self.function.arity().num_args;
                let register = if i < num_args {
                    i + 1
                } else {
                    num_args + self.function.num_locals + 1 + (i - num_args)
                };
                Register(register).as_local()
            }
        };
        self.set_location(txn, target, value)
    }

    /// Store argument `i` by value; a `ref` parameter refuses a value.
    pub(crate) fn push_arg(&mut self, txn: &TxnHandle, i: usize, value: LpcRef) -> Result<()> {
        if self.function.prototype.is_ref_param(i) {
            return Err(self.runtime_error(format!(
                "argument {} of `{}` must be passed by reference",
                i + 1,
                self.function.name()
            )));
        }
        self.store_arg(txn, i, value)
    }

    /// Bind argument `i` to `cell`: the callee's parameter aliases it.
    pub(crate) fn push_ref(&mut self, i: usize, cell: VarId) -> Result<()> {
        let name = self.function.name();
        if !self.function.prototype.is_ref_param(i) {
            return Err(self.runtime_error(format!(
                "`{name}` does not take argument {} by reference",
                i + 1
            )));
        }
        let Some(RegisterVariant::Upvalue(reg)) = self.function.arg_locations.get(i).copied()
        else {
            return Err(self.runtime_bug(format!(
                "ref parameter {} of `{name}` was not laid out as a cell",
                i + 1
            )));
        };
        let Some(slot) = self.upvalue_ptrs.get_mut(reg.index() as usize) else {
            return Err(self.runtime_bug(format!(
                "upvalue {} is outside this frame's {} cells",
                reg.index(),
                self.upvalue_ptrs.len()
            )));
        };
        *slot = cell;
        Ok(())
    }

    /// The cell behind `location`, for passing by reference.
    pub(crate) fn ref_cell(&self, location: RegisterVariant) -> Result<VarId> {
        match self.slot(location)? {
            Slot::Cell(cell) => Ok(cell),
            Slot::Register(_) => Err(self.runtime_bug(format!(
                "by-reference argument {location} is a register, not a cell"
            ))),
            Slot::Constant(_) => Err(self.runtime_bug(format!(
                "by-reference argument {location} is a constant, not a cell"
            ))),
        }
    }

    /// Resolve `location` to its slot in this frame.
    pub(crate) fn slot(&self, location: RegisterVariant) -> Result<Slot> {
        match location {
            RegisterVariant::Local(reg) => Ok(Slot::Register(reg)),
            RegisterVariant::Global(reg) => Ok(Slot::Cell(self.global(reg))),
            RegisterVariant::Upvalue(reg) => self.upvalue(reg).map(Slot::Cell),
            RegisterVariant::Constant(reg) => Ok(Slot::Constant(reg)),
        }
    }

    /// The world cell behind global `reg`.
    #[inline(always)]
    fn global(&self, reg: Register) -> VarId {
        self.process.var_id(reg.into())
    }

    /// The captured cell `reg` names.
    #[inline(always)]
    fn upvalue(&self, reg: Register) -> Result<VarId> {
        match self.upvalue_ptrs.get(reg.index() as usize) {
            Some(&cell) => Ok(cell),
            None => Err(self.bad_upvalue(reg)),
        }
    }

    /// The pool entry a `Constant` operand names, as a value.
    #[inline(always)]
    fn constant(&self, reg: Register) -> Result<LpcRef> {
        match self.function.constants.get(reg.index() as usize) {
            Some(constant) => Ok(LpcRef::from(constant)),
            None => Err(self.bad_constant(reg)),
        }
    }

    /// The runtime bug for an upvalue past this frame's cells.
    #[cold]
    #[inline(never)]
    fn bad_upvalue(&self, reg: Register) -> LpcError {
        self.runtime_bug(format!(
            "upvalue {} is outside this frame's {} cells",
            reg.index(),
            self.upvalue_ptrs.len()
        ))
    }

    /// The runtime bug for a constant past the function's pool.
    #[cold]
    #[inline(never)]
    fn bad_constant(&self, reg: Register) -> LpcError {
        self.runtime_bug(format!(
            "constant k{} is outside this function's {} entries",
            reg.index(),
            self.function.constants.len()
        ))
    }

    /// The runtime bug for a write to a `Constant` operand.
    #[cold]
    #[inline(never)]
    fn write_through_constant(&self, location: RegisterVariant) -> LpcError {
        self.runtime_bug(format!("write through constant {location}"))
    }

    /// Read the [`LpcRef`] at `location`; an unwritten cell reads `NULL`.
    #[inline(always)]
    pub(crate) fn get_location(
        &self,
        txn: &TxnHandle,
        location: RegisterVariant,
    ) -> Result<Cow<'_, LpcRef>> {
        Ok(match location {
            RegisterVariant::Local(reg) => Cow::Borrowed(&self.registers[reg]),
            RegisterVariant::Constant(reg) => Cow::Owned(self.constant(reg)?),
            RegisterVariant::Global(reg) => Cow::Owned(read_cell(txn, self.global(reg))),
            RegisterVariant::Upvalue(reg) => Cow::Owned(read_cell(txn, self.upvalue(reg)?)),
        })
    }

    /// The int at `location` when it sits in a register or the pool; `None`
    /// for any other value, and for a cell, which only a full read tracks.
    #[inline(always)]
    pub(crate) fn peek_int(&self, location: RegisterVariant) -> Option<LpcIntInner> {
        match location {
            RegisterVariant::Local(reg) => match &self.registers[reg] {
                LpcRef::Int(x) => Some(x.0),
                _ => None,
            },
            RegisterVariant::Constant(reg) => {
                match self.function.constants.get(reg.index() as usize) {
                    Some(LpcConstant::Int(x)) => Some(*x),
                    _ => None,
                }
            }
            RegisterVariant::Global(_) | RegisterVariant::Upvalue(_) => None,
        }
    }

    /// Store int `value` at `location`. An int register is overwritten in
    /// place: a fresh `LpcRef`'s spill and wide reload stalled the eval loop.
    #[inline(always)]
    pub(crate) fn set_int(
        &mut self,
        txn: &TxnHandle,
        location: RegisterVariant,
        value: LpcIntInner,
    ) -> Result<()> {
        match location {
            RegisterVariant::Local(reg) => {
                let slot = &mut self.registers[reg];
                match slot {
                    LpcRef::Int(x) => x.0 = value,
                    _ => *slot = LpcRef::Int(LpcInt(value)),
                }
            }
            RegisterVariant::Global(reg) => {
                write_cell(txn, self.global(reg), LpcRef::Int(LpcInt(value)))
            }
            RegisterVariant::Upvalue(reg) => {
                write_cell(txn, self.upvalue(reg)?, LpcRef::Int(LpcInt(value)))
            }
            RegisterVariant::Constant(_) => return Err(self.write_through_constant(location)),
        }
        Ok(())
    }

    /// Assign an [`LpcRef`] to a specific location, based on the [`RegisterVariant`]
    #[inline(always)]
    pub(crate) fn set_location(
        &mut self,
        txn: &TxnHandle,
        location: RegisterVariant,
        lpc_ref: LpcRef,
    ) -> Result<()> {
        match location {
            RegisterVariant::Local(reg) => self.registers[reg] = lpc_ref,
            RegisterVariant::Global(reg) => write_cell(txn, self.global(reg), lpc_ref),
            RegisterVariant::Upvalue(reg) => write_cell(txn, self.upvalue(reg)?, lpc_ref),
            RegisterVariant::Constant(_) => return Err(self.write_through_constant(location)),
        }
        Ok(())
    }

    /// Add `delta` (`++`/`--`, so ±1) to the int at `location`.
    #[inline(always)]
    pub(crate) fn bump_in_location(
        &mut self,
        txn: &TxnHandle,
        location: RegisterVariant,
        delta: LpcIntInner,
    ) -> Result<()> {
        match location {
            RegisterVariant::Local(reg) => bump(&mut self.registers[reg], delta),
            RegisterVariant::Global(reg) => bump_cell(txn, self.global(reg), delta),
            RegisterVariant::Upvalue(reg) => bump_cell(txn, self.upvalue(reg)?, delta),
            RegisterVariant::Constant(_) => Err(self.write_through_constant(location)),
        }
    }

    /// Convenience to return a list of the local variables in this frame.
    /// Intended for debugging and testing.
    #[cfg(test)]
    pub(crate) fn local_variables(&self, txn: &TxnHandle) -> Vec<LocalVariable> {
        self.function
            .local_variables
            .iter()
            .map(|var| {
                let Some(loc) = var.location else {
                    // This should be unreachable.
                    return LocalVariable::new(var.name.clone(), NULL);
                };

                let value = self.get_location(txn, loc).map_or(NULL, Cow::into_owned);
                LocalVariable::new(var.name.clone(), value)
            })
            .collect()
    }

    /// get the debug span for the current instruction
    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        // subtract 1, because we increment the pc just after fetching
        // an instruction, but before evaluating it.
        let idx = self.pc.saturating_sub(1);
        self.function.debug_spans.get(idx).and_then(|s| *s)
    }

    /// set the pc to a specific value
    #[inline]
    pub fn set_pc<T>(&mut self, new_val: T)
    where
        T: Into<usize>,
    {
        self.pc = new_val.into();
    }

    /// increment the pc
    #[inline]
    pub fn inc_pc(&mut self) {
        self.pc += 1;
    }

    /// get the pc value
    #[inline]
    pub fn pc(&self) -> usize {
        self.pc
    }

    /// get the current instruction
    #[inline]
    pub fn instruction(&self) -> Option<Instruction> {
        self.function.instructions.get(self.pc).copied()
    }

    /// a convenience method to generate a runtime error
    #[inline]
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::runtime(msg).with_span(self.current_debug_span())
    }

    /// a convenience method to generate a runtime bug
    #[inline]
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::runtime_bug(msg).with_span(self.current_debug_span())
    }

    /// get a string representation of the frame's current current location
    #[inline]
    pub fn to_stack_trace_format(&self) -> String {
        self.current_debug_span()
            .map(|span| format!("{} in {}()", span, self.function.name()))
            .unwrap_or_else(|| format!("(unknown) in {}()", self.function.name()))
    }
}

impl Display for CallFrame {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_stack_trace_format())
    }
}

/// Read `cell` in `txn`; an unwritten cell reads `NULL`.
#[inline(never)]
fn read_cell(txn: &TxnHandle, cell: VarId) -> LpcRef {
    txn.with(|t| t.read(cell).unwrap_or(NULL))
}

/// Write `value` to `cell` in `txn`. A blind write: the read that computed
/// `value` was already tracked when the caller read it.
#[inline(never)]
fn write_cell(txn: &TxnHandle, cell: VarId, value: LpcRef) {
    txn.with(|t| t.write(cell, value))
}

/// `++`/`--` on `x` in place.
#[inline(always)]
fn bump(x: &mut LpcRef, delta: LpcIntInner) -> Result<()> {
    if delta >= 0 { x.inc() } else { x.dec() }
}

/// Add `delta` to the int in `cell`. An int (or nothing) records a merge
/// write — no read is tracked, so concurrent bumps commute — and any other
/// value takes the tracked read-modify-write path to produce the typed error.
#[inline(never)]
fn bump_cell(txn: &TxnHandle, cell: VarId, delta: LpcIntInner) -> Result<()> {
    txn.with(|t| {
        if t.peek_int(cell) {
            t.merge(cell, MergeOp::IntAdd(delta));
            Ok(())
        } else {
            let mut cur = t.read(cell).unwrap_or(NULL);
            bump(&mut cur, delta)?;
            t.write(cell, cur);
            Ok(())
        }
    })
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::{function_arity::FunctionArity, lpc_type::LpcType};
    use lpc_rs_function_support::{
        constant::LpcConstant, function_prototype::FunctionPrototypeBuilder,
    };
    use lpc_rs_utils::lpc_string::LpcString;
    use ustr::ustr;

    use super::*;
    use crate::interpreter::program::Program;

    #[test]
    fn new_sets_up_registers() {
        let process = Process::default();

        let prototype = FunctionPrototypeBuilder::default()
            .name("my_function")
            .filename(Arc::new("my_function".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(4))
            .build()
            .unwrap();

        let fs = ProgramFunction::new(prototype, 7);

        let frame = CallFrame::new(process, Arc::new(fs), 4, None::<&[VarId]>);

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &NULL));
    }

    #[test]
    fn slot_resolves_each_variant() {
        let program = Program {
            num_globals: 1,
            ..Program::default()
        };
        let prototype = FunctionPrototypeBuilder::default()
            .name("my_function")
            .filename(Arc::new("my_function".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let mut pf = ProgramFunction::new(prototype, 0);
        pf.num_upvalues = 1;

        let frame = CallFrame::new(Process::new(program), Arc::new(pf), 0, None::<&[VarId]>);

        assert_eq!(
            frame.slot(Register(2).as_local()).unwrap(),
            Slot::Register(Register(2))
        );
        assert_eq!(
            frame.slot(Register(0).as_global()).unwrap(),
            Slot::Cell(frame.process.var_id(0))
        );
        let cell = frame.upvalue_ptrs[0];
        assert_eq!(
            frame.slot(Register(0).as_upvalue()).unwrap(),
            Slot::Cell(cell)
        );
    }

    /// A function whose pool holds an int and a string.
    fn pooled_function() -> Arc<ProgramFunction> {
        let prototype = FunctionPrototypeBuilder::default()
            .name("pooled")
            .filename(Arc::new("pooled".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let mut pf = ProgramFunction::new(prototype, 0);
        pf.constants = vec![
            LpcConstant::Int(5),
            LpcConstant::String(Arc::new(LpcString::Static(ustr("hi")))),
        ];
        Arc::new(pf)
    }

    #[test]
    fn a_constant_operand_reads_the_pool() {
        let txn = TxnHandle::empty();
        let frame = CallFrame::new(Process::default(), pooled_function(), 0, None::<&[VarId]>);

        assert_eq!(
            frame.slot(Register(0).as_constant()).unwrap(),
            Slot::Constant(Register(0))
        );
        assert_eq!(
            *frame.get_location(&txn, Register(0).as_constant()).unwrap(),
            LpcRef::from(5)
        );
        assert_eq!(
            frame
                .get_location(&txn, Register(1).as_constant())
                .unwrap()
                .as_str(),
            Some("hi")
        );
    }

    #[test]
    fn a_constant_past_the_pool_is_a_runtime_bug() {
        let txn = TxnHandle::empty();
        let frame = CallFrame::new(Process::default(), pooled_function(), 0, None::<&[VarId]>);

        let err = frame
            .get_location(&txn, Register(2).as_constant())
            .unwrap_err();

        assert!(err.to_string().contains("k2"), "{err}");
    }

    #[test]
    fn a_write_through_a_constant_operand_is_a_runtime_bug() {
        let txn = TxnHandle::empty();
        let mut frame = CallFrame::new(Process::default(), pooled_function(), 0, None::<&[VarId]>);

        let err = frame
            .set_location(&txn, Register(0).as_constant(), LpcRef::from(1))
            .unwrap_err();
        assert!(err.to_string().contains("k0"), "{err}");

        let err = frame
            .bump_in_location(&txn, Register(0).as_constant(), 1)
            .unwrap_err();
        assert!(err.to_string().contains("k0"), "{err}");

        assert_eq!(
            *frame.get_location(&txn, Register(0).as_constant()).unwrap(),
            LpcRef::from(5)
        );
    }

    /// A function `f(int ref x)`: one cell, the parameter living in it.
    fn ref_function() -> Arc<ProgramFunction> {
        let prototype = FunctionPrototypeBuilder::default()
            .name("f")
            .filename(Arc::new("f.c".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Int(false)])
            .ref_params(vec![true])
            .build()
            .unwrap();
        let mut pf = ProgramFunction::new(prototype, 0);
        pf.num_upvalues = 1;
        pf.arg_locations = vec![Register(0).as_upvalue()];
        Arc::new(pf)
    }

    fn value_function() -> Arc<ProgramFunction> {
        let prototype = FunctionPrototypeBuilder::default()
            .name("g")
            .filename(Arc::new("g.c".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(1))
            .arg_types(vec![LpcType::Int(false)])
            .build()
            .unwrap();
        let mut pf = ProgramFunction::new(prototype, 0);
        pf.arg_locations = vec![Register(1).as_local()];
        Arc::new(pf)
    }

    #[test]
    fn push_ref_aliases_the_callers_cell() {
        let txn = TxnHandle::empty();
        let cell = VarId::new();
        txn.with(|t| t.write(cell, LpcRef::from(41)));
        let mut frame = CallFrame::new(Process::default(), ref_function(), 1, None::<&[VarId]>);
        frame.push_ref(0, cell).unwrap();
        assert_eq!(
            frame.slot(Register(0).as_upvalue()).unwrap(),
            Slot::Cell(cell)
        );
        assert_eq!(
            *frame.get_location(&txn, Register(0).as_upvalue()).unwrap(),
            LpcRef::from(41)
        );
    }

    #[test]
    fn a_value_into_a_ref_parameter_is_a_runtime_error() {
        let txn = TxnHandle::empty();
        let mut frame = CallFrame::new(Process::default(), ref_function(), 1, None::<&[VarId]>);
        let err = frame
            .push_arg(&txn, 0, LpcRef::from(1))
            .unwrap_err()
            .to_string();
        assert!(
            err.contains("argument 1 of `f` must be passed by reference"),
            "{err}"
        );
    }

    #[test]
    fn a_ref_into_a_value_parameter_is_a_runtime_error() {
        let mut frame = CallFrame::new(Process::default(), value_function(), 1, None::<&[VarId]>);
        let err = frame.push_ref(0, VarId::new()).unwrap_err().to_string();
        assert!(
            err.contains("`g` does not take argument 1 by reference"),
            "{err}"
        );
    }

    #[test]
    fn a_register_location_is_not_a_cell() {
        let frame = CallFrame::new(Process::default(), value_function(), 0, None::<&[VarId]>);
        let err = frame
            .ref_cell(Register(1).as_local())
            .unwrap_err()
            .to_string();
        assert!(err.contains("is a register, not a cell"), "{err}");
    }

    #[test]
    fn an_upvalue_past_the_frame_is_an_error() {
        let prototype = FunctionPrototypeBuilder::default()
            .name("my_function")
            .filename(Arc::new("my_function".into()))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let frame = CallFrame::new(
            Process::new(Program::default()),
            Arc::new(ProgramFunction::new(prototype, 0)),
            0,
            None::<&[VarId]>,
        );

        let err = frame.slot(Register(0).as_upvalue()).unwrap_err();
        assert!(err.is_bug(), "{err}");
    }

    #[test]
    fn every_door_rejects_an_upvalue_past_the_frame() {
        let txn = TxnHandle::empty();
        let mut frame = CallFrame::new(Process::default(), value_function(), 0, None::<&[VarId]>);
        let u0 = Register(0).as_upvalue();

        assert!(frame.get_location(&txn, u0).unwrap_err().is_bug());
        assert!(
            frame
                .set_location(&txn, u0, LpcRef::from(1))
                .unwrap_err()
                .is_bug()
        );
        assert!(frame.bump_in_location(&txn, u0, 1).unwrap_err().is_bug());
    }

    #[test]
    fn a_global_round_trips_through_its_cell() {
        let txn = TxnHandle::empty();
        let program = Program {
            num_globals: 1,
            ..Program::default()
        };
        let mut frame =
            CallFrame::new(Process::new(program), value_function(), 0, None::<&[VarId]>);
        let g0 = Register(0).as_global();

        assert_eq!(*frame.get_location(&txn, g0).unwrap(), NULL);
        frame.set_location(&txn, g0, LpcRef::from(7)).unwrap();
        frame.bump_in_location(&txn, g0, 1).unwrap();
        assert_eq!(*frame.get_location(&txn, g0).unwrap(), LpcRef::from(8));
    }

    #[test]
    fn peek_int_reads_registers_and_the_pool_only() {
        let txn = TxnHandle::empty();
        let program = Program {
            num_globals: 1,
            ..Program::default()
        };
        let mut frame = CallFrame::new(
            Process::new(program),
            pooled_function(),
            0,
            None::<&[VarId]>,
        );
        let (l0, k0, k1, g0) = (
            Register(0).as_local(),
            Register(0).as_constant(),
            Register(1).as_constant(),
            Register(0).as_global(),
        );
        frame.set_location(&txn, l0, LpcRef::from(3)).unwrap();
        frame.set_location(&txn, g0, LpcRef::from(4)).unwrap();

        assert_eq!(
            (
                frame.peek_int(l0),
                frame.peek_int(k0),
                frame.peek_int(k1),
                frame.peek_int(g0)
            ),
            (Some(3), Some(5), None, None)
        );
    }

    #[test]
    fn set_int_stores_through_every_writable_door() {
        let txn = TxnHandle::empty();
        let program = Program {
            num_globals: 1,
            ..Program::default()
        };
        let mut frame = CallFrame::new(
            Process::new(program),
            pooled_function(),
            0,
            None::<&[VarId]>,
        );
        let (l0, g0, k0) = (
            Register(0).as_local(),
            Register(0).as_global(),
            Register(0).as_constant(),
        );

        frame.set_int(&txn, l0, 6).unwrap();
        frame.set_int(&txn, g0, 7).unwrap();

        assert_eq!(
            (
                *frame.get_location(&txn, l0).unwrap() == LpcRef::from(6),
                *frame.get_location(&txn, g0).unwrap() == LpcRef::from(7),
                frame.set_int(&txn, k0, 8).unwrap_err().is_bug(),
            ),
            (true, true, true)
        );
    }

    #[test]
    fn set_int_replaces_a_register_holding_another_type() {
        let txn = TxnHandle::empty();
        let mut frame = CallFrame::new(Process::default(), value_function(), 0, None::<&[VarId]>);
        let l0 = Register(0).as_local();
        frame.set_location(&txn, l0, LpcRef::from("s")).unwrap();

        frame.set_int(&txn, l0, 6).unwrap();

        assert_eq!(*frame.get_location(&txn, l0).unwrap(), LpcRef::from(6));
    }

    mod test_with_minimum_arg_capacity {
        use super::*;

        #[test]
        fn sets_up_registers_if_greater_max_is_passed() {
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .filename(Arc::new("my_function".into()))
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(4))
                .build()
                .unwrap();

            let fs = ProgramFunction::new(prototype, 7);

            let frame = CallFrame::with_minimum_arg_capacity(
                process,
                Arc::new(fs),
                4,
                30,
                None::<&[VarId]>,
            );

            assert_eq!(frame.registers.len(), 38);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }

        #[test]
        fn sets_up_registers_if_lesser_max_is_passed() {
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .filename(Arc::new("my_function".into()))
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(4))
                .build()
                .unwrap();

            let fs = ProgramFunction::new(prototype, 7);

            let frame =
                CallFrame::with_minimum_arg_capacity(process, Arc::new(fs), 4, 2, None::<&[VarId]>);

            assert_eq!(frame.registers.len(), 12);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }
    }

    mod test_populate_upvalues {
        use super::*;
        use crate::test_support::factories::SymbolFactory;

        #[test]
        fn populates_upvalues() {
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .filename(Arc::new("my_function".into()))
                .return_type(LpcType::Void)
                .build()
                .unwrap();

            let mut pf = ProgramFunction::new(prototype, 0);
            let symbol_factory = SymbolFactory::new();
            let a = symbol_factory.build(|s| {
                s.name = "a".to_string();
                s.location = Some(Register(0).as_upvalue())
            });
            let b = symbol_factory.build(|s| {
                s.name = "b".to_string();
                s.location = Some(Register(1).as_upvalue())
            });
            pf.local_variables.extend([a, b]);
            pf.num_upvalues = 2;
            let frame = CallFrame::new(process, Arc::new(pf), 0, None::<&[VarId]>);

            assert_eq!(frame.upvalue_ptrs.len(), 2);
            assert_ne!(frame.upvalue_ptrs[0], frame.upvalue_ptrs[1]);

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .filename(Arc::new("my_function".into()))
                .return_type(LpcType::Void)
                .build()
                .unwrap();

            let mut pf = ProgramFunction::new(prototype, 0);
            let symbol_factory = SymbolFactory::new();
            let a = symbol_factory.build(|s| {
                s.name = "a".to_string();
                s.location = Some(Register(0).as_upvalue())
            });
            let b = symbol_factory.build(|s| {
                s.name = "b".to_string();
                s.location = Some(Register(1).as_upvalue())
            });
            let c = symbol_factory.build(|s| {
                s.name = "c".to_string();
                s.location = Some(Register(2).as_upvalue())
            });
            pf.local_variables.extend([a, b, c]);
            pf.num_upvalues = 3;

            let frame = CallFrame::new(frame.process, Arc::new(pf), 0, None::<&[VarId]>);
            assert_eq!(frame.upvalue_ptrs.len(), 3);
        }
    }
}
