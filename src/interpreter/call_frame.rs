use std::{
    borrow::Cow,
    fmt,
    fmt::{Display, Formatter},
    sync::Arc,
};

use derive_builder::Builder;
use educe::Educe;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::{
    RegisterSize,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use thin_vec::ThinVec;
use tracing::{instrument, trace};

use crate::interpreter::{
    bank::RefBank,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::{TxnHandle, VarId},
};

/// Where a [`RegisterVariant`] resolves in a frame: a local register, or the
/// world cell behind a global or an upvalue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Slot {
    Register(Register),
    Cell(VarId),
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

    /// The actual locations of all arguments that were passed-in.
    /// Necessary for populating `argv` in ellipsis functions, e.g.
    #[builder(default)]
    pub arg_locations: ThinVec<RegisterVariant>,

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
}

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
        let process = process.into();
        let ups = upvalue_ptrs.map(Into::into).unwrap_or_default();

        let mut instance = Self {
            registers: RefBank::initialized_for_function(&function, arg_capacity),
            process,
            function,
            arg_locations: ThinVec::with_capacity(called_with_num_args as usize),
            pc: 0,
            called_with_num_args,
            upvalue_ptrs: ups,
        };

        instance.populate_upvalues();

        instance
    }

    /// Mint this call's own captured cells, after the inherited ones. A cell
    /// is an identity only; its value lives in the committer's world once written.
    #[instrument(level = "debug", skip_all)]
    fn populate_upvalues(&mut self) {
        let num_upvalues = self.function.num_upvalues;
        trace!("populating upvalues: {}", num_upvalues);
        self.upvalue_ptrs
            .extend((0..num_upvalues).map(|_| VarId::new()));
    }

    /// Bind the captured variable at `location` to a fresh cell; pointers
    /// that copied the old cell keep it.
    pub(crate) fn new_upvalue(&mut self, location: RegisterVariant) -> Result<()> {
        let RegisterVariant::Upvalue(reg) = location else {
            return Err(self.runtime_bug(format!("new_upvalue on a non-upvalue {location}")));
        };
        let Some(cell) = self.upvalue_ptrs.get_mut(reg.index() as usize) else {
            return Err(self.runtime_bug(format!(
                "upvalue {} is outside this frame's {} cells",
                reg.index(),
                self.upvalue_ptrs.len()
            )));
        };
        *cell = VarId::new();
        Ok(())
    }

    /// Store argument `i` where the function declares it, or in the next
    /// local register past the last argument for one beyond the declared list.
    pub(crate) fn push_arg(&mut self, txn: &TxnHandle, i: usize, value: LpcRef) -> Result<()> {
        let target = self
            .function
            .arg_locations
            .get(i)
            .copied()
            .unwrap_or_else(|| {
                let next = self
                    .arg_locations
                    .iter()
                    .rev()
                    .find_map(|loc| match loc {
                        RegisterVariant::Local(r) => Some(r.index() + 1),
                        _ => None,
                    })
                    .unwrap_or(1);
                Register(next).as_local()
            });
        self.arg_locations.push(target);
        self.set_location(txn, target, value)
    }

    /// Resolve `location` to its slot in this frame.
    pub(crate) fn slot(&self, location: RegisterVariant) -> Result<Slot> {
        match location {
            RegisterVariant::Local(reg) => Ok(Slot::Register(reg)),
            RegisterVariant::Global(reg) => Ok(Slot::Cell(self.process.var_id(reg.into()))),
            RegisterVariant::Upvalue(reg) => {
                let Some(&cell) = self.upvalue_ptrs.get(reg.index() as usize) else {
                    return Err(self.runtime_bug(format!(
                        "upvalue {} is outside this frame's {} cells",
                        reg.index(),
                        self.upvalue_ptrs.len()
                    )));
                };
                Ok(Slot::Cell(cell))
            }
        }
    }

    /// Read the [`LpcRef`] at `location`; an unwritten cell reads `NULL`.
    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) fn get_location(
        &self,
        txn: &TxnHandle,
        location: RegisterVariant,
    ) -> Result<Cow<'_, LpcRef>> {
        Ok(match self.slot(location)? {
            Slot::Register(reg) => Cow::Borrowed(&self.registers[reg]),
            Slot::Cell(cell) => Cow::Owned(txn.with(|t| t.read(cell).unwrap_or(NULL))),
        })
    }

    /// Assign an [`LpcRef`] to a specific location, based on the [`RegisterVariant`]
    #[inline]
    pub(crate) fn set_location(
        &mut self,
        txn: &TxnHandle,
        location: RegisterVariant,
        lpc_ref: LpcRef,
    ) -> Result<()> {
        match self.slot(location)? {
            Slot::Register(reg) => self.registers[reg] = lpc_ref,
            // A blind in-txn write: the read that computed `lpc_ref` was
            // already tracked when the caller read it.
            Slot::Cell(cell) => txn.with(|t| t.write(cell, lpc_ref)),
        }
        Ok(())
    }

    /// Apply `func` to the [`LpcRef`] at `location`, in place.
    pub(crate) fn apply_in_location<F>(
        &mut self,
        txn: &TxnHandle,
        location: RegisterVariant,
        func: F,
    ) -> Result<()>
    where
        F: FnOnce(&mut LpcRef) -> Result<()>,
    {
        match self.slot(location)? {
            Slot::Register(reg) => func(&mut self.registers[reg]),
            // In-txn read-modify-write: the read is tracked, the write
            // lands in the in-flight changeset.
            Slot::Cell(cell) => txn.with(|t| {
                let mut cur = t.read(cell).unwrap_or(NULL);
                func(&mut cur)?;
                t.write(cell, cur);
                Ok(())
            }),
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::{function_arity::FunctionArity, lpc_type::LpcType};
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

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
