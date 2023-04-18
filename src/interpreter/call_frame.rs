use std::{
    fmt,
    fmt::{Debug, Display, Formatter},
    sync::Arc,
};

use bit_set::BitSet;
use derive_builder::Builder;
use educe::Educe;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::register::{Register, RegisterVariant};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use parking_lot::RwLock;
use tracing::{instrument, trace};

use crate::interpreter::{
    bank::RefBank,
    gc::{gc_bank::GcRefBank, mark::Mark, unique_id::UniqueId},
    lpc_ref::{LpcRef, NULL},
    process::Process,
};

/// A representation of a local variable name and value.
/// This exists only so we can stick a `Display` impl on it for
/// testing and debugging.
#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub name: String,
    pub value: LpcRef,
}

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
    pub arg_locations: Vec<RegisterVariant>,

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
    pub called_with_num_args: usize,

    /// The upvalue indexes (into `vm_upvalues`) for this specific call
    #[builder(default, setter(into))]
    pub upvalue_ptrs: Vec<Register>,

    /// The upvalue data from the [`Vm`](crate::interpreter::vm::Vm)
    #[builder(setter(into))]
    pub vm_upvalues: Arc<RwLock<GcRefBank>>,

    /// This object's unique ID, for garbage collection purposes
    #[builder(default)]
    pub unique_id: UniqueId,
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
    /// * `upvalue_ptrs` - The indexes pointing to the real data, contained in `vm_upvalues`
    /// * `vm_upvalues` - The upvalue data from the [`Vm`](crate::interpreter::vm::Vm)
    pub fn new<P>(
        process: P,
        function: Arc<ProgramFunction>,
        called_with_num_args: usize,
        upvalue_ptrs: Option<&Vec<Register>>,
        vm_upvalues: Arc<RwLock<GcRefBank>>,
    ) -> Self
    where
        P: Into<Arc<Process>>,
    {
        // add +1 for r0 (where return value is stored)
        let reg_len = std::cmp::max(function.arity().num_args, called_with_num_args)
            + function.num_locals
            + 1;
        let process = process.into();
        let ups = upvalue_ptrs.cloned().unwrap_or_default();

        let mut instance = Self {
            process,
            function,
            arg_locations: Vec::with_capacity(called_with_num_args),
            registers: RefBank::new(vec![NULL; reg_len]),
            pc: 0,
            called_with_num_args,
            upvalue_ptrs: ups,
            vm_upvalues,
            unique_id: UniqueId::new(),
        };

        instance.populate_upvalues();

        instance
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
    /// * `upvalue_ptrs` - The indexes pointing to the real data, contained in `vm_upvalues`
    /// * `vm_upvalues` - The upvalue data from the [`Vm`](crate::interpreter::vm::Vm)
    pub fn with_minimum_arg_capacity<P>(
        process: P,
        function: Arc<ProgramFunction>,
        called_with_num_args: usize,
        arg_capacity: usize,
        upvalue_ptrs: Option<&Vec<Register>>,
        vm_upvalues: Arc<RwLock<GcRefBank>>,
    ) -> Self
    where
        P: Into<Arc<Process>>,
    {
        Self {
            registers: RefBank::initialized_for_function(&function, arg_capacity),
            ..Self::new(
                process,
                function,
                called_with_num_args,
                upvalue_ptrs,
                vm_upvalues,
            )
        }
    }

    /// Reserve space for the upvalues that this call will initialize
    /// Returns the index in the [`Vm`](crate::interpreter::vm::Vm)'s `upvalues` array where the
    ///   newly-populated upvalues will be stored
    #[instrument(skip_all)]
    fn populate_upvalues(&mut self) {
        let num_upvalues = self.function.num_upvalues;

        trace!("populating upvalues: {}", num_upvalues);
        if num_upvalues == 0 {
            return;
        }

        let mut upvalues = self.vm_upvalues.write();

        // Reserve space in the proc for the actual values
        upvalues.reserve(num_upvalues);

        // Reserve space in me for the indexes
        self.upvalue_ptrs.reserve(num_upvalues);

        for _ in 0..num_upvalues {
            let idx = upvalues.insert(NULL);
            self.upvalue_ptrs.push(Register(idx));
        }
    }

    /// Assign an [`LpcRef`] to a specific location, based on the [`RegisterVariant`]
    #[inline]
    pub fn set_location(&mut self, location: RegisterVariant, lpc_ref: LpcRef) {
        match location {
            RegisterVariant::Local(reg) => {
                self.registers[reg] = lpc_ref;
            }
            RegisterVariant::Global(reg) => {
                self.process.globals.write()[reg] = lpc_ref;
            }
            RegisterVariant::Upvalue(reg) => {
                let upvalues = &self.upvalue_ptrs;
                let idx = upvalues[reg.index()];

                let mut upvalues = self.vm_upvalues.write();
                upvalues[idx] = lpc_ref;
            }
        }
    }

    /// Convenience to return a list of the local variables in this frame.
    /// Intended for debugging and testing.
    pub fn local_variables(&self) -> Vec<LocalVariable> {
        self.function
            .local_variables
            .iter()
            .map(|var| {
                let Some(loc) = var.location else {
                // This should be unreachable.
                return LocalVariable::new(var.name.clone(), NULL);
            };

                let lpc_ref = match loc {
                    RegisterVariant::Local(reg) => self.registers[reg].clone(),
                    RegisterVariant::Global(reg) => self.process.globals.read()[reg].clone(),
                    RegisterVariant::Upvalue(ptr_reg) => {
                        let upvalues = &self.upvalue_ptrs;
                        let data_reg = upvalues[ptr_reg.index()];

                        self.vm_upvalues.read()[data_reg].clone()
                    }
                };

                LocalVariable::new(var.name.clone(), lpc_ref)
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
        LpcError::new(format!("runtime error: {}", msg.as_ref()))
            .with_span(self.current_debug_span())
    }

    /// a convenience method to generate a runtime bug
    #[inline]
    pub fn runtime_bug<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::new_bug(format!("runtime bug: {}", msg.as_ref()))
            .with_span(self.current_debug_span())
    }

    /// get a string representation of the frame's current current location
    #[inline]
    pub fn to_stack_trace_format(&self) -> String {
        self.current_debug_span()
            .map(|span| format!("{} in {}()", span, self.function.name()))
            .unwrap_or_else(|| format!("(unknown) in {}()", self.function.name()))
    }
}

impl Mark for CallFrame {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        trace!("marking call frame {}", self.unique_id);
        if !processed.insert(*self.unique_id.as_ref()) {
            return Ok(());
        }

        trace!("marking upvalue ptrs: {:?}", &self.upvalue_ptrs);

        let ptrs = self.upvalue_ptrs.iter().copied().map(Register::index);

        marked.extend(ptrs);

        for lpc_ref in self.registers.iter() {
            lpc_ref.mark(marked, processed)?;
        }

        Ok(())
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
    use crate::interpreter::gc::gc_bank::GcBank;

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
        let vm_upvalues = RwLock::new(GcBank::default());

        let frame = CallFrame::new(
            process,
            Arc::new(fs),
            4,
            None,
            vm_upvalues.into(),
        );

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &NULL));
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
            let vm_upvalues = RwLock::new(GcBank::default());

            let frame = CallFrame::with_minimum_arg_capacity(
                process,
                Arc::new(fs),
                4,
                30,
                None,
                vm_upvalues.into(),
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
            let vm_upvalues = RwLock::new(GcBank::default());

            let frame = CallFrame::with_minimum_arg_capacity(
                process,
                Arc::new(fs),
                4,
                2,
                None,
                vm_upvalues.into(),
            );

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

            let vm_upvalues = Arc::new(RwLock::new(GcBank::default()));
            let frame = CallFrame::new(
                process,
                Arc::new(pf),
                0,
                None,
                vm_upvalues.clone(),
            );

            assert_eq!(frame.upvalue_ptrs, vec![Register(0), Register(1)]);
            assert_eq!(frame.vm_upvalues.read().len(), 2);

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

            let frame = CallFrame::new(frame.process, Arc::new(pf), 0, None, vm_upvalues);
            assert_eq!(
                frame.upvalue_ptrs,
                vec![Register(2), Register(3), Register(4)]
            );
            assert_eq!(frame.vm_upvalues.read().len(), 5);
        }
    }

    mod test_mark {
        use super::*;

        #[test]
        fn test_mark() {
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .filename(Arc::new("my_file".into()))
                .return_type(LpcType::Void)
                .build()
                .unwrap();

            let fs = ProgramFunction::new(prototype, 7);
            let vm_upvalues = RwLock::new(GcBank::<LpcRef>::default());

            let frame = CallFrameBuilder::default()
                .process(process)
                .function(Arc::new(fs))
                .vm_upvalues(vm_upvalues)
                .upvalue_ptrs(vec![Register(2), Register(5)])
                .build()
                .unwrap();

            let mut marked = BitSet::new();
            let mut processed = BitSet::new();

            frame.mark(&mut marked, &mut processed).unwrap();

            assert_eq!(marked.len(), 2);
            assert!(marked.contains(5));
            assert!(marked.contains(2));

            assert_eq!(processed.len(), 1);
            assert!(processed.contains(*frame.unique_id.as_ref()));

            marked.clear();

            frame.mark(&mut marked, &mut processed).unwrap();

            assert_eq!(marked.len(), 0); // still empty because we already processed the frame
        }
    }
}
