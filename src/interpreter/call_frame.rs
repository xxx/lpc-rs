use std::{
    cell::Cell,
    collections::HashSet,
    fmt,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

use bit_set::BitSet;
use educe::Educe;
use lpc_rs_asm::instruction::{Address, Instruction};
use lpc_rs_core::register::{Register, RegisterVariant};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use qcell::{QCell, QCellOwner};
use tracing::instrument;

use crate::{
    interpreter::{
        gc::unique_id::{GcMark, UniqueId},
        lpc_ref::{LpcRef, NULL},
        process::Process,
        ref_bank::RefBank,
    },
    util::qcell_debug,
};
use crate::interpreter::gc::gc_bank::{GcBank, GcRefBank};

/// A representation of a local variable name and value.
/// This exists only so we can stick a `Display` impl on it for
/// testing and debugging.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct LocalVariable {
    pub name: String,
    #[educe(Debug(method = "qcell_debug"))]
    pub value: LpcRef,
}

impl LocalVariable {
    fn new(name: String, value: LpcRef) -> Self {
        Self { name, value }
    }
}

impl Display for LocalVariable {
    fn fmt(&self, _f: &mut Formatter<'_>) -> fmt::Result {
        todo!("fix this up")
        // write!(f, "{}: {}", self.name, self.value)
    }
}

/// A representation of a function call's context.
#[derive(Educe, Clone)]
#[educe(Debug)]
pub struct CallFrame {
    /// A pointer to the process that owns the function being called
    #[educe(Debug(method = "qcell_debug"))]
    pub process: Rc<QCell<Process>>,
    /// The function that this frame is a call to
    pub function: Rc<ProgramFunction>,
    /// The actual locations of all arguments that were passed-in.
    /// Necessary for populating `argv` in ellipsis functions, e.g.
    pub arg_locations: Vec<RegisterVariant>,
    /// Our registers. By convention, `registers[0]` is for the return value of
    /// the call.
    pub registers: RefBank,
    /// Track where the pc is pointing in this frame's function's instructions.
    pc: Cell<usize>,
    /// How many explicit arguments were passed to the call that created this
    /// frame? This will include partially-applied arguments in the case
    /// that the CallFrame is for a call to a function pointer.
    pub called_with_num_args: usize,
    /// The upvalue indexes (into `vm_upvalues`) for this specific call
    pub upvalue_ptrs: Vec<Register>,
    /// The upvalue data from the [`Vm`]
    #[educe(Debug(method = "qcell_debug"))]
    pub vm_upvalues: Rc<QCell<GcRefBank>>,
    /// This object's unique ID, for garbage collection purposes
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
    /// * `upvalues` - The upvalues from the calling Function (i.e. Frame)
    pub fn new<P>(
        process: P,
        function: Rc<ProgramFunction>,
        called_with_num_args: usize,
        upvalues: Option<&Vec<Register>>,
        vm_upvalues: Rc<QCell<GcRefBank>>,
        cell_key: &mut QCellOwner,
    ) -> Self
    where
        P: Into<Rc<QCell<Process>>>,
    {
        // add +1 for r0 (where return value is stored)
        let reg_len = function.arity().num_args + function.num_locals + 1;
        let process = process.into();
        let ups = upvalues.cloned().unwrap_or_default();

        let mut instance = Self {
            process,
            function,
            arg_locations: Vec::with_capacity(called_with_num_args),
            registers: RefBank::new(vec![NULL; reg_len]),
            pc: 0.into(),
            called_with_num_args,
            upvalue_ptrs: ups,
            vm_upvalues,
            unique_id: UniqueId::new(),
        };

        instance.populate_upvalues(cell_key);

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
    /// * `upvalues` - The indexes pointing to the real data, contained in `vm_upvalues`
    /// * `vm_upvalues` - The upvalue data from the [`Vm`]
    pub fn with_minimum_arg_capacity<P>(
        process: P,
        function: Rc<ProgramFunction>,
        called_with_num_args: usize,
        arg_capacity: usize,
        upvalues: Option<&Vec<Register>>,
        vm_upvalues: Rc<QCell<GcRefBank>>,
        cell_key: &mut QCellOwner,
    ) -> Self
    where
        P: Into<Rc<QCell<Process>>>,
    {
        Self {
            registers: RefBank::initialized_for_function(&function, arg_capacity),
            ..Self::new(process, function, called_with_num_args, upvalues, vm_upvalues, cell_key)
        }
    }

    /// Create a new [`CallFrame`] instance, using the passed [`RefBank`]
    /// as the registers
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    /// * `called_with_num_args` - how many arguments were explicitly passed in
    ///   the call to this function?
    /// * `registers` - The registers that the CallFrame will use
    /// * `upvalues` - The upvalues from the calling Function (i.e. Frame)
    pub fn with_registers<P>(
        process: P,
        function: Rc<ProgramFunction>,
        called_with_num_args: usize,
        registers: RefBank,
        upvalues: Option<&Vec<Register>>,
        vm_upvalues: Rc<QCell<GcRefBank>>,
        cell_key: &mut QCellOwner,
    ) -> Self
    where
        P: Into<Rc<QCell<Process>>>,
    {
        Self {
            registers,
            ..Self::new(process, function, called_with_num_args, upvalues, vm_upvalues, cell_key)
        }
    }

    /// Reserve space for the upvalues that this call will initialize
    /// Returns the index in the Process' `upvalues` array where the
    ///   newly-populated upvalues will be stored
    fn populate_upvalues(&mut self, cell_key: &mut QCellOwner) {
        let num_upvalues = self.function.num_upvalues;

        let upvalues = self.vm_upvalues.rw(cell_key);

        // Reserve space in the proc for the actual values
        upvalues.reserve(num_upvalues);

        // Reserve space in me for the indexes
        self.upvalue_ptrs.reserve(num_upvalues);

        for _ in 0..num_upvalues {
            let idx = upvalues.insert(NULL);
            self.upvalue_ptrs.push(Register(idx));
        }
    }

    /// Assign an [`LpcRef`] to a specific location, based on the variant
    pub fn set_location(
        &mut self,
        location: RegisterVariant,
        lpc_ref: LpcRef,
        cell_key: &mut QCellOwner,
    ) {
        match location {
            RegisterVariant::Local(reg) => {
                self.registers[reg] = lpc_ref;
            }
            RegisterVariant::Global(reg) => {
                let proc = self.process.rw(cell_key);
                proc.globals[reg] = lpc_ref;
            }
            RegisterVariant::Upvalue(reg) => {
                let upvalues = &self.upvalue_ptrs;
                let idx = upvalues[reg.index()];

                let upvalues = self.vm_upvalues.rw(cell_key);
                upvalues[idx] = lpc_ref;
            }
        }
    }

    /// Convenience to return a list of the local variables in this frame.
    /// Intended for debugging and testing.
    pub fn local_variables(&self, cell_key: &QCellOwner) -> Vec<LocalVariable> {
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
                    RegisterVariant::Global(reg) => self.process.ro(cell_key).globals[reg].clone(),
                    RegisterVariant::Upvalue(ptr_reg) => {
                        let upvalues = &self.upvalue_ptrs;
                        let data_reg = upvalues[ptr_reg.index()];

                        self.vm_upvalues.ro(cell_key)[data_reg].clone()
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
        let idx = self.pc.get().saturating_sub(1);
        self.function.debug_spans.get(idx).and_then(|s| *s)
    }

    /// set the pc to a specific value
    #[inline]
    pub fn set_pc(&self, new_val: usize) {
        self.pc.replace(new_val);
    }

    /// Set the pc to the address for the passed label.
    /// Returns an error if the label is not found.
    #[inline]
    pub fn set_pc_from_label(&self, label: &str) -> Result<()> {
        match self.lookup_label(label) {
            Some(a) => {
                self.pc.replace(*a);
                Ok(())
            }
            None => Err(self.runtime_error(format!("Unable to find address for {label}"))),
        }
    }

    /// increment the pc
    #[inline]
    pub fn inc_pc(&self) {
        self.pc.replace(self.pc.get() + 1);
    }

    /// get the pc value
    #[inline]
    pub fn pc(&self) -> usize {
        self.pc.get()
    }

    /// get the current instruction
    #[inline]
    #[instrument(skip(self))]
    pub fn instruction(&self) -> Option<&Instruction> {
        self.function.instructions.get(self.pc.get())
    }

    /// lookup a label's address by name
    #[inline]
    #[instrument(skip(self))]
    pub fn lookup_label<T>(&self, label: T) -> Option<&Address>
    where
        T: AsRef<str> + Debug,
    {
        self.function.labels.get(label.as_ref())
    }

    /// a convenience method to generate a runtime error
    #[inline]
    pub fn runtime_error<T: AsRef<str>>(&self, msg: T) -> LpcError {
        LpcError::new(format!("runtime error: {}", msg.as_ref()))
            .with_span(self.current_debug_span())
    }

    /// get a string representation of the frame's current current location
    pub fn to_stack_trace_format(&self) -> String {
        self.current_debug_span()
            .map(|span| format!("{} in {}()", span, self.function.name()))
            .unwrap_or_else(|| format!("(unknown) in {}()", self.function.name()))
    }
}

impl GcMark for CallFrame {
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut HashSet<UniqueId>,
        _cell_key: &QCellOwner,
    ) -> Result<()> {
        if !processed.insert(self.unique_id) {
            return Ok(());
        }

        marked.extend(self.upvalue_ptrs.iter().copied().map(Register::index));

        for lpc_ref in self.registers.iter() {
            lpc_ref.mark(marked, processed, _cell_key)?;
        }

        Ok(())
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_stack_trace_format())
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::{function_arity::FunctionArity, lpc_type::LpcType};
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

    use super::*;

    #[test]
    fn new_sets_up_registers() {
        let mut cell_key = QCellOwner::new();
        let process = Process::default();

        let prototype = FunctionPrototypeBuilder::default()
            .name("my_function")
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(4))
            .build()
            .unwrap();

        let fs = ProgramFunction::new(prototype, 7);
        let vm_upvalues = cell_key.cell(GcBank::default());

        let frame = CallFrame::new(
            cell_key.cell(process),
            Rc::new(fs),
            4,
            None,
            vm_upvalues.into(),
            &mut cell_key
        );

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &NULL));
    }

    mod test_with_minimum_arg_capacity {
        use super::*;

        #[test]
        fn sets_up_registers_if_greater_max_is_passed() {
            let mut cell_key = QCellOwner::new();
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(4))
                .build()
                .unwrap();

            let fs = ProgramFunction::new(prototype, 7);
            let vm_upvalues = cell_key.cell(GcBank::default());

            let frame = CallFrame::with_minimum_arg_capacity(
                cell_key.cell(process),
                Rc::new(fs),
                4,
                30,
                None,
                vm_upvalues.into(),
                &mut cell_key,
            );

            assert_eq!(frame.registers.len(), 38);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }

        #[test]
        fn sets_up_registers_if_lesser_max_is_passed() {
            let mut cell_key = QCellOwner::new();
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(4))
                .build()
                .unwrap();

            let fs = ProgramFunction::new(prototype, 7);
            let vm_upvalues = cell_key.cell(GcBank::default());

            let frame = CallFrame::with_minimum_arg_capacity(
                cell_key.cell(process),
                Rc::new(fs),
                4,
                2,
                None,
                vm_upvalues.into(),
                &mut cell_key,
            );

            assert_eq!(frame.registers.len(), 12);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }
    }

    mod test_with_registers {
        use super::*;

        #[test]
        fn uses_the_passed_registers() {
            let mut cell_key = QCellOwner::new();
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(4))
                .build()
                .unwrap();

            let fs = ProgramFunction::new(prototype, 7);

            let registers = RefBank::new(vec![NULL; 21]);
            let vm_upvalues = cell_key.cell(GcBank::default());

            let frame = CallFrame::with_registers(
                cell_key.cell(process),
                Rc::new(fs),
                4,
                registers,
                None,
                vm_upvalues.into(),
                &mut cell_key,
            );

            assert_eq!(frame.registers.len(), 21);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }
    }

    mod test_populate_upvalues {
        use super::*;
        use crate::test_support::factories::SymbolFactory;

        #[test]
        fn populates_upvalues() {
            let mut cell_key = QCellOwner::new();
            let process = Process::default();

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
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

            let vm_upvalues = Rc::new(cell_key.cell(GcBank::default()));
            let frame = CallFrame::new(cell_key.cell(process), Rc::new(pf), 0, None, vm_upvalues.clone(), &mut cell_key);

            assert_eq!(frame.upvalue_ptrs, vec![Register(0), Register(1)]);
            assert_eq!(frame.vm_upvalues.ro(&cell_key).len(), 2);

            let prototype = FunctionPrototypeBuilder::default()
                .name("my_function")
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

            let frame = CallFrame::new(frame.process, Rc::new(pf), 0, None, vm_upvalues.clone(), &mut cell_key);
            assert_eq!(frame.upvalue_ptrs, vec![Register(2), Register(3), Register(4)]);
            assert_eq!(frame.vm_upvalues.ro(&cell_key).len(), 5);
        }
    }
}
