use std::{
    cell::{Cell, RefCell},
    fmt,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

use lpc_rs_asm::instruction::{Address, Instruction};
use lpc_rs_errors::{span::Span, LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::instrument;
use lpc_rs_core::register::{Register, RegisterVariant};

use crate::interpreter::{process::Process, register_bank::RegisterBank};
use crate::interpreter::lpc_ref::{LpcRef, NULL};

/// A representation of a local variable name and value.
/// This exists only so we can stick a Display impl on it for
/// testing and debugging.
#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub name: String,
    pub value: LpcRef
}

impl LocalVariable {
    fn new(name: String, value: LpcRef) -> Self {
        Self { name, value }
    }
}

impl Display for LocalVariable {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.value)
    }
}

/// A representation of a function call's context.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// A pointer to the process that owns the function being called
    pub process: Rc<RefCell<Process>>,
    /// The function that this frame is a call to
    pub function: Rc<ProgramFunction>,
    /// Our registers. By convention, `registers[0]` is for the return value of
    /// the call.
    pub registers: RegisterBank,
    /// Track where the pc is pointing in this frame's function's instructions.
    pc: Cell<usize>,
    /// How many explicit arguments were passed to the call that created this
    /// frame? This will include partially-applied arguments in the case
    /// that the CallFrame is for a call to a function pointer.
    pub called_with_num_args: usize,
    /// The upvalue indexes for this specific call
    pub upvalues: Vec<Register>
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
    pub fn new<P>(process: P, function: Rc<ProgramFunction>, called_with_num_args: usize, upvalues: Option<&Vec<Register>>) -> Self
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        // add +1 for r0 (where return value is stored)
        let reg_len = function.arity().num_args + function.num_locals + 1;
        let process = process.into();
        let ups = upvalues.cloned().unwrap_or_default();

        {
            let pr = process.borrow();
            println!("calling: {} - new frame upvalues: {:?} || process upvalues: {:?}", &function.name(), upvalues, pr.upvalues);
        }

        let mut instance = Self {
            process,
            function,
            registers: RegisterBank::new(vec![NULL; reg_len]),
            pc: 0.into(),
            called_with_num_args,
            upvalues: ups,
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
    /// * `upvalues` - The upvalues from the calling Function (i.e. Frame)
    pub fn with_minimum_arg_capacity<P>(
        process: P,
        function: Rc<ProgramFunction>,
        called_with_num_args: usize,
        arg_capacity: usize,
        upvalues: Option<&Vec<Register>>,
    ) -> Self
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        Self {
            registers: RegisterBank::initialized_for_function(&*function, arg_capacity),
            ..Self::new(process, function, called_with_num_args, upvalues)
        }
    }

    /// Create a new [`CallFrame`] instance, using the passed [`RegisterBank`]
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
        registers: RegisterBank,
        upvalues: Option<&Vec<Register>>,
    ) -> Self
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        Self {
            registers,
            ..Self::new(process, function, called_with_num_args, upvalues)
        }
    }

    /// Reserve space for the upvalues that this call will initialize
    fn populate_upvalues(&mut self) -> usize {
        let num_upvalues = self.function.num_upvalues;

        let start_idx = {
            let mut proc = self.process.borrow_mut();
            let upvalues = &mut proc.upvalues;
            let idx = upvalues.len();

            upvalues.reserve(num_upvalues);
            for _ in 0..num_upvalues {
                upvalues.push(NULL);
            }

            idx
        };

        self.upvalues.reserve(num_upvalues);
        for i in 0..num_upvalues {
            self.upvalues.push(Register(start_idx + i));
        }

        start_idx
    }

    /// Assign an [`LpcRef`] to a specific location, based on the variant
    pub fn set_location(&mut self, location: &RegisterVariant, lpc_ref: LpcRef) {
        match location {
            RegisterVariant::Local(reg) => {
                self.registers[*reg] = lpc_ref;
            }
            RegisterVariant::Global(reg) => {
                let mut proc = self.process.borrow_mut();
                proc.globals[*reg] = lpc_ref;
            },
            RegisterVariant::Upvalue(reg) => {
                let upvalues = &self.upvalues;
                let idx = upvalues[reg.index()];

                let mut proc = self.process.borrow_mut();
                proc.upvalues[idx] = lpc_ref;
            }
        }
    }

    /// Convenience to return a list of the local variables in this frame.
    /// Intended for debugging and testing.
    pub fn local_variables(&self) -> Vec<LocalVariable> {
        self.function.local_variables.iter().map(|var| {
            let Some(loc) = var.location else {
                // This should be unreachable.
                return LocalVariable::new(var.name.clone(), NULL);
            };

            let lpc_ref = match loc {
                RegisterVariant::Local(reg) => self.registers[reg].clone(),
                RegisterVariant::Global(reg) => self.process.borrow().globals[reg].clone(),
                RegisterVariant::Upvalue(ptr_reg) => {
                    let upvalues = &self.upvalues;
                    let data_reg = upvalues[ptr_reg.index()];

                    self.process.borrow().upvalues[data_reg].clone()
                }
            };

            LocalVariable::new(var.name.clone(), lpc_ref)
        }).collect()
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
            None => Err(self.runtime_error(format!("Unable to find address for {}", label))),
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
    // TODO: make this the Display implementation
    pub fn to_stack_trace_format(&self) -> String {
        self.current_debug_span()
            .map(|span| format!("{} in {}()", span, self.function.name()))
            .unwrap_or_else(|| format!("(unknown) in {}()", self.function.name()))
    }
}

impl Display for CallFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Calling {}; Process {}\n\n",
            self.function.name(),
            self.process.borrow().filename
        )
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::{
        function_arity::FunctionArity, function_flags::FunctionFlags, lpc_type::LpcType,
    };
    use lpc_rs_function_support::function_prototype::FunctionPrototype;

    use super::*;

    #[test]
    fn new_sets_up_registers() {
        let process = Process::default();

        let prototype = FunctionPrototype::new(
            "my_function",
            LpcType::Void,
            FunctionArity::new(4),
            FunctionFlags::default(),
            None,
            vec![],
            vec![],
        );

        let fs = ProgramFunction::new(prototype, 7);

        let frame = CallFrame::new(process, Rc::new(fs), 4, None);

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &NULL));
    }

    mod test_with_minimum_arg_capacity {
        use super::*;

        #[test]
        fn sets_up_registers_if_greater_max_is_passed() {
            let process = Process::default();

            let prototype = FunctionPrototype::new(
                "my_function",
                LpcType::Void,
                FunctionArity::new(4),
                FunctionFlags::default(),
                None,
                vec![],
                vec![],
            );

            let fs = ProgramFunction::new(prototype, 7);

            let frame = CallFrame::with_minimum_arg_capacity(process, Rc::new(fs), 4, 30, None);

            assert_eq!(frame.registers.len(), 38);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }

        #[test]
        fn sets_up_registers_if_lesser_max_is_passed() {
            let process = Process::default();

            let prototype = FunctionPrototype::new(
                "my_function",
                LpcType::Void,
                FunctionArity::new(4),
                FunctionFlags::default(),
                None,
                vec![],
                vec![],
            );

            let fs = ProgramFunction::new(prototype, 7);

            let frame = CallFrame::with_minimum_arg_capacity(process, Rc::new(fs), 4, 2, None);

            assert_eq!(frame.registers.len(), 12);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }
    }

    mod test_with_registers {
        use super::*;

        #[test]
        fn uses_the_passed_registers() {
            let process = Process::default();

            let prototype = FunctionPrototype::new(
                "my_function",
                LpcType::Void,
                FunctionArity::new(4),
                FunctionFlags::default(),
                None,
                vec![],
                vec![],
            );

            let fs = ProgramFunction::new(prototype, 7);

            let registers = RegisterBank::new(vec![NULL; 21]);

            let frame = CallFrame::with_registers(process, Rc::new(fs), 4, registers, None);

            assert_eq!(frame.registers.len(), 21);
            assert!(frame.registers.iter().all(|r| r == &NULL));
        }
    }

    mod test_populate_upvalues {
        use crate::test_support::factories::SymbolFactory;
        use super::*;

        #[test]
        fn populates_upvalues() {
            let process = Process::default();

            let prototype = FunctionPrototype::new(
                "foo",
                LpcType::Void,
                Default::default(),
                Default::default(),
                None,
                vec![],
                vec![]
            );

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

            let frame = CallFrame::new(process, Rc::new(pf), 0, None);

            assert_eq!(frame.upvalues, vec![Register(0), Register(1)]);
            assert_eq!(frame.process.borrow().upvalues.len(), 2);

            let prototype = FunctionPrototype::new(
                "foo",
                LpcType::Void,
                Default::default(),
                Default::default(),
                None,
                vec![],
                vec![]
            );

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

            let frame = CallFrame::new(frame.process.clone(), Rc::new(pf), 0, None);
            assert_eq!(frame.upvalues, vec![Register(2), Register(3), Register(4)]);
            assert_eq!(frame.process.borrow().upvalues.len(), 5);
        }
    }
}
