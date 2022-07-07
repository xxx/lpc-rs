use crate::{
    asm::instruction::{Address, Instruction},
    errors::LpcError,
    interpreter::{
        function_type::FunctionName, lpc_ref::LpcRef, lpc_value::LpcValue, process::Process,
        register_bank::RegisterBank,
    },
    parser::span::Span,
    semantic::program_function::ProgramFunction,
    try_extract_value, Result,
};
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    fmt,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};
use tracing::instrument;

/// A representation of a function call's context.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// A pointer to the process that owns the function being called
    pub process: Rc<RefCell<Process>>,
    /// The function that this frame is a call to
    pub function: Rc<ProgramFunction>,
    /// Our registers. By convention, `registers[0]` is for the return value of the call.
    pub registers: RegisterBank,
    /// Track where the pc is pointing in this frame's function's instructions.
    pc: Cell<usize>,
    /// How many explicit arguments were passed to the call that created this frame?
    /// This will include partially-applied arguments in the case that
    /// the CallFrame is for a call to a function pointer.
    pub called_with_num_args: usize,
}

impl CallFrame {
    /// Create a new [`CallFrame`] instance
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    /// * `called_with_num_args` - how many arguments were explicitly passed in the
    ///      call to this function?
    pub fn new<P>(process: P, function: Rc<ProgramFunction>, called_with_num_args: usize) -> Self
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        // add +1 for r0 (where return value is stored)
        let reg_len = function.arity().num_args + function.num_locals + 1;

        Self {
            process: process.into(),
            function,
            registers: RegisterBank::new(vec![LpcRef::Int(0); reg_len]),
            pc: 0.into(),
            called_with_num_args,
        }
    }

    /// Create a new [`CallFrame`] instance with space for at least `arg_capacity` registers.
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    /// * `called_with_num_args` - how many arguments were explicitly passed in the
    ///      call to this function?
    /// * `arg_capacity` - Reserve space for at least this many registers
    ///     (this is used for ellipsis args and `call_other`)
    pub fn with_minimum_arg_capacity<P>(
        process: P,
        function: Rc<ProgramFunction>,
        called_with_num_args: usize,
        arg_capacity: usize,
    ) -> Self
    where
        P: Into<Rc<RefCell<Process>>>,
    {
        // add +1 for r0 (where return value is stored)
        let reg_len = function.arity().num_args + function.num_locals + 1;
        let arg_len = arg_capacity + function.num_locals + 1;
        let reservation = std::cmp::max(reg_len, arg_len);

        Self {
            registers: RegisterBank::new(vec![LpcRef::Int(0); reservation]),
            ..Self::new(process, function, called_with_num_args)
        }
    }

    /// Resolve a register
    #[inline]
    pub fn resolve_lpc_ref<I>(&self, register: I) -> LpcRef
    where
        I: Into<usize>,
    {
        self.registers[register.into()].clone()
    }

    /// Get the true name of the function to call.
    #[instrument(skip(self))]
    pub fn resolve_function_name<'a>(&'a self, name: &'a FunctionName) -> Result<Cow<'a, str>> {
        match name {
            FunctionName::Var(reg) => {
                let name_ref = self.resolve_lpc_ref(reg.index());

                if let LpcRef::String(s) = name_ref {
                    let b = s.borrow();
                    let str = try_extract_value!(*b, LpcValue::String);
                    Ok(str.clone().into())
                } else {
                    Err(self.runtime_error("Found function var that didn't resolve to a string?"))
                }
            }
            FunctionName::Literal(s) => Ok(s.into()),
        }
    }

    /// get the debug span for the current instruction
    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        // subtract 1, because we increment the pc after fetching an instruction,
        // but before evaluating it.
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
    use super::*;
    use crate::{
        core::{function_arity::FunctionArity, lpc_type::LpcType},
        semantic::{function_flags::FunctionFlags, function_prototype::FunctionPrototype},
    };

    #[test]
    fn new_sets_up_registers() {
        let process = Process::default();

        let prototype = FunctionPrototype::new(
            "my_function",
            LpcType::Void,
            FunctionArity::new(4),
            FunctionFlags::default(),
            None,
            Vec::new(),
            Vec::new(),
        );

        let fs = ProgramFunction::new(prototype, 7);

        let frame = CallFrame::new(process, Rc::new(fs), 4);

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
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
                Vec::new(),
                Vec::new(),
            );

            let fs = ProgramFunction::new(prototype, 7);

            let frame = CallFrame::with_minimum_arg_capacity(process, Rc::new(fs), 4, 30);

            assert_eq!(frame.registers.len(), 38);
            assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
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
                Vec::new(),
                Vec::new(),
            );

            let fs = ProgramFunction::new(prototype, 7);

            let frame = CallFrame::with_minimum_arg_capacity(process, Rc::new(fs), 4, 2);

            assert_eq!(frame.registers.len(), 12);
            assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
        }
    }
}
