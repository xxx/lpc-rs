use crate::{
    interpreter::{lpc_ref::LpcRef, process::Process},
    semantic::program_function::ProgramFunction,
};
use std::{
    fmt,
    fmt::{Display, Formatter},
    rc::Rc,
};
use std::cell::Cell;
use crate::parser::span::Span;
use crate::asm::instruction::{Instruction, Address};

/// A representation of a function call's context.
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// A pointer to the process that owns the function being called
    pub process: Rc<Process>,
    /// The function that this frame is a call to
    pub function: Rc<ProgramFunction>,
    /// Our registers. By convention, `registers[0]` is for the return value of the call.
    pub registers: Vec<LpcRef>,
    /// Track where the pc is pointing in this frame's function's instructions.
    pc: Cell<usize>,
}

impl StackFrame {
    /// Create a new [`StackFrame`] instance
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    pub fn new(process: Rc<Process>, function: Rc<ProgramFunction>) -> Self {
        // add +1 for r0 (where return value is stored)
        let reg_len = function.num_args + function.num_locals + 1;

        Self {
            process,
            function,
            registers: vec![LpcRef::Int(0); reg_len],
            pc: 0.into(),
        }
    }

    /// Create a new [`StackFrame`] instance with space for at least `arg_capacity` registers.
    ///
    /// # Arguments
    ///
    /// * `process` - The process that owns the function being called
    /// * `function` - The function being called
    /// * `arg_capacity` - Reserve space for at least this many registers
    ///     (this is used for ellipsis args and `call_other`)
    pub fn with_minimum_arg_capacity(
        process: Rc<Process>,
        function: Rc<ProgramFunction>,
        arg_capacity: usize,
    ) -> Self {
        // add +1 for r0 (where return value is stored)
        let reg_len = function.num_args + function.num_locals + 1;
        let arg_len = arg_capacity + function.num_locals + 1;
        let reservation = std::cmp::max(reg_len, arg_len);

        Self {
            registers: vec![LpcRef::Int(0); reservation],
            ..Self::new(process, function)
        }
    }

    /// Resolve a register
    pub fn resolve_lpc_ref<I>(&self, register: I) -> LpcRef
    where
        I: Into<usize>
    {
        self.registers[register.into()].clone()
    }

    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        match self.function.debug_spans.get(self.pc.get()) {
            Some(o) => *o,
            None => None,
        }
    }

    #[inline]
    pub fn set_pc(&self, new_val: usize) {
        self.pc.replace(new_val);
    }

    #[inline]
    pub fn inc_pc(&self) {
        self.pc.replace(self.pc.get() + 1);
    }

    #[inline]
    pub fn pc(&self) -> usize {
        self.pc.get()
    }

    #[inline]
    pub fn instruction(&self) -> Option<&Instruction> {
        self.function.instructions.get(self.pc.get())
    }

    #[inline]
    pub fn lookup_label<T>(&self, label: T) -> Option<&Address>
    where
        T: AsRef<str>
    {
        self.function.labels.get(label.as_ref())
    }
}

impl Display for StackFrame {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Calling {}; Process {}\n\n",
            self.function.name, self.process.filename
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_sets_up_registers() {
        let process = Process::default();

        let fs = ProgramFunction::new("my_function", 4, 7);

        let frame = StackFrame::new(Rc::new(process), Rc::new(fs));

        assert_eq!(frame.registers.len(), 12);
        assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
    }

    mod test_with_minimum_arg_capacity {
        use super::*;

        #[test]
        fn sets_up_registers_if_greater_max_is_passed() {
            let process = Process::default();

            let fs = ProgramFunction::new("my_function", 4, 7);

            let frame = StackFrame::with_minimum_arg_capacity(Rc::new(process), Rc::new(fs), 30);

            assert_eq!(frame.registers.len(), 38);
            assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
        }

        #[test]
        fn sets_up_registers_if_lesser_max_is_passed() {
            let process = Process::default();

            let fs = ProgramFunction::new("my_function", 4, 7);

            let frame = StackFrame::with_minimum_arg_capacity(Rc::new(process), Rc::new(fs), 2);

            assert_eq!(frame.registers.len(), 12);
            assert!(frame.registers.iter().all(|r| r == &LpcRef::Int(0)));
        }
    }
}
