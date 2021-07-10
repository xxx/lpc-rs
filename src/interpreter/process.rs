use crate::{
    asm::instruction::Instruction,
    interpreter::{lpc_ref::LpcRef, program::Program},
    parser::span::Span,
};
use delegate::delegate;
use std::{
    borrow::Cow,
    cell::{Cell, RefCell},
    ops::Deref,
    path::Path,
};
use std::rc::Rc;

/// A wrapper type to allow the VM to keep the immutable `program` and its
/// mutable runtime pieces together.
#[derive(PartialEq, Eq, Debug, Default)]
pub struct Process {
    pub program: Rc<Program>,
    pub globals: Vec<RefCell<LpcRef>>,
    pc: Cell<usize>,
    /// What is the clone ID of this process? If `None`, this is a master object
    clone_id: Option<usize>,
}

impl Process {
    pub fn new(program: Program) -> Self {
        let num_globals = program.num_globals;

        Self {
            program: program.into(),
            globals: vec![RefCell::new(LpcRef::Int(0)); num_globals],
            pc: Cell::new(0),
            clone_id: None
        }
    }

    pub fn new_clone(program: Rc<Program>, clone_id: usize) -> Self {
        let num_globals = program.num_globals;

        Self {
            program,
            globals: vec![RefCell::new(LpcRef::Int(0)); num_globals],
            pc: Cell::new(0),
            clone_id: Some(clone_id)
        }
    }

    delegate! {
        to self.program {
            /// Get the program's current working directory
            pub fn cwd(&self) -> Cow<'_, Path>;
        }
    }

    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        match self.program.debug_spans.get(self.pc.get()) {
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
        self.instructions.get(self.pc.get())
    }

    /// Get the filename of this process, including the clone ID suffix if present.
    #[inline]
    pub fn filename(&self) -> Cow<str> {
        match self.clone_id {
            Some(x) => Cow::Owned(format!("{}#{}", self.program.filename, x)),
            None => Cow::Borrowed(&self.program.filename),
        }
    }

    /// Get the filename with the passed prefix stripped off, defaulting to the `program` filename
    /// if that fails.
    #[inline]
    pub fn localized_filename(&self, prefix: &str) -> String {
        self
            .filename()
            .strip_prefix(prefix)
            .unwrap_or_else(|| self.program.filename.as_ref())
            .into()
    }
}

// This is an officially sanctioned abuse of Deref.
impl Deref for Process {
    type Target = Program;

    fn deref(&self) -> &Self::Target {
        &self.program
    }
}
