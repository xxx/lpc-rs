use crate::interpreter::program::Program;
use crate::interpreter::lpc_ref::LpcRef;
use std::ops::{DerefMut, Deref};
use std::cell::RefCell;

/// A wrapper type to allow the VM to keep the immutable program and its
/// mutable runtime pieces together.
#[derive(Debug)]
pub struct InterpreterProgram {
    pub program: Program,
    pub globals: Vec<RefCell<LpcRef>>,
    // pub clones: Vec<>
}

impl InterpreterProgram {
    pub fn new(program: Program) -> Self {
        let num_globals = program.num_globals;

        Self {
            program,
            globals: vec![RefCell::new(LpcRef::Int(0)); num_globals]
        }
    }
}

impl Deref for InterpreterProgram {
    type Target = Program;

    fn deref(&self) -> &Self::Target {
        &self.program
    }
}

impl DerefMut for InterpreterProgram {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.program
    }
}