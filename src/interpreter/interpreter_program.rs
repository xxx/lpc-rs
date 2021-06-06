use crate::interpreter::{lpc_ref::LpcRef, program::Program};
use std::{cell::RefCell, ops::Deref};
use crate::parser::span::Span;
use std::cell::Cell;
use crate::asm::instruction::Instruction;

/// A wrapper type to allow the VM to keep the immutable program and its
/// mutable runtime pieces together.
#[derive(Debug)]
pub struct InterpreterProgram {
    pub program: Program,
    pub globals: Vec<RefCell<LpcRef>>,
    pc: Cell<usize>
    // pub clones: Vec<>
}

impl InterpreterProgram {
    pub fn new(program: Program) -> Self {
        let num_globals = program.num_globals;

        Self {
            program,
            globals: vec![RefCell::new(LpcRef::Int(0)); num_globals],
            pc: Cell::new(0)
        }
    }

    #[inline]
    pub fn current_debug_span(&self) -> Option<Span> {
        match self.program.debug_spans.get(self.pc.get()) {
            Some(o) => {
                *o
            }
            None => None
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
}

// This is an officially sanctioned abuse of Deref.
impl Deref for InterpreterProgram {
    type Target = Program;

    fn deref(&self) -> &Self::Target {
        &self.program
    }
}
