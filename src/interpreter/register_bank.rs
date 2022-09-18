use std::{
    ops::{Index, IndexMut, Range, RangeFrom, RangeInclusive},
    slice::Iter,
};

use delegate::delegate;
use lpc_rs_core::register::Register;
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::interpreter::lpc_ref::{LpcRef, NULL};

/// A type to handle data movement (the arena itself stores the actual data)
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RegisterBank {
    /// Our storage. By convention, `registers[0]` is for the return value
    /// function calls.
    pub registers: Vec<LpcRef>,
}

impl RegisterBank {
    delegate! {
        to self.registers {
            /// Get the length of the register bank.
            pub fn len(&self) -> usize;

            /// Get whether the register bank is empty.
            pub fn is_empty(&self) -> bool;

            /// Get an iterator over the registers
            pub fn iter(&self) -> Iter<LpcRef>;

            /// Push a new LpcRef onto the end of the registers.
            pub fn push(&mut self, value: LpcRef);

            /// Reserve additional space in the underlying Vec
            pub fn reserve(&mut self, additional: usize);
        }
    }

    pub fn new(registers: Vec<LpcRef>) -> Self {
        Self { registers }
    }

    /// Get a proper-sized [`RegisterBank`] for the passed function and runtime
    /// arg count
    pub fn initialized_for_function(
        function: &ProgramFunction,
        runtime_arg_count: usize,
    ) -> RegisterBank {
        // add +1 for r0 (where return value is stored)
        let static_length = function.arity().num_args + function.num_locals + 1;
        let dynamic_length = runtime_arg_count + function.num_locals + 1;
        let reservation = std::cmp::max(static_length, dynamic_length);

        RegisterBank::new(vec![NULL; reservation])
    }
}

impl Index<Register> for RegisterBank {
    type Output = LpcRef;

    fn index(&self, register: Register) -> &LpcRef {
        &self.registers[register.index()]
    }
}

impl Index<&Register> for RegisterBank {
    type Output = LpcRef;

    fn index(&self, register: &Register) -> &LpcRef {
        &self.registers[register.index()]
    }
}

impl IndexMut<Register> for RegisterBank {
    fn index_mut(&mut self, register: Register) -> &mut LpcRef {
        &mut self.registers[register.index()]
    }
}

impl IndexMut<&Register> for RegisterBank {
    fn index_mut(&mut self, register: &Register) -> &mut LpcRef {
        &mut self.registers[register.index()]
    }
}

impl Index<usize> for RegisterBank {
    type Output = LpcRef;

    fn index(&self, index: usize) -> &LpcRef {
        &self.registers[index]
    }
}

impl IndexMut<usize> for RegisterBank {
    fn index_mut(&mut self, index: usize) -> &mut LpcRef {
        &mut self.registers[index]
    }
}

impl Index<Range<usize>> for RegisterBank {
    type Output = [LpcRef];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.registers[index]
    }
}

impl IndexMut<Range<usize>> for RegisterBank {
    fn index_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
        &mut self.registers[index]
    }
}

impl Index<RangeInclusive<usize>> for RegisterBank {
    type Output = [LpcRef];

    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.registers[index]
    }
}

impl IndexMut<RangeInclusive<usize>> for RegisterBank {
    fn index_mut(&mut self, index: RangeInclusive<usize>) -> &mut Self::Output {
        &mut self.registers[index]
    }
}

impl Index<RangeFrom<usize>> for RegisterBank {
    type Output = [LpcRef];

    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.registers[index]
    }
}

impl IndexMut<RangeFrom<usize>> for RegisterBank {
    fn index_mut(&mut self, index: RangeFrom<usize>) -> &mut Self::Output {
        &mut self.registers[index]
    }
}
