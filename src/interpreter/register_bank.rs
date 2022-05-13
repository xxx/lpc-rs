use crate::{asm::register::Register, interpreter::lpc_ref::LpcRef};
use delegate::delegate;
use std::{
    ops::{Index, IndexMut, Range, RangeInclusive},
    slice::Iter,
};

/// A type to handle data movement (the arena itself stores the actual data)
#[derive(Debug, Clone, Default)]
pub struct RegisterBank {
    /// Our storage. By convention, `registers[0]` is for the return value function calls.
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
        }
    }

    pub fn new(registers: Vec<LpcRef>) -> Self {
        Self { registers }
    }
}

impl Index<Register> for RegisterBank {
    type Output = LpcRef;

    fn index(&self, register: Register) -> &LpcRef {
        &self.registers[register.index()]
    }
}

impl IndexMut<Register> for RegisterBank {
    fn index_mut(&mut self, register: Register) -> &mut LpcRef {
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
