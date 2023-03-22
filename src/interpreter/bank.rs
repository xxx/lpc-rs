use core::slice::SliceIndex;
use std::{
    fmt::{Display, Formatter},
    ops::{Deref, Index, IndexMut, Range, RangeFrom, RangeInclusive},
    slice::Iter,
    vec::IntoIter,
};

use bit_set::BitSet;
use delegate::delegate;
use lpc_rs_core::register::Register;
use lpc_rs_function_support::program_function::ProgramFunction;
use qcell::QCellOwner;

use crate::interpreter::{
    gc::mark::Mark,
    lpc_ref::{LpcRef, NULL},
};

pub type RefBank = Bank<LpcRef>;

impl RefBank {
    /// Get a proper-sized [`RefBank`] for the passed function and runtime
    /// arg count.
    pub fn initialized_for_function(
        function: &ProgramFunction,
        runtime_arg_count: usize,
    ) -> RefBank {
        // add +1 for r0 (where return value is stored)
        let static_length = function.arity().num_args + function.num_locals + 1;
        let dynamic_length = runtime_arg_count + function.num_locals + 1;
        let reservation = std::cmp::max(static_length, dynamic_length);

        RefBank::new(vec![NULL; reservation])
    }
}

/// A type to handle data movement (the arena itself stores the actual data)
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Bank<T> {
    /// Our storage.
    pub registers: Vec<T>,
}

impl<T> Bank<T> {
    delegate! {
        to self.registers {
            /// Get the length of the register bank.
            pub fn len(&self) -> usize;

            /// Get whether the register bank is empty.
            pub fn is_empty(&self) -> bool;

            /// Get an iterator over the registers.
            pub fn iter(&self) -> Iter<T>;

            /// Push a new T onto the end of the registers.
            pub fn push(&mut self, value: T);

            /// Reserve additional space in the underlying Vec.
            pub fn reserve(&mut self, additional: usize);

            /// Get an element.
            pub fn get<I>(&self, index: I) -> Option<&I::Output> where I: SliceIndex<[T]>;
        }
    }

    /// Create a new [`Bank`] from the passed [`Vec`] of `T`s.
    #[inline]
    pub fn new(registers: Vec<T>) -> Self {
        Self { registers }
    }
}

impl<T> Display for Bank<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = "[".to_string();
        for (i, register) in self.registers.iter().enumerate() {
            if i != 0 {
                s.push_str(", ");
            }
            s.push_str(&format!("{register}"));
        }
        s.push(']');
        write!(f, "{s}")
    }
}

impl<T> Mark for Bank<T>
where
    T: Mark,
{
    fn mark(
        &self,
        marked: &mut BitSet,
        processed: &mut BitSet,
        cell_key: &QCellOwner,
    ) -> lpc_rs_errors::Result<()> {
        for register in self.registers.iter() {
            register.mark(marked, processed, cell_key)?;
        }
        Ok(())
    }
}

impl<T> Index<Register> for Bank<T> {
    type Output = T;

    #[inline]
    fn index(&self, register: Register) -> &T {
        &self.registers[register.index()]
    }
}

impl<T> IndexMut<Register> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, register: Register) -> &mut T {
        &mut self.registers[register.index()]
    }
}

impl<T> Index<usize> for Bank<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &T {
        &self.registers[index]
    }
}

impl<T> IndexMut<usize> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut T {
        &mut self.registers[index]
    }
}

impl<T> Index<Range<usize>> for Bank<T> {
    type Output = [T];

    #[inline]
    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.registers[index]
    }
}

impl<T> IndexMut<Range<usize>> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
        &mut self.registers[index]
    }
}

impl<T> Index<RangeInclusive<usize>> for Bank<T> {
    type Output = [T];

    #[inline]
    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.registers[index]
    }
}

impl<T> IndexMut<RangeInclusive<usize>> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, index: RangeInclusive<usize>) -> &mut Self::Output {
        &mut self.registers[index]
    }
}

impl<T> Index<RangeFrom<usize>> for Bank<T> {
    type Output = [T];

    #[inline]
    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.registers[index]
    }
}

impl<T> IndexMut<RangeFrom<usize>> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, index: RangeFrom<usize>) -> &mut Self::Output {
        &mut self.registers[index]
    }
}

impl<T> IntoIterator for Bank<T> {
    type Item = T;
    type IntoIter = IntoIter<Self::Item>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.registers.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Bank<T> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.registers.iter()
    }
}

impl<T> Deref for Bank<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.registers
    }
}
