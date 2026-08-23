use std::{
    fmt::{Display, Formatter},
    ops::{Deref, DerefMut, Index, IndexMut},
};

use lpc_rs_core::{RegisterSize, register::Register};
use lpc_rs_function_support::program_function::ProgramFunction;
use thin_vec::ThinVec;

use crate::interpreter::lpc_ref::{LpcRef, NULL};

pub type RefBank = Bank<LpcRef>;

impl RefBank {
    /// Get a proper-sized [`RefBank`] for the passed function and runtime
    /// arg count.
    pub fn initialized_for_function(
        function: &ProgramFunction,
        runtime_arg_count: RegisterSize,
    ) -> RefBank {
        // add +1 for r0 (where return value is stored)
        let static_length = function.arity().num_args + function.num_locals + 1;
        let dynamic_length = runtime_arg_count + function.num_locals + 1;
        let reservation = std::cmp::max(static_length, dynamic_length);

        RefBank::new(vec![NULL; reservation as usize])
    }
}

/// A type to handle data movement (the arena itself stores the actual data)
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Bank<T> {
    /// Our storage.
    pub registers: ThinVec<T>,
}

impl<T> Bank<T> {
    /// Create a new [`Bank`] from the passed [`Vec`] of `T`s.
    #[inline]
    pub fn new(registers: Vec<T>) -> Self {
        Self {
            registers: ThinVec::from(registers),
        }
    }

    /// Push a new T onto the end of the registers.
    #[inline]
    pub fn push(&mut self, value: T) {
        self.registers.push(value);
    }

    /// Reserve additional space in the underlying Vec.
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.registers.reserve(additional);
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

impl<T> Index<Register> for Bank<T> {
    type Output = T;

    #[inline]
    fn index(&self, register: Register) -> &T {
        &self.registers[register.index() as usize]
    }
}

impl<T> IndexMut<Register> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, register: Register) -> &mut T {
        &mut self.registers[register.index() as usize]
    }
}

impl<T> Index<RegisterSize> for Bank<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: RegisterSize) -> &T {
        &self.registers[index as usize]
    }
}

impl<T> IndexMut<RegisterSize> for Bank<T> {
    #[inline]
    fn index_mut(&mut self, index: RegisterSize) -> &mut T {
        &mut self.registers[index as usize]
    }
}

impl<T> Deref for Bank<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.registers
    }
}

impl<T> DerefMut for Bank<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.registers
    }
}
