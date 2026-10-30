use lpc_rs_core::null_mode::NullMode;
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
        // usize: runtime_arg_count + num_locals + 1 can exceed u16::MAX even though each fits one.
        let num_locals = usize::from(function.num_locals);
        let static_length = usize::from(function.arity().num_args) + num_locals + 1;
        let dynamic_length = usize::from(runtime_arg_count) + num_locals + 1;
        let reservation = std::cmp::max(static_length, dynamic_length);

        let mut registers = ThinVec::with_capacity(reservation);
        registers.extend((0..reservation).map(|_| NULL));

        if function.null_mode == NullMode::Distinct {
            registers[0] = LpcRef::default_for(function.return_type(), function.null_mode);
            for (index, type_) in function.prototype.arg_types.iter().enumerate() {
                let location = function.arg_locations.get(index).copied().unwrap_or(
                    lpc_rs_core::register::RegisterVariant::Local(Register(
                        (index + 1) as RegisterSize,
                    )),
                );
                if let lpc_rs_core::register::RegisterVariant::Local(reg) = location {
                    registers[usize::from(reg.index())] =
                        LpcRef::default_for(*type_, function.null_mode);
                }
            }
        }
        RefBank { registers }
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

#[cfg(test)]
mod tests {
    use super::*;
    use lpc_rs_core::{function_arity::FunctionArity, lpc_type::LpcType};
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;
    use std::sync::Arc;

    #[test]
    fn unmapped_parameters_keep_return_and_argument_defaults_in_distinct_mode() {
        let prototype = FunctionPrototypeBuilder::default()
            .name("defaults")
            .filename(Arc::new("/defaults.c".into()))
            .return_type(LpcType::String(false))
            .arity(FunctionArity::new(2))
            .arg_types(vec![LpcType::Int(false), LpcType::String(false)])
            .build()
            .unwrap();
        let mut function = ProgramFunction::new(prototype, 1);
        function.null_mode = NullMode::Distinct;
        let bank = RefBank::initialized_for_function(&function, 4);
        assert_eq!(
            bank.registers.as_slice(),
            [
                LpcRef::Nil,
                0.into(),
                LpcRef::Nil,
                0.into(),
                0.into(),
                0.into()
            ]
        );
    }

    #[test]
    fn bank_storage_reserves_grows_and_exposes_consistent_register_views() {
        let mut bank = Bank::new(vec![10, 20]);
        bank.reserve(4);
        assert!(bank.registers.capacity() >= 6);
        bank.push(30);
        bank[Register(1)] = 21;
        let index: RegisterSize = 2;
        bank[index] = 31;
        let slice: &mut [i32] = &mut bank;
        slice[0] = 11;
        assert_eq!(bank[Register(1)], 21);
        assert_eq!(bank[index], 31);
        let slice: &[i32] = &bank;
        assert_eq!(slice, [11, 21, 31]);
        assert_eq!(bank.to_string(), "[11, 21, 31]");
    }
}
