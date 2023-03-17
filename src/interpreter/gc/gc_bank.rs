use std::ops::{Index, IndexMut};
use bit_set::BitSet;

use delegate::delegate;
use qcell::QCellOwner;
use lpc_rs_core::register::Register;
use slab::Slab;
use lpc_rs_errors::LpcError;
use crate::interpreter::gc::unique_id::GcSweep;
use lpc_rs_errors::Result;

use crate::interpreter::lpc_ref::LpcRef;

pub type GcRefBank = GcBank<LpcRef>;

/// A [`Slab`]-backed store that can re-use freed indices.
#[derive(Debug, Default)]
pub struct GcBank<T> {
    registers: Slab<T>,
}

impl<T> GcBank<T> {
    delegate! {
        to self.registers {
            pub fn is_empty(&self) -> bool;
            pub fn len(&self) -> usize;
            pub fn insert(&mut self, value: T) -> usize;
            pub fn reserve(&mut self, additional: usize);
            pub fn try_remove(&mut self, index: usize) -> Option<T>;
        }
    }

    /// A Sweep function without the key
    pub fn keyless_sweep(&mut self, marked: &BitSet) -> Result<()> {
        for idx in marked {
            if self.try_remove(idx).is_none() {
                return Err(LpcError::new_bug(format!(
                    concat!(
                    "Failed to remove upvalue at index {} when doing a GC sweep.",
                    " This should not happen."
                    ),
                    idx
                )));
            };
        }

        Ok(())
    }

}

impl<T> GcSweep for GcBank<T> {
    fn sweep(&mut self, marked: &BitSet, _cell_key: &mut QCellOwner) -> Result<()> {
        self.keyless_sweep(marked)
    }
}

impl<T> PartialEq for GcBank<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        if self.registers.len() != other.registers.len() {
            return false;
        }

        for (i, val) in &self.registers {
            let Some(other) = other.registers.get(i) else {
                return false;
            };

            if val != other {
                return false;
            }
        }
        true
    }
}

impl<T> Eq for GcBank<T> where T: Eq {}

impl<T> Index<Register> for GcBank<T> {
    type Output = T;

    #[inline]
    fn index(&self, register: Register) -> &T {
        &self.registers[register.index()]
    }
}

impl<T> IndexMut<Register> for GcBank<T> {
    #[inline]
    fn index_mut(&mut self, register: Register) -> &mut T {
        &mut self.registers[register.index()]
    }
}

impl<T> Index<usize> for GcBank<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &T {
        &self.registers[index]
    }
}
