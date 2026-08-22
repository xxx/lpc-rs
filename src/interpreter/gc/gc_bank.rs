use std::ops::{Index, IndexMut};

use delegate::delegate;
use lpc_rs_core::register::Register;
use slab::Slab;

use crate::interpreter::stm::VarId;

/// The Vm's upvalue bank. Each slot holds a transactional [`VarId`]
/// (see the note on `SVar` in `crate::interpreter::stm`): the slab owns
/// only the slot's *identity* for GC-root purposes, while the committed
/// value lives in the committer's world. Index re-use after a sweep is
/// harmless — a reused slot simply mints a fresh `VarId`.
pub(crate) type GcVarIdBank = GcBank<VarId>;

/// A [`Slab`]-backed store that can re-use freed indices.
#[derive(Debug)]
pub(crate) struct GcBank<T> {
    registers: Slab<T>,
}

impl<T> Default for GcBank<T> {
    fn default() -> Self {
        Self {
            registers: Slab::new(),
        }
    }
}

impl<T> GcBank<T> {
    delegate! {
        to self.registers {
            #[cfg(test)]
            pub fn len(&self) -> usize;
            pub fn insert(&mut self, value: T) -> usize;
            pub fn reserve(&mut self, additional: usize);
            pub fn iter(&self) -> impl Iterator<Item = (usize, &T)>;
            pub fn retain<F>(&mut self, f: F)
            where
                F: FnMut(usize, &mut T) -> bool;
        }
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
        &self.registers[register.index() as usize]
    }
}

impl<T> IndexMut<Register> for GcBank<T> {
    #[inline]
    fn index_mut(&mut self, register: Register) -> &mut T {
        &mut self.registers[register.index() as usize]
    }
}

impl<T> Index<usize> for GcBank<T> {
    type Output = T;

    #[inline]
    fn index(&self, index: usize) -> &T {
        &self.registers[index]
    }
}
