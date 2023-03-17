use std::ops::{Index, IndexMut};

use delegate::delegate;
use lpc_rs_core::register::Register;
use slab::Slab;
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
