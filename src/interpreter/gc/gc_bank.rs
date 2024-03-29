use std::ops::{Index, IndexMut};

use bit_set::BitSet;
use delegate::delegate;
use lpc_rs_core::register::Register;
use lpc_rs_errors::Result;
use slab::{Iter as SlabIter, Slab};
use tracing::instrument;

use crate::interpreter::{gc::sweep::Sweep, lpc_ref::LpcRef};

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
            pub fn get(&self, index: usize) -> Option<&T>;
            pub fn iter(&self) -> SlabIter<'_, T>;
        }
    }
}

impl<T> Sweep for GcBank<T> {
    #[instrument(skip(self))]
    fn sweep(&mut self, marked: &BitSet) -> Result<()> {
        // `marked` is what's still alive. The rest can be culled.
        self.registers.retain(|idx, _e| marked.contains(idx));

        Ok(())
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
