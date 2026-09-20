//! A program and the global cells addressed by its executable layout.

use std::sync::Arc;

use lpc_rs_core::RegisterSize;

use crate::interpreter::{
    lpc_ref::LpcRef,
    program::Program,
    stm::{SVar, VarId},
};

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct ProgramImage {
    pub program: Arc<Program>,
    pub generation: VarId,
    globals: Box<[SVar<LpcRef>]>,
}

impl ProgramImage {
    pub(crate) fn new(program: Arc<Program>) -> Self {
        let mut globals: Vec<_> = (0..program.num_globals).map(|_| SVar::new()).collect();
        for &slot in program.global_views.iter() {
            globals.push(globals[usize::from(slot)].clone());
        }
        Self {
            program,
            generation: VarId::new(),
            globals: globals.into_boxed_slice(),
        }
    }

    pub(crate) fn global(&self, index: usize) -> VarId {
        self.globals[index].id
    }

    pub(crate) fn var_id(&self, index: RegisterSize) -> VarId {
        self.global(usize::from(index))
    }

    pub(crate) fn migrate(
        program: Arc<Program>,
        old: &Self,
        retained: &[(RegisterSize, RegisterSize)],
    ) -> Self {
        let mut image = Self::new(program);
        for &(old_slot, new_slot) in retained {
            image.globals[usize::from(new_slot)] = old.globals[usize::from(old_slot)].clone();
        }
        for (index, &slot) in image.program.global_views.iter().enumerate() {
            image.globals[usize::from(image.program.num_globals) + index] =
                image.globals[usize::from(slot)].clone();
        }
        image
    }

    pub(crate) fn world_var_ids(&self) -> impl Iterator<Item = VarId> + '_ {
        self.globals[..usize::from(self.program.num_globals)]
            .iter()
            .map(|slot| slot.id)
            .chain(std::iter::once(self.program.clones.id))
    }
}
