//! Inheritance projects global declarations into the child while keeping executable code immutable.
//! Each defining layout gets a view of the child's canonical slots, packed once for process construction.

use std::{collections::HashMap, ops::Deref, sync::Arc};

use lpc_rs_core::{
    RegisterSize,
    register::{Register, RegisterVariant},
};
use lpc_rs_errors::{LpcError, Result, lpc_error, span::Span};
use lpc_rs_function_support::symbol::Symbol;

use super::{Function, GlobalVariable, Program, Region};

/// A parent with global metadata projected into the child being compiled.
#[derive(Debug, Clone)]
pub struct InheritedProgram {
    pub global_variables: Box<HashMap<String, Symbol>>,
    pub global_variable_info: Box<[GlobalVariable]>,
    program: Program,
    slots: Vec<RegisterSize>,
}

impl Deref for InheritedProgram {
    type Target = Program;
    fn deref(&self) -> &Program {
        &self.program
    }
}

impl From<Program> for InheritedProgram {
    fn from(mut program: Program) -> Self {
        Self {
            global_variables: std::mem::take(&mut program.global_variables),
            global_variable_info: std::mem::take(&mut program.global_variable_info),
            slots: (0..program.num_globals).collect(),
            program,
        }
    }
}

impl InheritedProgram {
    pub(crate) fn place(
        program: Program,
        layout: &mut Vec<Region>,
        num_globals: &mut RegisterSize,
        span: Option<Span>,
    ) -> Result<Self> {
        let targets = place(layout, num_globals, &program.layout, span)?;
        let mut parent = Self::from(program);
        for (region, target) in parent.program.layout.iter().zip(targets) {
            for i in 0..region.count {
                parent.slots[usize::from(region.base + i)] = target + i;
            }
        }
        for symbol in parent.global_variables.values_mut() {
            if let Some(RegisterVariant::Global(reg)) = &mut symbol.location {
                *reg = Register(parent.slots[usize::from(reg.index())]);
            }
        }
        for variable in &mut parent.global_variable_info {
            variable.slot = parent.slots[usize::from(variable.slot)];
        }
        Ok(parent)
    }

    pub(crate) fn functions(
        &self,
        linker: &mut ProgramLinker,
    ) -> Result<Vec<(ustr::Ustr, Function)>> {
        let mut views = HashMap::new();
        self.program
            .functions
            .iter()
            .map(|(&name, function)| {
                let key = (function.globals_start(), function.global_count());
                let start = if let Some(&start) = views.get(&key) {
                    start
                } else {
                    let slots = (0..function.global_count())
                        .map(|i| {
                            let slot = self
                                .program
                                .global_slot(function.globals_start() as usize + usize::from(i));
                            self.slots[usize::from(slot)]
                        })
                        .collect::<Vec<_>>();
                    let start = linker.view(slots)?;
                    views.insert(key, start);
                    start
                };
                Ok((
                    name,
                    Function::new(function.code.clone(), start, function.global_count()),
                ))
            })
            .collect()
    }
}

/// Packs global views while assembling a program's resolved functions.
pub(crate) struct ProgramLinker {
    globals: usize,
    aliases: Vec<RegisterSize>,
    views: HashMap<Vec<RegisterSize>, u32>,
}

impl ProgramLinker {
    pub(crate) fn new(globals: RegisterSize) -> Self {
        Self {
            globals: usize::from(globals),
            aliases: Vec::new(),
            views: HashMap::new(),
        }
    }

    fn view(&mut self, slots: Vec<RegisterSize>) -> Result<u32> {
        if slots.iter().any(|&slot| usize::from(slot) >= self.globals) {
            return Err(LpcError::bug(
                "inherited global is outside the child layout",
            ));
        }
        let first = slots.first().copied().unwrap_or(0);
        if slots
            .iter()
            .enumerate()
            .all(|(i, &slot)| usize::from(slot) == usize::from(first) + i)
        {
            return Ok(u32::from(first));
        }
        if let Some(&start) = self.views.get(&slots) {
            return Ok(start);
        }
        let start = self
            .globals
            .checked_add(self.aliases.len())
            .ok_or_else(|| LpcError::bug("global view is too large"))?;
        start
            .checked_add(slots.len())
            .ok_or_else(|| LpcError::bug("global view is too large"))?;
        let start =
            u32::try_from(start).map_err(|_| LpcError::bug("global view offset is too large"))?;
        self.aliases.extend_from_slice(&slots);
        self.views.insert(slots, start);
        Ok(start)
    }

    pub(crate) fn finish(self) -> Arc<[RegisterSize]> {
        self.aliases.into()
    }
}

/// Places each declaration region once, including ancestors reached through a diamond.
pub(crate) fn place(
    layout: &mut Vec<Region>,
    num_globals: &mut RegisterSize,
    imported: &[Region],
    span: Option<Span>,
) -> Result<Vec<RegisterSize>> {
    let mut targets = Vec::with_capacity(imported.len());
    for region in imported {
        match layout.iter().find(|held| held.filename == region.filename) {
            Some(held) if held.count != region.count => {
                return Err(lpc_error!(
                    span,
                    "inherited two different versions of `{}`",
                    region.filename
                ));
            }
            Some(held) => targets.push(held.base),
            None => {
                let base = *num_globals;
                *num_globals = num_globals
                    .checked_add(region.count)
                    .ok_or_else(|| lpc_error!(span, "too many global variables"))?;
                layout.push(Region {
                    base,
                    ..region.clone()
                });
                targets.push(base);
            }
        }
    }
    Ok(targets)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn view_offsets_can_exceed_the_global_register_range() {
        let mut linker = ProgramLinker::new(40_000);
        let mut slots: Vec<_> = (0..40_000).rev().collect();
        assert_eq!(linker.view(slots.clone()).unwrap(), 40_000);
        slots.rotate_left(1);
        assert_eq!(linker.view(slots).unwrap(), 80_000);
        assert_eq!(linker.finish().len(), 80_000);
    }
}
