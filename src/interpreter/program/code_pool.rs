use std::{
    cell::Cell,
    collections::HashMap,
    hash::BuildHasher,
    sync::{Arc, Weak},
};

use lpc_rs_core::{RegisterSize, lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::{constant::LpcConstant, program_function::ProgramFunction};
use parking_lot::Mutex;

/// Shares completed code without retaining it or bypassing compilation.
#[derive(Debug, Default)]
pub struct CodePool {
    entries: Mutex<Entries>,
}

/// A declaration region in the coordinates used by a function's code.
#[derive(Debug, Clone, Eq, PartialEq)]
pub(crate) struct GlobalRegion {
    pub filename: Arc<LpcPath>,
    pub base: RegisterSize,
    pub count: RegisterSize,
}

#[derive(Debug)]
struct PublishedCode {
    code: Weak<ProgramFunction>,
    layout: Arc<[GlobalRegion]>,
}

#[derive(Debug, Default)]
struct Entries {
    functions: HashMap<u64, Vec<PublishedCode>>,
    publications: usize,
}

impl CodePool {
    pub(crate) fn publish(
        &self,
        code: ProgramFunction,
        layout: Arc<[GlobalRegion]>,
    ) -> Result<Arc<ProgramFunction>> {
        let globals = layout
            .last()
            .map_or(0, |r| usize::from(r.base) + usize::from(r.count));
        let invalid = Cell::new(false);
        let check = |register| {
            if let RegisterVariant::Global(reg) = register
                && usize::from(reg.index()) >= globals
            {
                invalid.set(true);
            }
            register
        };
        for instruction in &code.instructions {
            instruction.map_registers(check);
        }
        for register in code
            .arg_lists
            .iter()
            .flatten()
            .map(|arg| arg.register())
            .chain(code.arg_locations.iter().copied())
        {
            check(register);
        }
        if invalid.get() {
            return Err(LpcError::bug(
                "function global is outside its defining layout",
            ));
        }
        let mut state = self.entries.lock();
        state.publications = state.publications.wrapping_add(1);
        if state.publications.is_multiple_of(256) {
            state.functions.retain(|_, bucket| {
                bucket.retain(|code| code.code.strong_count() != 0);
                !bucket.is_empty()
            });
        }
        let entries = &mut state.functions;
        let hash = entries.hasher().hash_one((
            code.prototype.filename.to_string(),
            &code.prototype.name,
            code.prototype.span,
            code.instructions.len(),
        ));
        let bucket = entries.entry(hash).or_default();
        bucket.retain(|code| code.code.strong_count() != 0);
        for old in bucket.iter().filter(|old| old.layout == layout) {
            if let Some(old) = old.code.upgrade()
                && equivalent(&old, &code)
            {
                return Ok(old);
            }
        }
        let code = Arc::new(code);
        bucket.push(PublishedCode {
            code: Arc::downgrade(&code),
            layout,
        });
        Ok(code)
    }
}

fn equivalent(left: &ProgramFunction, right: &ProgramFunction) -> bool {
    left == right
        && left
            .constants
            .iter()
            .zip(&right.constants)
            .all(|(left, right)| match (left, right) {
                (LpcConstant::Float(a), LpcConstant::Float(b)) => {
                    a.into_inner().to_bits() == b.into_inner().to_bits()
                }
                _ => true,
            })
}

#[cfg(test)]
mod tests {
    use lpc_rs_asm::instruction::Instruction;
    use lpc_rs_core::lpc_type::LpcType;
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

    use super::*;

    fn function(value: f64) -> ProgramFunction {
        let prototype = FunctionPrototypeBuilder::default()
            .name("f")
            .filename(lpc_rs_core::lpc_path::LpcPath::in_game("/test.c".into()))
            .return_type(LpcType::Float(false))
            .build()
            .unwrap();
        let mut code = ProgramFunction::new(prototype, 0);
        code.push_instruction(Instruction::Ret, None);
        code.constants.push(LpcConstant::Float(value.into()));
        code
    }

    #[test]
    fn equal_publications_share_code_without_pinning_it() {
        let pool = CodePool::default();
        let a = pool.publish(function(1.0), Arc::default()).unwrap();
        let b = pool.publish(function(1.0), Arc::default()).unwrap();
        assert!(Arc::ptr_eq(&a, &b));
        let weak = Arc::downgrade(&a);
        drop((a, b));
        assert!(weak.upgrade().is_none());
        let c = pool.publish(function(1.0), Arc::default()).unwrap();
        assert_eq!(c.constants, function(1.0).constants);
    }

    #[test]
    fn hash_collisions_do_not_merge_different_constants() {
        let pool = CodePool::default();
        let a = pool.publish(function(1.0), Arc::default()).unwrap();
        let b = pool.publish(function(2.0), Arc::default()).unwrap();
        assert!(!Arc::ptr_eq(&a, &b));
        assert_ne!(a.constants, b.constants);
    }

    #[test]
    fn matching_code_requires_the_same_defining_layout() {
        let pool = CodePool::default();
        let layout = |name: &str| -> Arc<[GlobalRegion]> {
            vec![GlobalRegion {
                filename: Arc::new(LpcPath::in_game(name.into())),
                base: 0,
                count: 1,
            }]
            .into()
        };
        let first = layout("/first.c");
        let a = pool.publish(function(1.0), first.clone()).unwrap();
        let b = pool.publish(function(1.0), layout("/second.c")).unwrap();
        let again = pool.publish(function(1.0), first).unwrap();
        assert!(!Arc::ptr_eq(&a, &b));
        assert!(Arc::ptr_eq(&a, &again));
    }

    #[test]
    fn signed_zero_is_preserved_even_when_numeric_equality_matches() {
        let pool = CodePool::default();
        let a = pool.publish(function(0.0), Arc::default()).unwrap();
        let b = pool.publish(function(-0.0), Arc::default()).unwrap();
        assert!(!Arc::ptr_eq(&a, &b));
        let LpcConstant::Float(value) = b.constants[0] else {
            panic!("a float");
        };
        assert!(value.into_inner().is_sign_negative());
    }

    #[test]
    fn concurrent_publications_converge_on_one_body() {
        let pool = CodePool::default();
        std::thread::scope(|scope| {
            let publications: Vec<_> = (0..8)
                .map(|_| scope.spawn(|| pool.publish(function(7.0), Arc::default()).unwrap()))
                .collect();
            let functions: Vec<_> = publications
                .into_iter()
                .map(|t| t.join().unwrap())
                .collect();
            assert!(functions.iter().all(|f| Arc::ptr_eq(f, &functions[0])));
        });
    }
}
