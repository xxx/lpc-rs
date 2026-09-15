//! Commutative writes: operations the committer applies to the committed
//! value at commit time, in commit order. Recording one tracks no read, so
//! merges never conflict with each other; a tracked reader of the cell
//! still conflicts with a committed merge.

use std::sync::Arc;

use lpc_rs_core::LpcIntInner;

use crate::{
    command::registry::RuleEdit,
    interpreter::{
        lpc_array::LpcArray, lpc_int::LpcInt, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
        stm::WorldValue,
    },
};

/// One commutative mutation of a cell.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MergeOp {
    /// Add to an int slot; an absent cell applies onto `NULL`'s 0.
    IntAdd(LpcIntInner),
    /// Append values to an array cell; an absent cell is the empty array.
    ArrayAppend(Vec<LpcRef>),
    /// Remove every element equal to the value (an object matches by
    /// identity); an absent cell stays the empty array.
    ArrayRemoveValue(LpcRef),
    /// Insert or overwrite one key; an absent cell is the empty mapping.
    MapInsert(LpcRef, LpcRef),
    /// Apply an ordered rule edit; an absent cell is the empty registry.
    Rules(RuleEdit),
}

/// The committed value no longer has the type the op needs. The commit is
/// rejected as a conflict; the re-run peeks the fresh snapshot and takes the
/// typed path, so the mismatch cannot recur.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MergeMismatch;

impl MergeOp {
    /// Apply this op onto a committed value; `None` is the absent cell.
    pub(crate) fn apply_to(&self, base: Option<&WorldValue>) -> Result<WorldValue, MergeMismatch> {
        let mut value = base.cloned().unwrap_or_else(|| self.identity());
        self.apply_in_place(&mut value)?;
        Ok(value)
    }

    /// Fold `ops` onto `base` in record order, cloning a shared payload
    /// once — a loop over `apply_to` clones it per op.
    pub(crate) fn fold_onto(
        base: Option<WorldValue>,
        ops: &[MergeOp],
    ) -> Result<Option<WorldValue>, MergeMismatch> {
        let Some(first) = ops.first() else {
            return Ok(base);
        };
        let mut value = base.unwrap_or_else(|| first.identity());
        for op in ops {
            op.apply_in_place(&mut value)?;
        }
        Ok(Some(value))
    }

    /// Apply this op onto `value` in place; a payload shared with the
    /// committed world is cloned, never mutated.
    pub(crate) fn apply_in_place(&self, value: &mut WorldValue) -> Result<(), MergeMismatch> {
        match (self, value) {
            (MergeOp::IntAdd(n), WorldValue::Ref(LpcRef::Int(i))) => {
                *i = i.wrapping_add(*n).into();
            }
            (MergeOp::ArrayAppend(values), WorldValue::Array(array)) => {
                Arc::make_mut(array).array.extend(values.iter().cloned());
            }
            (MergeOp::ArrayRemoveValue(value), WorldValue::Array(array)) => {
                Arc::make_mut(array).array.retain(|item| item != value);
            }
            (MergeOp::MapInsert(key, value), WorldValue::Mapping(mapping)) => {
                Arc::make_mut(mapping).insert(key.clone(), value.clone());
            }
            (MergeOp::Rules(edit), WorldValue::Rules(rules)) => edit.apply(rules),
            _ => return Err(MergeMismatch),
        }
        Ok(())
    }

    /// What this op applies onto for an absent cell.
    fn identity(&self) -> WorldValue {
        match self {
            MergeOp::IntAdd(_) => WorldValue::Ref(LpcRef::Int(LpcInt(0))),
            MergeOp::ArrayAppend(_) | MergeOp::ArrayRemoveValue(_) => {
                WorldValue::Array(Arc::new(LpcArray::default()))
            }
            MergeOp::MapInsert(..) => WorldValue::Mapping(Arc::new(LpcMapping::default())),
            MergeOp::Rules(_) => WorldValue::Rules(Default::default()),
        }
    }

    /// Fold `next` into this op when the kinds compose; a returned op did
    /// not fold and keeps its own slot.
    #[inline(always)]
    pub(crate) fn fold(&mut self, next: MergeOp) -> Option<MergeOp> {
        match (self, next) {
            (MergeOp::IntAdd(a), MergeOp::IntAdd(b)) => {
                *a = a.wrapping_add(b);
                None
            }
            (MergeOp::ArrayAppend(a), MergeOp::ArrayAppend(b)) => {
                a.extend(b);
                None
            }
            (MergeOp::Rules(first), MergeOp::Rules(next)) => first.fold(next).map(MergeOp::Rules),
            (_, next) => Some(next),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    #[test]
    fn apply_in_place_leaves_a_shared_payload_untouched() {
        let shared = Arc::new(LpcMapping::default());
        let mut value = WorldValue::Mapping(shared.clone());
        MergeOp::MapInsert("a".into(), 1.into())
            .apply_in_place(&mut value)
            .expect("a mapping takes an insert");

        assert!(shared.is_empty());
        let WorldValue::Mapping(own) = value else {
            panic!("a mapping stays a mapping");
        };
        assert!(!Arc::ptr_eq(&shared, &own));
        assert_eq!(own.get(&"a".into()), Some(&1.into()));
    }
}
