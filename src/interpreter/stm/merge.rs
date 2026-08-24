//! Commutative writes: operations the committer applies to the committed
//! value at commit time, in commit order. Recording one tracks no read, so
//! merges never conflict with each other; a tracked reader of the cell
//! still conflicts with a committed merge.

use std::sync::Arc;

use lpc_rs_core::LpcIntInner;

use crate::interpreter::{
    lpc_array::LpcArray, lpc_int::LpcInt, lpc_mapping::LpcMapping, lpc_ref::LpcRef, stm::WorldValue,
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
}

/// The committed value no longer has the type the op needs. The commit is
/// rejected as a conflict; the re-run peeks the fresh snapshot and takes the
/// typed path, so the mismatch cannot recur.
#[derive(Debug, PartialEq, Eq)]
pub(crate) struct MergeMismatch;

impl MergeOp {
    /// Apply this op onto a committed value; `None` is the absent cell.
    pub(crate) fn apply_to(&self, base: Option<&WorldValue>) -> Result<WorldValue, MergeMismatch> {
        match self {
            MergeOp::IntAdd(n) => match base {
                None => Ok(WorldValue::Ref(LpcRef::Int(LpcInt(*n)))),
                Some(WorldValue::Ref(LpcRef::Int(i))) => {
                    Ok(WorldValue::Ref(LpcRef::Int(i.wrapping_add(*n).into())))
                }
                Some(_) => Err(MergeMismatch),
            },
            MergeOp::ArrayAppend(values) => {
                let mut array = base_array(base)?;
                array.array.extend(values.iter().cloned());
                Ok(WorldValue::Array(Arc::new(array)))
            }
            MergeOp::ArrayRemoveValue(value) => {
                let mut array = base_array(base)?;
                array.array.retain(|item| item != value);
                Ok(WorldValue::Array(Arc::new(array)))
            }
            MergeOp::MapInsert(key, value) => {
                let mut mapping = base_mapping(base)?;
                mapping.insert(key.clone(), value.clone());
                Ok(WorldValue::Mapping(Arc::new(mapping)))
            }
        }
    }

    /// Fold `next` into this op when the kinds compose; a returned op did
    /// not fold and keeps its own slot.
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
            (_, next) => Some(next),
        }
    }
}

/// The array a container op starts from: the committed contents, or empty
/// for an absent cell.
fn base_array(base: Option<&WorldValue>) -> Result<LpcArray, MergeMismatch> {
    match base {
        None => Ok(LpcArray::default()),
        Some(WorldValue::Array(arc)) => Ok((**arc).clone()),
        Some(_) => Err(MergeMismatch),
    }
}

/// The mapping a container op starts from, as in [`base_array`].
fn base_mapping(base: Option<&WorldValue>) -> Result<LpcMapping, MergeMismatch> {
    match base {
        None => Ok(LpcMapping::default()),
        Some(WorldValue::Mapping(arc)) => Ok((**arc).clone()),
        Some(_) => Err(MergeMismatch),
    }
}
