//! Commutative writes: operations the committer applies to the committed
//! value at commit time, in commit order. Recording one tracks no read, so
//! merges never conflict with each other; a tracked reader of the cell
//! still conflicts with a committed merge.

use lpc_rs_core::LpcIntInner;

use crate::interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, stm::WorldValue};

/// One commutative mutation of a cell.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum MergeOp {
    /// Add to an int slot; an absent cell applies onto `NULL`'s 0.
    IntAdd(LpcIntInner),
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
        }
    }

    /// Fold `next` into this op when the kinds compose; `false` keeps both.
    pub(crate) fn fold(&mut self, next: &MergeOp) -> bool {
        match (self, next) {
            (MergeOp::IntAdd(a), MergeOp::IntAdd(b)) => {
                *a = a.wrapping_add(*b);
                true
            }
        }
    }
}
