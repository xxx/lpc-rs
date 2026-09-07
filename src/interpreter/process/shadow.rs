//! The shadow chain: which objects shadow an object, and what an object
//! shadows. Two transactional cells and two hint bits per object.

use std::sync::{
    Arc,
    atomic::{AtomicBool, Ordering},
};

use crate::interpreter::{
    lpc_array::LpcArray,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::{MergeOp, SVar, Transaction, TxnHandle},
};

/// An object's place in a shadow chain.
#[derive(Debug)]
pub struct ShadowLinks {
    /// The objects shadowing this one, in attach order, inner to outer.
    pub shadows: SVar<LpcArray>,
    /// The object this one shadows; NULL when it is not a shadow.
    pub shadowing: SVar<LpcRef>,
    /// Set by the first attach to this object, never cleared: clear means
    /// `shadows` was never written, so a door skips the read.
    pub ever_shadowed: AtomicBool,
    /// Set when this object first attaches to another, never cleared:
    /// clear means `shadowing` was never written.
    pub ever_shadowing: AtomicBool,
}

impl Default for ShadowLinks {
    fn default() -> Self {
        Self {
            shadows: SVar::new(),
            shadowing: SVar::new(),
            ever_shadowed: AtomicBool::new(false),
            ever_shadowing: AtomicBool::new(false),
        }
    }
}

#[expect(dead_code)]
impl Process {
    /// The live objects shadowing `target`, inner to outer, through `txn`;
    /// empty without a read when nothing ever attached.
    pub(crate) fn shadows_of(txn: &TxnHandle, target: &Arc<Process>) -> Vec<Arc<Process>> {
        if !target.shadow.ever_shadowed.load(Ordering::Acquire) {
            return Vec::new();
        }
        txn.with(|t| Self::shadows_in(t, target))
    }

    /// [`Self::shadows_of`] inside an in-flight transaction.
    pub(crate) fn shadows_in(t: &mut Transaction, target: &Arc<Process>) -> Vec<Arc<Process>> {
        t.read_array(target.shadow.shadows.id)
            .map(|members| {
                members
                    .iter()
                    .filter_map(|item| match item {
                        LpcRef::Object(weak) => weak.upgrade(),
                        _ => None,
                    })
                    .collect()
            })
            .unwrap_or_default()
    }

    /// The live object `ob` shadows, through `txn`; `None` without a read
    /// when `ob` never attached to anything.
    pub(crate) fn shadow_target(txn: &TxnHandle, ob: &Arc<Process>) -> Option<Arc<Process>> {
        if !ob.shadow.ever_shadowing.load(Ordering::Acquire) {
            return None;
        }
        txn.with(|t| Self::shadow_target_in(t, ob))
    }

    /// [`Self::shadow_target`] inside an in-flight transaction.
    pub(crate) fn shadow_target_in(t: &mut Transaction, ob: &Arc<Process>) -> Option<Arc<Process>> {
        t.read(ob.shadow.shadowing.id)
            .and_then(|value| match value {
                LpcRef::Object(weak) => weak.upgrade(),
                _ => None,
            })
    }

    /// Link `shadow` as the outermost shadow of `target`. The hint bits are
    /// set before the writes so a reader that sees the cell sees the bit.
    pub(crate) fn attach_shadow(txn: &TxnHandle, shadow: &Arc<Process>, target: &Arc<Process>) {
        target.shadow.ever_shadowed.store(true, Ordering::Release);
        shadow.shadow.ever_shadowing.store(true, Ordering::Release);
        txn.with(|t| {
            t.merge(
                target.shadow.shadows.id,
                MergeOp::ArrayAppend(vec![LpcRef::Object(Arc::downgrade(shadow))]),
            );
            t.write(
                shadow.shadow.shadowing.id,
                LpcRef::Object(Arc::downgrade(target)),
            );
        });
    }

    /// Unlink `shadow` from `target`'s chain; the chain closes around it.
    pub(crate) fn detach_shadow(t: &mut Transaction, shadow: &Arc<Process>, target: &Arc<Process>) {
        t.merge(
            target.shadow.shadows.id,
            MergeOp::ArrayRemoveValue(LpcRef::Object(Arc::downgrade(shadow))),
        );
        t.write(shadow.shadow.shadowing.id, NULL);
    }

    /// `shadow(ob, 0)`: the object directly outside `ob` in its chain — for
    /// a target its innermost shadow, for a shadow the next one out.
    pub(crate) fn shadow_after(txn: &TxnHandle, ob: &Arc<Process>) -> Option<Arc<Process>> {
        match Self::shadow_target(txn, ob) {
            Some(target) => {
                let chain = Self::shadows_of(txn, &target);
                let at = chain.iter().position(|s| Arc::ptr_eq(s, ob))?;
                chain.get(at + 1).cloned()
            }
            None => Self::shadows_of(txn, ob).into_iter().next(),
        }
    }
}

/// Where a walk over `chain` (inner to outer) starts for a call on the
/// object at `called` (`None` for the chain's target) from `caller`: just
/// inside `caller` when it sits further out than the called object, else
/// the outermost; `None` when the walk has nowhere to start. CD's rule:
/// from the called object walk outward until the next shadow out is the
/// caller.
#[allow(dead_code)]
pub(crate) fn entry_index(
    chain: &[Arc<Process>],
    called: Option<usize>,
    caller: &Arc<Process>,
) -> Option<usize> {
    let caller_at = chain.iter().position(|s| Arc::ptr_eq(s, caller));
    match (caller_at, called) {
        (Some(k), None) => k.checked_sub(1),
        (Some(k), Some(c)) if k > c => Some(k - 1),
        _ => chain.len().checked_sub(1),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn objects(n: usize) -> Vec<Arc<Process>> {
        (0..n).map(|_| Arc::new(Process::default())).collect()
    }

    #[test]
    fn an_empty_chain_has_no_entry() {
        let caller = Arc::new(Process::default());
        assert_eq!(entry_index(&[], None, &caller), None);
    }

    #[test]
    fn a_caller_outside_the_chain_enters_at_the_outermost() {
        let chain = objects(3);
        let caller = Arc::new(Process::default());
        assert_eq!(entry_index(&chain, None, &caller), Some(2));
        assert_eq!(entry_index(&chain, Some(0), &caller), Some(2));
    }

    #[test]
    fn a_caller_outside_the_called_object_enters_just_inside_itself() {
        let chain = objects(3);
        assert_eq!(entry_index(&chain, None, &chain[2]), Some(1));
        assert_eq!(entry_index(&chain, Some(0), &chain[2]), Some(1));
        assert_eq!(entry_index(&chain, None, &chain[1]), Some(0));
    }

    #[test]
    fn a_shadow_calling_itself_or_inward_enters_at_the_outermost() {
        let chain = objects(3);
        assert_eq!(entry_index(&chain, Some(2), &chain[2]), Some(2));
        assert_eq!(entry_index(&chain, Some(1), &chain[1]), Some(2));
        assert_eq!(entry_index(&chain, Some(2), &chain[0]), Some(2));
    }

    #[test]
    fn the_innermost_shadow_calling_the_target_reaches_it_directly() {
        let chain = objects(2);
        assert_eq!(entry_index(&chain, None, &chain[0]), None);
    }
}
