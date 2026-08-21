//! The value type of the committer world. Slot vars (globals, upvalues)
//! hold an [`LpcRef`]; payload vars (arrays, mappings) hold their contents
//! directly, keyed by their own var identity.

use std::sync::Arc;

use bit_set::BitSet;
use lpc_rs_errors::Result;

use crate::interpreter::{
    gc::mark::Mark, lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef, process::Process,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum WorldValue {
    Ref(LpcRef),
    Array(Arc<LpcArray>),
    Mapping(Arc<LpcMapping>),
    /// The object-space cell: the committed `Process` for one object path.
    /// Held strongly so a committed object stays alive regardless of the
    /// physical `ObjectSpace` map's timing (the physical map is applied after
    /// commit; this entry is what makes the object resolvable in between).
    Process(Arc<Process>),
}

impl WorldValue {
    pub(crate) fn ref_of(value: LpcRef) -> Self {
        Self::Ref(value)
    }

    /// The slot value. Slot vars (globals, upvalues) are always the `Ref`
    /// kind; a payload world value here is a bug.
    pub(crate) fn lpc_ref(self) -> LpcRef {
        match self {
            Self::Ref(lpc_ref) => lpc_ref,
            Self::Array(_) | Self::Mapping(_) | Self::Process(_) => {
                unreachable!("a payload var read through a slot access")
            }
        }
    }

    /// The value a missing var reads back as: `NULL`, as committed reads see it.
    pub(crate) fn null() -> Self {
        Self::Ref(LpcRef::from(0))
    }
}

impl Mark for WorldValue {
    /// Mark the values a committed world entry holds. Slot entries mark
    /// their `LpcRef` as usual; payload entries mark their contents, whose
    /// inner elements are `LpcRef`s.
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        match self {
            Self::Ref(lpc_ref) => lpc_ref.mark(marked, processed),
            Self::Array(array) => array.mark(marked, processed),
            Self::Mapping(mapping) => mapping.mark(marked, processed),
            // `Process::mark` is a no-op (processes are identity, not
            // markable payloads); keep the arm so the arm-set stays
            // exhaustive.
            Self::Process(process) => process.mark(marked, processed),
        }
    }
}
