//! The value type of the committer world. Slot vars (globals, upvalues)
//! hold an [`LpcRef`]; payload vars (arrays, mappings) hold their contents
//! directly, keyed by their own var identity.

use std::sync::Arc;

use bit_set::BitSet;
use lpc_rs_errors::Result;

use crate::{
    interpreter::{
        gc::mark::Mark, lpc_array::LpcArray, lpc_mapping::LpcMapping, lpc_ref::LpcRef,
        process::Process,
    },
    telnet::connection::Connection,
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
    /// The connection-binding cell on a `Process`: the `Connection` attached
    /// to it, or absent. `Connection` is a socket handle, not an `LpcRef`, so
    /// it is carried here as an identity (as `Process` is) rather than in a
    /// slot. Held strongly so the binding stays alive between an in-transaction
    /// write and the commit that makes it visible; `None` clears the binding.
    Connection(Option<Arc<Connection>>),
}

impl WorldValue {
    pub(crate) fn ref_of(value: LpcRef) -> Self {
        Self::Ref(value)
    }

    /// The slot value. Slot vars (globals, upvalues) are always the `Ref`
    /// kind; any other world value here is a bug.
    pub(crate) fn lpc_ref(self) -> LpcRef {
        match self {
            Self::Ref(lpc_ref) => lpc_ref,
            Self::Array(_) | Self::Mapping(_) | Self::Process(_) | Self::Connection(_) => {
                unreachable!("a payload or identity var read through a slot access")
            }
        }
    }

    /// The value a missing var reads back as: `NULL`, as committed reads see it.
    #[cfg(test)]
    pub(crate) fn null() -> Self {
        Self::Ref(LpcRef::from(0))
    }

    /// The array payload, or `None` for any other kind.
    pub(crate) fn into_array(self) -> Option<Arc<LpcArray>> {
        match self {
            Self::Array(array) => Some(array),
            _ => None,
        }
    }

    /// The mapping payload, or `None` for any other kind.
    pub(crate) fn into_mapping(self) -> Option<Arc<LpcMapping>> {
        match self {
            Self::Mapping(mapping) => Some(mapping),
            _ => None,
        }
    }

    /// The object in an object-space cell, or `None` for any other kind.
    pub(crate) fn into_process(self) -> Option<Arc<Process>> {
        match self {
            Self::Process(process) => Some(process),
            _ => None,
        }
    }

    /// The bound connection, or `None` for an unbound cell or any other kind.
    pub(crate) fn into_connection(self) -> Option<Arc<Connection>> {
        match self {
            Self::Connection(connection) => connection,
            _ => None,
        }
    }
}

impl Mark for WorldValue {
    /// Mark the values a committed world entry holds. Slot entries mark
    /// their `LpcRef` as usual; payload entries mark their contents, whose
    /// inner elements are `LpcRef`s. A connection entry marks its
    /// `input_to` callback: that `FunctionPtr` keeps the object alive for
    /// the next input line, so it must survive like any other strong ref.
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        match self {
            Self::Ref(lpc_ref) => lpc_ref.mark(marked, processed),
            Self::Array(array) => array.mark(marked, processed),
            Self::Mapping(mapping) => mapping.mark(marked, processed),
            // `Process::mark` is a no-op (processes are identity, not
            // markable payloads); keep the arm so the arm-set stays
            // exhaustive.
            Self::Process(process) => process.mark(marked, processed),
            Self::Connection(maybe_connection) => {
                if let Some(connection) = maybe_connection
                    && let Some(input_to) = connection.input_to.load_full()
                {
                    input_to.ptr.mark(marked, processed)?;
                }
                Ok(())
            }
        }
    }
}
