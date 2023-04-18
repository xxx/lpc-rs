use std::{
    fmt::{Display, Formatter},
    sync::Weak,
};

use bit_set::BitSet;
use itertools::Itertools;
use lpc_rs_core::register::Register;
use parking_lot::RwLock;
use tracing::{instrument, trace};

use crate::interpreter::{
    function_type::function_address::FunctionAddress,
    gc::{mark::Mark, unique_id::UniqueId},
    heap::Heap,
    into_lpc_ref::IntoLpcRef,
    lpc_ref::LpcRef,
    process::Process,
};

/// A pointer to a function, created with the `&` syntax.
#[derive(Debug, Clone)]
pub struct FunctionPtr {
    /// The object that this pointer was declared in.
    /// *note* This is *not* necessarily the object that the function is
    ///        defined within.
    pub owner: Weak<Process>,

    /// Address of the function, in either the receiver or owner
    pub address: FunctionAddress,

    /// Arguments to be passed to the call. `None` arguments in this vector
    /// are expected to be filled at call time, in the case of pointers that
    /// are partially-applied.
    pub partial_args: Vec<Option<LpcRef>>,

    /// Does this pointer use `call_other`?
    pub call_other: bool,

    /// The variables that I need from the environment, at the time this
    /// [`FunctionPtr`] ss created.
    pub upvalue_ptrs: Vec<Register>,

    /// A globally-unique ID for this function pointer, used for GC purposes.
    pub unique_id: UniqueId,
}

impl FunctionPtr {
    /// Get the name of the function being called.
    /// Will return the variable name in those cases.
    #[inline]
    pub fn name(&self) -> &str {
        self.address.function_name()
    }

    /// How many arguments do we expect to be called with at runtime?
    #[inline]
    pub fn arity(&self) -> usize {
        self.partial_args.iter().filter(|x| x.is_none()).count()
    }

    /// Get a clone of this function pointer, with a new unique ID.
    /// This is used partially-apply an existing function to additional arguments.
    #[inline]
    pub fn clone_with_new_id(&self) -> Self {
        Self {
            unique_id: UniqueId::new(),
            ..self.clone()
        }
    }

    /// partially apply this function pointer to the passed arguments, filling in any existing
    /// holes first, then appending to the end of the list.
    pub fn partially_apply(&mut self, args: &[LpcRef]) {
        let mut arg_iter = args.iter();

        for arg in self.partial_args.iter_mut() {
            if arg.is_none() {
                *arg = arg_iter.next().cloned();
            }
        }

        self.partial_args.extend(arg_iter.cloned().map(Some));
    }
}

impl Mark for FunctionPtr {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> lpc_rs_errors::Result<()> {
        trace!("marking function ptr");

        if !processed.insert(*self.unique_id.as_ref()) {
            return Ok(());
        }

        trace!("marking upvalue ptrs: {:?}", &self.upvalue_ptrs);

        marked.extend(self.upvalue_ptrs.iter().copied().map(Register::index));

        Ok(())
    }
}

impl Display for FunctionPtr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();

        s.push_str("FunctionPtr { ");
        match self.owner.upgrade() {
            Some(owner) => {
                s.push_str(&format!("owner: {}, ", owner));
            }
            None => {
                s.push_str("owner: < destructed >, ");
            }
        }
        s.push_str(&format!("address: {}, ", self.address));

        let partial_args = &self
            .partial_args
            .iter()
            .map(|arg| match arg {
                Some(a) => a.to_string(),
                None => "<None>".to_string(),
            })
            .join(", ");
        s.push_str(&format!("partial_args: [{partial_args}], "));
        s.push_str(&format!(
            "upvalues: [{}]",
            self.upvalue_ptrs.iter().map(|x| format!("{x}")).join(", ")
        ));
        s.push('}');

        write!(f, "{s}")
    }
}

impl IntoLpcRef for FunctionPtr {
    fn into_lpc_ref(self, memory: &Heap) -> LpcRef {
        memory.alloc_function(self)
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use factori::create;

    use super::*;
    use crate::test_support::factories::*;

    #[test]
    fn test_mark() {
        let mut ptr = create!(
            FunctionPtr,
            owner: Arc::downgrade(&Arc::new(Process::default())),
        );

        ptr.upvalue_ptrs.push(Register(3));
        ptr.upvalue_ptrs.push(Register(5));

        let mut marked = BitSet::new();
        let mut processed = BitSet::new();
        ptr.mark(&mut marked, &mut processed).unwrap();
        assert_eq!(marked.len(), 2);
        assert!(marked.contains(3));
        assert!(marked.contains(5));

        assert_eq!(processed.len(), 1);
        assert!(processed.contains(*ptr.unique_id.as_ref()));

        marked.clear();

        ptr.mark(&mut marked, &mut processed).unwrap();

        assert_eq!(marked.len(), 0);
    }
}
