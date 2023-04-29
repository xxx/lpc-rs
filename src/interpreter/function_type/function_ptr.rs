use std::{
    fmt::{Display, Formatter},
    sync::{Arc, Weak},
};

use bit_set::BitSet;
use derive_builder::Builder;
use itertools::Itertools;
use lpc_rs_core::register::Register;
use lpc_rs_errors::{lpc_bug, lpc_error, LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use lpc_rs_utils::config::Config;
use parking_lot::RwLock;
use thin_vec::ThinVec;
use tracing::{instrument, trace};

use crate::{
    interpreter::{
        efun::EFUN_FUNCTIONS,
        function_type::function_address::FunctionAddress,
        gc::{mark::Mark, unique_id::UniqueId},
        heap::Heap,
        into_lpc_ref::IntoLpcRef,
        lpc_ref::{LpcRef, NULL},
        object_space::ObjectSpace,
        process::Process,
    },
    util::get_simul_efuns,
};

type PtrTriple = (Arc<Process>, Arc<ProgramFunction>, Vec<LpcRef>);

/// A pointer to a function, created with the `&` syntax.
#[derive(Debug, Clone, Builder)]
pub struct FunctionPtr {
    /// The object that this pointer was declared in.
    /// *note* This is *not* necessarily the object that the function is
    ///        defined within.
    #[builder(default)]
    pub owner: Weak<Process>,

    /// Address of the function, in either the receiver or owner
    pub address: FunctionAddress,

    /// Arguments to be passed to the call. `None` arguments in this vector
    /// are expected to be filled at call time, in the case of pointers that
    /// are partially-applied.
    #[builder(default)]
    pub partial_args: ThinVec<Option<LpcRef>>,

    /// Does this pointer use `call_other`?
    #[builder(default)]
    pub call_other: bool,

    /// The variables that I need from the environment, at the time this
    /// [`FunctionPtr`] is created.
    #[builder(default)]
    pub upvalue_ptrs: ThinVec<Register>,

    /// A globally-unique ID for this function pointer, used for GC purposes.
    #[builder(default)]
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

    /// Prepare this function pointer for a call, by getting all of the pieces out of it, and into separate variables.
    pub fn triple(
        ptr_arc: &RwLock<FunctionPtr>,
        config: &Config,
        object_space: &ObjectSpace,
    ) -> Result<PtrTriple> {
        let ptr = ptr_arc.read();

        // We won't get any additional args passed to us, so just set up the partial args.
        // Use int 0 for any that were not applied at the time the pointer was created.
        // TODO: error instead of int 0?
        let args = ptr
            .partial_args
            .iter()
            .map(|arg| match arg {
                Some(lpc_ref) => lpc_ref.clone(),
                None => NULL,
            })
            .collect::<Vec<_>>();

        match ptr.address {
            FunctionAddress::Local(ref proc, ref function) => {
                if let Some(proc) = proc.upgrade() {
                    Ok((proc, function.clone(), args))
                } else {
                    Err(lpc_error!(
                        "attempted to call a function pointer with a dead process",
                    ))
                }
            }
            FunctionAddress::Dynamic(name) => {
                let Some(Some(LpcRef::Object(proc))) = ptr.partial_args.first() else {
                    return Err(lpc_error!(
                        "attempted to call a dynamic receiver that is not an object",
                    ));
                };

                let Some(proc) = proc.upgrade() else {
                    return Err(lpc_error!(
                        "attempted to call a dynamic receiver that has been destructed",
                    ));
                };

                let func = proc.program.lookup_function(name).ok_or_else(|| {
                    LpcError::new(format!("attempted to call unknown function `{}`", name))
                })?;

                let func = func.clone();
                Ok((proc, func, args))
            }
            FunctionAddress::SimulEfun(name) => match get_simul_efuns(config, object_space) {
                Some(simul_efuns) => match simul_efuns.program.lookup_function(name) {
                    Some(function) => Ok((simul_efuns.clone(), function.clone(), args)),
                    None => Err(lpc_error!(
                        "call to unknown simul_efun `{}`", name
                    )),
                },
                None => Err(lpc_bug!(
                    "function pointer to simul_efun passed, but no simul_efuns?",
                )),
            },
            FunctionAddress::Efun(name) => {
                let pf = EFUN_FUNCTIONS.get(name.as_str()).unwrap();

                Ok((Arc::new(Process::default()), pf.clone(), args))
            }
        }
    }
}

impl Mark for FunctionPtr {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
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
