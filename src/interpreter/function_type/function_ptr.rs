use std::{
    fmt::{Display, Formatter},
    path::PathBuf,
    sync::{Arc, Weak},
};

use bit_set::BitSet;
use derive_builder::Builder;
use itertools::Itertools;
use lpc_rs_core::{lpc_path::LpcPath, register::Register};
use lpc_rs_errors::{Result, lpc_bug, lpc_error};
use lpc_rs_function_support::program_function::ProgramFunction;
use thin_vec::ThinVec;
use tracing::{instrument, trace};

use crate::interpreter::{
    efun::EFUN_FUNCTIONS,
    function_type::function_address::FunctionAddress,
    gc::{mark::Mark, unique_id::UniqueId},
    lpc_ref::{LpcRef, NULL},
    process::Process,
    task_context::{ObjectLookup, TaskContext},
};

/// A pointer resolved for one call: the receiver, the function, its arguments.
pub struct ResolvedCall {
    pub process: Arc<Process>,
    pub function: Arc<ProgramFunction>,
    pub args: Vec<LpcRef>,
}

/// A pointer to a function, created with the `&` syntax.
#[derive(Debug, Builder)]
#[builder(pattern = "owned")]
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
    /// are partially-applied. A published pointer is never mutated: partial
    /// application builds a new pointer over a cloned list.
    #[builder(default)]
    partial_args: ThinVec<Option<LpcRef>>,

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

    pub fn partial_args(&self) -> &[Option<LpcRef>] {
        &self.partial_args
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
    /// holes first, then appending to the end of the list. Consumes the pointer and returns
    /// it; callers clone first (the published original is immutable).
    pub fn partially_apply(mut self, args: &[LpcRef]) -> Self {
        let mut arg_iter = args.iter();

        for partial_arg in self.partial_args.iter_mut() {
            if partial_arg.is_none() {
                *partial_arg = arg_iter.next().cloned();
            }
        }

        self.partial_args.extend(arg_iter.cloned().map(Some));

        self
    }

    /// The argument list for a call with `passed`: the holes in the partial
    /// args fill left to right, the rest append, an unfilled hole is `0`.
    pub fn bound_args(&self, passed: &[LpcRef]) -> Vec<LpcRef> {
        let mut passed = passed.iter();
        let mut args: Vec<LpcRef> = self
            .partial_args
            .iter()
            .map(|arg| match arg {
                Some(lpc_ref) => lpc_ref.clone(),
                None => passed.next().cloned().unwrap_or(NULL),
            })
            .collect();
        args.extend(passed.cloned());
        args
    }

    /// Whether a call can find this pointer's receiver without arguments:
    /// always, unless it is `&->name()` with the receiver hole unfilled.
    pub fn receiver_bound(&self) -> bool {
        !matches!(self.address, FunctionAddress::Dynamic(_))
            || matches!(self.partial_args.first(), Some(Some(_)))
    }

    /// Resolve this pointer for a call with `passed` arguments: the receiver
    /// (a dynamic receiver is the first bound argument, created on a miss
    /// through `ctx`), the function, and the bound arguments.
    /// `Ok(None)`: a dynamic receiver has no function by this name, so the
    /// call yields `0` as `call_other` would.
    pub async fn prepare_call(
        &self,
        passed: &[LpcRef],
        ctx: &TaskContext,
    ) -> Result<Option<ResolvedCall>> {
        let txn = ctx.txn();
        let mut args = self.bound_args(passed);

        let (process, function) = match &self.address {
            FunctionAddress::Local(receiver, function) => {
                let Some(process) = receiver.upgrade().filter(|p| p.is_live(txn)) else {
                    return Err(lpc_error!(
                        "attempted to call a pointer to a function in a destructed object: {}",
                        self
                    ));
                };
                (process, function.clone())
            }
            FunctionAddress::Dynamic(name) => {
                let receiver = if args.is_empty() {
                    NULL
                } else {
                    args.remove(0)
                };
                let process = match &receiver {
                    LpcRef::Object(_) => {
                        let Some(process) = receiver.live_object(txn) else {
                            return Err(lpc_error!(
                                "attempted to call `{}` on a destructed object",
                                name
                            ));
                        };
                        process
                    }
                    LpcRef::String(_) => {
                        let path = receiver.with_string(|s| LpcPath::InGame(PathBuf::from(s)))?;
                        match ctx.find_object(&path) {
                            ObjectLookup::Found(process) => process,
                            ObjectLookup::Removed => {
                                return Err(lpc_error!(
                                    "attempted to call `{}` on a destructed object `{}`",
                                    name,
                                    path
                                ));
                            }
                            ObjectLookup::NotCreated => {
                                let process = ctx.compile_process(&path).await?;
                                ctx.insert_process_transactional(&process);
                                process
                            }
                        }
                    }
                    _ => {
                        return Err(lpc_error!(
                            "`&->{}()` needs an object or path as its receiver, got `{}`",
                            name,
                            receiver
                        ));
                    }
                };
                let Some(function) = process.program.lookup_function(name).cloned() else {
                    return Ok(None);
                };
                (process, function)
            }
            FunctionAddress::Efun(name) => {
                let Some(owner) = self.owner.upgrade().filter(|p| p.is_live(txn)) else {
                    return Err(lpc_error!(
                        "attempted to call an efun pointer whose owner is destructed: {}",
                        self
                    ));
                };
                (owner, EFUN_FUNCTIONS[name.as_str()].clone())
            }
            FunctionAddress::SimulEfun(name) => {
                let Some(simul_efuns) = ctx.simul_efuns() else {
                    return Err(lpc_bug!("simul_efun pointer without simul_efuns: {}", self));
                };
                let Some(function) = simul_efuns.program.lookup_function(name) else {
                    return Err(lpc_error!("call to unknown simul_efun `{}`", name));
                };
                (simul_efuns.clone(), function.clone())
            }
        };

        Ok(Some(ResolvedCall {
            process,
            function,
            args,
        }))
    }
}

impl Clone for FunctionPtr {
    fn clone(&self) -> Self {
        Self {
            owner: self.owner.clone(),
            address: self.address.clone(),
            partial_args: self.partial_args.clone(),
            upvalue_ptrs: self.upvalue_ptrs.clone(),
            unique_id: UniqueId::new(),
        }
    }
}

impl Mark for FunctionPtr {
    #[instrument(skip(self))]
    fn mark(&self, marked: &mut BitSet, processed: &mut BitSet) -> Result<()> {
        trace!("marking function ptr");

        if !self.unique_id.first_visit(processed) {
            return Ok(());
        }

        trace!("marking upvalue ptrs: {:?}", &self.upvalue_ptrs);

        marked.extend(
            self.upvalue_ptrs
                .iter()
                .copied()
                .map(|r| r.index() as usize),
        );

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

        let partial_args = self
            .partial_args()
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use factori::create;
    use thin_vec::thin_vec;

    use super::*;
    use crate::test_support::factories::*;

    #[test]
    fn bound_args_fills_holes_left_to_right_then_appends() {
        let ptr = create!(
            FunctionPtr,
            partial_args: thin_vec![None, None, Some(LpcRef::from("x"))],
        );

        assert_eq!(
            ptr.bound_args(&[LpcRef::from(1), LpcRef::from(2), LpcRef::from(3)]),
            vec![
                LpcRef::from(1),
                LpcRef::from(2),
                LpcRef::from("x"),
                LpcRef::from(3)
            ]
        );
        assert_eq!(
            ptr.bound_args(&[LpcRef::from(1)]),
            vec![LpcRef::from(1), NULL, LpcRef::from("x")]
        );
    }

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
        assert_eq!(marked.count(), 2);
        assert!(marked.contains(3));
        assert!(marked.contains(5));

        assert_eq!(processed.count(), 1);
        assert!(processed.contains(*ptr.unique_id.as_ref() as usize));

        marked.make_empty();

        ptr.mark(&mut marked, &mut processed).unwrap();

        assert_eq!(marked.count(), 0);
    }
}
