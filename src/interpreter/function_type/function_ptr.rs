use std::{
    convert::Infallible,
    fmt::{Display, Formatter},
    sync::{Arc, Weak},
};

use derive_builder::Builder;
use itertools::Itertools;
use lpc_rs_core::lpc_path::LpcPath;
use lpc_rs_errors::{LpcError, Result};
use lpc_rs_function_support::program_function::ProgramFunction;
use thin_vec::ThinVec;

use crate::interpreter::stm::{TxnHandle, VarId};
use crate::interpreter::{
    efun::EFUN_FUNCTIONS,
    function_type::function_address::FunctionAddress,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    task_context::{Caller, Callers, Loader, ObjectLookup, TaskContext},
};

/// A pointer resolved for one call: the receiver, the function, its arguments.
#[derive(Debug)]
pub struct ResolvedCall {
    pub process: Arc<Process>,
    pub function: Arc<ProgramFunction>,
    pub args: Vec<LpcRef>,
}

/// A pointer to a function, created with the `&` syntax.
#[derive(Debug, Clone, Builder)]
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

    /// The captured cells the pointed-to closure continues, from the frame that created it.
    #[builder(default)]
    pub upvalue_ptrs: ThinVec<VarId>,

    /// The file whose code wrote this pointer; `None` for a pointer the
    /// driver minted itself.
    #[builder(default)]
    pub origin: Option<Arc<LpcPath>>,
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
        let mut args = Vec::with_capacity(self.bound_len(passed.len()));
        let Ok(()) = self.each_bound(
            passed.iter().cloned().map(Ok::<_, Infallible>),
            |_, value| {
                args.push(value);
                Ok(())
            },
        );
        args
    }

    /// How many arguments a call with `passed` values runs with.
    pub(crate) fn bound_len(&self, passed: usize) -> usize {
        self.partial_args.len() + passed.saturating_sub(self.arity())
    }

    /// Every argument a call with `passed` values runs with, in order, to
    /// `sink`: a hole takes the next passed value (0 when none is left), the
    /// extras follow.
    pub(crate) fn each_bound<E>(
        &self,
        mut passed: impl Iterator<Item = std::result::Result<LpcRef, E>>,
        mut sink: impl FnMut(usize, LpcRef) -> std::result::Result<(), E>,
    ) -> std::result::Result<(), E> {
        let mut i = 0;
        for arg in &self.partial_args {
            let value = match arg {
                Some(value) => value.clone(),
                None => passed.next().transpose()?.unwrap_or(NULL),
            };
            sink(i, value)?;
            i += 1;
        }
        for value in passed {
            sink(i, value?)?;
            i += 1;
        }
        Ok(())
    }

    /// Whether a call can find this pointer's receiver without arguments:
    /// always, unless it is `&->name()` with the receiver hole unfilled.
    pub fn receiver_bound(&self) -> bool {
        !matches!(self.address, FunctionAddress::Dynamic(_))
            || matches!(self.partial_args.first(), Some(Some(_)))
    }

    /// Whether [`prepare_call`](Self::prepare_call) would find its receiver alive.
    pub(crate) fn receiver_is_live(&self, txn: &TxnHandle) -> bool {
        match &self.address {
            FunctionAddress::Local(receiver, _) => {
                receiver.upgrade().is_some_and(|p| p.is_live(txn))
            }
            FunctionAddress::Dynamic(_) => match self.partial_args.first() {
                Some(Some(receiver @ LpcRef::Object(_))) => receiver.live_object(txn).is_some(),
                _ => true,
            },
            FunctionAddress::Efun(_) | FunctionAddress::SimulEfun(_) => true,
        }
    }

    /// The identity a load this pointer triggers runs under: its owner in
    /// front of `callers`, the chain where it fired, and the file that wrote
    /// it (`0` when it has none).
    pub(crate) fn loader(&self, lib_dir: &str, callers: Callers) -> Result<Loader> {
        let Some(owner) = self.owner.upgrade() else {
            return Err(LpcError::runtime(format!(
                "attempted to call a function pointer whose owner is destructed: {}",
                self
            )));
        };
        let program = self.origin.as_ref().map_or(NULL, |origin| {
            LpcRef::from(origin.as_in_game(lib_dir).display().to_string())
        });
        Ok(Loader {
            func: "call_other".to_string(),
            chain: Caller::link(owner, callers),
            program,
        })
    }

    /// Resolve this pointer for a call with `passed` arguments: the receiver
    /// (a dynamic receiver is the first bound argument, created on a miss
    /// through `ctx` for the owner standing in the chain `callers` yields),
    /// the function, and the bound arguments.
    /// `Ok(None)`: a dynamic receiver has no function by this name, so the
    /// call yields `0` as `call_other` would.
    pub async fn prepare_call(
        &self,
        passed: &[LpcRef],
        ctx: &TaskContext,
        callers: impl FnOnce() -> Result<Callers>,
    ) -> Result<Option<ResolvedCall>> {
        let txn = ctx.txn();
        let mut args = self.bound_args(passed);

        let (process, function) = match &self.address {
            FunctionAddress::Local(receiver, function) => {
                let Some(process) = receiver.upgrade().filter(|p| p.is_live(txn)) else {
                    return Err(LpcError::runtime(format!(
                        "attempted to call a pointer to a function in a destructed object: {}",
                        self
                    )));
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
                            return Err(LpcError::runtime(format!(
                                "attempted to call `{}` on a destructed object",
                                name
                            )));
                        };
                        process
                    }
                    LpcRef::String(_) => {
                        let path = receiver
                            .with_string(|s| ctx.object_path(s.to_str(), "/", "call_other"))??;
                        match ctx.find_object(&path) {
                            ObjectLookup::Found(process) => process,
                            ObjectLookup::Removed => {
                                return Err(LpcError::runtime(format!(
                                    "attempted to call `{}` on a destructed object `{}`",
                                    name, path
                                )));
                            }
                            ObjectLookup::NotCreated => {
                                let loader =
                                    self.loader(ctx.config().lib_dir.as_str(), callers()?)?;
                                let process = ctx.compile_process(&path, &loader).await?;
                                ctx.insert_and_initialize(loader.callers(), &process)
                                    .await?;
                                process
                            }
                        }
                    }
                    _ => {
                        return Err(LpcError::runtime(format!(
                            "`&->{}()` needs an object or path as its receiver, got `{}`",
                            name, receiver
                        )));
                    }
                };
                let Some(function) = process.program.lookup_function(name).cloned() else {
                    return Ok(None);
                };
                (process, function)
            }
            FunctionAddress::Efun(name) => {
                let Some(owner) = self.owner.upgrade().filter(|p| p.is_live(txn)) else {
                    return Err(LpcError::runtime(format!(
                        "attempted to call an efun pointer whose owner is destructed: {}",
                        self
                    )));
                };
                (owner, EFUN_FUNCTIONS[name.as_str()].clone())
            }
            FunctionAddress::SimulEfun(name) => {
                // Links by name to the resident of the task firing it, like a
                // direct call.
                let Some(simul_efuns) = ctx.simul_efuns() else {
                    return Err(LpcError::runtime(format!(
                        "call to simul efun `{name}`: no simul-efun object is loaded"
                    )));
                };
                let Some(function) = simul_efuns.program.lookup_function(name) else {
                    return Err(LpcError::runtime(format!(
                        "call to unknown simul efun `{name}`"
                    )));
                };
                (simul_efuns.clone(), function.clone())
            }
        };

        if let Some(i) = function.prototype.first_ref_param() {
            return Err(LpcError::runtime(format!(
                "`{}` takes argument {} by reference; call it directly",
                function.name(),
                i + 1
            )));
        }

        Ok(Some(ResolvedCall {
            process,
            function,
            args,
        }))
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
            self.upvalue_ptrs
                .iter()
                .map(|x| format!("{x:?}"))
                .join(", ")
        ));
        s.push('}');

        write!(f, "{s}")
    }
}

#[cfg(test)]
mod tests {
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
}
