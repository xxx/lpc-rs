//! Driving a frame's pending call: sync first, the async arm only to load
//! or initialize a receiver.

use std::sync::Arc;

use async_recursion::async_recursion;
use lpc_rs_core::{RegisterSize, lpc_path::LpcPath};
use lpc_rs_errors::{LpcError, Result, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;

use crate::interpreter::{
    call_frame::{CallFrame, CollectionCall},
    continuation::{Callee, Next, Pending},
    efun::Efun,
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::VarId,
    task::{
        Task,
        handle_call_fp::{Called, Passed},
        handle_call_other::Standing,
    },
};

/// What `advance_pending` left: a callee frame on top or the slot empty,
/// or a step only the async arm can take.
pub(crate) enum Advance {
    Running,
    Suspends,
}

/// What the async arm did with the suspended step.
enum Resolved {
    /// The callee frame is on top; its `Ret` continues the walk.
    Framed,
    /// No frame; walk on, `answered` when the callee's answer is in `r0`.
    Continue { answered: bool },
}

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Advance the top frame's pending call — `answered` when a callee's
    /// answer sits in its `r0` — until a callee frame is on top, the call
    /// finishes into `r0`, or a step needs the async arm. On an error the
    /// slot stays empty: the frame is unwound, or its `catch` clears it.
    pub(crate) fn advance_pending(&mut self, answered: bool) -> Result<Advance> {
        let owner = self.stack.len().saturating_sub(1);
        let frame = self.stack.current_frame_mut()?;
        let Some(mut pending) = frame.pending.take() else {
            return Err(self.runtime_bug("advance_pending on a frame with no pending call"));
        };
        let result = answered.then(|| std::mem::take(&mut frame.registers[0]));
        let outcome = match &mut *pending {
            Pending::Collection(call) => match self.advance_collection(call, result)? {
                Collected::Framed => {
                    self.restore_pending(owner, pending)?;
                    Ok(Advance::Running)
                }
                Collected::Done(value) => {
                    self.stack.current_frame_mut()?.registers[0] = value;
                    Ok(Advance::Running)
                }
                Collected::Suspends => {
                    self.restore_pending(owner, pending)?;
                    return Ok(Advance::Suspends);
                }
            },
            Pending::Efun(cont) => {
                let (efun, span) = (cont.efun, cont.span);
                let next = cont
                    .state
                    .advance(result, &self.context.txn)
                    .map_err(|e| e.or_span(span))?;
                match next {
                    Next::Done(value) => {
                        self.stack.current_frame_mut()?.registers[0] = value;
                        Ok(Advance::Running)
                    }
                    Next::Call(callee) => {
                        match self.call_callee_now(&callee).map_err(|e| e.or_span(span))? {
                            Called::Framed => {
                                self.restore_pending(owner, pending)?;
                                Ok(Advance::Running)
                            }
                            Called::Pending => {
                                self.restore_pending(owner, pending)?;
                                return Ok(Advance::Suspends);
                            }
                            Called::Unresolved => Err(unresolved(efun, span)),
                            Called::Suspends => {
                                cont.suspended = Some(callee);
                                self.restore_pending(owner, pending)?;
                                return Ok(Advance::Suspends);
                            }
                        }
                    }
                }
            }
        };
        if outcome.is_ok() {
            debug_assert!(
                self.stack.len() - 1 > owner
                    || self.stack.get(owner).is_none_or(|f| f.pending.is_none()),
                "a frame with a pending call is never stepped"
            );
        }
        outcome
    }

    /// Make `callee`'s call where no loading is needed.
    fn call_callee_now(&mut self, callee: &Callee) -> Result<Called> {
        match callee {
            Callee::Pointer { ptr, args } => {
                self.call_pointer_now(Passed::Values { ptr, values: args })
            }
            Callee::Function {
                process,
                function,
                args,
            } => {
                self.push_external_frame(
                    process.clone(),
                    function.clone(),
                    args.iter().cloned(),
                    None,
                )?;
                Ok(Called::Framed)
            }
        }
    }

    /// Put `pending` back on frame `owner`, the one it was taken from; the
    /// callee frames a step pushed sit above it.
    fn restore_pending(&mut self, owner: usize, pending: Box<Pending>) -> Result<()> {
        match self.stack.get_mut(owner) {
            Some(frame) => {
                frame.pending = Some(pending);
                Ok(())
            }
            None => Err(self.runtime_bug("the frame with the pending call is gone")),
        }
    }

    /// One collection step: record `result`, then call the next receiver.
    fn advance_collection(
        &mut self,
        call: &mut CollectionCall,
        result: Option<LpcRef>,
    ) -> Result<Collected> {
        if let Some(value) = result {
            call.results.push(value);
        }
        loop {
            let Some(receiver) = call.remaining.pop() else {
                return Ok(Collected::Done(self.collected(call)));
            };
            match Self::standing(&receiver, &self.context)? {
                Standing::Ready(process) => {
                    let function = process
                        .program
                        .lookup_function(&call.name)
                        .filter(|function| function.public())
                        .cloned();
                    match function {
                        Some(function) => {
                            debug_assert!(
                                !function.prototype.is_efun(),
                                "a `->` callee has a body"
                            );
                            let args = call.args.clone();
                            self.push_external_frame(process, function, args.into_iter(), None)?;
                            return Ok(Collected::Framed);
                        }
                        None => call.results.push(NULL),
                    }
                }
                Standing::Dead | Standing::Removed(_) => call.results.push(NULL),
                Standing::Uncreated(_) | Standing::Uninitialized(_) => {
                    call.remaining.push(receiver);
                    return Ok(Collected::Suspends);
                }
            }
        }
    }

    /// The finished collection's value: an array, or the mapping over `keys`.
    fn collected(&self, call: &mut CollectionCall) -> LpcRef {
        let results = std::mem::take(&mut call.results);
        match call.keys.take() {
            None => LpcRef::Array(
                self.context
                    .txn
                    .with(|t| t.mint_array(LpcArray::new(results))),
            ),
            Some(keys) => {
                debug_assert_eq!(keys.len(), results.len());
                LpcRef::Mapping(self.context.txn.with(|t| {
                    t.mint_mapping(LpcMapping::new(keys.into_iter().zip(results).collect()))
                }))
            }
        }
    }

    /// The async arm of a suspended step: load or initialize the receiver,
    /// then push its frame or record 0. On an error the slot stays empty:
    /// the frame is unwound, or its `catch` clears it.
    ///
    /// Boxed: an efun fired through the slow door can itself settle a new
    /// pending call, cycling back here.
    #[async_recursion]
    async fn resolve_suspended(&mut self) -> Result<Resolved> {
        let owner = self.stack.len().saturating_sub(1);
        let frame = self.stack.current_frame_mut()?;
        let Some(mut pending) = frame.pending.take() else {
            return Err(self.runtime_bug("resolve_suspended on a frame with no pending call"));
        };
        let resolved = match &mut *pending {
            Pending::Collection(call) => {
                let Some(receiver) = call.remaining.pop() else {
                    return Err(self.runtime_bug("a suspended collection with no receiver left"));
                };
                let name = call.name.clone();
                let callee =
                    Self::resolve_call_other_receiver(&receiver, &name, &self.context, || {
                        self.loader()
                    })
                    .await?
                    .filter(|(_, function)| function.public());
                match callee {
                    Some((process, function)) => {
                        debug_assert!(!function.prototype.is_efun(), "a `->` callee has a body");
                        let args = call.args.clone();
                        self.push_external_frame(process, function, args.into_iter(), None)?;
                        Resolved::Framed
                    }
                    None => {
                        call.results.push(NULL);
                        Resolved::Continue { answered: false }
                    }
                }
            }
            Pending::Efun(cont) => {
                let (efun, span) = (cont.efun, cont.span);
                let Some(Callee::Pointer { ptr, args }) = cont.suspended.take() else {
                    return Err(self.runtime_bug("a suspended callback with no pointer to call"));
                };
                match self
                    .call_pointer_slow(&ptr, args.to_vec())
                    .await
                    .map_err(|e| e.or_span(span))?
                {
                    Called::Framed => Resolved::Framed,
                    Called::Unresolved => return Err(unresolved(efun, span)),
                    Called::Suspends | Called::Pending => {
                        return Err(self.runtime_bug("the slow pointer door suspended"));
                    }
                }
            }
        };
        self.restore_pending(owner, pending)?;
        Ok(resolved)
    }

    /// `advance_pending`, taking the async arm whenever a step suspends.
    pub(crate) async fn advance_pending_async(&mut self, answered: bool) -> Result<()> {
        let mut answered = answered;
        loop {
            match self.advance_pending(answered)? {
                Advance::Running => return Ok(()),
                Advance::Suspends => match self.resolve_suspended().await? {
                    Resolved::Framed => return Ok(()),
                    Resolved::Continue { answered: again } => answered = again,
                },
            }
        }
    }

    /// The dispatch of `AsyncCall::Pending`: the suspended step, then on.
    pub(crate) async fn continue_pending(&mut self) -> Result<()> {
        match self.resolve_suspended().await? {
            Resolved::Framed => Ok(()),
            Resolved::Continue { answered } => self.advance_pending_async(answered).await,
        }
    }

    /// Push a frame for `function` on `process` entered through a door,
    /// with `args` stored in place and `origin` the pointer's writer.
    pub(crate) fn push_external_frame(
        &mut self,
        process: Arc<Process>,
        function: Arc<ProgramFunction>,
        args: impl ExactSizeIterator<Item = LpcRef>,
        origin: Option<Arc<LpcPath>>,
    ) -> Result<()> {
        if let Some(i) = function.prototype.first_ref_param() {
            return Err(self.runtime_error(format!(
                "argument {} of `{}` must be passed by reference",
                i + 1,
                function.name()
            )));
        }
        let mut frame = CallFrame::new(
            process,
            function,
            RegisterSize::try_from(args.len())?,
            None::<&[VarId]>,
        );
        for (i, arg) in args.enumerate() {
            frame.push_arg(&self.context.txn, i, arg)?;
        }
        frame.origin = origin;
        frame.external = true;
        self.stack.push(frame)
    }
}

/// The error for a callback pointer that names no function.
fn unresolved(efun: Efun, span: Option<Span>) -> LpcError {
    LpcError::runtime(format!("{}: the function no longer resolves", efun.name())).with_span(span)
}

/// What one collection step produced.
enum Collected {
    Framed,
    Done(LpcRef),
    Suspends,
}
