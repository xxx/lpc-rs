//! What a frame is waiting on while callee frames above it run: a collection
//! `->`, or an efun's callbacks.

use std::{fmt::Debug, sync::Arc};

use lpc_rs_errors::{Result, span::Span};
use lpc_rs_function_support::program_function::ProgramFunction;
use smallvec::SmallVec;

use crate::interpreter::{
    call_frame::CollectionCall, efun::Efun, function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef, process::Process, stm::TxnHandle,
};

/// An efun's callbacks one at a time: the next call, or the answer.
pub(crate) trait Continuation: Send + Sync + Debug {
    /// The step after `result`, the last callee's answer (none before the
    /// first call); an error carries no span, the loop adds the call site.
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next>;

    /// A copy, for the frame's `Clone`.
    fn clone_box(&self) -> Box<dyn Continuation>;
}

impl Clone for Box<dyn Continuation> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

/// What a continuation asks for next.
#[derive(Debug)]
pub(crate) enum Next {
    Call(Callee),
    Done(LpcRef),
}

/// One call a continuation makes.
#[derive(Debug, Clone)]
pub(crate) enum Callee {
    /// `ptr` with `args`; the pointer's partial application binds on top.
    Pointer {
        ptr: Arc<FunctionPtr>,
        args: SmallVec<[LpcRef; 4]>,
    },
    /// `function` on `process` with `args`, entered through a door.
    #[expect(dead_code, reason = "no continuation constructs this yet")]
    Function {
        process: Arc<Process>,
        function: Arc<ProgramFunction>,
        args: SmallVec<[LpcRef; 4]>,
    },
}

/// An efun's walk over its callbacks, on the frame that called the efun.
#[derive(Debug, Clone)]
pub(crate) struct EfunContinuation {
    /// The efun that installed this walk.
    pub efun: Efun,
    /// The efun's own state: what to call next, or the answer.
    pub state: Box<dyn Continuation>,
    /// The efun's call site: the frame's pc has moved past it by the time
    /// an answer comes back.
    pub span: Option<Span>,
    /// The callee the async arm owes when a step suspended.
    pub suspended: Option<Callee>,
}

/// The call a frame has in flight; the eval loop advances it on every
/// `Ret` into the frame.
#[derive(Debug, Clone)]
pub(crate) enum Pending {
    /// A collection `->` mid-way.
    Collection(CollectionCall),
    /// An efun waiting on callbacks.
    Efun(EfunContinuation),
}
