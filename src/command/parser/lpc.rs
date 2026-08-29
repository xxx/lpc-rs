//! The parser package's `Ask` in the driver: handlers applied in LPC,
//! phrases through the resolver, candidates from the walk.

use std::sync::Arc;

use lpc_rs_errors::Result;

use super::{
    attempt::{Ask, Slot, Target},
    handlers::{self, Family, Reply},
};
use crate::{
    command::{
        frontend::parser::ParserRule,
        resolve::{Kind as ResolveKind, LpcVocabulary, Resolved, Resolver},
        scope::Candidate,
    },
    interpreter::{
        lpc_array::LpcArray, lpc_ref::LpcRef, process::Process, task_context::TaskContext,
    },
};

/// The driver's answers for one rule over one scope.
pub(crate) struct Lpc<'a> {
    ctx: &'a TaskContext,
    actor: &'a Arc<Process>,
    owner: &'a Arc<Process>,
    rule: &'a ParserRule,
    candidates: &'a [Candidate],
    resolver: Resolver<LpcVocabulary<'a>>,
    arrays: Vec<(usize, LpcRef)>,
}

impl<'a> Lpc<'a> {
    /// The adapter for one rule over `candidates`, in the walk's order.
    pub(crate) fn new(
        ctx: &'a TaskContext,
        actor: &'a Arc<Process>,
        owner: &'a Arc<Process>,
        rule: &'a ParserRule,
        candidates: &'a [Candidate],
        resolver: Resolver<LpcVocabulary<'a>>,
    ) -> Self {
        Lpc {
            ctx,
            actor,
            owner,
            rule,
            candidates,
            resolver,
            arrays: Vec::new(),
        }
    }

    /// Forget the memoized slot arrays; the next parse mints its own.
    pub(crate) fn reset(&mut self) {
        self.arrays.clear();
    }

    /// `candidate` as an LPC object reference.
    fn object(&self, candidate: usize) -> LpcRef {
        LpcRef::from(Arc::downgrade(&self.candidates[candidate].object))
    }

    /// `({ ob... })`.
    pub(crate) fn objects(&self, candidates: &[usize]) -> LpcRef {
        let array: LpcArray = candidates.iter().map(|&c| self.object(c)).collect();
        LpcRef::Array(self.ctx.txn().with(|t| t.mint_array(array)))
    }

    /// The array of the slot at capture index `slot`: minted once per parse,
    /// so every call over that slot receives the same one. Keying by the
    /// candidates instead would alias two slots that picked the same ones.
    fn slot_array(&mut self, slot: usize, candidates: &[usize]) -> LpcRef {
        if let Some((_, array)) = self.arrays.iter().find(|(index, _)| *index == slot) {
            return array.clone();
        }
        let array = self.objects(candidates);
        self.arrays.push((slot, array.clone()));
        array
    }

    /// A slot as the handler receives it.
    fn value(&mut self, slot: &Slot) -> LpcRef {
        match slot {
            Slot::Text(text) => LpcRef::from(text.as_str()),
            Slot::Empty => LpcRef::from(0),
            Slot::Object(c) => self.object(*c),
            Slot::Objects(index, cs) => self.slot_array(*index, cs),
            Slot::Mixed(cs, reasons) => {
                let array: LpcArray = cs
                    .iter()
                    .map(|&c| self.object(c))
                    .chain(reasons.iter().map(|r| LpcRef::from(r.as_str())))
                    .collect();
                LpcRef::Array(self.ctx.txn().with(|t| t.mint_array(array)))
            }
        }
    }

    /// The process a target names.
    pub(crate) fn process(&self, target: Target) -> &Arc<Process> {
        match target {
            Target::Owner => self.owner,
            Target::Candidate(c) => &self.candidates[c].object,
        }
    }
}

impl Ask for Lpc<'_> {
    async fn call(&mut self, family: Family, target: Target, args: &[Slot]) -> Result<Reply> {
        let mut values: Vec<LpcRef> = Vec::with_capacity(args.len());
        for slot in args {
            values.push(self.value(slot));
        }
        handlers::call(
            self.ctx,
            self.actor,
            self.process(target),
            family,
            self.rule,
            &values,
        )
        .await
    }

    async fn resolve(&mut self, kind: ResolveKind, phrase: &str) -> Result<Option<Resolved>> {
        self.resolver.resolve(kind, phrase).await
    }

    fn is_live(&self, candidate: usize) -> bool {
        self.candidates[candidate].object.is_live(self.ctx.txn())
    }

    fn reachable(&self, candidate: usize) -> bool {
        self.candidates[candidate].reachable
    }
}
