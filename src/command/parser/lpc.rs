//! The parser package's `Ask` in the driver: handlers applied in LPC,
//! phrases through the resolver, candidates from the walk.

use std::sync::Arc;

use lpc_rs_errors::Result;

use super::{
    Completion, Verdict,
    attempt::{Ask, Slot, Target},
    handlers::{self, Family, Reply, Tier},
};
use crate::{
    command::{
        frontend::parser::{Access, ParserRule},
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
    completion: Completion,
    arguments: Option<Vec<LpcRef>>,
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
        completion: Completion,
    ) -> Self {
        Lpc {
            ctx,
            actor,
            owner,
            rule,
            candidates,
            resolver,
            arrays: Vec::new(),
            completion,
            arguments: None,
        }
    }

    /// The successful final action call either executed or retained its arguments.
    pub(crate) fn into_verdict(self) -> Verdict {
        self.arguments.map_or(Verdict::Handled, Verdict::Arguments)
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
            Slot::Empty => self.ctx.txn().nil(),
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
            Target::Actor => self.actor,
            Target::Owner => self.owner,
            Target::Candidate(c) => &self.candidates[c].object,
        }
    }
}

impl Ask for Lpc<'_> {
    fn begin_parse(&mut self) {
        self.arrays.clear();
        self.arguments = None;
    }

    async fn call(&mut self, family: Family, target: Target, args: &[Slot]) -> Result<Reply> {
        let mut values: Vec<LpcRef> = Vec::with_capacity(args.len());
        for slot in args {
            values.push(self.value(slot));
        }
        if !self.process(target).is_live(self.ctx.txn()) {
            return Ok(Reply::Absent);
        }
        if family == Family::Do && self.completion == Completion::Arguments {
            self.arguments = Some(Tier::Rule.arguments(self.ctx, self.rule, &values));
            return Ok(Reply::Yes);
        }
        for source in [target, Target::Owner] {
            if source == Target::Owner
                && target != Target::Owner
                && Arc::ptr_eq(self.process(target), self.owner)
            {
                break;
            }
            let process = self.process(source);
            if process.is_live(self.ctx.txn())
                && let Some(value) =
                    handlers::call(self.ctx, self.actor, process, family, self.rule, &values)
                        .await?
            {
                return Ok(Reply::from_value(&value, source));
            }
            if target == Target::Owner {
                break;
            }
        }
        Ok(Reply::Absent)
    }

    async fn resolve(&mut self, kind: ResolveKind, phrase: &str) -> Result<Option<Resolved>> {
        self.resolver.resolve(kind, phrase).await
    }

    fn is_live(&self, candidate: usize) -> bool {
        self.candidates[candidate].object.is_live(self.ctx.txn())
    }

    fn accessible(&self, slot: usize, candidate: usize) -> bool {
        self.rule.access[slot] == Access::Visible || self.candidates[candidate].reachable
    }
}
