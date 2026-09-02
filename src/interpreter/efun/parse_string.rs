//! `parse_string`: parse a string under a DGD grammar and fold the first
//! derivation that survives its `? func` actions into a flat array.

use std::{
    collections::HashMap,
    hash::{DefaultHasher, Hash, Hasher},
    ops::Range,
    sync::Arc,
};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::dgd::{self, CompiledGrammar},
        grammar::{Child, DEFAULT_MAX_DEPTH, Ending, Limits, Node, Parse, ProdId, parse},
    },
    interpreter::{
        apply::apply_nested,
        efun::efun_context::EfunContext,
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        process::Process,
        task_context::{Callers, TaskContext},
    },
};

/// What one `parse_string` call may spend: 64 derivations pulled before it
/// gives up, 2²⁰ steps, the engine's default depth.
pub(crate) const LIMITS: Limits = Limits {
    max_parses: 64,
    max_steps: 1 << 20,
    max_depth: DEFAULT_MAX_DEPTH,
};

/// `parse_string(grammar, str, alternatives)`: the flat array of the first
/// derivation whose actions all accept it, or 0 when none does.
pub async fn parse_string<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let grammar = context.resolve_local_register(1 as RegisterSize).clone();
    let input = context.resolve_local_register(2 as RegisterSize).clone();
    let (Some(grammar), Some(input)) = (grammar.as_str(), input.as_str()) else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    if context.frame().called_with_num_args >= 3
        && !matches!(
            context.resolve_local_register(3 as RegisterSize),
            LpcRef::Int(LpcInt(0))
        )
    {
        return Err(context.runtime_error("parse_string: alternatives are not supported"));
    }
    let compiled = dgd::compile_cached(grammar)
        .map_err(|e| context.runtime_error(format!("parse_string: {e}")))?;
    // The frame's own object, same as `this_object()`, not `TaskContext::process`.
    let this = context.process().clone();

    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for.
    let outcome = Box::pin(first_surviving(
        context.task_context(),
        Some(context.chain()),
        &this,
        &compiled,
        input,
    ))
    .await?;
    match outcome {
        Outcome::Values(values) => context.return_array(values),
        Outcome::Ended(Ending::Done) => context.return_efun_result(LpcRef::from(0)),
        Outcome::Ended(Ending::Exhausted) => {
            return Err(context.runtime_error("parse_string: parse budget exhausted"));
        }
        Outcome::Ended(Ending::TooDeep) => {
            return Err(context.runtime_error(format!(
                "parse_string: parse deeper than {}",
                LIMITS.max_depth
            )));
        }
    }
    Ok(())
}

/// How one call ended.
enum Outcome {
    /// A derivation survived; its flat values.
    Values(Vec<LpcRef>),
    /// No derivation survived; how the enumeration stopped.
    Ended(Ending),
}

/// Derivations of `input` in the engine's order, each evaluated until one
/// survives.
async fn first_surviving(
    ctx: &TaskContext,
    callers: Callers,
    this: &Arc<Process>,
    compiled: &CompiledGrammar,
    input: &str,
) -> Result<Outcome> {
    let mut parses = parse(&compiled.grammar, input, LIMITS);
    let mut evaluator = Evaluator {
        ctx,
        callers,
        this: this.clone(),
        compiled,
        memo: HashMap::new(),
    };
    for parsed in parses.by_ref() {
        if let Some((_, values)) = evaluator.evaluate(&parsed, parsed.root()).await? {
            return Ok(Outcome::Values(values));
        }
    }
    Ok(Outcome::Ended(parses.ending()))
}

/// A blocked subtree is `None`; a surviving one is its values.
type Evaluated = Option<Vec<LpcRef>>;

/// One open node during evaluation: the next child to visit, the values
/// gathered so far, and the digest of the children's keys so far.
struct Frame<'n> {
    node: &'n Node,
    next: usize,
    values: Vec<LpcRef>,
    hasher: DefaultHasher,
}

impl<'n> Frame<'n> {
    fn open(node: &'n Node) -> Self {
        Frame {
            node,
            next: 0,
            values: Vec::new(),
            hasher: DefaultHasher::new(),
        }
    }
}

/// A derivation node's structural identity: its production and token span
/// plus a digest of its children's keys in order (a token contributes its
/// index) — a 64-bit digest collision would alias two distinct subtrees,
/// accepted odds.
type Key = (ProdId, Range<usize>, u64);

/// `Evaluator::evaluate`'s result: the node's key paired with its values,
/// or `None` once anything blocks.
type NodeResult = Result<Option<(Key, Vec<LpcRef>)>>;

/// Bottom-up, left-to-right evaluation of derivation nodes, each distinct
/// subtree's action run once per call.
struct Evaluator<'a> {
    ctx: &'a TaskContext,
    /// The chain the actions are entered through: `parse_string`'s caller.
    callers: Callers,
    /// The object actions apply in: the frame's own object at the time
    /// `parse_string` was called, not [`TaskContext::process`].
    this: Arc<Process>,
    compiled: &'a CompiledGrammar,
    memo: HashMap<Key, Evaluated>,
}

impl Evaluator<'_> {
    /// `root`'s structural key paired with its values: its tokens' text,
    /// its children's values, and its action's array in their place — or
    /// `None` once anything blocks. An explicit stack of frames, one per
    /// open node — the tree may be `Limits::max_depth` deep.
    async fn evaluate(&mut self, parsed: &Parse<'_>, root: &Node) -> NodeResult {
        let mut stack = vec![Frame::open(root)];
        loop {
            let top = stack.last_mut().expect("the root frame closes last");
            if let Some(child) = top.node.children.get(top.next) {
                top.next += 1;
                match child {
                    Child::Token(i) => {
                        i.hash(&mut top.hasher);
                        top.values.push(LpcRef::from(parsed.token_text(*i)));
                    }
                    Child::Node(inner) => stack.push(Frame::open(inner)),
                }
                continue;
            }
            let frame = stack.pop().expect("the frame just inspected");
            let key: Key = (
                frame.node.production,
                frame.node.span.clone(),
                frame.hasher.finish(),
            );
            let action = self.compiled.action(frame.node.production);
            // Action-less nodes skip the memo — their values are just their
            // already-evaluated children's, so caching saves nothing.
            let evaluated = match action {
                None => Some(frame.values),
                Some(name) => match self.memo.get(&key) {
                    Some(hit) => hit.clone(),
                    None => {
                        let evaluated = self.apply(name, frame.values).await?;
                        self.memo.insert(key.clone(), evaluated.clone());
                        evaluated
                    }
                },
            };
            let Some(values) = evaluated else {
                return Ok(None);
            };
            let Some(parent) = stack.last_mut() else {
                return Ok(Some((key, values)));
            };
            key.hash(&mut parent.hasher);
            parent.values.extend(values);
        }
    }

    /// Apply `name` on [`Self::this`] with the subtree's array; its array
    /// result replaces the subtree, anything else — or no such function —
    /// blocks.
    async fn apply(&self, name: &str, values: Vec<LpcRef>) -> Result<Evaluated> {
        let Some(function) = self.this.program.unmangled_functions.get(name).cloned() else {
            return Ok(None);
        };
        let tree = LpcRef::Array(
            self.ctx
                .txn()
                .with(|t| t.mint_array(values.into_iter().collect())),
        );
        let result = apply_nested(
            self.ctx,
            self.callers.clone(),
            &self.this,
            function,
            &[tree],
        )
        .await?;
        match &result {
            LpcRef::Array(_) => result
                .with_array(self.ctx.txn(), |a| a.iter().cloned().collect())
                .map(Some),
            _ => Ok(None),
        }
    }
}
