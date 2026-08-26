//! `parse_string`: parse a string under a DGD grammar and fold the first
//! derivation that survives its `? func` actions into a flat array.

use std::{
    collections::HashMap,
    future::Future,
    hash::{DefaultHasher, Hash, Hasher},
    ops::Range,
    pin::Pin,
    sync::Arc,
};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::dgd::{self, Compiled},
        grammar::{Child, Node, Parse, ProdId, parse},
    },
    interpreter::{
        efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef, process::Process,
        task::apply_function::apply_function, task_context::TaskContext,
    },
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
    // The frame's own object, not `TaskContext::process` (documented
    // inaccurate across call_others and function-pointer calls): actions
    // run where the caller actually is, same as `this_object()`.
    let this = context.frame().process.clone();

    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for.
    let outcome = Box::pin(first_surviving(
        context.task_context(),
        &this,
        &compiled,
        input,
    ))
    .await?;
    match outcome {
        Outcome::Values(values) => context.return_array(values),
        Outcome::None => context.return_efun_result(LpcRef::from(0)),
        Outcome::OverBudget => {
            return Err(context.runtime_error("parse_string: parse budget exhausted"));
        }
    }
    Ok(())
}

/// How one call ended.
enum Outcome {
    /// A derivation survived; its flat values.
    Values(Vec<LpcRef>),
    /// Every derivation was blocked, or there was none.
    None,
    /// The step budget ran out before a derivation survived.
    OverBudget,
}

/// Derivations of `input` in the engine's order, each evaluated until one
/// survives.
async fn first_surviving(
    ctx: &TaskContext,
    this: &Arc<Process>,
    compiled: &Compiled,
    input: &str,
) -> Result<Outcome> {
    let mut parses = parse(&compiled.grammar, input);
    let mut evaluator = Evaluator {
        ctx,
        this: this.clone(),
        compiled,
        memo: HashMap::new(),
    };
    for parsed in parses.by_ref() {
        if let Some((_, values)) = evaluator.evaluate(&parsed, parsed.root()).await? {
            return Ok(Outcome::Values(values));
        }
    }
    Ok(if parses.over_budget() {
        Outcome::OverBudget
    } else {
        Outcome::None
    })
}

/// A blocked subtree is `None`; a surviving one is its values.
type Evaluated = Option<Vec<LpcRef>>;

/// A derivation node's structural identity: its production and token span,
/// plus a digest of its children's keys in order (a token contributes its
/// index). Two nodes key equal exactly when their subtrees look identical
/// — production, span, and every descendant in the same shape — so a
/// subtree shared by several derivations runs its action once, without the
/// memo cloning or hashing whole node trees. A 64-bit digest collision
/// would wrongly alias two distinct subtrees under one key; accepted odds.
type Key = (ProdId, Range<usize>, u64);

/// `Evaluator::evaluate`'s result: the node's key paired with its values,
/// or `None` once anything blocks.
type NodeResult = Result<Option<(Key, Vec<LpcRef>)>>;

/// Bottom-up, left-to-right evaluation of derivation nodes, each distinct
/// subtree's action run once per call.
struct Evaluator<'a> {
    ctx: &'a TaskContext,
    /// The object actions apply in: the frame's own object at the time
    /// `parse_string` was called, not [`TaskContext::process`].
    this: Arc<Process>,
    compiled: &'a Compiled,
    memo: HashMap<Key, Evaluated>,
}

impl Evaluator<'_> {
    /// `node`'s structural key paired with its values: its tokens' text,
    /// its children's values, and its action's array in their place — or
    /// `None` once anything blocks.
    fn evaluate<'s>(
        &'s mut self,
        parsed: &'s Parse<'_>,
        node: &'s Node,
    ) -> Pin<Box<dyn Future<Output = NodeResult> + Send + 's>> {
        Box::pin(async move {
            let mut values = Vec::new();
            let mut hasher = DefaultHasher::new();
            for child in &node.children {
                match child {
                    Child::Token(i) => {
                        i.hash(&mut hasher);
                        values.push(LpcRef::from(parsed.token_text(*i)));
                    }
                    Child::Node(inner) => match self.evaluate(parsed, inner).await? {
                        Some((child_key, inner_values)) => {
                            child_key.hash(&mut hasher);
                            values.extend(inner_values);
                        }
                        None => return Ok(None),
                    },
                }
            }
            let key: Key = (node.production, node.span.clone(), hasher.finish());
            if let Some(hit) = self.memo.get(&key) {
                return Ok(hit.clone().map(|v| (key, v)));
            }
            let evaluated = match self.compiled.action(node.production) {
                None => Some(values),
                Some(name) => self.apply(name, values).await?,
            };
            self.memo.insert(key.clone(), evaluated.clone());
            Ok(evaluated.map(|v| (key, v)))
        })
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
        let nested = self.ctx.clone().with_process(self.this.clone());
        let timeout = self.ctx.config().max_execution_time;
        let result = apply_function(function, &[tree], nested, Some(timeout)).await?;
        match &result {
            LpcRef::Array(_) => result
                .with_array(self.ctx.txn(), |a| a.iter().cloned().collect())
                .map(Some),
            _ => Ok(None),
        }
    }
}
