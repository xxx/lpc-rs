//! `parse_string`: parse a string under a DGD grammar and fold the first
//! derivation that survives its `? func` actions into a flat array.

use std::{collections::HashMap, future::Future, pin::Pin};

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::dgd::{self, Compiled},
        grammar::{Child, Node, Parse, parse},
    },
    interpreter::{
        efun::efun_context::EfunContext, lpc_int::LpcInt, lpc_ref::LpcRef,
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

    // Boxed to stay out of `call_efun`'s unboxed future union, which every
    // efun call pays for.
    let outcome = Box::pin(first_surviving(context.task_context(), &compiled, input)).await?;
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
async fn first_surviving(ctx: &TaskContext, compiled: &Compiled, input: &str) -> Result<Outcome> {
    let mut parses = parse(&compiled.grammar, input);
    let mut evaluator = Evaluator {
        ctx,
        compiled,
        memo: HashMap::new(),
    };
    for parsed in parses.by_ref() {
        if let Some(values) = evaluator.evaluate(&parsed, parsed.root()).await? {
            return Ok(Outcome::Values(values));
        }
    }
    Ok(if parses.over_budget() {
        Outcome::OverBudget
    } else {
        Outcome::None
    })
}

/// A blocked subtree is `None`; otherwise its flat values.
type Evaluated = Option<Vec<LpcRef>>;

/// Bottom-up, left-to-right evaluation of derivation nodes, each distinct
/// subtree once per call.
struct Evaluator<'a> {
    ctx: &'a TaskContext,
    compiled: &'a Compiled,
    memo: HashMap<Node, Evaluated>,
}

impl Evaluator<'_> {
    /// The values of `node`: its tokens' text, its children's values, and
    /// its action's array in their place — or `None` once anything blocks.
    fn evaluate<'s>(
        &'s mut self,
        parsed: &'s Parse<'_>,
        node: &'s Node,
    ) -> Pin<Box<dyn Future<Output = Result<Evaluated>> + Send + 's>> {
        Box::pin(async move {
            if let Some(hit) = self.memo.get(node) {
                return Ok(hit.clone());
            }
            let mut values = Vec::new();
            for child in &node.children {
                match child {
                    Child::Token(i) => values.push(LpcRef::from(parsed.token_text(*i))),
                    Child::Node(inner) => match self.evaluate(parsed, inner).await? {
                        Some(inner_values) => values.extend(inner_values),
                        None => {
                            self.memo.insert(node.clone(), None);
                            return Ok(None);
                        }
                    },
                }
            }
            let evaluated = match self.compiled.action(node.production) {
                None => Some(values),
                Some(name) => self.apply(name, values).await?,
            };
            self.memo.insert(node.clone(), evaluated.clone());
            Ok(evaluated)
        })
    }

    /// Apply `name` in the caller with the subtree's array; its array result
    /// replaces the subtree, anything else — or no such function — blocks.
    async fn apply(&self, name: &str, values: Vec<LpcRef>) -> Result<Evaluated> {
        let this = self.ctx.process();
        let Some(function) = this.program.unmangled_functions.get(name).cloned() else {
            return Ok(None);
        };
        let tree = LpcRef::Array(
            self.ctx
                .txn()
                .with(|t| t.mint_array(values.into_iter().collect())),
        );
        let nested = self.ctx.clone().with_process(this.clone());
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
