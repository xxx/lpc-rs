//! The parser package's protocol for one matched rule: scope, resolution
//! with `direct_`/`indirect_` filtering, disambiguation, `can_` and `do_`,
//! and the failure the master is asked to describe.

mod attempt;
mod handlers;
mod lpc;

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::{
        frontend::native::CaptureKind,
        frontend::parser::ParserRule,
        resolve::{LpcVocabulary, Resolver},
        scope::{self, Candidate, SuppliedScope},
    },
    interpreter::{
        PARSER_ERROR_MESSAGE,
        apply::{apply_nested, as_actor},
        lpc_ref::LpcRef,
        process::Process,
        task_context::TaskContext,
    },
};

use attempt::attempt;
use handlers::{Arg, Kind, furthest};
use lpc::Lpc;

/// How one parser rule fared before consulting the master's error hook.
pub(crate) enum Verdict {
    /// `do_` ran.
    Handled,
    /// Permissions accepted; these are the generic action arguments.
    Arguments(Vec<LpcRef>),
    /// No parse of the line fitted the rule's grammar.
    NoParse,
    /// Matching or permission checks failed.
    Rejected(Rejection),
}

/// A failed rule's result when the master gives no message.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum RejectionCode {
    /// A handler refused.
    Refused,
    /// An object phrase did not resolve.
    Unresolved,
}

/// A failed rule's diagnostic, retained until every rule has failed.
pub(crate) struct Rejection {
    pub(crate) code: RejectionCode,
    pub(crate) progress: usize,
    args: Option<[LpcRef; 4]>,
}

impl Rejection {
    /// Snapshot candidate references before the rule's scope is discarded.
    fn new(ctx: &TaskContext, ask: &Lpc<'_>, failure: handlers::Failure) -> Self {
        let args = (failure.kind != Kind::Refused).then(|| {
            let arg = match failure.arg {
                Arg::None => ctx.txn().nil(),
                Arg::Text(text) => LpcRef::from(text.as_str()),
                Arg::Count(n) => LpcRef::from(n),
                Arg::Objects(candidates) => ask.objects(&candidates),
            };
            let object = failure.object.map_or_else(
                || ctx.txn().nil(),
                |target| LpcRef::from(Arc::downgrade(ask.process(target))),
            );
            [
                LpcRef::from(failure.kind as i64),
                object,
                arg,
                LpcRef::from(i64::from(failure.flag)),
            ]
        });
        Self {
            code: failure.silent,
            progress: failure.progress,
            args,
        }
    }

    /// Ask the master for a message after selection has exhausted all rules.
    pub(crate) async fn report(
        self,
        ctx: &TaskContext,
        actor: &Arc<Process>,
    ) -> Result<Option<String>> {
        let Some(args) = self.args else {
            return Ok(None);
        };
        let Some(master) = ctx.master_object() else {
            return Ok(None);
        };
        let Some(function) = master
            .program(ctx.txn())
            .unmangled_functions
            .get(PARSER_ERROR_MESSAGE)
            .cloned()
        else {
            return Ok(None);
        };
        match apply_nested(ctx, as_actor(ctx, actor), &master, function, &args).await? {
            LpcRef::String(message) => Ok(Some(message.to_string())),
            _ => Ok(None),
        }
    }
}

/// A `parse_sentence` nickname: `name` also names `object`.
#[derive(Clone, Debug)]
pub(crate) struct Nickname {
    pub(crate) name: String,
    pub(crate) object: Arc<Process>,
}

/// What to do after a parse passes permission checks.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum Completion {
    /// Find and call an action handler.
    #[default]
    Execute,
    /// Return the generic action arguments without looking up an action.
    Arguments,
}

/// Scope overrides and completion policy for a parser search.
#[derive(Clone, Copy, Default)]
pub(crate) struct Options<'a> {
    pub(crate) scope: Option<&'a SuppliedScope>,
    pub(crate) nicknames: &'a [Nickname],
    pub(crate) completion: Completion,
}

/// Run `rule` (owned by `owner`) for `actor` over `rest`, the line after
/// its verb, using the search's scope and completion policy.
pub(crate) async fn run(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    owner: &Arc<Process>,
    rule: &ParserRule,
    rest: &str,
    options: Options<'_>,
) -> Result<Verdict> {
    let candidates = match options.scope {
        Some(supplied) => supplied.walk(ctx, actor).await?,
        None => scope::walk(ctx, actor).await?,
    };
    let remote_from = candidates.len();
    let needs_livings = rule
        .compiled
        .kinds
        .iter()
        .any(|k| matches!(k, CaptureKind::Liv | CaptureKind::Living));
    let mut all = candidates;
    if needs_livings {
        for user in scope::users(ctx, actor).await? {
            if !all.iter().any(|c| Arc::ptr_eq(&c.object, &user)) {
                all.push(Candidate {
                    object: user,
                    reachable: true,
                });
            }
        }
    }
    let objects: Vec<Arc<Process>> = all.iter().map(|c| c.object.clone()).collect();
    let extras: Vec<Vec<String>> = objects
        .iter()
        .map(|object| {
            options
                .nicknames
                .iter()
                .filter(|n| Arc::ptr_eq(&n.object, object))
                .map(|n| n.name.clone())
                .collect()
        })
        .collect();
    let vocabulary =
        LpcVocabulary::with_extras(ctx, as_actor(ctx, actor), objects, extras, remote_from);
    let resolver = Resolver::new(vocabulary, None);
    let mut ask = Lpc::new(ctx, actor, owner, rule, &all, resolver, options.completion);

    let mut failures = Vec::new();
    let mut any_parse = false;
    for caps in rule.compiled.captures_of(rest) {
        any_parse = true;
        match attempt(&mut ask, &caps).await? {
            Ok(()) => return Ok(ask.into_verdict()),
            Err(failure) => failures.push(failure),
        }
    }
    if !any_parse {
        return Ok(Verdict::NoParse);
    }
    let Some(failure) = furthest(failures) else {
        return Ok(Verdict::NoParse);
    };
    Ok(Verdict::Rejected(Rejection::new(ctx, &ask, failure)))
}
