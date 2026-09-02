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
        scope::{self, Candidate},
    },
    interpreter::{
        PARSER_ERROR_MESSAGE,
        apply::{apply_on, as_actor},
        lpc_ref::LpcRef,
        process::Process,
        task_context::TaskContext,
    },
};

use attempt::attempt;
use handlers::{Arg, Failure, Kind, furthest};
use lpc::Lpc;

/// How one parser rule fared against a line.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Verdict {
    /// `do_` ran.
    Handled,
    /// No parse of the line fitted the rule's grammar.
    NoParse,
    /// A handler refused and the master gave no message.
    Refused,
    /// An object phrase did not resolve and the master gave no message.
    Unresolved,
    /// The master's message for the failure.
    Message(String),
}

/// A `parse_sentence` nickname: `name` also names `object`.
#[derive(Clone, Debug)]
pub(crate) struct Nickname {
    pub(crate) name: String,
    pub(crate) object: Arc<Process>,
}

/// Run `rule` (owned by `owner`) for `actor` over `rest`, the line after
/// its verb; `scope` replaces the default walk when given.
pub(crate) async fn run(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    owner: &Arc<Process>,
    rule: &ParserRule,
    rest: &str,
    scope: Option<Vec<Arc<Process>>>,
    nicknames: &[Nickname],
) -> Result<Verdict> {
    let candidates = match scope {
        Some(objects) => objects
            .into_iter()
            .map(|object| Candidate {
                object,
                reachable: true,
            })
            .collect(),
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
            nicknames
                .iter()
                .filter(|n| Arc::ptr_eq(&n.object, object))
                .map(|n| n.name.clone())
                .collect()
        })
        .collect();
    let vocabulary =
        LpcVocabulary::with_extras(ctx, as_actor(ctx, actor), objects, extras, remote_from);
    let resolver = Resolver::new(vocabulary, None);
    let mut ask = Lpc::new(ctx, actor, owner, rule, &all, resolver);

    let mut failures: Vec<Failure> = Vec::new();
    let mut any_parse = false;
    for caps in rule.compiled.captures_of(rest) {
        any_parse = true;
        match attempt(&mut ask, &caps).await? {
            Ok(()) => return Ok(Verdict::Handled),
            Err(failure) => failures.push(failure),
        }
    }
    if !any_parse {
        return Ok(Verdict::NoParse);
    }
    let Some(failure) = furthest(failures) else {
        return Ok(Verdict::NoParse);
    };
    report(ctx, actor, &ask, failure).await
}

/// Ask the master to describe `failure`; a string is the verdict's
/// message, anything else falls back to `failure.silent`. `Kind::Refused`
/// never reaches the master — it carries no message. The hook is looked up
/// before `arg` is built, so nothing is minted for a master without one.
async fn report(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    ask: &Lpc<'_>,
    failure: Failure,
) -> Result<Verdict> {
    let silent = failure.silent;
    if failure.kind == Kind::Refused {
        return Ok(silent);
    }
    let Some(master) = ctx.object_space().master_object() else {
        return Ok(silent);
    };
    let Some(function) = master
        .program
        .unmangled_functions
        .get(PARSER_ERROR_MESSAGE)
        .cloned()
    else {
        return Ok(silent);
    };
    let arg = match failure.arg {
        Arg::None => LpcRef::from(0),
        Arg::Text(text) => LpcRef::from(text.as_str()),
        Arg::Count(n) => LpcRef::from(n),
        Arg::Objects(candidates) => ask.objects(&candidates),
    };
    let object = failure.object.map_or(LpcRef::from(0), |target| {
        LpcRef::from(Arc::downgrade(ask.process(target)))
    });
    let args = [
        LpcRef::from(failure.kind as i64),
        object,
        arg,
        LpcRef::from(i64::from(failure.flag)),
    ];
    match apply_on(ctx, as_actor(ctx, actor), &master, actor, function, &args).await? {
        LpcRef::String(message) => Ok(Verdict::Message(message.to_string())),
        _ => Ok(silent),
    }
}
