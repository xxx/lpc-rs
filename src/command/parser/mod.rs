//! The parser package's protocol for one matched rule: scope, resolution
//! with `direct_`/`indirect_` filtering, disambiguation, `can_` and `do_`,
//! and the failure the master is asked to describe.

mod handlers;
mod scope;

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::{
        dispatch::apply_on,
        frontend::native::{Capture, CaptureKind},
        frontend::parser::ParserRule,
        resolve::{Kind as ResolveKind, LpcVocabulary, Resolved, Resolver},
    },
    interpreter::{
        PARSER_ERROR_MESSAGE, lpc_array::LpcArray, lpc_ref::LpcRef, process::Process,
        task_context::TaskContext,
    },
};

use handlers::{Arg, Failure, Family, Kind, Reply, best_reason, call, furthest};
use scope::Candidate;

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

/// One object slot chosen in `attempt`: its capture index, the objects
/// picked for it, and the reasons the candidates that did not qualify
/// returned.
type ChosenSlot = (usize, Vec<Arc<Process>>, Vec<(usize, Reply)>);

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
    let vocabulary = LpcVocabulary::with_extras(ctx, objects, extras, remote_from);
    let mut resolver = Resolver::new(vocabulary, None);

    let mut failures: Vec<Failure> = Vec::new();
    let mut any_parse = false;
    for caps in rule.compiled.captures_of(rest) {
        if caps
            .iter()
            .any(|c| c.kind == CaptureKind::Words && c.text.is_empty())
        {
            continue; // STR is one or more words
        }
        any_parse = true;
        match attempt(ctx, actor, owner, rule, &caps, &all, &mut resolver).await? {
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
    report(ctx, actor, failure).await
}

/// The words of each object slot as typed, in slot order.
fn typed_words(caps: &[Capture]) -> Vec<LpcRef> {
    caps.iter()
        .filter(|c| c.kind.is_object())
        .map(|c| LpcRef::from(c.text.as_str()))
        .collect()
}

/// The slot values plus the typed words of the object slots, in that order:
/// what every handler family receives.
fn with_words(values: &[LpcRef], words: &[LpcRef]) -> Vec<LpcRef> {
    values
        .iter()
        .cloned()
        .chain(words.iter().cloned())
        .collect()
}

/// A failure whose silent fallback (when the master gives no message) is
/// `Refused`: from the actor's own handlers — `can_`, the all-filled
/// re-ask, or a missing `do_`.
fn refused(
    kind: Kind,
    object: Option<Arc<Process>>,
    arg: Arg,
    flag: bool,
    progress: usize,
) -> Failure {
    Failure {
        kind,
        object,
        arg,
        flag,
        progress,
        silent: Verdict::Refused,
    }
}

/// A failure whose silent fallback is `Unresolved`: from resolving or
/// disambiguating an object slot.
fn unresolved(
    kind: Kind,
    object: Option<Arc<Process>>,
    arg: Arg,
    flag: bool,
    progress: usize,
) -> Failure {
    Failure {
        kind,
        object,
        arg,
        flag,
        progress,
        silent: Verdict::Unresolved,
    }
}

/// One parse: `can_`, each object slot resolved and filtered, the final
/// re-ask, `do_`. `Err` is this parse's failure.
async fn attempt(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    owner: &Arc<Process>,
    rule: &ParserRule,
    caps: &[Capture],
    candidates: &[Candidate],
    resolver: &mut Resolver<LpcVocabulary<'_>>,
) -> Result<std::result::Result<(), Failure>> {
    let words = typed_words(caps);
    // Slot values: strings for WRD/STR, 0 until an object slot is chosen.
    let mut values: Vec<LpcRef> = caps
        .iter()
        .map(|c| {
            if c.kind.is_object() {
                LpcRef::from(0)
            } else {
                LpcRef::from(c.text.as_str())
            }
        })
        .collect();

    match call(
        ctx,
        actor,
        owner,
        Family::Can,
        rule,
        &with_words(&values, &words),
    )
    .await?
    {
        Reply::No => return Ok(Err(refused(Kind::Refused, None, Arg::None, false, 0))),
        Reply::Reason { text, .. } => {
            return Ok(Err(refused(
                Kind::Allocated,
                Some(owner.clone()),
                Arg::Text(text),
                false,
                0,
            )));
        }
        Reply::Yes | Reply::Absent => {}
    }

    let mut chosen: Vec<ChosenSlot> = Vec::new();
    let mut object_slot = 0usize;
    for (index, cap) in caps.iter().enumerate() {
        if !cap.kind.is_object() {
            continue;
        }
        let family = if object_slot == 0 {
            Family::Direct
        } else {
            Family::Indirect
        };
        let kind = match cap.kind {
            CaptureKind::Liv | CaptureKind::Living => ResolveKind::Living,
            _ => ResolveKind::Items,
        };
        let progress = object_slot;
        let Some(Resolved::Items {
            numeral,
            candidates: matched,
        }) = resolver.resolve(kind, &cap.text).await?
        else {
            let living_slot = matches!(cap.kind, CaptureKind::Liv | CaptureKind::Living);
            // This second resolve re-applies `parse_command_numeral`; only whether
            // the phrase names any non-living object matters here.
            let kind = if living_slot
                && resolver
                    .resolve(ResolveKind::Items, &cap.text)
                    .await?
                    .is_some()
            {
                Kind::NotLiving
            } else {
                Kind::ThereIsNo
            };
            let plural = cap.kind.is_many();
            return Ok(Err(unresolved(
                kind,
                None,
                Arg::Text(cap.text.clone()),
                plural,
                progress,
            )));
        };
        let mut qualified: Vec<Arc<Process>> = Vec::new();
        let mut reasons: Vec<(usize, Reply)> = Vec::new();
        let mut unreachable = false;
        for &candidate in &matched {
            let object = &candidates[candidate];
            // A neighbour an earlier handler destructed is skipped silently.
            if !object.object.is_live(ctx.txn()) {
                continue;
            }
            if !object.reachable {
                unreachable = true;
                continue;
            }
            // The candidate sits in its own slot for this call — a bare object
            // even for a many slot — then reverts to `0`.
            values[index] = LpcRef::from(Arc::downgrade(&object.object));
            let reply = call(
                ctx,
                actor,
                &object.object,
                family,
                rule,
                &with_words(&values, &words),
            )
            .await?;
            values[index] = LpcRef::from(0);
            match reply {
                Reply::Yes => qualified.push(object.object.clone()),
                Reply::No | Reply::Absent => {}
                reason @ Reply::Reason { .. } => reasons.push((candidate, reason)),
            }
        }
        if qualified.is_empty() {
            return Ok(Err(match best_reason(&reasons) {
                Some((candidate, text)) => unresolved(
                    Kind::Allocated,
                    Some(candidates[candidate].object.clone()),
                    Arg::Text(text),
                    false,
                    progress,
                ),
                None if unreachable => unresolved(
                    Kind::NotAccessible,
                    None,
                    Arg::Text(cap.text.clone()),
                    cap.kind.is_many(),
                    progress,
                ),
                None => unresolved(
                    Kind::ThereIsNo,
                    None,
                    Arg::Text(cap.text.clone()),
                    cap.kind.is_many(),
                    progress,
                ),
            }));
        }
        let picked: Vec<Arc<Process>> = if cap.kind.is_many() {
            match numeral {
                n if n > 0 => qualified.iter().take(n as usize).cloned().collect(),
                n if n < 0 => match qualified.get(n.unsigned_abs() as usize - 1) {
                    Some(one) => vec![one.clone()],
                    None => {
                        return Ok(Err(unresolved(
                            Kind::Ordinal,
                            None,
                            Arg::Count(qualified.len() as i64),
                            false,
                            progress,
                        )));
                    }
                },
                _ => qualified.clone(),
            }
        } else {
            match numeral {
                0 => {
                    return Ok(Err(unresolved(
                        Kind::BadMultiple,
                        None,
                        Arg::None,
                        false,
                        progress,
                    )));
                }
                n if n > 1 => {
                    return Ok(Err(unresolved(
                        Kind::BadMultiple,
                        None,
                        Arg::None,
                        false,
                        progress,
                    )));
                }
                n if n < 0 => match qualified.get(n.unsigned_abs() as usize - 1) {
                    Some(one) => vec![one.clone()],
                    None => {
                        return Ok(Err(unresolved(
                            Kind::Ordinal,
                            None,
                            Arg::Count(qualified.len() as i64),
                            false,
                            progress,
                        )));
                    }
                },
                _ if qualified.len() > 1 => {
                    return Ok(Err(unresolved(
                        Kind::Ambig,
                        None,
                        Arg::Objects(qualified),
                        false,
                        progress,
                    )));
                }
                _ => qualified.clone(),
            }
        };
        values[index] = if cap.kind.is_many() {
            mint_objects(ctx, &picked)
        } else {
            LpcRef::from(Arc::downgrade(&picked[0]))
        };
        chosen.push((index, picked, reasons));
        object_slot += 1;
    }

    // The all-filled re-ask: the chosen objects only, never the reasons.
    for (slot_number, (_, picked, _)) in chosen.iter().enumerate() {
        let family = if slot_number == 0 {
            Family::Direct
        } else {
            Family::Indirect
        };
        for object in picked {
            // A handler run earlier in this parse may have destructed a
            // neighbour; it no longer gets a say.
            if !object.is_live(ctx.txn()) {
                continue;
            }
            match call(
                ctx,
                actor,
                object,
                family,
                rule,
                &with_words(&values, &words),
            )
            .await?
            {
                Reply::No => {
                    return Ok(Err(refused(
                        Kind::Refused,
                        Some(object.clone()),
                        Arg::None,
                        false,
                        chosen.len(),
                    )));
                }
                Reply::Reason { text, .. } => {
                    return Ok(Err(refused(
                        Kind::Allocated,
                        Some(object.clone()),
                        Arg::Text(text),
                        false,
                        chosen.len(),
                    )));
                }
                Reply::Yes | Reply::Absent => {}
            }
        }
    }

    // Only `do_` sees a many slot as the mixed array of objects and reasons.
    let mut do_values = values.clone();
    for (index, picked, reasons) in &chosen {
        if rule.compiled.kinds[*index].is_many() {
            do_values[*index] = mint_mixed(ctx, picked, reasons);
        }
    }
    match call(
        ctx,
        actor,
        owner,
        Family::Do,
        rule,
        &with_words(&do_values, &words),
    )
    .await?
    {
        Reply::Absent => Ok(Err(refused(
            Kind::Refused,
            None,
            Arg::None,
            false,
            chosen.len(),
        ))),
        _ => Ok(Ok(())),
    }
}

/// `({ ob... })`: a many slot's value everywhere but `do_` (`can_`,
/// `direct_`/`indirect_`, the re-ask), and an ambiguous failure's
/// `Arg::Objects` reported to the master.
fn mint_objects(ctx: &TaskContext, objects: &[Arc<Process>]) -> LpcRef {
    let array: LpcArray = objects
        .iter()
        .map(|o| LpcRef::from(Arc::downgrade(o)))
        .collect();
    LpcRef::Array(ctx.txn().with(|t| t.mint_array(array)))
}

/// `({ ob... })` for a many slot's `do_` argument: the qualifying objects,
/// then each plain (non-`#`) reason from `reasons`, in candidate order.
fn mint_mixed(ctx: &TaskContext, picked: &[Arc<Process>], reasons: &[(usize, Reply)]) -> LpcRef {
    let array: LpcArray = picked
        .iter()
        .map(|o| LpcRef::from(Arc::downgrade(o)))
        .chain(reasons.iter().filter_map(|(_, reply)| match reply {
            Reply::Reason { text, soft: false } => Some(LpcRef::from(text.as_str())),
            _ => None,
        }))
        .collect();
    LpcRef::Array(ctx.txn().with(|t| t.mint_array(array)))
}

/// Ask the master to describe `failure`; a string is the verdict's
/// message, anything else falls back to `failure.silent`. `Kind::Refused`
/// never reaches the master — it carries no message.
async fn report(ctx: &TaskContext, actor: &Arc<Process>, failure: Failure) -> Result<Verdict> {
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
        Arg::Objects(objects) => mint_objects(ctx, &objects),
    };
    let object = failure
        .object
        .map_or(LpcRef::from(0), |o| LpcRef::from(Arc::downgrade(&o)));
    let args = [
        LpcRef::from(failure.kind as i64),
        object,
        arg,
        LpcRef::from(i64::from(failure.flag)),
    ];
    match apply_on(ctx, &master, actor, function, &args).await? {
        LpcRef::String(message) => Ok(Verdict::Message(message.to_string())),
        _ => Ok(silent),
    }
}
