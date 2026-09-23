//! The four handler families of a parser rule: their names, the arguments
//! they receive, how their replies are read, and which failure is reported.

use std::sync::Arc;

use lpc_rs_errors::Result;

use super::{RejectionCode, attempt::Target};
use crate::{
    command::frontend::parser::{HandlerArg, ParserRule},
    interpreter::{
        apply::{apply_on, as_actor, diagnostics},
        lpc_int::LpcInt,
        lpc_ref::LpcRef,
        process::Process,
        task_context::TaskContext,
    },
};

/// Which handler is being called.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Family {
    /// Permission before any object slot is filled, on the actor then owner.
    Can,
    /// Permission on a direct-object candidate, then the owner.
    Direct,
    /// Permission on an indirect-object candidate, then the owner.
    Indirect,
    /// `do_<verb>_<slug>`: on the verb object, once every slot is chosen.
    Do,
}

/// The name and argument layout selected on one target.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Tier {
    Exact,
    Words,
    Shape,
    Rule,
}

impl Family {
    /// Names in lookup order, retaining the earliest layout on a collision.
    fn names(self, rule: &ParserRule) -> Vec<(String, Tier)> {
        let prefix = match self {
            Self::Can => "can",
            Self::Direct => "direct",
            Self::Indirect => "indirect",
            Self::Do => "do",
        };
        let (slug, words) = if self == Self::Do {
            (rule.do_slug, rule.do_word_slug)
        } else {
            (rule.can_slug, rule.can_word_slug)
        };
        let name = |verb: &str, slug: &str| {
            if slug.is_empty() {
                format!("{prefix}_{verb}")
            } else {
                format!("{prefix}_{verb}_{slug}")
            }
        };
        let mut names = Vec::with_capacity(4);
        for entry in [
            (name(rule.verb.as_str(), slug.as_str()), Tier::Exact),
            (name(rule.verb.as_str(), words.as_str()), Tier::Words),
            (name("verb", words.as_str()), Tier::Shape),
            (format!("{prefix}_verb_rule"), Tier::Rule),
        ] {
            if !names.iter().any(|(name, _)| name == &entry.0) {
                names.push(entry);
            }
        }
        names
    }
}

impl Tier {
    /// Capture values precede the source-text tail supplied by the protocol.
    fn arguments(self, rule: &ParserRule, args: &[LpcRef], typed_verb: &str) -> Vec<LpcRef> {
        let (captures, words) = args.split_at(rule.compiled.kinds.len());
        let generic = matches!(self, Self::Shape | Self::Rule);
        let mut result = Vec::new();
        if generic {
            result.push(LpcRef::from(rule.verb.as_str()));
        }
        if self == Self::Rule {
            result.push(LpcRef::from(rule.rule.as_str()));
        }
        if matches!(self, Self::Words | Self::Shape) {
            result.extend(rule.word_args.iter().map(|arg| match arg {
                HandlerArg::Capture(index) => captures[*index].clone(),
                HandlerArg::Literal(word) => LpcRef::from(word.as_str()),
            }));
        } else {
            result.extend_from_slice(captures);
        }
        if generic {
            result.push(LpcRef::from(typed_verb));
        }
        result.extend_from_slice(words);
        result
    }
}

/// What a handler answered.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Reply {
    /// `1`, or any value that is neither `0` nor a string.
    Yes,
    /// `0`.
    No,
    /// A string: a reason; `#` stripped and remembered as soft.
    Reason {
        /// The text, without a leading `#`.
        text: String,
        /// Whether the reason was `#`-prefixed.
        soft: bool,
        /// The object whose handler returned this reason.
        object: Target,
    },
    /// No handler exists on any eligible target.
    Absent,
}

impl Reply {
    /// Read a handler's return value.
    pub(crate) fn from_value(value: &LpcRef, object: Target) -> Self {
        match value {
            LpcRef::Nil | LpcRef::Int(LpcInt(0)) => Reply::No,
            LpcRef::String(_) => {
                let text = value.as_str().unwrap_or_default();
                match text.strip_prefix('#') {
                    Some(rest) => Reply::Reason {
                        text: rest.to_owned(),
                        soft: true,
                        object,
                    },
                    None => Reply::Reason {
                        text: text.to_owned(),
                        soft: false,
                        object,
                    },
                }
            }
            _ => Reply::Yes,
        }
    }
}

/// Call the first defined tier on this target; only absence permits fallback.
pub(crate) async fn call(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    target: &Arc<Process>,
    family: Family,
    rule: &ParserRule,
    args: &[LpcRef],
) -> Result<Option<LpcRef>> {
    let program = target.program(ctx.txn());
    for (name, tier) in family.names(rule) {
        let Some(function) = program.unmangled_functions.get(name.as_str()) else {
            diagnostics::missing(&name, Some(target));
            continue;
        };
        let typed = ctx.with_command(|state| {
            state
                .map(|state| state.verb_typed.clone())
                .unwrap_or_default()
        });
        let args = tier.arguments(rule, args, &typed);
        return apply_on(
            ctx,
            as_actor(ctx, actor),
            target,
            actor,
            function.clone(),
            &args,
        )
        .await
        .map(Some);
    }
    Ok(None)
}

/// The reason to report when no candidate qualified: the earliest plain
/// one, else the earliest soft one, with the object that answered.
pub(crate) fn best_reason(reasons: &[(usize, Reply)]) -> Option<(Target, String)> {
    let pick = |want_soft: bool| {
        reasons.iter().find_map(|(_, reply)| match reply {
            Reply::Reason { text, soft, object } if *soft == want_soft => {
                Some((*object, text.clone()))
            }
            _ => None,
        })
    };
    pick(false).or_else(|| pick(true))
}

/// The failure kinds the master's `parser_error_message` receives, with
/// MudOS's numbers; `Refused` is not a kind — it carries no message.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    /// A living slot's phrase matched only non-livings.
    NotLiving = 2,
    /// A match exists but nothing on its path is `inventory_accessible()`.
    NotAccessible = 3,
    /// Several qualifiers survived filtering with no ordinal to choose one.
    Ambig = 4,
    /// An ordinal named a position past the available matches.
    Ordinal = 5,
    /// A handler's own string reason, `#` stripped.
    Allocated = 6,
    /// No candidate's vocabulary named the phrase at all.
    ThereIsNo = 7,
    /// `all` or a plural on a single-object slot.
    BadMultiple = 8,
    /// A handler said `0`; reported as `-2`, never to the master.
    Refused = 0,
}

/// The `arg` of a failure.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Arg {
    /// No argument (`Kind::Refused`, `Kind::BadMultiple`).
    None,
    /// A phrase or a handler's reason text.
    Text(String),
    /// The available count an ordinal reached past.
    Count(i64),
    /// The qualifying candidates of an ambiguous slot.
    Objects(Vec<usize>),
}

/// One parse's failure, with how far it got.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Failure {
    /// What went wrong, as the master's `parser_error_message` numbers it.
    pub(crate) kind: Kind,
    /// The target whose reply caused the failure, when one did.
    pub(crate) object: Option<Target>,
    /// The message's third argument: a phrase, a reason, a count, or the
    /// qualifying candidates.
    pub(crate) arg: Arg,
    /// The message's fourth argument: whether the phrase named a many slot.
    pub(crate) flag: bool,
    /// Object slots resolved before failing.
    pub(crate) progress: usize,
    /// The verdict when the master's `parser_error_message` gives no
    /// string: `Refused` from the actor's own handlers (`can_`, the
    /// pair checks, the re-ask, a missing `do_`), `Unresolved` from resolving or
    /// disambiguating an object slot.
    pub(crate) silent: RejectionCode,
}

/// The failure to report: the furthest, ties to the earliest.
pub(crate) fn furthest(failures: Vec<Failure>) -> Option<Failure> {
    failures
        .into_iter()
        .enumerate()
        .max_by(|(ia, a), (ib, b)| a.progress.cmp(&b.progress).then(ib.cmp(ia)))
        .map(|(_, f)| f)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(verb: &str, text: &str) -> ParserRule {
        crate::command::frontend::parser::compile(verb, text).unwrap()
    }

    #[test]
    fn names_preserve_plural_actions_and_literal_word_tiers() {
        let r = rule("look", "at OBS");
        for (family, prefix, token) in [
            (Family::Can, "can", "obj"),
            (Family::Direct, "direct", "obj"),
            (Family::Indirect, "indirect", "obj"),
            (Family::Do, "do", "obs"),
        ] {
            assert_eq!(
                family.names(&r),
                vec![
                    (format!("{prefix}_look_at_{token}"), Tier::Exact),
                    (format!("{prefix}_look_word_{token}"), Tier::Words),
                    (format!("{prefix}_verb_word_{token}"), Tier::Shape),
                    (format!("{prefix}_verb_rule"), Tier::Rule),
                ]
            );
        }
    }

    #[test]
    fn duplicate_names_keep_the_first_layout() {
        assert_eq!(
            Family::Do.names(&rule("verb", "word STR")),
            vec![
                ("do_verb_word_str".into(), Tier::Exact),
                ("do_verb_rule".into(), Tier::Rule),
            ]
        );
        assert_eq!(
            Family::Can.names(&rule("look", "")),
            vec![
                ("can_look".into(), Tier::Exact),
                ("can_verb".into(), Tier::Shape),
                ("can_verb_rule".into(), Tier::Rule),
            ]
        );
    }

    #[test]
    fn a_reply_retains_the_object_that_answered() {
        assert_eq!(
            Reply::from_value(&LpcRef::from(1), Target::Owner),
            Reply::Yes
        );
        assert_eq!(
            Reply::from_value(&LpcRef::from(0), Target::Owner),
            Reply::No
        );
        assert_eq!(Reply::from_value(&LpcRef::Nil, Target::Actor), Reply::No);
        for (value, text, soft, object) in [
            ("no way", "no way", false, Target::Owner),
            ("#not me", "not me", true, Target::Actor),
        ] {
            assert_eq!(
                Reply::from_value(&LpcRef::from(value), object),
                Reply::Reason {
                    text: text.into(),
                    soft,
                    object,
                }
            );
        }
    }

    #[test]
    fn the_best_reason_prefers_plain_over_soft_and_earliest() {
        let reasons = vec![
            (
                0,
                Reply::from_value(&LpcRef::from("#a"), Target::Candidate(0)),
            ),
            (1, Reply::from_value(&LpcRef::from("b"), Target::Owner)),
            (
                2,
                Reply::from_value(&LpcRef::from("c"), Target::Candidate(2)),
            ),
        ];
        assert_eq!(best_reason(&reasons), Some((Target::Owner, "b".to_owned())));
        assert_eq!(
            best_reason(&reasons[..1]),
            Some((Target::Candidate(0), "a".to_owned()))
        );
        assert_eq!(best_reason(&[]), None);
    }

    #[test]
    fn the_furthest_failure_wins_ties_to_the_earliest() {
        let a = Failure {
            kind: Kind::ThereIsNo,
            object: None,
            arg: Arg::Text("x".into()),
            flag: false,
            progress: 1,
            silent: RejectionCode::Unresolved,
        };
        let b = Failure {
            kind: Kind::Refused,
            object: None,
            arg: Arg::None,
            flag: false,
            progress: 2,
            silent: RejectionCode::Refused,
        };
        let c = Failure {
            kind: Kind::ThereIsNo,
            object: None,
            arg: Arg::Text("y".into()),
            flag: false,
            progress: 2,
            silent: RejectionCode::Unresolved,
        };
        assert_eq!(
            furthest(vec![a.clone(), b.clone(), c]).map(|f| (f.arg, f.silent)),
            Some((Arg::None, RejectionCode::Refused))
        );
        assert_eq!(furthest(vec![a.clone()]).map(|f| f.progress), Some(1));
        assert!(furthest(vec![]).is_none());
    }
}
