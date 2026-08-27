//! The four handler families of a parser rule: their names, the arguments
//! they receive, how their replies are read, and which failure is reported.

use std::sync::Arc;

use lpc_rs_errors::Result;

use super::Verdict;
use crate::{
    command::{dispatch::apply_on, frontend::parser::ParserRule},
    interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, process::Process, task_context::TaskContext},
};

/// Which handler is being called.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Family {
    /// `can_<verb>_<slug>`: on the verb object, before any object slot is
    /// filled.
    Can,
    /// `direct_<verb>_<slug>`: on a direct-object candidate.
    Direct,
    /// `indirect_<verb>_<slug>`: on an indirect-object candidate.
    Indirect,
    /// `do_<verb>_<slug>`: on the verb object, once every slot is chosen.
    Do,
}

impl Family {
    /// The specific name (`can_look_at_obj`) and the generic fallback
    /// (`can_verb_rule`).
    pub(crate) fn names(self, rule: &ParserRule) -> (String, &'static str) {
        let (prefix, generic, slug) = match self {
            Family::Can => ("can", "can_verb_rule", &rule.can_slug),
            Family::Direct => ("direct", "direct_verb_rule", &rule.can_slug),
            Family::Indirect => ("indirect", "indirect_verb_rule", &rule.can_slug),
            Family::Do => ("do", "do_verb_rule", &rule.do_slug),
        };
        let specific = if slug.is_empty() {
            format!("{prefix}_{}", rule.verb)
        } else {
            format!("{prefix}_{}_{slug}", rule.verb)
        };
        (specific, generic)
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
    },
    /// Neither the specific nor the generic function exists.
    Absent,
}

impl From<&LpcRef> for Reply {
    /// Read a handler's return value.
    fn from(value: &LpcRef) -> Self {
        match value {
            LpcRef::Int(LpcInt(0)) => Reply::No,
            LpcRef::String(_) => {
                let text = value.as_str().unwrap_or_default();
                match text.strip_prefix('#') {
                    Some(rest) => Reply::Reason {
                        text: rest.to_owned(),
                        soft: true,
                    },
                    None => Reply::Reason {
                        text: text.to_owned(),
                        soft: false,
                    },
                }
            }
            _ => Reply::Yes,
        }
    }
}

/// Call `family`'s handler of `rule` on `target`: the specific name, else
/// the generic one with the verb and rule text prepended; `Absent` when
/// `target` defines neither.
pub(crate) async fn call(
    ctx: &TaskContext,
    actor: &Arc<Process>,
    target: &Arc<Process>,
    family: Family,
    rule: &ParserRule,
    args: &[LpcRef],
) -> Result<Reply> {
    let (specific, generic) = family.names(rule);
    let functions = &target.program.unmangled_functions;
    let (function, args): (_, Vec<LpcRef>) = if let Some(f) = functions.get(specific.as_str()) {
        (f.clone(), args.to_vec())
    } else if let Some(f) = functions.get(generic) {
        let mut prefixed = vec![
            LpcRef::from(rule.verb.as_str()),
            LpcRef::from(rule.rule.as_str()),
        ];
        prefixed.extend_from_slice(args);
        (f.clone(), prefixed)
    } else {
        return Ok(Reply::Absent);
    };
    let result = apply_on(ctx, target, actor, function, &args).await?;
    Ok(Reply::from(&result))
}

/// The reason to report when no candidate qualified: the earliest plain
/// one, else the earliest soft one, with the candidate it came from.
pub(crate) fn best_reason(reasons: &[(usize, Reply)]) -> Option<(usize, String)> {
    let pick = |want_soft: bool| {
        reasons.iter().find_map(|(candidate, reply)| match reply {
            Reply::Reason { text, soft } if *soft == want_soft => Some((*candidate, text.clone())),
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
    /// An ordinal named a position past the number of qualifiers.
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
#[derive(Clone, Debug)]
pub(crate) enum Arg {
    /// No argument (`Kind::Refused`, `Kind::BadMultiple`).
    None,
    /// A phrase or a handler's reason text.
    Text(String),
    /// The qualifier count an ordinal reached past.
    Count(i64),
    /// The qualifying objects of an ambiguous slot.
    Objects(Vec<Arc<Process>>),
}

impl PartialEq for Arg {
    /// `Objects` compares element-wise by pointer identity; `Process` has
    /// no `PartialEq` of its own.
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Arg::None, Arg::None) => true,
            (Arg::Text(a), Arg::Text(b)) => a == b,
            (Arg::Count(a), Arg::Count(b)) => a == b,
            (Arg::Objects(a), Arg::Objects(b)) => {
                a.len() == b.len() && a.iter().zip(b).all(|(x, y)| Arc::ptr_eq(x, y))
            }
            _ => false,
        }
    }
}

/// One parse's failure, with how far it got.
#[derive(Clone, Debug)]
pub(crate) struct Failure {
    pub(crate) kind: Kind,
    pub(crate) object: Option<Arc<Process>>,
    pub(crate) arg: Arg,
    pub(crate) flag: bool,
    /// Object slots chosen before failing.
    pub(crate) progress: usize,
    /// The verdict when the master's `parser_error_message` gives no
    /// string: `Refused` from the actor's own handlers (`can_`, the
    /// re-ask, a missing `do_`), `Unresolved` from resolving or
    /// disambiguating an object slot.
    pub(crate) silent: Verdict,
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
    fn names_are_verb_slug_and_the_generic_fallback() {
        let r = rule("look", "at OBS");
        assert_eq!(
            Family::Can.names(&r),
            ("can_look_at_obj".to_owned(), "can_verb_rule")
        );
        assert_eq!(
            Family::Direct.names(&r),
            ("direct_look_at_obj".to_owned(), "direct_verb_rule")
        );
        assert_eq!(
            Family::Do.names(&r),
            ("do_look_at_obs".to_owned(), "do_verb_rule")
        );
        let bare = rule("look", "");
        assert_eq!(Family::Can.names(&bare).0, "can_look");
    }

    #[test]
    fn a_reply_is_read_from_the_lpc_value() {
        assert_eq!(Reply::from(&LpcRef::from(1)), Reply::Yes);
        assert_eq!(Reply::from(&LpcRef::from(0)), Reply::No);
        assert_eq!(
            Reply::from(&LpcRef::from("no way")),
            Reply::Reason {
                text: "no way".into(),
                soft: false
            }
        );
        assert_eq!(
            Reply::from(&LpcRef::from("#not me")),
            Reply::Reason {
                text: "not me".into(),
                soft: true
            }
        );
    }

    #[test]
    fn the_best_reason_prefers_plain_over_soft_and_earliest() {
        let reasons = vec![
            (
                0,
                Reply::Reason {
                    text: "a".into(),
                    soft: true,
                },
            ),
            (
                1,
                Reply::Reason {
                    text: "b".into(),
                    soft: false,
                },
            ),
            (
                2,
                Reply::Reason {
                    text: "c".into(),
                    soft: false,
                },
            ),
        ];
        assert_eq!(best_reason(&reasons), Some((1, "b".to_owned())));
        let soft_only = vec![(
            3,
            Reply::Reason {
                text: "z".into(),
                soft: true,
            },
        )];
        assert_eq!(best_reason(&soft_only), Some((3, "z".to_owned())));
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
            silent: Verdict::Unresolved,
        };
        let b = Failure {
            kind: Kind::Refused,
            object: None,
            arg: Arg::None,
            flag: false,
            progress: 2,
            silent: Verdict::Refused,
        };
        let c = Failure {
            kind: Kind::ThereIsNo,
            object: None,
            arg: Arg::Text("y".into()),
            flag: false,
            progress: 2,
            silent: Verdict::Unresolved,
        };
        assert_eq!(
            furthest(vec![a.clone(), b.clone(), c]).map(|f| (f.arg, f.silent)),
            Some((Arg::None, Verdict::Refused))
        );
        assert_eq!(furthest(vec![a.clone()]).map(|f| f.progress), Some(1));
        assert!(furthest(vec![]).is_none());
    }
}
