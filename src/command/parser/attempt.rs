//! The protocol for one parse — `can_`, each object slot resolved and
//! filtered, the final re-ask, `do_` — decided without the world: all it
//! needs of the world it asks for through [`Ask`].

use lpc_rs_errors::Result;

use super::{
    Verdict,
    handlers::{Arg, Failure, Family, Kind, Reply, best_reason},
};
use crate::command::{
    frontend::native::{Capture, CaptureKind},
    resolve::{Kind as ResolveKind, Resolved},
};

/// Whom a handler call goes to.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Target {
    /// The rule's owner: `can_` and `do_`.
    Owner,
    /// A candidate by index: `direct_`/`indirect_` and the re-ask.
    Candidate(usize),
}

/// One handler argument as the core lays it out; the adapter makes the value.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Slot {
    /// A `WRD`/`STR` capture, or the typed words of an object slot.
    Text(String),
    /// An object slot not yet chosen: `0`.
    Empty,
    /// One object: a single slot's choice, or the candidate being asked
    /// about the slot it is in — even a many slot.
    Object(usize),
    /// A many slot's choices, and the capture index of the slot they fill:
    /// one array per slot per parse, so the re-ask sees the same one.
    Objects(usize, Vec<usize>),
    /// A many slot as `do_` sees it: the choices, then the plain reasons.
    Mixed(Vec<usize>, Vec<String>),
}

/// What one parse needs from the world. A `candidate` is an index into the
/// adapter's own scope, as `resolve` reports it in `Resolved::Items`.
pub(crate) trait Ask {
    /// Called first in [`attempt`]: anything the adapter holds for one parse
    /// — `Lpc`'s minted many-slot arrays — is dropped here.
    fn begin_parse(&mut self) {}
    /// `family`'s handler on `target` with `args`.
    async fn call(&mut self, family: Family, target: Target, args: &[Slot]) -> Result<Reply>;
    /// The candidates `phrase` names for a slot of `kind`.
    async fn resolve(&mut self, kind: ResolveKind, phrase: &str) -> Result<Option<Resolved>>;
    /// Whether `candidate` is not destructed.
    fn is_live(&self, candidate: usize) -> bool;
    /// Whether the path to `candidate` is accessible.
    fn reachable(&self, candidate: usize) -> bool;
}

/// How a slot's qualifiers become its choice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Pick {
    /// The first `n`.
    First(usize),
    /// The one at `index`.
    Nth(usize),
    /// All of them.
    All,
}

/// One object slot chosen in [`attempt`]: its capture index, the candidates
/// picked for it, and the reasons the candidates that did not qualify
/// returned.
type ChosenSlot = (usize, Vec<usize>, Vec<(usize, Reply)>);

/// The selection table: `numeral > 0` a count, `< 0` an ordinal, `0` all or
/// a plural; a single slot takes exactly one. `Err` is the failure's kind,
/// `Ordinal` and `Ambig` carrying what the caller reports.
pub(crate) fn choose(
    numeral: i64,
    many: bool,
    qualified: usize,
) -> std::result::Result<Pick, Kind> {
    let nth = |n: i64| {
        let index = n.unsigned_abs() as usize - 1;
        if index < qualified {
            Ok(Pick::Nth(index))
        } else {
            Err(Kind::Ordinal)
        }
    };
    match (many, numeral) {
        (true, n) if n > 0 => Ok(Pick::First(n as usize)),
        (true, n) if n < 0 => nth(n),
        (true, _) => Ok(Pick::All),
        (false, 0) => Err(Kind::BadMultiple),
        (false, n) if n > 1 => Err(Kind::BadMultiple),
        (false, n) if n < 0 => nth(n),
        (false, _) if qualified > 1 => Err(Kind::Ambig),
        (false, _) => Ok(Pick::All),
    }
}

/// The slot values plus the typed words of the object slots, in that order:
/// what every handler family receives.
fn with_words(values: &[Slot], words: &[Slot]) -> Vec<Slot> {
    values.iter().chain(words).cloned().collect()
}

/// The plain (non-`#`) reasons of `reasons`, in candidate order.
fn plain_reasons(reasons: &[(usize, Reply)]) -> Vec<String> {
    reasons
        .iter()
        .filter_map(|(_, reply)| match reply {
            Reply::Reason { text, soft: false } => Some(text.clone()),
            _ => None,
        })
        .collect()
}

/// A failure whose silent fallback (when the master gives no message) is
/// `Refused`: from the actor's own handlers — `can_`, the all-filled
/// re-ask, or a missing `do_`.
fn refused(kind: Kind, object: Option<Target>, arg: Arg, flag: bool, progress: usize) -> Failure {
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
    object: Option<Target>,
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
pub(crate) async fn attempt<A: Ask>(
    ask: &mut A,
    caps: &[Capture],
) -> Result<std::result::Result<(), Failure>> {
    ask.begin_parse();
    let words: Vec<Slot> = caps
        .iter()
        .filter(|c| c.kind.is_object())
        .map(|c| Slot::Text(c.text.clone()))
        .collect();
    let mut values: Vec<Slot> = caps
        .iter()
        .map(|c| {
            if c.kind.is_object() {
                Slot::Empty
            } else {
                Slot::Text(c.text.clone())
            }
        })
        .collect();

    match ask
        .call(Family::Can, Target::Owner, &with_words(&values, &words))
        .await?
    {
        Reply::No => return Ok(Err(refused(Kind::Refused, None, Arg::None, false, 0))),
        Reply::Reason { text, .. } => {
            return Ok(Err(refused(
                Kind::Allocated,
                Some(Target::Owner),
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
        let (numeral, matched) = match ask.resolve(kind, &cap.text).await? {
            Some(Resolved::Items {
                numeral,
                candidates,
            }) if !candidates.is_empty() => (numeral, candidates),
            found => {
                let kind = if found.is_some() {
                    Kind::NotLiving
                } else {
                    Kind::ThereIsNo
                };
                return Ok(Err(unresolved(
                    kind,
                    None,
                    Arg::Text(cap.text.clone()),
                    cap.kind.is_many(),
                    progress,
                )));
            }
        };
        let mut qualified: Vec<usize> = Vec::new();
        let mut reasons: Vec<(usize, Reply)> = Vec::new();
        let mut unreachable = false;
        for &candidate in &matched {
            // A neighbour an earlier handler destructed is skipped silently.
            if !ask.is_live(candidate) {
                continue;
            }
            if !ask.reachable(candidate) {
                unreachable = true;
                continue;
            }
            // The slot reverts after the ask: no other candidate sees this one.
            values[index] = Slot::Object(candidate);
            let reply = ask
                .call(
                    family,
                    Target::Candidate(candidate),
                    &with_words(&values, &words),
                )
                .await?;
            values[index] = Slot::Empty;
            match reply {
                Reply::Yes => qualified.push(candidate),
                Reply::No | Reply::Absent => {}
                reason @ Reply::Reason { .. } => reasons.push((candidate, reason)),
            }
        }
        if qualified.is_empty() {
            return Ok(Err(match best_reason(&reasons) {
                Some((candidate, text)) => unresolved(
                    Kind::Allocated,
                    Some(Target::Candidate(candidate)),
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
        let picked: Vec<usize> = match choose(numeral, cap.kind.is_many(), qualified.len()) {
            Ok(Pick::First(n)) => qualified.iter().take(n).copied().collect(),
            Ok(Pick::Nth(i)) => vec![qualified[i]],
            Ok(Pick::All) => qualified.clone(),
            Err(kind) => {
                return Ok(Err(unresolved(
                    kind,
                    None,
                    match kind {
                        Kind::Ordinal => Arg::Count(qualified.len() as i64),
                        Kind::Ambig => Arg::Objects(qualified),
                        _ => Arg::None,
                    },
                    false,
                    progress,
                )));
            }
        };
        values[index] = if cap.kind.is_many() {
            Slot::Objects(index, picked.clone())
        } else {
            Slot::Object(picked[0])
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
        for &candidate in picked {
            // A handler run earlier in this parse may have destructed a
            // neighbour; it no longer gets a say.
            if !ask.is_live(candidate) {
                continue;
            }
            match ask
                .call(
                    family,
                    Target::Candidate(candidate),
                    &with_words(&values, &words),
                )
                .await?
            {
                Reply::No => {
                    return Ok(Err(refused(
                        Kind::Refused,
                        Some(Target::Candidate(candidate)),
                        Arg::None,
                        false,
                        chosen.len(),
                    )));
                }
                Reply::Reason { text, .. } => {
                    return Ok(Err(refused(
                        Kind::Allocated,
                        Some(Target::Candidate(candidate)),
                        Arg::Text(text),
                        false,
                        chosen.len(),
                    )));
                }
                Reply::Yes | Reply::Absent => {}
            }
        }
    }

    let mut do_values = values.clone();
    for (index, picked, reasons) in &chosen {
        if caps[*index].kind.is_many() {
            do_values[*index] = Slot::Mixed(picked.clone(), plain_reasons(reasons));
        }
    }
    match ask
        .call(Family::Do, Target::Owner, &with_words(&do_values, &words))
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

#[cfg(test)]
mod tests {
    use super::*;

    /// An `Ask` answering from a script: replies in call order per
    /// (family, target), phrases from a table, liveness and reach from
    /// vectors; every call is recorded for the assertions.
    struct Recorded {
        replies: Vec<((Family, Target), Reply)>,
        phrases: Vec<(&'static str, ResolveKind, Option<Resolved>)>,
        live: Vec<bool>,
        reachable: Vec<bool>,
        calls: Vec<(Family, Target, Vec<Slot>)>,
    }

    impl Ask for Recorded {
        async fn call(&mut self, family: Family, target: Target, args: &[Slot]) -> Result<Reply> {
            self.calls.push((family, target, args.to_vec()));
            let at = self
                .replies
                .iter()
                .position(|(key, _)| *key == (family, target));
            Ok(match at {
                Some(i) => self.replies.remove(i).1,
                None => Reply::Absent,
            })
        }
        async fn resolve(&mut self, kind: ResolveKind, phrase: &str) -> Result<Option<Resolved>> {
            Ok(self
                .phrases
                .iter()
                .find(|(p, k, _)| *p == phrase && *k == kind)
                .and_then(|(_, _, r)| r.clone()))
        }
        fn is_live(&self, c: usize) -> bool {
            self.live[c]
        }
        fn reachable(&self, c: usize) -> bool {
            self.reachable[c]
        }
    }

    /// A `Recorded` over two candidates, both live and reachable.
    fn recorded(
        replies: Vec<((Family, Target), Reply)>,
        phrases: Vec<(&'static str, ResolveKind, Option<Resolved>)>,
    ) -> Recorded {
        Recorded {
            replies,
            phrases,
            live: vec![true, true],
            reachable: vec![true, true],
            calls: Vec::new(),
        }
    }

    /// The first parse of `line` against `rule_text`.
    fn caps(rule_text: &str, line: &str) -> Vec<Capture> {
        let rule = crate::command::frontend::parser::compile("verb", rule_text).unwrap();
        rule.compiled.captures_of(line).next().unwrap()
    }

    fn yes(family: Family, target: Target) -> ((Family, Target), Reply) {
        ((family, target), Reply::Yes)
    }

    fn no(family: Family, target: Target) -> ((Family, Target), Reply) {
        ((family, target), Reply::No)
    }

    fn reason(family: Family, target: Target, text: &str) -> ((Family, Target), Reply) {
        let reply = match text.strip_prefix('#') {
            Some(rest) => Reply::Reason {
                text: rest.to_owned(),
                soft: true,
            },
            None => Reply::Reason {
                text: text.to_owned(),
                soft: false,
            },
        };
        ((family, target), reply)
    }

    fn items(numeral: i64, candidates: Vec<usize>) -> Option<Resolved> {
        Some(Resolved::Items {
            numeral,
            candidates,
        })
    }

    fn text(word: &str) -> Slot {
        Slot::Text(word.to_owned())
    }

    async fn failure(ask: &mut Recorded, caps: &[Capture]) -> Failure {
        attempt(ask, caps).await.unwrap().unwrap_err()
    }

    #[test]
    fn the_selection_table() {
        // (numeral, many, qualified) → pick
        let table = [
            ((2, true, 3), Ok(Pick::First(2))),
            ((5, true, 3), Ok(Pick::First(5))),
            ((-2, true, 3), Ok(Pick::Nth(1))),
            ((-4, true, 3), Err(Kind::Ordinal)),
            ((0, true, 3), Ok(Pick::All)),
            ((1, true, 1), Ok(Pick::First(1))),
            ((0, false, 1), Err(Kind::BadMultiple)),
            ((2, false, 1), Err(Kind::BadMultiple)),
            ((-1, false, 2), Ok(Pick::Nth(0))),
            ((-3, false, 2), Err(Kind::Ordinal)),
            ((1, false, 2), Err(Kind::Ambig)),
            ((1, false, 1), Ok(Pick::All)),
        ];
        for ((numeral, many, qualified), expected) in table {
            assert_eq!(
                choose(numeral, many, qualified),
                expected,
                "{numeral} {many} {qualified}"
            );
        }
    }

    #[tokio::test]
    async fn can_is_asked_first_with_empty_object_slots_then_the_words() {
        let caps = caps("OBS in OBJ", "swords in bag");
        let mut ask = recorded(vec![], vec![]);
        attempt(&mut ask, &caps).await.unwrap().unwrap_err();
        assert_eq!(
            ask.calls[0],
            (
                Family::Can,
                Target::Owner,
                vec![Slot::Empty, Slot::Empty, text("swords"), text("bag")]
            )
        );
    }

    #[tokio::test]
    async fn can_refusing_fails_at_progress_zero() {
        let caps = caps("OBJ", "sword");
        let mut ask = recorded(vec![no(Family::Can, Target::Owner)], vec![]);
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::Refused);
        assert_eq!(failed.progress, 0);
        assert_eq!(failed.silent, Verdict::Refused);
        assert_eq!(ask.calls.len(), 1);
    }

    #[tokio::test]
    async fn a_candidate_is_asked_with_itself_in_its_slot_even_for_a_many_slot() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![],
            vec![("swords", ResolveKind::Items, items(0, vec![0, 1]))],
        );
        attempt(&mut ask, &caps).await.unwrap().unwrap_err();
        let asked: Vec<&(Family, Target, Vec<Slot>)> = ask
            .calls
            .iter()
            .filter(|(family, _, _)| *family == Family::Direct)
            .collect();
        assert_eq!(
            asked,
            vec![
                &(
                    Family::Direct,
                    Target::Candidate(0),
                    vec![Slot::Object(0), text("swords")]
                ),
                &(
                    Family::Direct,
                    Target::Candidate(1),
                    vec![Slot::Object(1), text("swords")]
                ),
            ]
        );
    }

    #[tokio::test]
    async fn the_re_ask_sees_the_chosen_objects_not_the_reasons() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![
                yes(Family::Direct, Target::Candidate(0)),
                reason(Family::Direct, Target::Candidate(1), "too heavy"),
            ],
            vec![("swords", ResolveKind::Items, items(0, vec![0, 1]))],
        );
        attempt(&mut ask, &caps).await.unwrap().unwrap_err();
        let on_zero: Vec<&Vec<Slot>> = ask
            .calls
            .iter()
            .filter(|(family, target, _)| {
                *family == Family::Direct && *target == Target::Candidate(0)
            })
            .map(|(_, _, args)| args)
            .collect();
        assert_eq!(on_zero.len(), 2, "{on_zero:?}");
        assert_eq!(
            on_zero[1],
            &vec![Slot::Objects(0, vec![0]), text("swords")],
            "{on_zero:?}"
        );
        let filled: Vec<usize> = ask
            .calls
            .iter()
            .flat_map(|(_, _, args)| args)
            .filter_map(|slot| match slot {
                Slot::Objects(index, _) => Some(*index),
                _ => None,
            })
            .collect();
        assert_eq!(filled, vec![0], "{:?}", ask.calls);
    }

    #[tokio::test]
    async fn only_do_sees_the_mixed_array() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![
                yes(Family::Direct, Target::Candidate(0)),
                reason(Family::Direct, Target::Candidate(1), "too heavy"),
                yes(Family::Do, Target::Owner),
            ],
            vec![("swords", ResolveKind::Items, items(0, vec![0, 1]))],
        );
        assert_eq!(attempt(&mut ask, &caps).await.unwrap(), Ok(()));
        let done = ask
            .calls
            .iter()
            .find(|(family, _, _)| *family == Family::Do)
            .unwrap();
        assert_eq!(
            done.2,
            vec![
                Slot::Mixed(vec![0], vec!["too heavy".to_owned()]),
                text("swords")
            ]
        );
    }

    #[tokio::test]
    async fn a_soft_reason_is_left_out_of_the_mixed_array() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![
                yes(Family::Direct, Target::Candidate(0)),
                reason(Family::Direct, Target::Candidate(1), "#not now"),
                yes(Family::Do, Target::Owner),
            ],
            vec![("swords", ResolveKind::Items, items(0, vec![0, 1]))],
        );
        assert_eq!(attempt(&mut ask, &caps).await.unwrap(), Ok(()));
        let done = ask
            .calls
            .iter()
            .find(|(family, _, _)| *family == Family::Do)
            .unwrap();
        assert_eq!(done.2, vec![Slot::Mixed(vec![0], vec![]), text("swords")]);
    }

    #[tokio::test]
    async fn no_qualifier_reports_the_plain_reason_over_the_soft_one() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![
                reason(Family::Direct, Target::Candidate(0), "#soft"),
                reason(Family::Direct, Target::Candidate(1), "plain"),
            ],
            vec![("swords", ResolveKind::Items, items(0, vec![0, 1]))],
        );
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::Allocated);
        assert_eq!(failed.object, Some(Target::Candidate(1)));
        assert_eq!(failed.arg, Arg::Text("plain".to_owned()));
        assert_eq!(failed.silent, Verdict::Unresolved);
    }

    #[tokio::test]
    async fn an_unreachable_match_is_not_accessible() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![],
            vec![("swords", ResolveKind::Items, items(0, vec![0]))],
        );
        ask.reachable = vec![false, true];
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::NotAccessible);
        assert_eq!(failed.arg, Arg::Text("swords".to_owned()));
        assert_eq!(failed.flag, caps[0].kind.is_many());
    }

    #[tokio::test]
    async fn a_living_slot_naming_only_things_is_not_living() {
        let caps = caps("LIV", "bob");
        let mut ask = recorded(vec![], vec![("bob", ResolveKind::Living, items(0, vec![]))]);
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::NotLiving);
        assert_eq!(failed.arg, Arg::Text("bob".to_owned()));
    }

    #[tokio::test]
    async fn a_phrase_naming_nothing_is_there_is_no() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(vec![], vec![]);
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::ThereIsNo);
        assert_eq!(failed.arg, Arg::Text("swords".to_owned()));
        assert_eq!(failed.flag, caps[0].kind.is_many());
    }

    #[tokio::test]
    async fn a_dead_candidate_is_skipped_silently() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![
                yes(Family::Direct, Target::Candidate(1)),
                yes(Family::Do, Target::Owner),
            ],
            vec![("swords", ResolveKind::Items, items(0, vec![0, 1]))],
        );
        ask.live = vec![false, true];
        assert_eq!(attempt(&mut ask, &caps).await.unwrap(), Ok(()));
        assert!(
            !ask.calls
                .iter()
                .any(|(_, target, _)| *target == Target::Candidate(0)),
            "{:?}",
            ask.calls
        );
    }

    #[tokio::test]
    async fn a_missing_do_is_refused_with_the_slots_chosen() {
        let caps = caps("OBJ", "sword");
        let mut ask = recorded(
            vec![yes(Family::Direct, Target::Candidate(0))],
            vec![("sword", ResolveKind::Items, items(1, vec![0]))],
        );
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::Refused);
        assert_eq!(failed.progress, 1);
        assert_eq!(failed.silent, Verdict::Refused);
        let done = ask
            .calls
            .iter()
            .find(|(family, _, _)| *family == Family::Do)
            .unwrap();
        assert_eq!(done.1, Target::Owner);
        assert_eq!(done.2, vec![Slot::Object(0), text("sword")]);
    }

    #[tokio::test]
    async fn an_ordinal_past_the_qualifiers_reports_their_count() {
        let caps = caps("OBS", "swords");
        let mut ask = recorded(
            vec![
                yes(Family::Direct, Target::Candidate(0)),
                yes(Family::Direct, Target::Candidate(1)),
            ],
            vec![("swords", ResolveKind::Items, items(-3, vec![0, 1]))],
        );
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::Ordinal);
        assert_eq!(failed.arg, Arg::Count(2));
    }

    #[tokio::test]
    async fn two_qualifiers_on_a_single_slot_is_ambig() {
        let caps = caps("OBJ", "sword");
        let mut ask = recorded(
            vec![
                yes(Family::Direct, Target::Candidate(0)),
                yes(Family::Direct, Target::Candidate(1)),
            ],
            vec![("sword", ResolveKind::Items, items(1, vec![0, 1]))],
        );
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::Ambig);
        assert_eq!(failed.arg, Arg::Objects(vec![0, 1]));
    }

    #[tokio::test]
    async fn the_second_slot_is_indirect_and_counts_as_progress_one() {
        let caps = caps("OBS in OBJ", "swords in bag");
        let mut ask = recorded(
            vec![yes(Family::Direct, Target::Candidate(0))],
            vec![("swords", ResolveKind::Items, items(0, vec![0]))],
        );
        let failed = failure(&mut ask, &caps).await;
        assert_eq!(failed.kind, Kind::ThereIsNo);
        assert_eq!(failed.progress, 1);
        assert!(
            ask.calls
                .iter()
                .any(|(family, _, _)| *family == Family::Direct)
        );
        assert!(
            !ask.calls
                .iter()
                .any(|(family, _, _)| *family == Family::Indirect)
        );
    }
}
