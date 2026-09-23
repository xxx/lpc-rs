//! Two-object permissions with at most one plural slot.

use super::*;
use std::collections::HashSet;

struct Noun {
    numeral: i64,
    objects: Vec<usize>,
}

struct Pair {
    objects: [usize; 2],
    target: usize,
    reply: Reply,
}

impl Pair {
    fn live(&self, ask: &impl Ask) -> bool {
        self.objects.iter().all(|&object| ask.is_live(object))
    }

    fn allowed(&self, ask: &impl Ask) -> bool {
        self.reply == Reply::Yes && self.live(ask)
    }
}

/// Resolve and pin ordinals before any permission handler can exclude matches.
async fn noun(
    ask: &mut impl Ask,
    cap: &Capture,
    progress: usize,
) -> Result<std::result::Result<Noun, Failure>> {
    let kind = match cap.kind {
        CaptureKind::Liv | CaptureKind::Living => ResolveKind::Living,
        _ => ResolveKind::Items,
    };
    let failure = |kind| {
        unresolved(
            kind,
            None,
            Arg::Text(cap.text.clone()),
            cap.kind.is_many(),
            progress,
        )
    };
    let (mut numeral, matched) = match ask.resolve(kind, &cap.text).await? {
        Some(Resolved::Items {
            numeral,
            candidates,
        }) if !candidates.is_empty() => (numeral, candidates),
        Some(_) => return Ok(Err(failure(Kind::NotLiving))),
        None => return Ok(Err(failure(Kind::ThereIsNo))),
    };
    let mut unreachable = false;
    let mut objects: Vec<usize> = matched
        .into_iter()
        .filter(|&object| {
            if !ask.is_live(object) {
                return false;
            }
            if !ask.reachable(object) {
                unreachable = true;
                return false;
            }
            true
        })
        .collect();
    if objects.is_empty() {
        return Ok(Err(failure(if unreachable {
            Kind::NotAccessible
        } else {
            Kind::ThereIsNo
        })));
    }
    match choose(numeral, cap.kind.is_many(), objects.len()) {
        Ok(Pick::Nth(index)) => {
            objects = vec![objects[index]];
            numeral = 1;
        }
        Err(Kind::Ambig) | Ok(_) => {}
        Err(kind) => {
            return Ok(Err(unresolved(
                kind,
                None,
                if kind == Kind::Ordinal {
                    Arg::Count(objects.len() as i64)
                } else {
                    Arg::None
                },
                false,
                progress,
            )));
        }
    }
    Ok(Ok(Noun { numeral, objects }))
}

/// Ask direct then indirect, stopping on refusal or destruction of either object.
async fn check(
    ask: &mut impl Ask,
    indices: [usize; 2],
    objects: [usize; 2],
    values: &mut [Slot],
    words: &[Slot],
) -> Result<Pair> {
    for (index, object) in indices.into_iter().zip(objects) {
        values[index] = Slot::Object(object);
    }
    let args = with_words(values, words);
    let mut pair = Pair {
        objects,
        target: objects[0],
        reply: Reply::Yes,
    };
    for (family, object) in [Family::Direct, Family::Indirect].into_iter().zip(objects) {
        if !pair.live(ask) {
            break;
        }
        pair.target = object;
        pair.reply = ask.call(family, Target::Candidate(object), &args).await?;
        if pair.reply != Reply::Yes {
            break;
        }
    }
    Ok(pair)
}

/// Prefer the first plain refusal, then the first soft one, across failed pairs.
fn rejection<'a>(pairs: impl IntoIterator<Item = &'a Pair>) -> Failure {
    let mut reason = None;
    for pair in pairs {
        if let Reply::Reason { text, soft } = &pair.reply {
            if !soft {
                reason = Some((pair.target, text));
                break;
            }
            reason.get_or_insert((pair.target, text));
        }
    }
    match reason {
        Some((object, text)) => refused(
            Kind::Allocated,
            Some(Target::Candidate(object)),
            Arg::Text(text.clone()),
            false,
            2,
        ),
        None => refused(Kind::Refused, None, Arg::None, false, 2),
    }
}

/// Select objects only after full pairs have passed both permission families.
pub(super) async fn select(
    ask: &mut impl Ask,
    caps: &[Capture],
    indices: [usize; 2],
    mut values: Vec<Slot>,
    words: &[Slot],
) -> Result<std::result::Result<Vec<Slot>, Failure>> {
    let direct = match noun(ask, &caps[indices[0]], 0).await? {
        Ok(noun) => noun,
        Err(failure) => return Ok(Err(failure)),
    };
    let indirect = match noun(ask, &caps[indices[1]], 1).await? {
        Ok(noun) => noun,
        Err(failure) => return Ok(Err(failure)),
    };
    let nouns = [direct, indirect];
    let mut pairs = Vec::new();
    for &direct in &nouns[0].objects {
        for &indirect in &nouns[1].objects {
            // Even missing handlers can leave a large pair search otherwise ready forever.
            tokio::task::yield_now().await;
            if ask.is_live(direct) && ask.is_live(indirect) {
                let pair = check(ask, indices, [direct, indirect], &mut values, words).await?;
                if matches!(pair.reply, Reply::Yes | Reply::Reason { .. }) {
                    pairs.push(pair);
                }
            }
        }
    }
    let mut supported = [HashSet::new(), HashSet::new()];
    for pair in &pairs {
        tokio::task::consume_budget().await;
        if pair.allowed(ask) {
            supported[0].insert(pair.objects[0]);
            supported[1].insert(pair.objects[1]);
        }
    }
    let mut qualified: [Vec<usize>; 2] = [Vec::new(), Vec::new()];
    for slot in 0..2 {
        qualified[slot] = nouns[slot]
            .objects
            .iter()
            .copied()
            .filter(|object| supported[slot].contains(object))
            .collect();
    }
    if qualified[0].is_empty() {
        return Ok(Err(rejection(&pairs)));
    }
    for slot in 0..2 {
        if !caps[indices[slot]].kind.is_many() && qualified[slot].len() > 1 {
            return Ok(Err(unresolved(
                Kind::Ambig,
                None,
                Arg::Objects(std::mem::take(&mut qualified[slot])),
                false,
                2,
            )));
        }
        if caps[indices[slot]].kind.is_many() && nouns[slot].numeral > 0 {
            qualified[slot].truncate(nouns[slot].numeral as usize);
        }
    }

    // At least one slot is singular, so its one selected object fixes every retained pair.
    let single = usize::from(caps[indices[0]].kind.is_many());
    let partner = qualified[single][0];
    let other = 1 - single;
    let picked: HashSet<usize> = qualified[other].iter().copied().collect();
    let mut selected = Vec::new();
    for (index, pair) in pairs.iter_mut().enumerate() {
        tokio::task::consume_budget().await;
        if pair.objects[single] == partner && picked.contains(&pair.objects[other]) {
            tokio::task::yield_now().await;
            *pair = check(ask, indices, pair.objects, &mut values, words).await?;
            selected.push(index);
        }
    }
    let accepted: Vec<usize> = selected
        .iter()
        .map(|&index| &pairs[index])
        .filter(|pair| pair.allowed(ask))
        .map(|pair| pair.objects[other])
        .collect();
    if accepted.is_empty() {
        return Ok(Err(rejection(
            pairs.iter().filter(|pair| pair.objects[single] == partner),
        )));
    }

    values[indices[single]] = Slot::Object(partner);
    values[indices[other]] = if caps[indices[other]].kind.is_many() {
        let mut reasons = Vec::new();
        for pair in &pairs {
            tokio::task::consume_budget().await;
            if pair.objects[single] == partner
                && pair.live(ask)
                && let Reply::Reason { text, soft: false } = &pair.reply
            {
                reasons.push(text.clone());
            }
        }
        Slot::Mixed(accepted, reasons)
    } else {
        Slot::Object(accepted[0])
    };
    Ok(Ok(values))
}

#[cfg(test)]
mod tests {
    use std::{
        sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        },
        time::Duration,
    };

    use super::*;

    struct Refusing {
        checks: Arc<AtomicUsize>,
    }

    impl Ask for Refusing {
        async fn call(&mut self, family: Family, _: Target, _: &[Slot]) -> Result<Reply> {
            if family == Family::Direct {
                self.checks.fetch_add(1, Ordering::Relaxed);
                Ok(Reply::No)
            } else {
                Ok(Reply::Absent)
            }
        }

        async fn resolve(&mut self, _: ResolveKind, _: &str) -> Result<Option<Resolved>> {
            Ok(Some(Resolved::Items {
                numeral: 1,
                candidates: (0..1024).collect(),
            }))
        }

        fn is_live(&self, _: usize) -> bool {
            true
        }
        fn reachable(&self, _: usize) -> bool {
            true
        }
    }

    #[tokio::test(start_paused = true)]
    async fn a_large_pair_search_yields_to_the_attempt_deadline() {
        let checks = Arc::new(AtomicUsize::new(0));
        let mut ask = Refusing {
            checks: checks.clone(),
        };
        let worker = tokio::spawn(async move {
            let rule = crate::command::frontend::parser::compile("get", "OBJ from OBJ").unwrap();
            let caps = rule.compiled.captures_of("sword from box").next().unwrap();
            tokio::time::timeout(Duration::from_secs(1), attempt(&mut ask, &caps)).await
        });
        while checks.load(Ordering::Relaxed) < 16 {
            tokio::task::yield_now().await;
        }
        tokio::time::advance(Duration::from_secs(2)).await;
        assert!(worker.await.unwrap().is_err());
        assert!(checks.load(Ordering::Relaxed) < 1024 * 1024);
    }
}
