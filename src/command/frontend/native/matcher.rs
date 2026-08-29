//! A native pattern against a line: the first parse whose nouns resolve,
//! and its captures as LPC values.

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_bug};

use super::{Capture, CaptureKind, Compiled};
use crate::{
    command::resolve::{LpcVocabulary, Resolved, Resolver, Vocabulary},
    interpreter::{lpc_array::LpcArray, lpc_ref::LpcRef, lpc_string::LpcString},
};

/// One capture, valued: its text, its int, or what the resolver found.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Value {
    /// `%w`/`%s`: the words as typed.
    Text(String),
    /// `%d`.
    Int(i64),
    /// A noun capture's candidates.
    Resolved(Resolved),
}

/// The first capture set in `parses` whose noun captures all resolve —
/// greedy splits come first — or `None` when no parse's phrases all name
/// something. A set with no noun capture asks the resolver nothing.
async fn first_resolved<V: Vocabulary>(
    parses: impl Iterator<Item = Vec<Capture>>,
    resolver: &mut Resolver<V>,
) -> Result<Option<Vec<Value>>> {
    'parses: for captures in parses {
        let mut values = Vec::with_capacity(captures.len());
        for capture in captures {
            let value = match capture.kind.resolver_kind() {
                None if capture.kind == CaptureKind::Number => match capture.text.parse() {
                    Ok(n) => Value::Int(n),
                    Err(_) => continue 'parses,
                },
                None => Value::Text(capture.text),
                Some(kind) => match resolver.resolve(kind, &capture.text).await? {
                    Some(Resolved::Items { candidates, .. }) if candidates.is_empty() => {
                        continue 'parses;
                    }
                    Some(found) => Value::Resolved(found),
                    None => continue 'parses,
                },
            };
            values.push(value);
        }
        return Ok(Some(values));
    }
    Ok(None)
}

/// `values` as the handler sees them: `%o` an object, `%i`/`%l`
/// `({ numeral, ob... })`, `%p` the matched entry as a string, `%w`/`%s`
/// a string, `%d` an int.
async fn lpc_values(
    values: &[Value],
    resolver: &mut Resolver<LpcVocabulary<'_>>,
) -> Result<Vec<LpcRef>> {
    // Without a caller's list, this fetch is the first thing to reach the master.
    let prepositions: Vec<String> = if values
        .iter()
        .any(|v| matches!(v, Value::Resolved(Resolved::Preposition(_))))
    {
        resolver.prepositions().await?
    } else {
        Vec::new()
    };
    let scope = resolver.vocabulary().scope();
    let txn = resolver.vocabulary().ctx().txn();
    let object = |candidate: usize| LpcRef::from(Arc::downgrade(&scope[candidate]));
    let mut out = Vec::with_capacity(values.len());
    for value in values {
        out.push(match value {
            Value::Text(text) => LpcString::from(text.as_str()).into(),
            Value::Int(n) => LpcRef::from(*n),
            Value::Resolved(Resolved::Object(candidate)) => object(*candidate),
            Value::Resolved(Resolved::Items {
                numeral,
                candidates,
            }) => {
                let items = std::iter::once(LpcRef::from(*numeral))
                    .chain(candidates.iter().map(|&candidate| object(candidate)))
                    .collect::<LpcArray>();
                LpcRef::Array(txn.with(|t| t.mint_array(items)))
            }
            Value::Resolved(Resolved::Preposition(index)) => match prepositions.get(*index) {
                Some(entry) => LpcRef::from(entry.as_str()),
                None => return Err(lpc_bug!("a preposition outside the list in force")),
            },
        });
    }
    Ok(out)
}

/// The handler's arguments for the first parse of `line` whose nouns
/// resolve, or `None` when the pattern is no match for the line.
pub(crate) async fn arguments(
    compiled: &Compiled,
    line: &str,
    resolver: &mut Resolver<LpcVocabulary<'_>>,
) -> Result<Option<Vec<LpcRef>>> {
    let Some(values) = first_resolved(compiled.captures_of(line), resolver).await? else {
        return Ok(None);
    };
    Ok(Some(lpc_values(&values, resolver).await?))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::{
        frontend::native::compile_pattern,
        resolve::{
            Lexicon, Resolved,
            fake::{Fake, scene},
        },
    };

    fn by_id(phrases: &'static [&'static str]) -> Fake {
        let mut fake = Fake::new(vec![Lexicon::IdFunction, Lexicon::IdFunction]);
        fake.id_true = phrases.to_vec();
        fake
    }

    #[tokio::test]
    async fn a_plain_only_parse_asks_the_vocabulary_nothing() {
        let c = compile_pattern("'give' %w 'to' %w %d").unwrap();
        let mut r = Resolver::new(scene(), None);
        let found = first_resolved(c.captures_of("give sword to bob 3"), &mut r)
            .await
            .unwrap();
        assert_eq!(
            found,
            Some(vec![
                Value::Text("sword".into()),
                Value::Text("bob".into()),
                Value::Int(3)
            ])
        );
        assert_eq!(r.vocabulary().defaults_asked, 0);
    }

    #[tokio::test]
    async fn the_next_parse_is_tried_when_a_phrase_names_nothing() {
        let c = compile_pattern("'x' %o %o").unwrap();
        let mut r = Resolver::new(by_id(&["sword", "red sword"]), None);
        let found = first_resolved(c.captures_of("x sword red sword"), &mut r)
            .await
            .unwrap();
        assert_eq!(
            found,
            Some(vec![
                Value::Resolved(Resolved::Object(0)),
                Value::Resolved(Resolved::Object(0))
            ])
        );
    }

    #[tokio::test]
    async fn no_parse_resolving_is_none() {
        let c = compile_pattern("'x' %o").unwrap();
        let mut r = Resolver::new(by_id(&[]), None);
        assert_eq!(
            first_resolved(c.captures_of("x nothing"), &mut r)
                .await
                .unwrap(),
            None
        );
    }

    #[tokio::test]
    async fn a_living_capture_naming_only_things_is_no_match() {
        let c = compile_pattern("'x' %l").unwrap();
        let mut r = Resolver::new(scene(), None);
        assert_eq!(
            first_resolved(c.captures_of("x sword"), &mut r)
                .await
                .unwrap(),
            None
        );
    }

    #[tokio::test]
    async fn a_resolved_item_set_keeps_its_numeral() {
        let c = compile_pattern("'get' %i").unwrap();
        let mut r = Resolver::new(scene(), None);
        let found = first_resolved(c.captures_of("get two swords"), &mut r)
            .await
            .unwrap();
        assert_eq!(
            found,
            Some(vec![Value::Resolved(Resolved::Items {
                numeral: 2,
                candidates: vec![0, 1]
            })])
        );
    }
}
