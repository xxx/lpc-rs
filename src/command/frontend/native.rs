//! The native rule shape: an `add_rule` pattern in the `parse_command`
//! dialect compiled to an engine grammar, and the handler's arguments taken
//! from the parse's captures.

use std::{
    fmt,
    sync::{Arc, LazyLock},
};

use dashmap::DashMap;
use ustr::Ustr;

use crate::{
    command::grammar::{
        Element, Grammar, GrammarBuilder, GrammarError, Label, Parse, lit, nt, tok,
    },
    interpreter::{lpc_ref::LpcRef, lpc_string::LpcString},
};

/// What a `%` capture yields to the handler.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CaptureKind {
    /// `%w`: one word, as a string.
    Word = 0,
    /// `%s`: zero or more words, as one string with its spacing intact.
    Words = 1,
    /// `%d`: a run of digits, as an int.
    Number = 2,
}

/// The low bits of a label hold the kind; the slot sits above them.
const KIND_BITS: u32 = 2;

impl CaptureKind {
    /// The label for this capture in slot `slot`.
    fn label(self, slot: u32) -> Label {
        Label((slot << KIND_BITS) | self as u32)
    }

    /// The slot and kind a label packs, or `None` for a label this module
    /// did not make.
    fn unpack(label: Label) -> Option<(u32, CaptureKind)> {
        let kind = match label.0 & ((1 << KIND_BITS) - 1) {
            0 => CaptureKind::Word,
            1 => CaptureKind::Words,
            2 => CaptureKind::Number,
            _ => return None,
        };
        Some((label.0 >> KIND_BITS, kind))
    }
}

/// Why a pattern does not compile; the text reaches the LPC caller.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PatternError {
    /// The pattern has no elements.
    Empty,
    /// The first element is not a quoted word.
    NoVerb,
    /// A word outside quotes.
    UnquotedWord(String),
    /// A `'` with no closing `'`.
    UnterminatedQuote,
    /// A `[` with no closing `]`.
    UnterminatedBracket,
    /// Nothing between quotes or brackets.
    EmptyWord,
    /// More than one word between quotes or brackets.
    NotOneWord(String),
    /// A `%` with nothing after it.
    BareCapture,
    /// A `%` capture letter this dialect does not have.
    UnknownCapture(char),
    /// A `%o`/`%l`/`%i`/`%p` capture, which needs the noun resolver.
    Unresolvable(char),
    /// A `/` not between two quoted words.
    BadAlternative,
    /// The engine rejected the built grammar.
    Grammar(GrammarError),
}

impl fmt::Display for PatternError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PatternError::Empty => write!(f, "the pattern is empty"),
            PatternError::NoVerb => {
                write!(f, "the pattern must start with a quoted verb, like 'look'")
            }
            PatternError::UnquotedWord(word) => write!(f, "`{word}` must be quoted: '{word}'"),
            PatternError::UnterminatedQuote => write!(f, "a quote is not closed"),
            PatternError::UnterminatedBracket => write!(f, "a `[` is not closed"),
            PatternError::EmptyWord => write!(f, "quotes and brackets must hold a word"),
            PatternError::NotOneWord(text) => write!(f, "`{text}` must be one word"),
            PatternError::BareCapture => write!(f, "`%` must be followed by w, s or d"),
            PatternError::UnknownCapture(c) => {
                write!(f, "`%{c}` is not a capture; use %w, %s or %d")
            }
            PatternError::Unresolvable(c) => write!(
                f,
                "`%{c}` needs the noun resolver, which this driver does not have yet"
            ),
            PatternError::BadAlternative => write!(f, "`/` must sit between quoted words"),
            PatternError::Grammar(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for PatternError {}

/// A compiled pattern: the verbs the pre-filter accepts and the grammar the
/// line must parse against.
#[derive(Clone, Debug)]
pub struct Compiled {
    /// The leading verb alternatives, in pattern order; never empty.
    pub verbs: Arc<[Ustr]>,
    /// `S → verb elements…`, shared by every rule registered from this pattern.
    pub grammar: Arc<Grammar>,
}

static PATTERNS: LazyLock<DashMap<String, Compiled>> = LazyLock::new(DashMap::new);

/// Compile `pattern`, once per text; only successes are cached, so a
/// malformed pattern reports its fault on every call.
pub fn compile(pattern: &str) -> Result<Compiled, PatternError> {
    if let Some(hit) = PATTERNS.get(pattern) {
        return Ok(hit.clone());
    }
    let compiled = build(&group(scan(pattern)?)?)?;
    PATTERNS.insert(pattern.to_owned(), compiled.clone());
    Ok(compiled)
}

/// One lexical piece of a pattern.
#[derive(Debug, PartialEq, Eq)]
enum Piece {
    /// `'word'`.
    Quoted(String),
    /// `%w`, `%s`, `%d`.
    Capture(CaptureKind),
    /// `[word]`.
    Optional(String),
    /// `/`.
    Slash,
}

/// Split `pattern` into pieces; a bare word is the porter's most common
/// mistake, so it is reported with the quoted form.
fn scan(pattern: &str) -> Result<Vec<Piece>, PatternError> {
    let mut pieces = Vec::new();
    let mut rest = pattern;
    while let Some(c) = rest.chars().next() {
        rest = match c {
            c if c.is_whitespace() => &rest[c.len_utf8()..],
            '\'' => {
                let body = &rest[1..];
                let Some(end) = body.find('\'') else {
                    return Err(PatternError::UnterminatedQuote);
                };
                pieces.push(Piece::Quoted(one_word(&body[..end])?));
                &body[end + 1..]
            }
            '[' => {
                let body = &rest[1..];
                let Some(end) = body.find(']') else {
                    return Err(PatternError::UnterminatedBracket);
                };
                pieces.push(Piece::Optional(one_word(&body[..end])?));
                &body[end + 1..]
            }
            '%' => {
                let Some(letter) = rest.chars().nth(1) else {
                    return Err(PatternError::BareCapture);
                };
                pieces.push(Piece::Capture(capture_kind(letter)?));
                &rest[1 + letter.len_utf8()..]
            }
            '/' => {
                pieces.push(Piece::Slash);
                &rest[1..]
            }
            _ => {
                let end = rest
                    .find(|ch: char| ch.is_whitespace() || "'[%/".contains(ch))
                    .unwrap_or(rest.len());
                return Err(PatternError::UnquotedWord(rest[..end].to_owned()));
            }
        };
    }
    Ok(pieces)
}

/// The single word between quotes or brackets.
fn one_word(text: &str) -> Result<String, PatternError> {
    let mut words = text.split_whitespace();
    match (words.next(), words.next()) {
        (Some(word), None) => Ok(word.to_owned()),
        (None, _) => Err(PatternError::EmptyWord),
        (Some(_), Some(_)) => Err(PatternError::NotOneWord(text.trim().to_owned())),
    }
}

fn capture_kind(letter: char) -> Result<CaptureKind, PatternError> {
    match letter {
        'w' => Ok(CaptureKind::Word),
        's' => Ok(CaptureKind::Words),
        'd' => Ok(CaptureKind::Number),
        'o' | 'l' | 'i' | 'p' => Err(PatternError::Unresolvable(letter)),
        other => Err(PatternError::UnknownCapture(other)),
    }
}

/// One element of the pattern after `/` has joined its neighbours.
#[derive(Debug, PartialEq, Eq)]
enum Group {
    /// Quoted words, one of which must appear.
    Words(Vec<String>),
    Capture(CaptureKind),
    Optional(String),
}

/// Join `/`-separated quoted words into one group; the first group must be
/// the verb.
fn group(pieces: Vec<Piece>) -> Result<Vec<Group>, PatternError> {
    let mut groups: Vec<Group> = Vec::new();
    // The words a `/` is joining, awaiting the word on its right.
    let mut pending: Option<Vec<String>> = None;
    for piece in pieces {
        match piece {
            Piece::Quoted(word) => {
                let mut words = pending.take().unwrap_or_default();
                words.push(word);
                groups.push(Group::Words(words));
            }
            Piece::Slash => {
                if pending.is_some() {
                    return Err(PatternError::BadAlternative);
                }
                match groups.pop() {
                    Some(Group::Words(words)) => pending = Some(words),
                    _ => return Err(PatternError::BadAlternative),
                }
            }
            Piece::Capture(kind) => {
                if pending.is_some() {
                    return Err(PatternError::BadAlternative);
                }
                groups.push(Group::Capture(kind));
            }
            Piece::Optional(word) => {
                if pending.is_some() {
                    return Err(PatternError::BadAlternative);
                }
                groups.push(Group::Optional(word));
            }
        }
    }
    if pending.is_some() {
        return Err(PatternError::BadAlternative);
    }
    match groups.first() {
        None => Err(PatternError::Empty),
        Some(Group::Words(_)) => Ok(groups),
        Some(_) => Err(PatternError::NoVerb),
    }
}

/// `S → group…` over plain words, one production; captures are labelled
/// with their slot and kind.
fn build(groups: &[Group]) -> Result<Compiled, PatternError> {
    let Some(Group::Words(verbs)) = groups.first() else {
        return Err(PatternError::NoVerb);
    };
    let mut b = GrammarBuilder::new();
    let s = b.nonterminal("S");
    let words = b.words_tokens();
    let mut slot = 0;
    let mut rhs: Vec<Element> = Vec::with_capacity(groups.len());
    for group in groups {
        rhs.push(match group {
            Group::Words(alternatives) => match alternatives.as_slice() {
                [word] => lit(word),
                many => nt(b.alternatives(many.iter().map(|w| lit(w)))),
            },
            Group::Optional(word) => nt(b.optional(lit(word))),
            Group::Capture(kind) => {
                let element = match kind {
                    CaptureKind::Word => nt(b.word_like(&words)),
                    CaptureKind::Words => nt(b.words_star(&words)),
                    CaptureKind::Number => tok(words.number),
                };
                let labeled = element.labeled(kind.label(slot));
                slot += 1;
                labeled
            }
        });
    }
    b.production(s, rhs);
    b.start(s);
    let grammar = b.build().map_err(PatternError::Grammar)?;
    Ok(Compiled {
        verbs: verbs.iter().map(|v| Ustr::from(v)).collect(),
        grammar: Arc::new(grammar),
    })
}

/// The handler's arguments, one per capture in slot order; `None` when a
/// `%d` does not fit an int, which makes the rule no match.
pub fn arguments(parse: &Parse) -> Option<Vec<LpcRef>> {
    let mut captures: Vec<(u32, LpcRef)> = Vec::new();
    for (label, text) in parse.captures() {
        let (slot, kind) = CaptureKind::unpack(label)?;
        let value = match kind {
            CaptureKind::Word | CaptureKind::Words => LpcString::from(text).into(),
            CaptureKind::Number => LpcRef::from(text.parse::<i64>().ok()?),
        };
        captures.push((slot, value));
    }
    captures.sort_by_key(|(slot, _)| *slot);
    Some(captures.into_iter().map(|(_, value)| value).collect())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use super::*;
    use crate::{
        command::grammar::parse,
        interpreter::{lpc_ref::LpcRef, lpc_string::LpcString},
    };

    fn s(text: &str) -> LpcRef {
        LpcString::from(text).into()
    }

    /// The arguments the first parse of `line` under `pattern` yields, or
    /// `None` when the line does not parse.
    fn args(pattern: &str, line: &str) -> Option<Vec<LpcRef>> {
        let compiled = compile(pattern).unwrap();
        let parsed = parse(&compiled.grammar, line).next()?;
        arguments(&parsed)
    }

    fn verbs(pattern: &str) -> Vec<String> {
        compile(pattern)
            .unwrap()
            .verbs
            .iter()
            .map(|v| v.to_string())
            .collect()
    }

    #[test]
    fn a_bare_verb_matches_only_that_word() {
        assert_eq!(args("'look'", "look"), Some(vec![]));
        assert_eq!(args("'look'", "look here"), None);
        assert_eq!(args("'look'", "peek"), None);
        assert_eq!(verbs("'look'"), vec!["look"]);
    }

    #[test]
    fn verb_alternatives_list_every_verb_and_all_of_them_parse() {
        let pattern = "'give' / 'hand' %w";
        assert_eq!(verbs(pattern), vec!["give", "hand"]);
        assert_eq!(args(pattern, "give sword"), Some(vec![s("sword")]));
        assert_eq!(args(pattern, "hand sword"), Some(vec![s("sword")]));
        assert_eq!(args(pattern, "throw sword"), None);
    }

    #[test]
    fn a_word_capture_takes_one_word_and_a_words_capture_the_rest_verbatim() {
        assert_eq!(
            args("'give' %w 'to' %s", "give sword to bob   the guard"),
            Some(vec![s("sword"), s("bob   the guard")])
        );
        assert_eq!(args("'give' %w 'to' %s", "give long sword to bob"), None);
    }

    #[test]
    fn a_words_capture_may_be_empty() {
        assert_eq!(args("'say' %s", "say"), Some(vec![s("")]));
        assert_eq!(args("'say' %s", "say hi there"), Some(vec![s("hi there")]));
    }

    #[test]
    fn a_number_capture_is_an_int_and_matches_digits_only() {
        assert_eq!(args("'take' %d", "take 5"), Some(vec![LpcRef::from(5)]));
        assert_eq!(args("'take' %d", "take five"), None);
        assert_eq!(args("'take' %d", "take -5"), None);
    }

    #[test]
    fn a_number_too_large_for_an_int_is_no_match() {
        assert_eq!(args("'take' %d", "take 99999999999999999999"), None);
    }

    #[test]
    fn an_optional_word_is_consumed_when_present() {
        assert_eq!(args("'look' [at] %w", "look at bob"), Some(vec![s("bob")]));
        assert_eq!(args("'look' [at] %w", "look bob"), Some(vec![s("bob")]));
        assert_eq!(args("'look' [at] %w", "look at"), Some(vec![s("at")]));
    }

    #[test]
    fn literal_alternatives_work_after_the_verb_too() {
        let pattern = "'put' %w 'in' / 'into' %w";
        assert_eq!(
            args(pattern, "put coin in box"),
            Some(vec![s("coin"), s("box")])
        );
        assert_eq!(
            args(pattern, "put coin into box"),
            Some(vec![s("coin"), s("box")])
        );
        assert_eq!(args(pattern, "put coin on box"), None);
        assert_eq!(verbs(pattern), vec!["put"]);
    }

    #[test]
    fn captures_keep_pattern_order_across_kinds() {
        assert_eq!(
            args("'buy' %d %w 'for' %s", "buy 3 apples for a song"),
            Some(vec![LpcRef::from(3), s("apples"), s("a song")])
        );
    }

    #[test]
    fn the_first_word_must_be_a_quoted_verb() {
        assert_eq!(compile("%w 'look'").unwrap_err(), PatternError::NoVerb);
        assert_eq!(compile("[the] 'look'").unwrap_err(), PatternError::NoVerb);
        assert_eq!(compile("").unwrap_err(), PatternError::Empty);
        assert_eq!(compile("   ").unwrap_err(), PatternError::Empty);
    }

    #[test]
    fn each_malformed_pattern_names_its_fault() {
        let cases = [
            ("look", PatternError::UnquotedWord("look".into())),
            ("'look' at", PatternError::UnquotedWord("at".into())),
            ("'look", PatternError::UnterminatedQuote),
            ("'look' [at", PatternError::UnterminatedBracket),
            ("''", PatternError::EmptyWord),
            ("'look' []", PatternError::EmptyWord),
            ("'put down'", PatternError::NotOneWord("put down".into())),
            ("'look' [at the]", PatternError::NotOneWord("at the".into())),
            ("'look' %", PatternError::BareCapture),
            ("'look' %x", PatternError::UnknownCapture('x')),
            ("/ 'look'", PatternError::BadAlternative),
            ("'look' /", PatternError::BadAlternative),
            ("'get' / / 'take'", PatternError::BadAlternative),
            ("'look' / %w", PatternError::BadAlternative),
            ("'look' %w / 'at'", PatternError::BadAlternative),
        ];
        for (pattern, expected) in cases {
            assert_eq!(compile(pattern).unwrap_err(), expected, "{pattern:?}");
        }
    }

    #[test]
    fn resolver_captures_are_rejected_until_the_resolver_exists() {
        for kind in ['o', 'l', 'i', 'p'] {
            assert_eq!(
                compile(&format!("'get' %{kind}")).unwrap_err(),
                PatternError::Unresolvable(kind)
            );
        }
        assert_eq!(
            PatternError::Unresolvable('o').to_string(),
            "`%o` needs the noun resolver, which this driver does not have yet"
        );
    }

    #[test]
    fn every_error_displays_as_one_sentence_without_a_period() {
        let errors = [
            PatternError::Empty,
            PatternError::NoVerb,
            PatternError::UnquotedWord("get".into()),
            PatternError::UnterminatedQuote,
            PatternError::UnterminatedBracket,
            PatternError::EmptyWord,
            PatternError::NotOneWord("put down".into()),
            PatternError::BareCapture,
            PatternError::UnknownCapture('x'),
            PatternError::BadAlternative,
        ];
        for error in errors {
            let text = error.to_string();
            assert!(!text.is_empty() && !text.ends_with('.'), "{text:?}");
        }
        assert_eq!(
            PatternError::UnquotedWord("get".into()).to_string(),
            "`get` must be quoted: 'get'"
        );
    }

    #[test]
    fn compiled_patterns_are_cached_by_text() {
        let a = compile("'wave' %s").unwrap();
        let b = compile("'wave' %s").unwrap();
        assert!(Arc::ptr_eq(&a.grammar, &b.grammar));
        let c = compile("'wave'  %s").unwrap();
        assert!(!Arc::ptr_eq(&a.grammar, &c.grammar));
    }

    #[test]
    fn a_label_round_trips_its_slot_and_kind() {
        for (slot, kind) in [
            (0, CaptureKind::Word),
            (7, CaptureKind::Words),
            (2, CaptureKind::Number),
        ] {
            assert_eq!(CaptureKind::unpack(kind.label(slot)), Some((slot, kind)));
        }
        assert_eq!(CaptureKind::unpack(Label(3)), None);
    }
}
