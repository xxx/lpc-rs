//! The native rule shape: an `add_rule` or `parse_command` pattern compiled
//! to an engine grammar, and the handler's arguments taken from the parse's
//! captures.

mod matcher;

use std::{
    fmt,
    sync::{Arc, LazyLock},
};

use dashmap::DashMap;
use ustr::Ustr;

use crate::command::{
    grammar::{Element, Grammar, GrammarBuilder, GrammarError, Label, Parse, lit, nt, parse, tok},
    resolve::Kind,
};

pub(crate) use matcher::arguments;

/// What a `%` capture yields to the handler.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CaptureKind {
    /// `%w`: one word, as a string.
    Word = 0,
    /// `%s`: zero or more words, as one string with its spacing intact.
    Words = 1,
    /// `%d`: a run of digits, as an int.
    Number = 2,
    /// `%o`: one or more words naming an object.
    Object = 3,
    /// `%l`: one or more words naming livings.
    Living = 4,
    /// `%i`: one or more words naming objects, with a numeral.
    Items = 5,
    /// `%p`: one or more words that are a preposition.
    Preposition = 6,
    /// `%L`: one or more words naming one living.
    Liv = 7,
}

impl CaptureKind {
    /// Whether the capture names objects (livings included); a preposition
    /// does not.
    pub fn is_object(self) -> bool {
        matches!(
            self,
            CaptureKind::Object | CaptureKind::Living | CaptureKind::Items | CaptureKind::Liv
        )
    }

    /// Whether the capture may name several objects.
    pub fn is_many(self) -> bool {
        matches!(self, CaptureKind::Items | CaptureKind::Living)
    }

    /// The resolver kind behind this capture, or `None` for a plain one.
    pub fn resolver_kind(self) -> Option<Kind> {
        match self {
            CaptureKind::Object => Some(Kind::Object),
            CaptureKind::Living => Some(Kind::Living),
            CaptureKind::Items => Some(Kind::Items),
            CaptureKind::Preposition => Some(Kind::Preposition),
            CaptureKind::Liv => Some(Kind::Liv),
            CaptureKind::Word | CaptureKind::Words | CaptureKind::Number => None,
        }
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
            PatternError::BareCapture => {
                write!(f, "`%` must be followed by w, s, d, o, l, L, i or p")
            }
            PatternError::UnknownCapture(c) => {
                write!(
                    f,
                    "`%{c}` is not a capture; use %w, %s, %d, %o, %l, %L, %i or %p"
                )
            }
            PatternError::BadAlternative => write!(f, "`/` must sit between quoted words"),
            PatternError::Grammar(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for PatternError {}

/// A compiled pattern: the verbs the pre-filter accepts, the grammar the
/// line must parse against, and the kind of each capture by slot.
#[derive(Clone, Debug)]
pub struct Compiled {
    /// The leading verb alternatives, in pattern order; empty for a
    /// `compile_pattern` pattern.
    pub verbs: Arc<[Ustr]>,
    /// `S → elements…`, shared by every rule registered from this pattern.
    pub grammar: Arc<Grammar>,
    /// The kind of each `%` capture, in slot order.
    pub kinds: Vec<CaptureKind>,
}

/// Whether the first element must be a quoted verb.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Verb {
    /// `add_rule`: the verb is the dispatch pre-filter.
    Required,
    /// `parse_command`: the verb was stripped before the call.
    Optional,
}

// The same text is a different grammar under each dialect.
static RULES: LazyLock<DashMap<String, Arc<Compiled>>> = LazyLock::new(DashMap::new);
static PATTERNS: LazyLock<DashMap<String, Arc<Compiled>>> = LazyLock::new(DashMap::new);

/// Compile an `add_rule` pattern, once per text; only successes are cached,
/// so a malformed pattern reports its fault on every call.
pub fn compile(pattern: &str) -> Result<Arc<Compiled>, PatternError> {
    compile_in(&RULES, pattern, Verb::Required)
}

/// Compile a `parse_command` pattern, which needs no leading verb; cached
/// like [`compile`].
pub fn compile_pattern(pattern: &str) -> Result<Arc<Compiled>, PatternError> {
    compile_in(&PATTERNS, pattern, Verb::Optional)
}

fn compile_in(
    cache: &DashMap<String, Arc<Compiled>>,
    pattern: &str,
    verb: Verb,
) -> Result<Arc<Compiled>, PatternError> {
    if let Some(hit) = cache.get(pattern) {
        return Ok(hit.clone());
    }
    let compiled = Arc::new(build(&group(scan(pattern)?, verb)?, verb)?);
    Ok(cache
        .entry(pattern.to_owned())
        .or_insert_with(|| compiled)
        .clone())
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

/// Split `pattern` into pieces; a bare word is reported with its quoted form.
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
        'o' => Ok(CaptureKind::Object),
        'l' => Ok(CaptureKind::Living),
        'i' => Ok(CaptureKind::Items),
        'p' => Ok(CaptureKind::Preposition),
        'L' => Ok(CaptureKind::Liv),
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

/// Join `/`-separated quoted words into one group; under [`Verb::Required`]
/// the first group must be the verb.
fn group(pieces: Vec<Piece>, verb: Verb) -> Result<Vec<Group>, PatternError> {
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
        None if verb == Verb::Required => Err(PatternError::Empty),
        None => Ok(groups),
        Some(Group::Words(_)) => Ok(groups),
        Some(_) if verb == Verb::Required => Err(PatternError::NoVerb),
        Some(_) => Ok(groups),
    }
}

/// `words` with later repeats of an already-seen word dropped, order kept.
fn dedup_words(words: &[String]) -> Vec<&String> {
    let mut out: Vec<&String> = Vec::with_capacity(words.len());
    for word in words {
        if !out.contains(&word) {
            out.push(word);
        }
    }
    out
}

/// `S → group…` over plain words, one production; captures are labelled
/// with their slot and kind.
fn build(groups: &[Group], verb: Verb) -> Result<Compiled, PatternError> {
    let verbs: Vec<&String> = match (verb, groups.first()) {
        (Verb::Required, Some(Group::Words(words))) => dedup_words(words),
        (Verb::Required, _) => return Err(PatternError::NoVerb),
        (Verb::Optional, _) => Vec::new(),
    };
    let mut b = GrammarBuilder::new();
    let s = b.nonterminal("S");
    let words = b.words_tokens();
    let mut kinds: Vec<CaptureKind> = Vec::new();
    let mut slot: u32 = 0;
    let mut rhs: Vec<Element> = Vec::with_capacity(groups.len());
    for group in groups {
        rhs.push(match group {
            Group::Words(alternatives) => {
                let alternatives = dedup_words(alternatives);
                match alternatives.as_slice() {
                    [word] => lit(word.as_str()),
                    many => nt(b.alternatives(many.iter().map(|w| lit(w.as_str())))),
                }
            }
            Group::Optional(word) => nt(b.optional(lit(word))),
            Group::Capture(kind) => {
                let element = match kind {
                    CaptureKind::Word => nt(b.word_like(&words)),
                    CaptureKind::Words => nt(b.words_star(&words)),
                    CaptureKind::Number => tok(words.number),
                    CaptureKind::Object
                    | CaptureKind::Living
                    | CaptureKind::Items
                    | CaptureKind::Preposition
                    | CaptureKind::Liv => nt(b.words_plus(&words)),
                };
                let labeled = element.labeled(Label(slot));
                kinds.push(*kind);
                slot += 1;
                labeled
            }
        });
    }
    b.production(s, rhs);
    b.start(s);
    let grammar = b.build().map_err(PatternError::Grammar)?;
    Ok(Compiled {
        verbs: verbs.iter().map(|v| Ustr::from(v.as_str())).collect(),
        grammar: Arc::new(grammar),
        kinds,
    })
}

/// One `%` capture of a parse.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Capture {
    /// Its position among the pattern's captures.
    pub slot: u32,
    /// What it yields.
    pub kind: CaptureKind,
    /// The captured input, spacing intact.
    pub text: String,
}

impl Compiled {
    /// The captures of each parse of `line`, greedy splits first, each set
    /// in slot order; a parse whose `%d` does not fit an int is skipped.
    pub fn captures_of<'a>(&'a self, line: &str) -> impl Iterator<Item = Vec<Capture>> + 'a {
        parse(&self.grammar, line).filter_map(|parsed| self.captures(&parsed))
    }

    fn captures(&self, parsed: &Parse) -> Option<Vec<Capture>> {
        let mut out = Vec::with_capacity(self.kinds.len());
        for (label, text) in parsed.captures() {
            let slot = label.0;
            let kind = *self.kinds.get(slot as usize)?;
            if kind == CaptureKind::Number {
                text.parse::<i64>().ok()?;
            }
            out.push(Capture {
                slot,
                kind,
                text: text.to_owned(),
            });
        }
        out.sort_by_key(|capture| capture.slot);
        Some(out)
    }
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

    /// A capture that needs no resolver: a string for `%w`/`%s`, an int for
    /// `%d`; `None` for a noun capture.
    fn plain_value(capture: &Capture) -> Option<LpcRef> {
        match capture.kind {
            CaptureKind::Word | CaptureKind::Words => {
                Some(LpcString::from(capture.text.as_str()).into())
            }
            CaptureKind::Number => capture.text.parse::<i64>().ok().map(LpcRef::from),
            CaptureKind::Object
            | CaptureKind::Living
            | CaptureKind::Items
            | CaptureKind::Preposition
            | CaptureKind::Liv => None,
        }
    }

    /// The arguments the first parse of `line` under `pattern` yields, or
    /// `None` when the line does not parse.
    fn args(pattern: &str, line: &str) -> Option<Vec<LpcRef>> {
        let compiled = compile(pattern).unwrap();
        compiled
            .captures_of(line)
            .next()?
            .iter()
            .map(plain_value)
            .collect()
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
    fn a_repeated_verb_alternative_registers_once() {
        assert_eq!(verbs("'get' / 'get' %w"), vec!["get"]);
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
    fn a_words_capture_is_greedy_before_a_later_literal() {
        assert_eq!(
            args("'say' %s 'to' %w", "say hi to bob to sam"),
            Some(vec![s("hi to bob"), s("sam")])
        );
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
    fn an_optional_word_in_final_position_may_be_absent() {
        assert_eq!(args("'look' [around]", "look around"), Some(vec![]));
        assert_eq!(args("'look' [around]", "look"), Some(vec![]));
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
    fn resolver_kinds_compile_to_word_run_captures() {
        let compiled = compile("'get' %i 'from' %o").unwrap();
        assert_eq!(
            compiled.kinds,
            vec![CaptureKind::Items, CaptureKind::Object]
        );
        let captures = compiled
            .captures_of("get red sword from old bag")
            .next()
            .unwrap();
        assert_eq!(captures.len(), 2);
        assert_eq!(
            (
                captures[0].slot,
                captures[0].kind,
                captures[0].text.as_str()
            ),
            (0, CaptureKind::Items, "red sword")
        );
        assert_eq!(
            (
                captures[1].slot,
                captures[1].kind,
                captures[1].text.as_str()
            ),
            (1, CaptureKind::Object, "old bag")
        );
        assert!(
            captures.iter().any(|c| plain_value(c).is_none()),
            "noun captures have no plain value"
        );
    }

    #[test]
    fn every_kind_letter_is_a_capture() {
        let compiled = compile("'x' %w %s %d %o %l %L %i %p").unwrap();
        assert_eq!(
            compiled.kinds,
            vec![
                CaptureKind::Word,
                CaptureKind::Words,
                CaptureKind::Number,
                CaptureKind::Object,
                CaptureKind::Living,
                CaptureKind::Liv,
                CaptureKind::Items,
                CaptureKind::Preposition,
            ]
        );
        assert_eq!(compile("'x'").unwrap().kinds, vec![]);
    }

    #[test]
    fn a_pattern_needs_no_verb_and_a_rule_still_does() {
        let compiled = compile_pattern("[the] %i").unwrap();
        assert!(compiled.verbs.is_empty());
        assert_eq!(compiled.kinds, vec![CaptureKind::Items]);
        assert_eq!(
            compiled.captures_of("the red sword").next().unwrap()[0].text,
            "red sword"
        );
        assert!(parse(&compiled.grammar, "sword").next().is_some());

        let with_verb = compile_pattern(" 'get' / 'take' %i ").unwrap();
        assert!(with_verb.verbs.is_empty());
        assert!(parse(&with_verb.grammar, "take sword").next().is_some());
        assert!(parse(&with_verb.grammar, "sword").next().is_none());

        assert_eq!(compile("%i").unwrap_err(), PatternError::NoVerb);
        assert_eq!(
            compile_pattern("look").unwrap_err(),
            PatternError::UnquotedWord("look".into())
        );
    }

    #[test]
    fn the_two_dialects_cache_separately() {
        let rule = compile("'get' %i").unwrap();
        let pattern = compile_pattern("'get' %i").unwrap();
        assert!(!Arc::ptr_eq(&rule.grammar, &pattern.grammar));
        assert!(Arc::ptr_eq(
            &pattern.grammar,
            &compile_pattern("'get' %i").unwrap().grammar
        ));
    }

    #[test]
    fn captures_of_reads_kinds_from_the_table_in_slot_order() {
        let c = compile_pattern("'give' %w 'to' %o %d").unwrap();
        let sets: Vec<Vec<Capture>> = c.captures_of("give sword to bob 3").collect();
        assert_eq!(sets.len(), 1);
        let kinds: Vec<CaptureKind> = sets[0].iter().map(|c| c.kind).collect();
        let texts: Vec<&str> = sets[0].iter().map(|c| c.text.as_str()).collect();
        let slots: Vec<u32> = sets[0].iter().map(|c| c.slot).collect();
        assert_eq!(
            kinds,
            vec![CaptureKind::Word, CaptureKind::Object, CaptureKind::Number]
        );
        assert_eq!(texts, vec!["sword", "bob", "3"]);
        assert_eq!(slots, vec![0, 1, 2]);
    }

    #[test]
    fn a_number_that_does_not_fit_an_int_makes_the_parse_no_match() {
        let c = compile_pattern("'n' %d").unwrap();
        assert_eq!(c.captures_of("n 99999999999999999999").count(), 0);
        assert_eq!(c.captures_of("n 7").count(), 1);
    }

    #[test]
    fn captures_of_yields_greedy_splits_first() {
        let c = compile_pattern("'x' %s %s").unwrap();
        let firsts: Vec<String> = c
            .captures_of("x a b c")
            .map(|caps| caps[0].text.clone())
            .collect();
        assert_eq!(firsts, vec!["a b c", "a b", "a", ""]);
    }

    #[test]
    fn kinds_know_their_shape() {
        assert!(CaptureKind::Object.is_object() && !CaptureKind::Object.is_many());
        assert!(CaptureKind::Items.is_object() && CaptureKind::Items.is_many());
        assert!(CaptureKind::Liv.is_object() && !CaptureKind::Liv.is_many());
        assert!(CaptureKind::Living.is_object() && CaptureKind::Living.is_many());
        assert!(!CaptureKind::Word.is_object() && !CaptureKind::Words.is_object());
        assert!(!CaptureKind::Number.is_object() && !CaptureKind::Preposition.is_object());
    }

    #[test]
    fn capture_error_texts_list_every_letter() {
        assert_eq!(
            PatternError::BareCapture.to_string(),
            "`%` must be followed by w, s, d, o, l, L, i or p"
        );
        assert_eq!(
            PatternError::UnknownCapture('x').to_string(),
            "`%x` is not a capture; use %w, %s, %d, %o, %l, %L, %i or %p"
        );
    }

    #[test]
    fn percent_capital_l_is_one_living() {
        let c = compile_pattern("'kick' %L").unwrap();
        assert_eq!(c.kinds, vec![CaptureKind::Liv]);
        assert_eq!(CaptureKind::Liv.resolver_kind(), Some(Kind::Liv));
    }

    #[test]
    fn an_empty_pattern_is_the_bare_verb_for_compile_pattern_only() {
        let c = compile_pattern("").unwrap();
        assert!(parse(&c.grammar, "").next().is_some());
        assert!(parse(&c.grammar, "x").next().is_none());
        assert_eq!(compile("").unwrap_err(), PatternError::Empty);
    }
}
