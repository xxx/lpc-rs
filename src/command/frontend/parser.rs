//! The parser package's rule shape: `parse_add_rule`'s tokens as pattern
//! groups, with the handler slugs the protocol needs.

use std::{
    fmt,
    sync::{Arc, LazyLock},
};

use dashmap::DashMap;
use ustr::Ustr;

use crate::command::{
    frontend::native::{self, CaptureKind, Compiled, Group},
    grammar::GrammarError,
};

/// Why a rule does not compile; the text reaches the LPC caller.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserRuleError {
    /// Two `STR` tokens.
    TwoStrs(String),
    /// A token glued to other letters (`OBJect`).
    TokenInWord(String),
    /// More than two object slots.
    TooManyObjects(String),
    /// The engine rejected the built grammar.
    Grammar(GrammarError),
}

impl fmt::Display for ParserRuleError {
    /// Display the error.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserRuleError::TwoStrs(rule) => write!(f, "two STR tokens in '{rule}'"),
            ParserRuleError::TokenInWord(rule) => write!(f, "a token inside a word in '{rule}'"),
            ParserRuleError::TooManyObjects(rule) => {
                write!(f, "more than two object slots in '{rule}'")
            }
            ParserRuleError::Grammar(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for ParserRuleError {}

/// One `parse_add_rule` rule: what its handlers are named and how the
/// line's captures map onto their arguments.
#[derive(Clone, Debug)]
pub struct ParserRule {
    /// The base verb the handlers are named for; a synonym keeps it.
    pub verb: Ustr,
    /// The rule as written.
    pub rule: String,
    /// The slug for `can_`, `direct_` and `indirect_` names (`at_obj`).
    pub can_slug: Ustr,
    /// The slug for `do_` names (`at_obs` for a many slot).
    pub do_slug: Ustr,
    /// The rule's tokens compiled as pattern groups.
    pub compiled: Arc<Compiled>,
}

const TOKENS: [(&str, CaptureKind); 6] = [
    ("OBJ", CaptureKind::Object),
    ("OBS", CaptureKind::Items),
    ("LIV", CaptureKind::Liv),
    ("LVS", CaptureKind::Living),
    ("WRD", CaptureKind::Word),
    ("STR", CaptureKind::Words),
];

/// One word of a rule: a capturing token or a literal.
enum Token {
    Capture(CaptureKind),
    Literal(String),
}

fn token(word: &str, rule: &str) -> Result<Token, ParserRuleError> {
    if let Some((_, kind)) = TOKENS.iter().find(|(name, _)| *name == word) {
        return Ok(Token::Capture(*kind));
    }
    if TOKENS.iter().any(|(name, _)| word.starts_with(name)) {
        return Err(ParserRuleError::TokenInWord(rule.to_owned()));
    }
    Ok(Token::Literal(word.to_owned()))
}

/// The pattern group for a token.
fn group_of(token: &Token) -> Group {
    match token {
        Token::Capture(CaptureKind::Words) => Group::Text,
        Token::Capture(kind) => Group::Capture(*kind),
        Token::Literal(word) => Group::Words(vec![word.clone()]),
    }
}

/// The slug word for a token: `obj`/`liv` for many slots in the `can_`
/// family, `obs`/`lvs` in the `do_` family.
fn slug_word(token: &Token, do_family: bool) -> String {
    match token {
        Token::Capture(CaptureKind::Items) if !do_family => "obj".to_owned(),
        Token::Capture(CaptureKind::Living) if !do_family => "liv".to_owned(),
        Token::Capture(CaptureKind::Object) => "obj".to_owned(),
        Token::Capture(CaptureKind::Items) => "obs".to_owned(),
        Token::Capture(CaptureKind::Liv) => "liv".to_owned(),
        Token::Capture(CaptureKind::Living) => "lvs".to_owned(),
        Token::Capture(CaptureKind::Word) => "wrd".to_owned(),
        Token::Capture(CaptureKind::Words) => "str".to_owned(),
        Token::Capture(CaptureKind::Number) => "num".to_owned(),
        Token::Capture(CaptureKind::Preposition) => "prp".to_owned(),
        Token::Literal(word) => word.to_lowercase(),
    }
}

/// Compiled patterns by rule text, one per dialect like the native caches.
static COMPILED: LazyLock<DashMap<String, Arc<Compiled>>> = LazyLock::new(DashMap::new);

/// The compiled pattern for `rule`, built from `tokens` once per rule text.
fn compiled_for(rule: &str, tokens: &[Token]) -> Result<Arc<Compiled>, ParserRuleError> {
    if let Some(hit) = COMPILED.get(rule) {
        return Ok(Arc::clone(&hit));
    }
    let groups: Vec<Group> = tokens.iter().map(group_of).collect();
    let compiled = native::compile_groups(&groups).map_err(ParserRuleError::Grammar)?;
    Ok(COMPILED.entry(rule.to_owned()).or_insert(compiled).clone())
}

/// Compile `rule` for `verb`: the tokens as pattern groups through the
/// native builder, once per rule text, and both slugs; the captures' kinds
/// are `compiled.kinds`.
pub fn compile(verb: &str, rule: &str) -> Result<ParserRule, ParserRuleError> {
    let tokens = rule
        .split_whitespace()
        .map(|word| token(word, rule))
        .collect::<Result<Vec<_>, _>>()?;
    let kinds: Vec<CaptureKind> = tokens
        .iter()
        .filter_map(|t| match t {
            Token::Capture(kind) => Some(*kind),
            Token::Literal(_) => None,
        })
        .collect();
    if kinds.iter().filter(|k| **k == CaptureKind::Words).count() > 1 {
        return Err(ParserRuleError::TwoStrs(rule.to_owned()));
    }
    if kinds.iter().filter(|k| k.is_object()).count() > 2 {
        return Err(ParserRuleError::TooManyObjects(rule.to_owned()));
    }
    let compiled = compiled_for(rule, &tokens)?;
    let slug = |do_family: bool| -> Ustr {
        tokens
            .iter()
            .map(|t| slug_word(t, do_family))
            .collect::<Vec<_>>()
            .join("_")
            .into()
    };
    Ok(ParserRule {
        verb: verb.into(),
        rule: rule.to_owned(),
        can_slug: slug(false),
        do_slug: slug(true),
        compiled,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::grammar::{Limits, parse};

    #[test]
    fn tokens_map_to_pattern_captures() {
        let r = compile("give", "OBJ to LIV").unwrap();
        assert_eq!(r.verb, "give");
        assert_eq!(r.rule, "OBJ to LIV");
        assert_eq!(
            r.compiled.kinds,
            vec![CaptureKind::Object, CaptureKind::Liv]
        );
        assert_eq!(
            compile("put", "OBS in OBJ").unwrap().compiled.kinds,
            vec![CaptureKind::Items, CaptureKind::Object]
        );
        assert_eq!(
            compile("say", "STR").unwrap().compiled.kinds,
            vec![CaptureKind::Words]
        );
        assert_eq!(
            compile("say", "WRD").unwrap().compiled.kinds,
            vec![CaptureKind::Word]
        );
        assert_eq!(
            compile("kill", "LVS").unwrap().compiled.kinds,
            vec![CaptureKind::Living]
        );
    }

    #[test]
    fn slugs_spell_many_slots_per_name_family() {
        let r = compile("look", "at OBS with OBJ").unwrap();
        assert_eq!(r.can_slug, "at_obj_with_obj");
        assert_eq!(r.do_slug, "at_obs_with_obj");
        let r = compile("kill", "LVS").unwrap();
        assert_eq!(r.can_slug, "liv");
        assert_eq!(r.do_slug, "lvs");
        let bare = compile("look", "").unwrap();
        assert_eq!(bare.can_slug, "");
        assert!(bare.compiled.kinds.is_empty());
        assert_eq!(compile("look", "at OBJ").unwrap().can_slug, "at_obj");
    }

    #[test]
    fn literal_words_are_matched_as_typed_and_case_kept() {
        let r = compile("look", "at OBJ");
        let g = &r.unwrap().compiled.grammar;
        assert!(parse(g, "at sword", Limits::default()).next().is_some());
        assert!(parse(g, "AT sword", Limits::default()).next().is_none());
    }

    #[test]
    fn faults_name_the_rule() {
        assert_eq!(
            compile("say", "STR STR").unwrap_err().to_string(),
            "two STR tokens in 'STR STR'"
        );
        assert_eq!(
            compile("look", "at OBJect").unwrap_err().to_string(),
            "a token inside a word in 'at OBJect'"
        );
        assert_eq!(
            compile("x", "OBJ OBJ OBJ").unwrap_err().to_string(),
            "more than two object slots in 'OBJ OBJ OBJ'"
        );
    }

    #[test]
    fn a_literal_with_a_quote_is_a_word_like_any_other() {
        let r = compile("look", "at bob's OBJ").unwrap();
        assert_eq!(r.can_slug, "at_bob's_obj");
        assert_eq!(
            r.compiled.captures_of("at bob's sword").next().unwrap()[0].text,
            "sword"
        );
    }

    #[test]
    fn str_is_one_or_more_words() {
        let r = compile("say", "STR").unwrap();
        assert!(r.compiled.captures_of("").next().is_none());
        assert_eq!(
            r.compiled.captures_of("hi there").next().unwrap()[0].text,
            "hi there"
        );
    }

    #[test]
    fn a_str_beside_an_object_slot_still_splits_greedily() {
        let r = compile("say", "STR to OBJ").unwrap();
        let parses: Vec<Vec<_>> = r.compiled.captures_of("a b to c d").collect();
        let firsts: Vec<&str> = parses.iter().map(|caps| caps[0].text.as_str()).collect();
        assert_eq!(firsts, vec!["a b"]);
        assert_eq!(parses[0][1].text, "c d");
    }

    #[test]
    fn a_rule_text_compiles_once_whatever_its_verb() {
        let a = compile("look", "at OBJ").unwrap();
        let b = compile("peer", "at OBJ").unwrap();
        assert!(Arc::ptr_eq(&a.compiled, &b.compiled));
    }
}
