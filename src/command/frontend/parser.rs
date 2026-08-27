//! The parser package's rule shape: `parse_add_rule`'s tokens rewritten
//! into the native dialect, with the handler slugs and slots the protocol
//! needs.

use std::{fmt, sync::Arc};

use ustr::Ustr;

use crate::command::frontend::native::{self, CaptureKind, Compiled, PatternError};

/// Why a rule does not compile; the text reaches the LPC caller.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserRuleError {
    /// Two `STR` tokens.
    TwoStrs(String),
    /// A token glued to other letters (`OBJect`).
    TokenInWord(String),
    /// A word holding `'`, which the native dialect quotes words with.
    QuoteInWord(String),
    /// More than two object slots.
    TooManyObjects(String),
    /// The rewritten pattern did not compile.
    Pattern(PatternError),
}

impl fmt::Display for ParserRuleError {
    /// Display the error.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserRuleError::TwoStrs(rule) => write!(f, "two STR tokens in '{rule}'"),
            ParserRuleError::TokenInWord(rule) => write!(f, "a token inside a word in '{rule}'"),
            ParserRuleError::QuoteInWord(rule) => write!(f, "a quote inside a word in '{rule}'"),
            ParserRuleError::TooManyObjects(rule) => {
                write!(f, "more than two object slots in '{rule}'")
            }
            ParserRuleError::Pattern(e) => write!(f, "{e}"),
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
    /// The rule compiled as a verbless native pattern.
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
    if word.contains('\'') {
        return Err(ParserRuleError::QuoteInWord(rule.to_owned()));
    }
    Ok(Token::Literal(word.to_owned()))
}

/// The native-dialect element for a token.
fn pattern_element(token: &Token) -> String {
    match token {
        Token::Capture(CaptureKind::Object) => "%o".to_owned(),
        Token::Capture(CaptureKind::Items) => "%i".to_owned(),
        Token::Capture(CaptureKind::Liv) => "%L".to_owned(),
        Token::Capture(CaptureKind::Living) => "%l".to_owned(),
        Token::Capture(CaptureKind::Word) => "%w".to_owned(),
        Token::Capture(CaptureKind::Words) => "%s".to_owned(),
        Token::Capture(CaptureKind::Number) => "%d".to_owned(),
        Token::Capture(CaptureKind::Preposition) => "%p".to_owned(),
        Token::Literal(word) => format!("'{word}'"),
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

/// Compile `rule` for `verb`: the tokens' pattern through the native
/// compiler and both slugs; the captures' kinds are `compiled.kinds`.
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
    let pattern = tokens
        .iter()
        .map(pattern_element)
        .collect::<Vec<_>>()
        .join(" ");
    let compiled = native::compile_pattern(&pattern).map_err(ParserRuleError::Pattern)?;
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
        assert!(
            crate::command::grammar::parse(g, "at sword")
                .next()
                .is_some()
        );
        assert!(
            crate::command::grammar::parse(g, "AT sword")
                .next()
                .is_none()
        );
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
        assert_eq!(
            compile("look", "at bob's OBJ").unwrap_err().to_string(),
            "a quote inside a word in 'at bob's OBJ'"
        );
    }
}
