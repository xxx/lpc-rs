//! The parser package's rule shape: `parse_add_rule`'s tokens rewritten
//! into the native dialect, with the handler slugs and slots the protocol
//! needs.

use std::fmt;

use ustr::Ustr;

use crate::command::{
    frontend::native::{self, PatternError},
    registry::{ParserRule, Slot},
};

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

const TOKENS: [(&str, Slot); 6] = [
    ("OBJ", Slot::Object),
    ("OBS", Slot::Objects),
    ("LIV", Slot::Living),
    ("LVS", Slot::Livings),
    ("WRD", Slot::Word),
    ("STR", Slot::Words),
];

/// One word of a rule: a capturing token or a literal.
enum Token {
    Slot(Slot),
    Literal(String),
}

fn token(word: &str, rule: &str) -> Result<Token, ParserRuleError> {
    if let Some((_, slot)) = TOKENS.iter().find(|(name, _)| *name == word) {
        return Ok(Token::Slot(*slot));
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
        Token::Slot(Slot::Object) => "%o".to_owned(),
        Token::Slot(Slot::Objects) => "%i".to_owned(),
        Token::Slot(Slot::Living) => "%L".to_owned(),
        Token::Slot(Slot::Livings) => "%l".to_owned(),
        Token::Slot(Slot::Word) => "%w".to_owned(),
        Token::Slot(Slot::Words) => "%s".to_owned(),
        Token::Literal(word) => format!("'{word}'"),
    }
}

/// The slug word for a token: `obj`/`liv` for many slots in the `can_`
/// family, `obs`/`lvs` in the `do_` family.
fn slug_word(token: &Token, do_family: bool) -> String {
    match token {
        Token::Slot(Slot::Objects) if !do_family => "obj".to_owned(),
        Token::Slot(Slot::Livings) if !do_family => "liv".to_owned(),
        Token::Slot(Slot::Object) => "obj".to_owned(),
        Token::Slot(Slot::Objects) => "obs".to_owned(),
        Token::Slot(Slot::Living) => "liv".to_owned(),
        Token::Slot(Slot::Livings) => "lvs".to_owned(),
        Token::Slot(Slot::Word) => "wrd".to_owned(),
        Token::Slot(Slot::Words) => "str".to_owned(),
        Token::Literal(word) => word.to_lowercase(),
    }
}

/// Compile `rule` for `verb`: the tokens' pattern through the native
/// compiler, the slots in rule order, and both slugs.
pub fn compile(verb: &str, rule: &str) -> Result<ParserRule, ParserRuleError> {
    let tokens = rule
        .split_whitespace()
        .map(|word| token(word, rule))
        .collect::<Result<Vec<_>, _>>()?;
    let slots: Vec<Slot> = tokens
        .iter()
        .filter_map(|t| match t {
            Token::Slot(slot) => Some(*slot),
            Token::Literal(_) => None,
        })
        .collect();
    if slots.iter().filter(|s| **s == Slot::Words).count() > 1 {
        return Err(ParserRuleError::TwoStrs(rule.to_owned()));
    }
    if slots.iter().filter(|s| s.is_object()).count() > 2 {
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
        slots,
        compiled,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::frontend::native::CaptureKind;

    #[test]
    fn tokens_map_to_pattern_captures_and_slots() {
        let r = compile("give", "OBJ to LIV").unwrap();
        assert_eq!(r.verb, "give");
        assert_eq!(r.rule, "OBJ to LIV");
        assert_eq!(r.slots, vec![Slot::Object, Slot::Living]);
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
        assert_eq!(compile("kill", "LVS").unwrap().slots, vec![Slot::Livings]);
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
        assert!(bare.slots.is_empty());
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
