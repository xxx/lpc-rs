//! The parser package's rule shape: `parse_add_rule`'s tokens as pattern
//! groups, with the handler slugs the protocol needs.

use std::{
    fmt,
    sync::{Arc, LazyLock},
};

use ustr::Ustr;

use crate::command::{
    frontend::native::{self, CaptureKind, Compiled, Group},
    grammar::GrammarError,
    memo::Memo,
};

/// Why a rule does not compile; the text reaches the LPC caller.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParserRuleError {
    /// An empty or whitespace-only verb name.
    EmptyVerb,
    /// A token glued to other letters (`OBJect`).
    TokenInWord(String),
    /// An unsupported modifier, or a modifier on a text token.
    Modifier(String),
    /// More than two object slots.
    TooManyObjects(String),
    /// The engine rejected the built grammar.
    Grammar(GrammarError),
}

impl fmt::Display for ParserRuleError {
    /// Display the error.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParserRuleError::EmptyVerb => write!(f, "the verb must contain at least one word"),
            ParserRuleError::TokenInWord(rule) => write!(f, "a token inside a word in '{rule}'"),
            ParserRuleError::Modifier(token) => write!(
                f,
                "unsupported parser modifier in '{token}'; only OBJ:v, OBS:v, LIV:v and LVS:v are supported"
            ),
            ParserRuleError::TooManyObjects(rule) => {
                write!(f, "more than two object slots in '{rule}'")
            }
            ParserRuleError::Grammar(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for ParserRuleError {}

/// Canonical parser key: whitespace-separated words, with spelling preserved.
pub(crate) fn normalize_verb(verb: &str) -> Result<Ustr, ParserRuleError> {
    let mut words = verb.split_whitespace();
    let first = words.next().ok_or(ParserRuleError::EmptyVerb)?;
    let mut key = first.to_owned();
    for word in words {
        key.push(' ');
        key.push_str(word);
    }
    Ok(key.into())
}

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
    /// Permission slug with literal words replaced by `word`.
    pub(crate) can_word_slug: Ustr,
    /// Action slug with literal words replaced by `word`.
    pub(crate) do_word_slug: Ustr,
    /// Semantic arguments for the literal-word handler tiers.
    pub(crate) word_args: Vec<HandlerArg>,
    /// Access requirement for each capture, in grammar order.
    pub(crate) access: Vec<Access>,
    /// The rule's tokens compiled as pattern groups.
    pub compiled: Arc<Compiled>,
}

/// The scope requirement for a parser capture.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Access {
    /// The object must be physically reachable.
    Reachable,
    /// Membership in the visible scope suffices.
    Visible,
}

/// An argument in the rule's literal-word handler layout.
#[derive(Clone, Debug)]
pub(crate) enum HandlerArg {
    /// A semantic capture value by capture index.
    Capture(usize),
    /// The registered spelling of a literal word.
    Literal(String),
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
    Capture(CaptureKind, Access),
    Literal(String),
}

fn token(word: &str, rule: &str) -> Result<Token, ParserRuleError> {
    if let Some((_, kind)) = TOKENS.iter().find(|(name, _)| *name == word) {
        return Ok(Token::Capture(*kind, Access::Reachable));
    }
    if let Some((name, modifier)) = word.split_once(':')
        && let Some((_, kind)) = TOKENS.iter().find(|(token, _)| *token == name)
    {
        if kind.is_object() && modifier == "v" {
            return Ok(Token::Capture(*kind, Access::Visible));
        }
        return Err(ParserRuleError::Modifier(word.to_owned()));
    }
    if TOKENS.iter().any(|(name, _)| word.starts_with(name)) {
        return Err(ParserRuleError::TokenInWord(rule.to_owned()));
    }
    Ok(Token::Literal(word.to_owned()))
}

/// The pattern group for a token.
fn group_of(token: &Token) -> Group {
    match token {
        Token::Capture(CaptureKind::Words, _) => Group::Text,
        Token::Capture(kind, _) => Group::Capture(*kind),
        Token::Literal(word) => Group::Words(vec![word.clone()]),
    }
}

/// The slug word for a token: `obj`/`liv` for many slots in the `can_`
/// family, `obs`/`lvs` in the `do_` family.
fn slug_word(token: &Token, do_family: bool) -> String {
    match token {
        Token::Capture(CaptureKind::Items, _) if !do_family => "obj".to_owned(),
        Token::Capture(CaptureKind::Living, _) if !do_family => "liv".to_owned(),
        Token::Capture(CaptureKind::Object, _) => "obj".to_owned(),
        Token::Capture(CaptureKind::Items, _) => "obs".to_owned(),
        Token::Capture(CaptureKind::Liv, _) => "liv".to_owned(),
        Token::Capture(CaptureKind::Living, _) => "lvs".to_owned(),
        Token::Capture(CaptureKind::Word, _) => "wrd".to_owned(),
        Token::Capture(CaptureKind::Words, _) => "str".to_owned(),
        Token::Capture(CaptureKind::Number, _) => "num".to_owned(),
        Token::Capture(CaptureKind::Preposition, _) => "prp".to_owned(),
        Token::Literal(word) => word.to_lowercase(),
    }
}

/// Compiled patterns kept, least recently used first out.
const PATTERN_CACHE: usize = 1024;

/// Compiled patterns by rule text, one per dialect like the native caches.
static COMPILED: LazyLock<Memo<String, Arc<Compiled>>> = LazyLock::new(|| Memo::new(PATTERN_CACHE));

/// The compiled pattern for `rule`, built from `tokens`, memoised by rule
/// text.
fn compiled_for(rule: &str, tokens: &[Token]) -> Result<Arc<Compiled>, ParserRuleError> {
    COMPILED.get_or_try_build(rule, || {
        let groups: Vec<Group> = tokens.iter().map(group_of).collect();
        native::compile_groups(&groups).map_err(ParserRuleError::Grammar)
    })
}

/// Compile `rule` for `verb`: the tokens as pattern groups through the
/// native builder, memoised by rule text, with handler slugs and argument layouts.
pub fn compile(verb: &str, rule: &str) -> Result<ParserRule, ParserRuleError> {
    let verb = normalize_verb(verb)?;
    let tokens = rule
        .split_whitespace()
        .map(|word| token(word, rule))
        .collect::<Result<Vec<_>, _>>()?;
    let kinds: Vec<CaptureKind> = tokens
        .iter()
        .filter_map(|t| match t {
            Token::Capture(kind, _) => Some(*kind),
            Token::Literal(_) => None,
        })
        .collect();
    if kinds.iter().filter(|k| k.is_object()).count() > 2 {
        return Err(ParserRuleError::TooManyObjects(rule.to_owned()));
    }
    let compiled = compiled_for(rule, &tokens)?;
    let slug = |do_family: bool, words: bool| -> Ustr {
        tokens
            .iter()
            .map(|t| match t {
                Token::Literal(_) if words => "word".to_owned(),
                _ => slug_word(t, do_family),
            })
            .collect::<Vec<_>>()
            .join("_")
            .into()
    };
    let access = tokens
        .iter()
        .filter_map(|token| match token {
            Token::Capture(_, access) => Some(*access),
            Token::Literal(_) => None,
        })
        .collect();
    let mut capture = 0;
    let word_args = tokens
        .iter()
        .map(|token| match token {
            Token::Capture(_, _) => {
                let arg = HandlerArg::Capture(capture);
                capture += 1;
                arg
            }
            Token::Literal(word) => HandlerArg::Literal(word.clone()),
        })
        .collect();
    Ok(ParserRule {
        verb,
        rule: rule.to_owned(),
        can_slug: slug(false, false),
        do_slug: slug(true, false),
        can_word_slug: slug(false, true),
        do_word_slug: slug(true, true),
        word_args,
        access,
        compiled,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::grammar::{Limits, parse};

    #[test]
    fn verb_keys_normalize_whitespace_without_changing_spelling_or_rule_text() {
        let rule = compile(" \tPréndre\u{2003} l'objet\n", " OBJ:v ").unwrap();
        assert_eq!(rule.verb.as_str(), "Préndre l'objet");
        assert_eq!(rule.rule, " OBJ:v ");
    }

    #[test]
    fn empty_verb_keys_are_rejected() {
        for verb in ["", " \t\n", "\u{2003}"] {
            assert_eq!(compile(verb, "").unwrap_err(), ParserRuleError::EmptyVerb);
        }
    }

    #[test]
    fn visible_modifiers_keep_capture_kinds_and_handler_slugs() {
        for token in ["OBJ", "OBS", "LIV", "LVS"] {
            let plain = compile("show", &format!("WRD {token} to OBJ")).unwrap();
            let visible = compile("show", &format!("WRD {token}:v to OBJ")).unwrap();
            assert_eq!(visible.compiled.kinds, plain.compiled.kinds);
            assert_eq!(visible.can_slug, plain.can_slug);
            assert_eq!(visible.do_slug, plain.do_slug);
            assert_eq!(visible.can_word_slug, plain.can_word_slug);
            assert_eq!(visible.do_word_slug, plain.do_word_slug);
            assert_eq!(
                visible.access,
                [Access::Reachable, Access::Visible, Access::Reachable]
            );
            assert_eq!(visible.compiled.captures_of("blue gem to bob").count(), 1);
        }
    }

    #[test]
    fn unsupported_modifiers_are_diagnosed_explicitly() {
        for token in ["OBJ", "OBS", "LIV", "LVS", "WRD", "STR"] {
            for suffix in ["", "x", "l", "p", "c", "vv", "vp", "v:"] {
                let word = format!("{token}:{suffix}");
                assert_eq!(
                    compile("test", &word).unwrap_err(),
                    ParserRuleError::Modifier(word)
                );
            }
        }
        for token in ["WRD:v", "STR:v"] {
            assert_eq!(
                compile("test", token).unwrap_err(),
                ParserRuleError::Modifier(token.into())
            );
        }
    }

    #[test]
    fn cached_patterns_do_not_share_access_requirements() {
        let visible = compile("see", "OBJ:v").unwrap();
        let reachable = compile("touch", "OBJ").unwrap();
        let synonym = compile("view", "OBJ:v").unwrap();
        assert_eq!(visible.access, [Access::Visible]);
        assert_eq!(reachable.access, [Access::Reachable]);
        assert_eq!(synonym.access, visible.access);
        assert!(Arc::ptr_eq(&visible.compiled, &synonym.compiled));
    }

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

    fn captured_text(rule: &str, input: &str) -> Vec<Vec<String>> {
        compile("test", rule)
            .unwrap()
            .compiled
            .captures_of(input)
            .map(|captures| captures.into_iter().map(|c| c.text).collect())
            .collect()
    }

    #[test]
    fn multiple_strings_try_longer_earlier_captures_first() {
        assert_eq!(
            captured_text("STR at STR", "spark at dusk at tower"),
            vec![
                vec!["spark at dusk", "tower"],
                vec!["spark", "dusk at tower"]
            ]
        );
    }

    #[test]
    fn adjacent_strings_allow_more_than_two_captures() {
        assert_eq!(
            captured_text("STR STR STR", "a b c d"),
            vec![
                vec!["a b", "c", "d"],
                vec!["a", "b c", "d"],
                vec!["a", "b", "c d"]
            ]
        );
        assert_eq!(
            captured_text("STR STR STR STR", "a b c d"),
            vec![vec!["a", "b", "c", "d"]]
        );
    }

    #[test]
    fn every_string_requires_words_and_separators_match_exactly() {
        for input in ["", "at", "a at", "at b", "a AT b", "a at, b"] {
            assert!(captured_text("STR at STR", input).is_empty(), "{input:?}");
        }
        for input in ["", "a", "a b"] {
            assert!(captured_text("STR STR STR", input).is_empty(), "{input:?}");
        }
    }

    #[test]
    fn multiple_strings_keep_unicode_spacing_and_equal_values_in_separate_slots() {
        assert_eq!(
            captured_text("STR at STR", "  Éclair!\t bleu at 遠い\u{2003}塔  "),
            vec![vec!["Éclair!\t bleu", "遠い\u{2003}塔"]]
        );
        assert_eq!(
            captured_text("STR at STR", "same at same"),
            vec![vec!["same", "same"]]
        );
    }

    #[test]
    fn strings_do_not_count_towards_the_two_object_limit() {
        let rule = compile("cast", "STR OBJ:v STR LVS STR").unwrap();
        assert_eq!(
            rule.compiled.kinds,
            [
                CaptureKind::Words,
                CaptureKind::Object,
                CaptureKind::Words,
                CaptureKind::Living,
                CaptureKind::Words
            ]
        );
        assert_eq!(rule.can_slug, "str_obj_str_liv_str");
        assert_eq!(rule.do_slug, "str_obj_str_lvs_str");
        assert_eq!(
            rule.access,
            [
                Access::Reachable,
                Access::Visible,
                Access::Reachable,
                Access::Reachable,
                Access::Reachable
            ]
        );
        let bad = "STR OBJ STR LIV STR OBJ";
        assert_eq!(
            compile("cast", bad).unwrap_err(),
            ParserRuleError::TooManyObjects(bad.into())
        );
    }

    #[test]
    fn multiple_strings_retain_the_default_parse_cap() {
        let rule = compile("test", "STR STR STR").unwrap();
        let input = "a b c d e f g h i j";
        assert_eq!(rule.compiled.captures_of(input).count(), 32);
        assert_eq!(
            parse(
                &rule.compiled.grammar,
                input,
                Limits {
                    max_parses: 100,
                    ..Limits::default()
                }
            )
            .count(),
            36
        );
    }

    #[test]
    fn multiple_string_patterns_share_the_cache_but_keep_their_verb() {
        let first = compile("cast", "STR at STR").unwrap();
        let second = compile("aim", "STR at STR").unwrap();
        assert!(Arc::ptr_eq(&first.compiled, &second.compiled));
        assert_eq!(first.verb, "cast");
        assert_eq!(second.verb, "aim");
    }
}
