//! The add_action family's rule shape: a verb as a grammar, the first-word
//! pre-filter, and the handler's argument taken from the token offsets.

use std::sync::{Arc, LazyLock};

use crate::{
    command::{
        grammar::{Grammar, GrammarBuilder, Label, Limits, Parse, lit, nt, parse},
        memo::Memo,
        registry::{ArgSpan, Reported, VerbMatch},
    },
    interpreter::{lpc_ref::LpcRef, lpc_string::LpcString},
};

/// The cache key: every prefix rule shares one grammar, an exact verb gets
/// its own.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Key {
    /// `S → 'verb' words_star⟨0⟩` for this verb.
    Exact(String),
    /// `S → word_like words_star⟨0⟩`, whatever the verb.
    Prefix(VerbMatch),
}

/// Verb grammars kept, least recently used first out.
const GRAMMAR_CACHE: usize = 1024;

static GRAMMARS: LazyLock<Memo<Key, Arc<Grammar>>> = LazyLock::new(|| Memo::new(GRAMMAR_CACHE));

/// The grammar for `verb` under `matching`, built once per key.
pub fn grammar_for(verb: &str, matching: VerbMatch) -> Arc<Grammar> {
    let key = match matching {
        VerbMatch::Exact => Key::Exact(verb.to_owned()),
        VerbMatch::Prefix { .. } => Key::Prefix(matching),
    };
    GRAMMARS.get_or_build(&key, || Arc::new(build(verb, matching)))
}

/// `S → 'verb' words_star⟨0⟩`, or `S → word_like words_star⟨0⟩` for a prefix
/// verb, whose first word [`verb_matches`] has already established.
fn build(verb: &str, matching: VerbMatch) -> Grammar {
    let mut b = GrammarBuilder::new();
    let s = b.nonterminal("S");
    let w = b.words_tokens();
    let star = b.words_star(&w);
    match matching {
        VerbMatch::Exact => {
            b.production(s, [lit(verb), nt(star).labeled(Label(0))]);
        }
        VerbMatch::Prefix { .. } => {
            let word_like = b.word_like(&w);
            b.production(s, [nt(word_like), nt(star).labeled(Label(0))]);
        }
    }
    b.build(s)
        .unwrap_or_else(|e| unreachable!("a verb grammar is built from fixed rules: {e}"))
}

/// Whether `first_word` can start a line for this verb; the cheap check
/// before the grammar runs.
pub fn verb_matches(verb: &str, matching: VerbMatch, first_word: &str) -> bool {
    match matching {
        VerbMatch::Exact => first_word == verb,
        VerbMatch::Prefix { .. } => first_word.starts_with(verb),
    }
}

/// The handler's argument: the rest of the line after an exact verb, or
/// the span `ArgSpan` names after a prefix verb, spacing intact.
pub fn argument(verb: &str, matching: VerbMatch, parse: &Parse, line: &str) -> String {
    match matching {
        VerbMatch::Exact => parse
            .captures()
            .into_iter()
            .find(|(label, _)| *label == Label(0))
            .map(|(_, text)| text.to_owned())
            .unwrap_or_default(),
        VerbMatch::Prefix { args, .. } => {
            let first = &parse.tokens()[0].range;
            let after_verb = first.start + verb.len();
            match args {
                ArgSpan::RestOfWord => line[after_verb..first.end].to_owned(),
                ArgSpan::RestOfLine => line[after_verb..].trim_start().to_owned(),
            }
        }
    }
}

/// What `query_verb()` reports for this match.
pub fn reported_verb(verb: &str, matching: VerbMatch, parse: &Parse, line: &str) -> String {
    match matching {
        VerbMatch::Prefix {
            reports: Reported::Full,
            ..
        } => line[parse.tokens()[0].range.clone()].to_owned(),
        VerbMatch::Exact
        | VerbMatch::Prefix {
            reports: Reported::Registered,
            ..
        } => verb.to_owned(),
    }
}

/// The handler's one argument and the verb `query_verb()` reports, for a
/// line whose first word [`verb_matches`]; `None` when the grammar does
/// not parse it.
pub fn arguments_and_verb(
    verb: &str,
    matching: VerbMatch,
    line: &str,
) -> Option<(Vec<LpcRef>, String)> {
    let grammar = grammar_for(verb, matching);
    let parsed = parse(&grammar, line, Limits::default()).next()?;
    Some((
        vec![LpcString::from(argument(verb, matching, &parsed, line).as_str()).into()],
        reported_verb(verb, matching, &parsed, line),
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::registry::{ArgSpan, Reported};

    const SHORT: VerbMatch = VerbMatch::Prefix {
        reports: Reported::Full,
        args: ArgSpan::RestOfLine,
    };
    const NOSPACE: VerbMatch = VerbMatch::Prefix {
        reports: Reported::Registered,
        args: ArgSpan::RestOfLine,
    };
    const IMM_ARGS: VerbMatch = VerbMatch::Prefix {
        reports: Reported::Registered,
        args: ArgSpan::RestOfWord,
    };

    /// The dispatch pipeline for one rule: the pre-filter, then the grammar.
    fn first(verb: &str, matching: VerbMatch, line: &str) -> Option<(String, String)> {
        let first_word = line.split_whitespace().next().unwrap_or("");
        if !verb_matches(verb, matching, first_word) {
            return None;
        }
        let g = grammar_for(verb, matching);
        let p = parse(&g, line, Limits::default()).next()?;
        Some((
            argument(verb, matching, &p, line),
            reported_verb(verb, matching, &p, line),
        ))
    }

    #[test]
    fn an_exact_verb_takes_the_rest_of_the_line_verbatim() {
        assert_eq!(
            first("look", VerbMatch::Exact, "look   at   me"),
            Some(("at   me".into(), "look".into()))
        );
        assert_eq!(
            first("look", VerbMatch::Exact, "look"),
            Some((String::new(), "look".into()))
        );
        assert_eq!(first("look", VerbMatch::Exact, "lookat me"), None);
        assert_eq!(first("look", VerbMatch::Exact, "Look at me"), None);
    }

    #[test]
    fn a_short_verb_reports_the_typed_word_and_joins_the_line() {
        assert_eq!(
            first("'", SHORT, "'hello there"),
            Some(("hello there".into(), "'hello".into()))
        );
        assert_eq!(
            first("'", SHORT, "' hello"),
            Some(("hello".into(), "'".into()))
        );
        assert_eq!(first("'", SHORT, "say hi"), None);
    }

    #[test]
    fn a_nospace_verb_reports_the_registered_verb() {
        assert_eq!(
            first("'", NOSPACE, "'hello there"),
            Some(("hello there".into(), "'".into()))
        );
    }

    #[test]
    fn an_imm_args_verb_takes_only_the_rest_of_the_word() {
        assert_eq!(
            first("'", IMM_ARGS, "'hello there"),
            Some(("hello".into(), "'".into()))
        );
    }

    #[test]
    fn a_later_word_starting_with_the_verb_still_parses() {
        assert_eq!(
            first("'", SHORT, "'he said 'hi'"),
            Some(("he said 'hi'".into(), "'he".into()))
        );
        assert_eq!(
            first("kill", SHORT, "kill killer"),
            Some(("killer".into(), "kill".into()))
        );
    }

    #[test]
    fn a_prefix_verb_with_regex_characters_is_literal() {
        assert_eq!(
            first("*", SHORT, "*wave"),
            Some(("wave".into(), "*wave".into()))
        );
    }

    #[test]
    fn the_pre_filter_agrees_with_the_grammar() {
        assert!(verb_matches("look", VerbMatch::Exact, "look"));
        assert!(!verb_matches("look", VerbMatch::Exact, "lookat"));
        assert!(verb_matches("'", SHORT, "'hello"));
        assert!(!verb_matches("'", SHORT, "hello"));
    }

    #[test]
    fn grammars_are_cached_per_verb_and_matching() {
        assert!(std::sync::Arc::ptr_eq(
            &grammar_for("look", VerbMatch::Exact),
            &grammar_for("look", VerbMatch::Exact)
        ));
        assert!(!std::sync::Arc::ptr_eq(
            &grammar_for("look", VerbMatch::Exact),
            &grammar_for("look", SHORT)
        ));
        assert!(!std::sync::Arc::ptr_eq(
            &grammar_for("look", VerbMatch::Exact),
            &grammar_for("peek", VerbMatch::Exact)
        ));
    }

    #[test]
    fn every_prefix_verb_shares_one_grammar() {
        assert!(std::sync::Arc::ptr_eq(
            &grammar_for("'", SHORT),
            &grammar_for("kill", SHORT)
        ));
    }
}
