//! Token rules compiled into one anchored DFA, DGD's tokenizer model: the
//! longest match at each position wins and the earliest rule breaks ties.

use std::ops::Range;

use regex_automata::{
    Anchored, Input, MatchKind,
    dfa::{Automaton, StartKind, dense},
    util::syntax,
};

use super::model::{GrammarError, TokenClass, TokenRule};

/// One token: its rule and its byte range in the input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// The token rule that matched.
    pub class: TokenClass,
    /// Byte range into the input.
    pub range: Range<usize>,
}

/// Every token rule as one multi-pattern DFA; the pattern id is the class.
#[derive(Debug)]
pub(crate) struct TokenSet {
    dfa: Option<dense::DFA<Vec<u32>>>,
    skip: Vec<bool>,
}

impl TokenSet {
    pub(crate) fn build(rules: &[TokenRule], case_insensitive: bool) -> Result<Self, GrammarError> {
        let syntax = syntax::Config::new().case_insensitive(case_insensitive);

        // A per-rule build attributes a syntax error to its rule; the joint build cannot.
        for rule in rules {
            dense::Builder::new()
                .syntax(syntax)
                .build(&rule.pattern)
                .map_err(|e| GrammarError::BadRegex {
                    class: rule.name.clone(),
                    message: e.to_string(),
                })?;
        }

        let patterns: Vec<&str> = rules.iter().map(|r| r.pattern.as_str()).collect();
        let dfa = if patterns.is_empty() {
            None
        } else {
            Some(
                dense::Builder::new()
                    .syntax(syntax)
                    .configure(
                        dense::Config::new()
                            .match_kind(MatchKind::All)
                            .start_kind(StartKind::Anchored),
                    )
                    .build_many(&patterns)
                    .map_err(|e| GrammarError::DfaBuild(e.to_string()))?,
            )
        };

        Ok(TokenSet {
            dfa,
            skip: rules.iter().map(|r| r.skip).collect(),
        })
    }

    /// The tokens of `input`, or `None` at the first position no rule matches;
    /// an empty match counts as no match.
    pub(crate) fn tokenize(&self, input: &str) -> Option<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut pos = 0;
        while pos < input.len() {
            let dfa = self.dfa.as_ref()?;
            let found = dfa
                .try_search_fwd(&Input::new(&input[pos..]).anchored(Anchored::Yes))
                .ok()
                .flatten()?;
            if found.offset() == 0 {
                return None;
            }
            let class = TokenClass(found.pattern().as_u32());
            let end = pos + found.offset();
            if !self.skip[class.0 as usize] {
                tokens.push(Token {
                    class,
                    range: pos..end,
                });
            }
            pos = end;
        }
        Some(tokens)
    }
}

/// One tokenized input: the text, its tokens, and the folded token texts when
/// the grammar matches case-insensitively.
#[derive(Debug)]
pub struct Scan {
    input: String,
    tokens: Vec<Token>,
    folded: Option<Vec<String>>,
}

impl Scan {
    pub(crate) fn new(input: &str, tokens: Vec<Token>, case_insensitive: bool) -> Self {
        let folded = case_insensitive.then(|| {
            tokens
                .iter()
                .map(|t| input[t.range.clone()].to_lowercase())
                .collect()
        });
        Scan {
            input: input.to_owned(),
            tokens,
            folded,
        }
    }

    /// The tokens in this scan.
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    /// The original input text.
    pub fn input(&self) -> &str {
        &self.input
    }

    /// The original text of token `i`.
    pub fn token_text(&self, i: usize) -> &str {
        &self.input[self.tokens[i].range.clone()]
    }

    /// The text literals compare against: folded under case-insensitivity.
    #[cfg_attr(not(test), expect(dead_code, reason = "used once the parser lands"))]
    pub(crate) fn match_text(&self, i: usize) -> &str {
        match &self.folded {
            Some(folded) => &folded[i],
            None => self.token_text(i),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rules(specs: &[(&str, &str, bool)]) -> Vec<TokenRule> {
        specs
            .iter()
            .map(|(name, pattern, skip)| TokenRule {
                name: (*name).to_owned(),
                pattern: (*pattern).to_owned(),
                skip: *skip,
            })
            .collect()
    }

    fn set(specs: &[(&str, &str, bool)]) -> TokenSet {
        TokenSet::build(&rules(specs), false).unwrap()
    }

    const DGD_PROBE: &[(&str, &str, bool)] = &[
        ("a_or_ab", "a|ab", false),
        ("ab", "ab", false),
        ("word", "[a-z]+", false),
        ("ws", r"\s+", true),
        ("op", "<|<=|>|>=", false),
    ];

    #[test]
    fn longest_match_wins_across_rules() {
        let tokens = set(DGD_PROBE).tokenize("abc x").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    class: TokenClass(2),
                    range: 0..3
                },
                Token {
                    class: TokenClass(2),
                    range: 4..5
                },
            ]
        );
    }

    #[test]
    fn alternation_inside_a_rule_is_longest_match() {
        let tokens = set(DGD_PROBE).tokenize("ab").unwrap();
        assert_eq!(
            tokens,
            vec![Token {
                class: TokenClass(0),
                range: 0..2
            }]
        );
        let tokens = set(DGD_PROBE).tokenize("<=").unwrap();
        assert_eq!(
            tokens,
            vec![Token {
                class: TokenClass(4),
                range: 0..2
            }]
        );
    }

    #[test]
    fn ties_go_to_the_earliest_rule() {
        let tokens = set(DGD_PROBE).tokenize("a").unwrap();
        assert_eq!(
            tokens,
            vec![Token {
                class: TokenClass(0),
                range: 0..1
            }]
        );
    }

    #[test]
    fn skip_rules_drop_tokens_and_keep_offsets() {
        let tokens = set(&[("ws", r"\s+", true), ("word", r"\S+", false)])
            .tokenize("a  b")
            .unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    class: TokenClass(1),
                    range: 0..1
                },
                Token {
                    class: TokenClass(1),
                    range: 3..4
                },
            ]
        );
    }

    #[test]
    fn unmatched_input_is_none() {
        assert_eq!(set(&[("word", "[a-z]+", false)]).tokenize("a!b"), None);
    }

    #[test]
    fn an_empty_match_is_none() {
        assert_eq!(set(&[("stars", "a*", false)]).tokenize("b"), None);
    }

    #[test]
    fn empty_input_has_no_tokens() {
        assert_eq!(set(&[("word", "[a-z]+", false)]).tokenize(""), Some(vec![]));
    }

    #[test]
    fn no_rules_tokenizes_only_the_empty_input() {
        let set = TokenSet::build(&[], false).unwrap();
        assert_eq!(set.tokenize(""), Some(vec![]));
        assert_eq!(set.tokenize("x"), None);
    }

    #[test]
    fn bad_regex_names_the_rule() {
        let err = TokenSet::build(&rules(&[("ok", "a", false), ("broken", "(", false)]), false)
            .unwrap_err();
        assert!(
            matches!(&err, GrammarError::BadRegex { class, .. } if class == "broken"),
            "{err:?}"
        );
    }

    #[test]
    fn case_insensitive_matches_and_folds() {
        let set = TokenSet::build(&rules(&[("word", "[a-z]+", false)]), true).unwrap();
        let tokens = set.tokenize("ABC").unwrap();
        assert_eq!(
            tokens,
            vec![Token {
                class: TokenClass(0),
                range: 0..3
            }]
        );
        let scan = Scan::new("ABC", tokens, true);
        assert_eq!(scan.token_text(0), "ABC");
        assert_eq!(scan.match_text(0), "abc");
    }

    #[test]
    fn case_sensitive_scan_matches_the_original_text() {
        let set = set(&[("word", "[A-Za-z]+", false)]);
        let tokens = set.tokenize("Abc").unwrap();
        let scan = Scan::new("Abc", tokens, false);
        assert_eq!(scan.match_text(0), "Abc");
        assert_eq!(scan.tokens().len(), 1);
        assert_eq!(scan.input(), "Abc");
    }
}
