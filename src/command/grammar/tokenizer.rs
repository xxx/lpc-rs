//! Token rules compiled into one anchored DFA, DGD's tokenizer model: the
//! longest match at each position wins and the earliest rule breaks ties.

use std::ops::Range;

use regex_automata::{
    Anchored, Input, MatchKind,
    dfa::{Automaton, StartKind, dense},
    util::syntax,
};

use super::model::{GrammarError, Pattern, TokenClass, TokenRule};

/// One token: its rule and its byte range in the input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    /// The token rule that matched.
    pub class: TokenClass,
    /// Byte range into the input.
    pub range: Range<usize>,
}

/// Every regex token rule as one multi-pattern DFA, plus the nomatch class
/// that takes the runs the DFA rejects.
#[derive(Debug)]
pub(crate) struct TokenSet {
    dfa: Option<dense::DFA<Vec<u32>>>,
    /// The class of each DFA pattern, in pattern order.
    classes: Vec<TokenClass>,
    /// Whether each class is dropped from the stream, by class.
    skip: Vec<bool>,
    nomatch: Option<TokenClass>,
}

impl TokenSet {
    /// Compile `rules` into one joint DFA; on failure, a per-rule build
    /// attributes the error to its rule, or to the combination if every rule
    /// builds alone. The last nomatch rule, if any, is the nomatch class.
    pub(crate) fn build(rules: &[TokenRule], case_insensitive: bool) -> Result<Self, GrammarError> {
        let syntax = syntax::Config::new().case_insensitive(case_insensitive);
        let mut patterns = Vec::new();
        let mut classes = Vec::new();
        let mut nomatch = None;
        for (i, rule) in rules.iter().enumerate() {
            let class = TokenClass(i as u32);
            match &rule.pattern {
                Pattern::Regex(pattern) => {
                    patterns.push((rule.name.as_str(), pattern.as_str()));
                    classes.push(class);
                }
                Pattern::Nomatch => nomatch = Some(class),
            }
        }

        let dfa = if patterns.is_empty() {
            None
        } else {
            let texts: Vec<&str> = patterns.iter().map(|(_, p)| *p).collect();
            match dense::Builder::new()
                .syntax(syntax)
                .configure(
                    dense::Config::new()
                        .match_kind(MatchKind::All)
                        .start_kind(StartKind::Anchored),
                )
                .build_many(&texts)
            {
                Ok(dfa) => Some(dfa),
                Err(e) => {
                    // The joint build failed; a per-rule build attributes the failure to its rule.
                    for (name, pattern) in &patterns {
                        dense::Builder::new()
                            .syntax(syntax)
                            .build(pattern)
                            .map_err(|e| GrammarError::BadRegex {
                                class: (*name).to_owned(),
                                message: e.to_string(),
                            })?;
                    }
                    return Err(GrammarError::DfaBuild(e.to_string()));
                }
            }
        };

        Ok(TokenSet {
            dfa,
            classes,
            skip: rules.iter().map(|r| r.skip).collect(),
            nomatch,
        })
    }

    /// The tokens of `input`, or `None` at the first position no rule matches
    /// when there is no nomatch rule; an empty match counts as no match.
    pub(crate) fn tokenize(&self, input: &str) -> Option<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut pos = 0;
        while pos < input.len() {
            let (class, end) = match self.longest_match(input, pos) {
                Some(found) => found,
                None => (self.nomatch?, self.next_match_start(input, pos)),
            };
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

    /// The longest non-empty match at `pos` and where it ends.
    fn longest_match(&self, input: &str, pos: usize) -> Option<(TokenClass, usize)> {
        let dfa = self.dfa.as_ref()?;
        // A MatchError is unreachable for an anchored dense DFA without quit
        // bytes, so `.ok().flatten()` only reshapes the type, never actually
        // maps a real error to `None`.
        let found = dfa
            .try_search_fwd(
                &Input::new(input)
                    .span(pos..input.len())
                    .anchored(Anchored::Yes),
            )
            .ok()
            .flatten()?;
        let end = found.offset();
        (end > pos).then(|| (self.classes[found.pattern().as_usize()], end))
    }

    /// The first character boundary after `from` where some rule matches, or
    /// the end of `input`.
    fn next_match_start(&self, input: &str, from: usize) -> usize {
        input[from..]
            .char_indices()
            .skip(1)
            .map(|(i, _)| from + i)
            .find(|&p| self.longest_match(input, p).is_some())
            .unwrap_or(input.len())
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
    /// Pair `tokens` with the input they were scanned from, folding each
    /// token's text to lowercase when the grammar is case-insensitive.
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
    pub(crate) fn match_text(&self, i: usize) -> &str {
        match &self.folded {
            Some(folded) => &folded[i],
            None => self.token_text(i),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::model::Pattern;
    use super::*;

    fn rules(specs: &[(&str, &str, bool)]) -> Vec<TokenRule> {
        specs
            .iter()
            .map(|(name, pattern, skip)| TokenRule {
                name: (*name).to_owned(),
                pattern: Pattern::Regex((*pattern).to_owned()),
                skip: *skip,
            })
            .collect()
    }

    /// `word`, skipped whitespace, and a nomatch rule named `other`.
    fn with_nomatch(skip_nomatch: bool) -> TokenSet {
        let mut rules = rules(&[("word", "[a-z]+", false), ("ws", r"\s+", true)]);
        rules.push(TokenRule {
            name: "other".to_owned(),
            pattern: Pattern::Nomatch,
            skip: skip_nomatch,
        });
        TokenSet::build(&rules, false).unwrap()
    }

    fn token(class: u32, range: Range<usize>) -> Token {
        Token {
            class: TokenClass(class),
            range,
        }
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

    #[test]
    fn multi_byte_input_keeps_byte_offsets() {
        let input = "look ünïcödé";
        let tokens = set(&[("whitespace", r"\s+", true), ("word", r"\S+", false)])
            .tokenize(input)
            .unwrap();
        let second_len = "ünïcödé".len();
        assert_eq!(
            tokens,
            vec![
                Token {
                    class: TokenClass(1),
                    range: 0..4
                },
                Token {
                    class: TokenClass(1),
                    range: 5..5 + second_len
                },
            ]
        );
    }

    #[test]
    fn a_nomatch_run_ends_where_the_next_rule_matches() {
        assert_eq!(
            with_nomatch(false).tokenize("ab!?cd").unwrap(),
            vec![token(0, 0..2), token(2, 2..4), token(0, 4..6)]
        );
    }

    #[test]
    fn a_nomatch_run_at_the_end_runs_to_the_end() {
        assert_eq!(
            with_nomatch(false).tokenize("ab!?").unwrap(),
            vec![token(0, 0..2), token(2, 2..4)]
        );
    }

    #[test]
    fn a_nomatch_run_stops_at_skipped_whitespace() {
        assert_eq!(
            with_nomatch(false).tokenize("ab !? cd").unwrap(),
            vec![token(0, 0..2), token(2, 3..5), token(0, 6..8)]
        );
    }

    #[test]
    fn a_skipped_nomatch_run_leaves_no_token() {
        assert_eq!(
            with_nomatch(true).tokenize("ab!?cd").unwrap(),
            vec![token(0, 0..2), token(0, 4..6)]
        );
    }

    #[test]
    fn a_nomatch_run_advances_by_whole_characters() {
        let input = "ab€cd";
        assert_eq!(
            with_nomatch(false).tokenize(input).unwrap(),
            vec![token(0, 0..2), token(2, 2..2 + "€".len()), token(0, 5..7)]
        );
    }
}
