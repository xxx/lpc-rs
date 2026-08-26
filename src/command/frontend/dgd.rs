//! DGD's `parse_string` grammar text: its lexer and rule parser, its regex
//! dialect translated to `regex-automata` syntax, and (Task 4) the compiler
//! onto the engine's builder with the grammar cache.

use std::{fmt, iter::Peekable, str::CharIndices};

use crate::command::grammar::GrammarError;

/// Why a grammar text failed to compile.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum DgdError {
    /// A fault in one rule, numbered from 1 in text order.
    Rule {
        /// The rule's number.
        number: usize,
        /// DGD's text for the fault.
        message: &'static str,
    },
    /// A name defined as a token rule and as a production rule.
    Redefined {
        /// The later rule's number.
        number: usize,
        /// `"token"` or `"production"`: the kind the name already had.
        kind: &'static str,
    },
    /// No `name = …` rule at all.
    NoTokens,
    /// No production rule at all.
    NoStartingRule,
    /// The engine rejected the built grammar.
    Grammar(GrammarError),
}

impl fmt::Display for DgdError {
    /// Display the error.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DgdError::Rule { number, message } => write!(f, "Rule {number}: {message}"),
            DgdError::Redefined { number, kind } => {
                write!(f, "Rule {number} previously defined as {kind} rule")
            }
            DgdError::NoTokens => write!(f, "No tokens"),
            DgdError::NoStartingRule => write!(f, "No starting rule"),
            DgdError::Grammar(e) => write!(f, "{e}"),
        }
    }
}

impl std::error::Error for DgdError {}

impl From<GrammarError> for DgdError {
    /// Wrap an engine error.
    fn from(e: GrammarError) -> Self {
        DgdError::Grammar(e)
    }
}

/// A token rule as written.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TokenRuleText {
    /// The rule's number in text order, from 1.
    pub number: usize,
    /// The token's name.
    pub name: String,
    /// What it matches.
    pub pattern: TokenPattern,
}

/// A token rule's right-hand side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TokenPattern {
    /// A regex, already translated to `regex-automata` syntax.
    Regex(String),
    /// `nomatch`.
    Nomatch,
}

/// A production rule as written.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ProductionText {
    /// The rule's number in text order, from 1.
    pub number: usize,
    /// The symbol it defines.
    pub lhs: String,
    /// Its right-hand side, in order.
    pub rhs: Vec<RhsText>,
    /// The `? func` action, if any.
    pub action: Option<String>,
}

/// One element of a production's right-hand side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum RhsText {
    /// A token or production name.
    Symbol(String),
    /// A `'string constant'`, unescaped.
    Constant(String),
}

/// A grammar text, parsed but not yet built.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Rules {
    /// Every `name = …` rule, in text order.
    pub token_rules: Vec<TokenRuleText>,
    /// Every `name : …` rule, in text order.
    pub productions: Vec<ProductionText>,
}

/// One lexical piece of a grammar text.
#[derive(Clone, Debug, PartialEq, Eq)]
enum Piece {
    Symbol(String),
    Colon,
    Equals,
    Question,
    Less,
    /// The raw text between the slashes.
    Regex(String),
    /// The unescaped text between the quotes.
    Constant(String),
    /// A character that begins no piece.
    Bad,
    /// A `/` never closed.
    UnterminatedRegex,
    /// A `'` never closed, or `''`.
    UnterminatedConstant,
}

/// Split `text` into pieces; a malformed regex or constant is a piece too,
/// so the rule parser can number the fault.
fn lex(text: &str) -> Vec<Piece> {
    let mut pieces = Vec::new();
    let mut chars = text.char_indices().peekable();
    while let Some((_, c)) = chars.next() {
        let piece = match c {
            ' ' | '\t' | '\n' | '\r' => continue,
            ':' => Piece::Colon,
            '=' => Piece::Equals,
            '?' => Piece::Question,
            '<' => Piece::Less,
            '/' => delimited(&mut chars, '/', true).map_or(Piece::UnterminatedRegex, Piece::Regex),
            '\'' => match delimited(&mut chars, '\'', false) {
                Some(s) if !s.is_empty() => Piece::Constant(s),
                _ => Piece::UnterminatedConstant,
            },
            c if c.is_ascii_alphabetic() || c == '_' => {
                let mut name = String::from(c);
                while let Some((_, n)) =
                    chars.next_if(|(_, n)| n.is_ascii_alphanumeric() || *n == '_')
                {
                    name.push(n);
                }
                Piece::Symbol(name)
            }
            _ => Piece::Bad,
        };
        pieces.push(piece);
    }
    pieces
}

/// The text up to the next unescaped `close`, consuming it; `None` when
/// the input ends first. With `keep_escapes` a backslash and the character
/// after it are kept as written (a regex); otherwise the backslash is dropped
/// (a string constant).
fn delimited(
    chars: &mut Peekable<CharIndices<'_>>,
    close: char,
    keep_escapes: bool,
) -> Option<String> {
    let mut out = String::new();
    loop {
        let (_, c) = chars.next()?;
        if c == close {
            return Some(out);
        }
        if c == '\\' {
            let (_, escaped) = chars.next()?;
            if keep_escapes {
                out.push('\\');
            }
            out.push(escaped);
        } else {
            out.push(c);
        }
    }
}

/// Parse a grammar text into its rules; the first fault is the error.
#[cfg_attr(
    not(test),
    expect(dead_code, reason = "consumed by the emitter (Task 4)")
)]
pub(crate) fn parse_rules(text: &str) -> Result<Rules, DgdError> {
    let pieces = lex(text);
    let mut parser = RuleParser {
        pieces: &pieces,
        at: 0,
        number: 0,
        rules: Rules {
            token_rules: Vec::new(),
            productions: Vec::new(),
        },
        seen_nomatch: false,
    };
    while parser.at < pieces.len() {
        parser.rule()?;
    }
    Ok(parser.rules)
}

/// Rule-level parsing over the pieces, numbering rules as they begin.
struct RuleParser<'p> {
    pieces: &'p [Piece],
    at: usize,
    number: usize,
    rules: Rules,
    seen_nomatch: bool,
}

impl RuleParser<'_> {
    fn peek(&self, ahead: usize) -> Option<&Piece> {
        self.pieces.get(self.at + ahead)
    }

    fn fault(&self, message: &'static str) -> DgdError {
        DgdError::Rule {
            number: self.number.max(1),
            message,
        }
    }

    /// Whether the piece at `ahead` is a symbol that opens a rule: one
    /// followed by `:` or `=`.
    fn rule_starts(&self, ahead: usize) -> bool {
        matches!(self.peek(ahead), Some(Piece::Symbol(_)))
            && matches!(self.peek(ahead + 1), Some(Piece::Colon | Piece::Equals))
    }

    /// One rule, starting at the current piece.
    fn rule(&mut self) -> Result<(), DgdError> {
        if !self.rule_starts(0) {
            return Err(match self.peek(0) {
                Some(Piece::Bad) => self.fault("bad token"),
                Some(Piece::UnterminatedRegex) => self.fault("malformed regular expression"),
                Some(Piece::UnterminatedConstant) => self.fault("malformed string constant"),
                _ => self.fault("unexpected token"),
            });
        }
        self.number += 1;
        let Some(Piece::Symbol(name)) = self.peek(0).cloned() else {
            return Err(self.fault("unexpected token"));
        };
        let is_token = matches!(self.peek(1), Some(Piece::Equals));
        self.at += 2;
        if is_token {
            self.token_rule(name)
        } else {
            self.production(name)
        }
    }

    fn token_rule(&mut self, name: String) -> Result<(), DgdError> {
        let pattern = match self.peek(0) {
            Some(Piece::Regex(raw)) => {
                let translated = translate_regex(raw)
                    .ok_or_else(|| self.fault("malformed regular expression"))?;
                TokenPattern::Regex(translated)
            }
            Some(Piece::UnterminatedRegex) => {
                return Err(self.fault("malformed regular expression"));
            }
            Some(Piece::Symbol(word)) if word == "nomatch" => {
                if self.seen_nomatch {
                    return Err(self.fault("extra nomatch rule"));
                }
                self.seen_nomatch = true;
                TokenPattern::Nomatch
            }
            _ => return Err(self.fault("regular expression expected")),
        };
        self.at += 1;
        if self.at < self.pieces.len() && !self.rule_starts(0) {
            return Err(self.fault("unexpected token"));
        }
        self.rules.token_rules.push(TokenRuleText {
            number: self.number,
            name,
            pattern,
        });
        Ok(())
    }

    fn production(&mut self, lhs: String) -> Result<(), DgdError> {
        let mut rhs = Vec::new();
        let mut action = None;
        while self.at < self.pieces.len() && !self.rule_starts(0) {
            match self.peek(0) {
                Some(Piece::Symbol(name)) => rhs.push(RhsText::Symbol(name.clone())),
                Some(Piece::Constant(text)) => rhs.push(RhsText::Constant(text.clone())),
                Some(Piece::UnterminatedConstant) => {
                    return Err(self.fault("malformed string constant"));
                }
                Some(Piece::Less) => {
                    // `< func` is parsed and has no effect.
                    self.at += 1;
                    self.function_name()?;
                    continue;
                }
                Some(Piece::Question) => {
                    self.at += 1;
                    action = Some(self.function_name()?);
                    if self.at < self.pieces.len() && !self.rule_starts(0) {
                        return Err(self.fault("unexpected token"));
                    }
                    break;
                }
                Some(Piece::Bad) => return Err(self.fault("bad token")),
                _ => return Err(self.fault("unexpected token")),
            }
            self.at += 1;
        }
        self.rules.productions.push(ProductionText {
            number: self.number,
            lhs,
            rhs,
            action,
        });
        Ok(())
    }

    /// The symbol after `?` or `<`, consumed.
    fn function_name(&mut self) -> Result<String, DgdError> {
        match self.peek(0) {
            Some(Piece::Symbol(name)) => {
                let name = name.clone();
                self.at += 1;
                Ok(name)
            }
            _ => Err(self.fault("function name expected")),
        }
    }
}

/// Translate a DGD regex to `regex-automata` syntax: DGD's metacharacters
/// are `. [ ] \ * + ? ( ) |`, everything else literal, `.` includes newline;
/// `None` when the regex is malformed under DGD's rules.
pub(crate) fn translate_regex(dgd: &str) -> Option<String> {
    if dgd.is_empty() {
        return None;
    }
    let mut out = String::new();
    let mut chars = dgd.chars().peekable();
    let mut depth = 0usize;
    // Whether a repeat may follow: false at the start, after `(`, after `|`, after a repeat.
    let mut repeatable = false;
    // Whether an operand is in hand to close a group, follow `|`, or end the regex; distinct
    // from `repeatable` — after a repeat (`a+`) there's an operand but no further repeat.
    let mut has_operand = false;
    while let Some(c) = chars.next() {
        match c {
            '\\' => {
                push_literal(&mut out, chars.next()?);
                repeatable = true;
                has_operand = true;
            }
            '.' => {
                out.push_str("(?s:.)");
                repeatable = true;
                has_operand = true;
            }
            '[' => {
                translate_set(&mut chars, &mut out)?;
                repeatable = true;
                has_operand = true;
            }
            '(' => {
                depth += 1;
                out.push('(');
                repeatable = false;
                has_operand = false;
            }
            ')' => {
                if depth == 0 || !has_operand {
                    return None;
                }
                depth -= 1;
                out.push(')');
                repeatable = true;
                has_operand = true;
            }
            '*' | '+' | '?' => {
                if !repeatable {
                    return None;
                }
                out.push(c);
                repeatable = false;
            }
            '|' => {
                if !has_operand {
                    return None;
                }
                out.push('|');
                repeatable = false;
                has_operand = false;
            }
            _ => {
                // `]` outside a set is a literal too: DGD's metacharacters do not include it.
                push_literal(&mut out, c);
                repeatable = true;
                has_operand = true;
            }
        }
    }
    (depth == 0 && has_operand).then_some(out)
}

/// Append `c` as a literal, escaped when Rust's regex syntax would otherwise
/// read it specially.
fn push_literal(out: &mut String, c: char) {
    if matches!(
        c,
        '.' | '+' | '*' | '?' | '(' | ')' | '|' | '[' | ']' | '{' | '}' | '^' | '$' | '\\'
    ) {
        out.push('\\');
    }
    out.push(c);
}

/// Translate a `[set]` whose `[` was consumed: an optional leading `^`,
/// then characters and `a-z` ranges, `\c` escaping any one; `None` for an
/// empty or unterminated set.
fn translate_set(
    chars: &mut std::iter::Peekable<std::str::Chars<'_>>,
    out: &mut String,
) -> Option<()> {
    out.push('[');
    if chars.next_if_eq(&'^').is_some() {
        out.push('^');
    }
    let mut members = 0usize;
    loop {
        let c = chars.next()?;
        if c == ']' {
            break;
        }
        let member = if c == '\\' { chars.next()? } else { c };
        push_set_member(out, member);
        members += 1;
        if chars.peek() == Some(&'-') {
            chars.next();
            let hi = match chars.next()? {
                '\\' => chars.next()?,
                ']' => {
                    // A trailing `-` is itself a member.
                    push_set_member(out, '-');
                    break;
                }
                hi => hi,
            };
            out.push('-');
            push_set_member(out, hi);
        }
    }
    if members == 0 {
        return None;
    }
    out.push(']');
    Some(())
}

/// Append `c` inside a class, escaped unless alphanumeric, so none of Rust's
/// class syntax (`&&`, `--`, `~~`, `[:name:]`) can form.
fn push_set_member(out: &mut String, c: char) {
    if !c.is_alphanumeric() {
        out.push('\\');
    }
    out.push(c);
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rules(text: &str) -> Rules {
        parse_rules(text).unwrap()
    }

    fn error(text: &str) -> String {
        parse_rules(text).unwrap_err().to_string()
    }

    #[test]
    fn a_token_rule_is_translated() {
        let r = rules("word = /[a-z]+/  S: word");
        assert_eq!(r.token_rules.len(), 1);
        assert_eq!(r.token_rules[0].name, "word");
        assert_eq!(r.token_rules[0].number, 1);
        assert_eq!(
            r.token_rules[0].pattern,
            TokenPattern::Regex("[a-z]+".to_owned())
        );
    }

    #[test]
    fn a_nomatch_rule_is_recognised() {
        let r = rules("rest = nomatch S: rest");
        assert_eq!(r.token_rules[0].pattern, TokenPattern::Nomatch);
    }

    #[test]
    fn a_production_keeps_symbols_constants_and_its_action() {
        let r = rules("word = /[a-z]+/ S: word 'to' word ? handle");
        let p = &r.productions[0];
        assert_eq!(p.number, 2);
        assert_eq!(p.lhs, "S");
        assert_eq!(
            p.rhs,
            vec![
                RhsText::Symbol("word".into()),
                RhsText::Constant("to".into()),
                RhsText::Symbol("word".into()),
            ]
        );
        assert_eq!(p.action.as_deref(), Some("handle"));
    }

    #[test]
    fn rules_end_where_the_next_rule_starts_regardless_of_newlines() {
        let one_line = rules("w = /[a-z]+/ S: w T T: w ? f");
        let lines = rules("w = /[a-z]+/\nS: w T\nT: w ? f\n");
        assert_eq!(one_line.productions.len(), 2);
        assert_eq!(one_line.productions[0].rhs.len(), 2);
        assert_eq!(one_line.productions, lines.productions);
    }

    #[test]
    fn an_empty_right_hand_side_is_allowed() {
        let r = rules("w = /a/ S: S: w");
        assert_eq!(r.productions[0].rhs, vec![]);
        assert_eq!(r.productions[1].rhs, vec![RhsText::Symbol("w".into())]);
    }

    #[test]
    fn a_less_function_is_accepted_and_ignored() {
        let r = rules("w = /a/ S: w < before ? after");
        assert_eq!(r.productions[0].action.as_deref(), Some("after"));
        let r = rules("w = /a/ S: w < before");
        assert_eq!(r.productions[0].action, None);
    }

    #[test]
    fn a_constant_escapes_its_quote() {
        let r = rules(r"w = /a/ S: 'it\'s'");
        assert_eq!(r.productions[0].rhs, vec![RhsText::Constant("it's".into())]);
    }

    #[test]
    fn error_texts_carry_the_rule_number() {
        assert_eq!(
            error("w = nomatch x = /a/ y = "),
            "Rule 3: regular expression expected"
        );
        assert_eq!(error("w = 'a'"), "Rule 1: regular expression expected");
        assert_eq!(error("w = /a/ S: w ?"), "Rule 2: function name expected");
        assert_eq!(
            error("w = /a/ S: w ? 'x'"),
            "Rule 2: function name expected"
        );
        assert_eq!(error("w = /a/ S: w <"), "Rule 2: function name expected");
        assert_eq!(error("w = /a"), "Rule 1: malformed regular expression");
        assert_eq!(error("w = /(a/"), "Rule 1: malformed regular expression");
        assert_eq!(
            error("w = /a/ S: 'oops"),
            "Rule 2: malformed string constant"
        );
        assert_eq!(error("w = /a/ S: ''"), "Rule 2: malformed string constant");
        assert_eq!(
            error("w = nomatch x = nomatch"),
            "Rule 2: extra nomatch rule"
        );
        assert_eq!(error("w = /a/ /b/"), "Rule 1: unexpected token");
        assert_eq!(error("/a/ S: w"), "Rule 1: unexpected token");
        assert_eq!(error("w = /a/ S: w @"), "Rule 2: bad token");
    }

    #[test]
    fn regex_translation_neutralises_rusts_extra_syntax() {
        let t = |s: &str| translate_regex(s).unwrap();
        assert_eq!(t(r"\d"), "d");
        assert_eq!(t(r"a\.b"), r"a\.b");
        assert_eq!(t(r"\/"), "/");
        assert_eq!(t("a{2}"), r"a\{2\}");
        assert_eq!(t("^a$"), r"\^a\$");
        assert_eq!(t("a.b"), "a(?s:.)b");
        assert_eq!(t("[a-z_]+"), r"[a-z\_]+");
        assert_eq!(t("[^ab]"), r"[^ab]");
        assert_eq!(t(r"[\]x]"), r"[\]x]");
        assert_eq!(t("[a&&b]"), r"[a\&\&b]");
        assert_eq!(t("[[:alpha:]]"), r"[\[\:alpha\:]\]");
        assert_eq!(t("a]"), r"a\]");
        assert_eq!(t("(a|b)*c?d+"), "(a|b)*c?d+");
    }

    #[test]
    fn malformed_regexes_are_rejected() {
        for bad in [
            "", "*a", "(a", "a)", "a|", "|a", "(|a)", "[]", "[^]", "[a", r"a\", "a**",
        ] {
            assert_eq!(translate_regex(bad), None, "{bad:?}");
        }
    }

    #[test]
    fn a_translated_regex_builds_and_matches_as_dgd_reads_it() {
        use regex_automata::{
            Anchored, Input,
            dfa::{Automaton, dense},
        };
        let pattern = translate_regex(r"\d[a-z.]{2}").unwrap();
        let dfa = dense::DFA::new(&pattern).unwrap();
        let hit = |s: &str| {
            dfa.try_search_fwd(&Input::new(s).anchored(Anchored::Yes))
                .unwrap()
                .map(|m| m.offset())
        };
        assert_eq!(hit("dx{2}"), Some(5));
        assert_eq!(hit("d.{2}"), Some(5));
        assert_eq!(hit("5x{2}"), None);
    }
}
