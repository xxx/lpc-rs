//! The grammar model and its validating builder.

use std::{collections::HashMap, fmt, iter};

use super::tokenizer::{Scan, TokenSet};

/// A nonterminal, by index in the grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NtId(pub u32);

/// A production, by index in the grammar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProdId(pub u32);

/// A token rule, by its position in the grammar's rule order.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TokenClass(pub u32);

/// A capture slot; opaque to the engine, meaningful to the frontend that set it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Label(pub u32);

/// One symbol of a right-hand side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Symbol {
    /// A token whose text equals this word.
    Literal(String),
    /// Any token of this class.
    Token(TokenClass),
    /// A nonterminal reference.
    NonTerminal(NtId),
}

/// A symbol occurrence on a right-hand side, optionally a capture slot.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Element {
    /// The symbol on this right-hand side.
    pub symbol: Symbol,
    /// The capture slot, if any.
    pub label: Option<Label>,
}

impl Element {
    /// Annotate this element with a capture slot.
    pub fn labeled(mut self, label: Label) -> Self {
        self.label = Some(label);
        self
    }
}

impl From<Symbol> for Element {
    /// Convert a symbol to an unlabeled element.
    fn from(symbol: Symbol) -> Self {
        Element {
            symbol,
            label: None,
        }
    }
}

/// A literal-word element.
pub fn lit(word: &str) -> Element {
    Symbol::Literal(word.to_owned()).into()
}

/// A token-class element.
pub fn tok(class: TokenClass) -> Element {
    Symbol::Token(class).into()
}

/// A nonterminal element.
pub fn nt(id: NtId) -> Element {
    Symbol::NonTerminal(id).into()
}

/// A context-free grammar production: a nonterminal and its right-hand side.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Production {
    /// The nonterminal being defined.
    pub lhs: NtId,
    /// The symbols on the right-hand side.
    pub rhs: Vec<Element>,
}

/// Settings that configure the grammar's parser behavior.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Options {
    /// Fold literals and input to lowercase before matching; token rules are
    /// also compiled case-insensitively, which changes token classification,
    /// not only literal comparison.
    pub case_insensitive: bool,
    /// Derivations enumerated per input before the iterator ends.
    pub max_parses: usize,
    /// Work one parse may do — chart item adds attempted, duplicates
    /// included, plus derivation levels opened — before it stops; unbounded
    /// by default.
    pub max_steps: usize,
    /// Nonterminal levels a derivation may nest; a deeper one ends the parse,
    /// which `Parses::too_deep` reports. 4096 by default: the tree's
    /// consumers recurse over it, and this depth fits a 2 MiB thread.
    pub max_depth: usize,
}

/// The default `Options::max_depth`.
pub const DEFAULT_MAX_DEPTH: usize = 4096;

impl Default for Options {
    /// The default grammar settings: case-sensitive, up to 32 parses per
    /// input, no step budget, derivations at most 4096 deep.
    fn default() -> Self {
        Options {
            case_insensitive: false,
            max_parses: 32,
            max_steps: usize::MAX,
            max_depth: DEFAULT_MAX_DEPTH,
        }
    }
}

/// What a token rule matches: a regex, or every run no regex rule matches.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Pattern {
    /// A regex in `regex-automata` syntax.
    Regex(String),
    /// The run of characters from a position no rule matches up to the next
    /// position one does; at most one rule per grammar takes effect.
    Nomatch,
}

/// A token rule as the builder collected it; the grammar compiles them together.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct TokenRule {
    /// The rule's name; becomes the token class's name in the grammar.
    pub name: String,
    /// What the rule matches.
    pub pattern: Pattern,
    /// Matched and dropped, never a token.
    pub skip: bool,
}

/// Errors that may occur during grammar construction and validation.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum GrammarError {
    /// The builder has no productions.
    EmptyGrammar,
    /// A nonterminal used on a right-hand side or as the start has no production.
    UnknownNonTerminal { name: String },
    /// A token rule pattern is not a valid regex.
    BadRegex { class: String, message: String },
    /// The joint-build fallback when per-rule attribution cannot reach the
    /// failure: a combination of rules exceeding the DFA's size limit.
    DfaBuild(String),
}

impl fmt::Display for GrammarError {
    /// Display the error.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GrammarError::EmptyGrammar => write!(f, "the grammar has no productions"),
            GrammarError::UnknownNonTerminal { name } => {
                write!(f, "nonterminal `{name}` has no production")
            }
            GrammarError::BadRegex { class, message } => {
                write!(f, "token rule `{class}`: {message}")
            }
            GrammarError::DfaBuild(message) => write!(f, "token rules: {message}"),
        }
    }
}

impl std::error::Error for GrammarError {}

/// A validated grammar, immutable once built.
#[derive(Debug)]
pub struct Grammar {
    token_rules: Vec<TokenRule>,
    tokens: TokenSet,
    nonterminals: Vec<String>,
    productions: Vec<Production>,
    by_lhs: Vec<Vec<ProdId>>,
    nullable: Vec<bool>,
    start: NtId,
    options: Options,
}

impl Grammar {
    /// The parser options for this grammar.
    pub fn options(&self) -> &Options {
        &self.options
    }

    /// The start symbol of this grammar.
    pub fn start(&self) -> NtId {
        self.start
    }

    /// The production at the given index.
    pub fn production(&self, id: ProdId) -> &Production {
        &self.productions[id.0 as usize]
    }

    /// All productions for a given nonterminal.
    pub fn productions_of(&self, nt: NtId) -> &[ProdId] {
        &self.by_lhs[nt.0 as usize]
    }

    /// Whether this nonterminal can derive the empty string.
    pub fn is_nullable(&self, nt: NtId) -> bool {
        self.nullable[nt.0 as usize]
    }

    /// The number of distinct nonterminals in this grammar.
    pub fn nonterminal_count(&self) -> usize {
        self.nonterminals.len()
    }

    /// The name of a nonterminal.
    pub fn nonterminal_name(&self, nt: NtId) -> &str {
        &self.nonterminals[nt.0 as usize]
    }

    /// The name of a token class.
    pub fn token_name(&self, class: TokenClass) -> &str {
        &self.token_rules[class.0 as usize].name
    }

    /// Tokenizes `input` under this grammar's rules, `None` where no rule matches.
    pub fn tokenize(&self, input: &str) -> Option<Scan> {
        let tokens = self.tokens.tokenize(input)?;
        Some(Scan::new(input, tokens, self.options.case_insensitive))
    }
}

/// Collects token rules, nonterminals, and productions, then validates them
/// into a [`Grammar`].
#[derive(Debug, Default)]
pub struct GrammarBuilder {
    token_rules: Vec<TokenRule>,
    nonterminals: Vec<String>,
    by_name: HashMap<String, NtId>,
    productions: Vec<Production>,
    start: Option<NtId>,
    options: Options,
}

impl GrammarBuilder {
    /// Create a new, empty grammar builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Append a token rule; rule order decides ties between equal-length matches.
    pub fn token(&mut self, name: &str, pattern: &str) -> TokenClass {
        self.push_rule(name, Pattern::Regex(pattern.to_owned()), false)
    }

    /// Append a token rule whose matches are dropped from the stream.
    pub fn skip_token(&mut self, name: &str, pattern: &str) -> TokenClass {
        self.push_rule(name, Pattern::Regex(pattern.to_owned()), true)
    }

    /// Append the rule that takes every run no other rule matches; a later
    /// nomatch rule replaces an earlier one.
    pub fn nomatch(&mut self, name: &str) -> TokenClass {
        self.push_rule(name, Pattern::Nomatch, false)
    }

    /// Append a nomatch rule whose runs are dropped from the stream.
    pub fn skip_nomatch(&mut self, name: &str) -> TokenClass {
        self.push_rule(name, Pattern::Nomatch, true)
    }

    fn push_rule(&mut self, name: &str, pattern: Pattern, skip: bool) -> TokenClass {
        self.token_rules.push(TokenRule {
            name: name.to_owned(),
            pattern,
            skip,
        });
        TokenClass(self.token_rules.len() as u32 - 1)
    }

    /// Get or create a nonterminal by name.
    pub fn nonterminal(&mut self, name: &str) -> NtId {
        if let Some(id) = self.by_name.get(name) {
            return *id;
        }
        let id = NtId(self.nonterminals.len() as u32);
        self.nonterminals.push(name.to_owned());
        self.by_name.insert(name.to_owned(), id);
        id
    }

    /// Add a production to the grammar.
    pub fn production(&mut self, lhs: NtId, rhs: impl IntoIterator<Item = Element>) -> ProdId {
        self.productions.push(Production {
            lhs,
            rhs: rhs.into_iter().collect(),
        });
        ProdId(self.productions.len() as u32 - 1)
    }

    /// Set the start symbol; the first production's left-hand side when unset.
    /// A grammar using the builtins (`word_like`, `words_plus`, `words_star`,
    /// `optional`, `alternatives`) must call this explicitly, since their
    /// injected productions come first.
    pub fn start(&mut self, nt: NtId) -> &mut Self {
        self.start = Some(nt);
        self
    }

    /// Toggle case-insensitive matching.
    pub fn case_insensitive(&mut self, yes: bool) -> &mut Self {
        self.options.case_insensitive = yes;
        self
    }

    /// Set the maximum number of parses per input.
    pub fn max_parses(&mut self, n: usize) -> &mut Self {
        self.options.max_parses = n;
        self
    }

    /// Set the step budget one parse may spend; see [`Options::max_steps`].
    pub fn max_steps(&mut self, n: usize) -> &mut Self {
        self.options.max_steps = n;
        self
    }

    /// Set how deep a derivation may nest; see [`Options::max_depth`].
    pub fn max_depth(&mut self, n: usize) -> &mut Self {
        self.options.max_depth = n;
        self
    }

    /// Whether `nt` has at least one production so far.
    pub(crate) fn is_defined(&self, nt: NtId) -> bool {
        self.productions.iter().any(|p| p.lhs == nt)
    }

    /// A nonterminal no name can collide with.
    pub(crate) fn fresh(&mut self, prefix: &str) -> NtId {
        let name = format!("{prefix}#{}", self.nonterminals.len());
        self.nonterminal(&name)
    }

    /// Validate and build the grammar; fails with `GrammarError` if invalid.
    pub fn build(self) -> Result<Grammar, GrammarError> {
        let Self {
            token_rules,
            nonterminals,
            mut productions,
            start,
            options,
            ..
        } = self;

        let Some(first) = productions.first() else {
            return Err(GrammarError::EmptyGrammar);
        };
        let start = start.unwrap_or(first.lhs);

        let mut by_lhs = vec![Vec::new(); nonterminals.len()];
        for (i, p) in productions.iter().enumerate() {
            if p.lhs.0 as usize >= nonterminals.len() {
                return Err(GrammarError::UnknownNonTerminal {
                    name: format!("#{}", p.lhs.0),
                });
            }
            by_lhs[p.lhs.0 as usize].push(ProdId(i as u32));
        }

        let referenced = iter::once(start).chain(productions.iter().flat_map(|p| {
            p.rhs.iter().filter_map(|e| match e.symbol {
                Symbol::NonTerminal(n) => Some(n),
                _ => None,
            })
        }));
        for n in referenced {
            if n.0 as usize >= nonterminals.len() {
                return Err(GrammarError::UnknownNonTerminal {
                    name: format!("#{}", n.0),
                });
            }
            if by_lhs[n.0 as usize].is_empty() {
                return Err(GrammarError::UnknownNonTerminal {
                    name: nonterminals[n.0 as usize].clone(),
                });
            }
        }

        if options.case_insensitive {
            for e in productions.iter_mut().flat_map(|p| p.rhs.iter_mut()) {
                if let Symbol::Literal(w) = &mut e.symbol {
                    *w = w.to_lowercase();
                }
            }
        }

        let tokens = TokenSet::build(&token_rules, options.case_insensitive)?;
        let nullable = compute_nullable(&productions, nonterminals.len());

        Ok(Grammar {
            token_rules,
            tokens,
            nonterminals,
            productions,
            by_lhs,
            nullable,
            start,
            options,
        })
    }
}

/// Compute which nonterminals derive the empty string, by fixpoint over the productions.
fn compute_nullable(productions: &[Production], count: usize) -> Vec<bool> {
    let mut nullable = vec![false; count];
    loop {
        let mut changed = false;
        for p in productions {
            if nullable[p.lhs.0 as usize] {
                continue;
            }
            let all_nullable = p
                .rhs
                .iter()
                .all(|e| matches!(e.symbol, Symbol::NonTerminal(n) if nullable[n.0 as usize]));
            if all_nullable {
                nullable[p.lhs.0 as usize] = true;
                changed = true;
            }
        }
        if !changed {
            return nullable;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nonterminal_is_idempotent_by_name() {
        let mut b = GrammarBuilder::new();
        let s1 = b.nonterminal("S");
        let s2 = b.nonterminal("S");
        let t = b.nonterminal("T");
        assert_eq!(s1, s2);
        assert_ne!(s1, t);
    }

    #[test]
    fn empty_grammar_is_an_error() {
        assert_eq!(
            GrammarBuilder::new().build().unwrap_err(),
            GrammarError::EmptyGrammar
        );
    }

    #[test]
    fn undefined_nonterminal_is_an_error() {
        let mut b = GrammarBuilder::new();
        let s = b.nonterminal("S");
        let ghost = b.nonterminal("Ghost");
        b.production(s, [nt(ghost)]);
        assert_eq!(
            b.build().unwrap_err(),
            GrammarError::UnknownNonTerminal {
                name: "Ghost".into()
            }
        );
    }

    #[test]
    fn a_foreign_nonterminal_id_is_an_error() {
        let mut b = GrammarBuilder::new();
        let s = b.nonterminal("S");
        b.production(s, [nt(NtId(99))]);
        assert_eq!(
            b.build().unwrap_err(),
            GrammarError::UnknownNonTerminal { name: "#99".into() }
        );
    }

    #[test]
    fn start_defaults_to_the_first_production() {
        let mut b = GrammarBuilder::new();
        let t = b.nonterminal("T");
        let s = b.nonterminal("S");
        b.production(t, [lit("t")]);
        b.production(s, [nt(t)]);
        let g = b.build().unwrap();
        assert_eq!(g.start(), t);
        assert_eq!(g.nonterminal_name(g.start()), "T");
    }

    #[test]
    fn explicit_start_wins() {
        let mut b = GrammarBuilder::new();
        let t = b.nonterminal("T");
        let s = b.nonterminal("S");
        b.production(t, [lit("t")]);
        b.production(s, [nt(t)]);
        b.start(s);
        assert_eq!(b.build().unwrap().start(), s);
    }

    #[test]
    fn case_insensitive_folds_literals_at_build() {
        let mut b = GrammarBuilder::new();
        b.case_insensitive(true);
        let s = b.nonterminal("S");
        let p = b.production(s, [lit("LOOK")]);
        let g = b.build().unwrap();
        assert_eq!(
            g.production(p).rhs[0].symbol,
            Symbol::Literal("look".into())
        );
        assert!(g.options().case_insensitive);
    }

    #[test]
    fn nullable_propagates_through_chains() {
        let mut b = GrammarBuilder::new();
        let s = b.nonterminal("S");
        let a = b.nonterminal("A");
        let c = b.nonterminal("C");
        let d = b.nonterminal("D");
        let e = b.nonterminal("E");
        b.production(s, [nt(a), nt(c)]);
        b.production(a, []);
        b.production(c, [nt(d)]);
        b.production(d, []);
        b.production(e, [lit("x")]);
        b.production(s, [nt(e)]);
        let g = b.build().unwrap();
        assert!(g.is_nullable(s));
        assert!(g.is_nullable(a));
        assert!(g.is_nullable(c));
        assert!(g.is_nullable(d));
        assert!(!g.is_nullable(e));
        assert_eq!(g.productions_of(s).len(), 2);
        assert_eq!(g.nonterminal_count(), 5);
    }

    #[test]
    fn labels_ride_on_elements() {
        let e = lit("get").labeled(Label(3));
        assert_eq!(e.label, Some(Label(3)));
        assert_eq!(e.symbol, Symbol::Literal("get".into()));
        assert_eq!(Element::from(Symbol::Token(TokenClass(1))).label, None);
    }

    #[test]
    fn grammar_tokenizes_with_its_rules() {
        let mut b = GrammarBuilder::new();
        b.skip_token("ws", r"\s+");
        let word = b.token("word", r"\S+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        let g = b.build().unwrap();
        let scan = g.tokenize("hi there").unwrap();
        assert_eq!(scan.tokens().len(), 2);
        assert_eq!(scan.token_text(1), "there");
        assert_eq!(g.token_name(word), "word");
        assert!(g.tokenize("").unwrap().tokens().is_empty());
    }
}
