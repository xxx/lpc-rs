//! Earley recognition over a token stream, with nullable nonterminals handled
//! by Aycock–Horspool prediction.

use std::collections::{HashMap, HashSet};

use super::{
    model::{Grammar, NtId, ProdId, Symbol},
    tokenizer::Scan,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Item {
    prod: ProdId,
    dot: usize,
    origin: usize,
}

/// One Earley set per token boundary plus the index of completed items that
/// derivation enumeration walks.
#[derive(Debug)]
pub(crate) struct Chart {
    sets: Vec<Vec<Item>>,
    seen: Vec<HashSet<Item>>,
    /// `completed[end][(nt, origin)]`: productions of `nt` spanning `origin..end`.
    completed: Vec<HashMap<(NtId, usize), Vec<ProdId>>>,
}

impl Chart {
    /// Build the Earley chart for `scan` under `grammar`.
    #[cfg_attr(not(test), expect(dead_code, reason = "used once derivations land"))]
    pub(crate) fn build(grammar: &Grammar, scan: &Scan) -> Chart {
        let n = scan.tokens().len();
        let mut chart = Chart {
            sets: vec![Vec::new(); n + 1],
            seen: vec![HashSet::new(); n + 1],
            completed: vec![HashMap::new(); n + 1],
        };

        for &prod in grammar.productions_of(grammar.start()) {
            chart.add(
                grammar,
                0,
                Item {
                    prod,
                    dot: 0,
                    origin: 0,
                },
            );
        }

        for i in 0..=n {
            let mut idx = 0;
            while idx < chart.sets[i].len() {
                let item = chart.sets[i][idx];
                idx += 1;
                let next = grammar
                    .production(item.prod)
                    .rhs
                    .get(item.dot)
                    .map(|e| &e.symbol);
                match next {
                    None => chart.complete(grammar, i, item),
                    Some(Symbol::NonTerminal(nt)) => chart.predict(grammar, i, item, *nt),
                    Some(terminal) => {
                        if i < n && terminal_matches(terminal, scan, i) {
                            chart.add(
                                grammar,
                                i + 1,
                                Item {
                                    dot: item.dot + 1,
                                    ..item
                                },
                            );
                        }
                    }
                }
            }
        }

        chart
    }

    fn add(&mut self, grammar: &Grammar, i: usize, item: Item) {
        if !self.seen[i].insert(item) {
            return;
        }
        self.sets[i].push(item);
        let prod = grammar.production(item.prod);
        if item.dot == prod.rhs.len() {
            self.completed[i]
                .entry((prod.lhs, item.origin))
                .or_default()
                .push(item.prod);
        }
    }

    fn predict(&mut self, grammar: &Grammar, i: usize, item: Item, nt: NtId) {
        for &prod in grammar.productions_of(nt) {
            self.add(
                grammar,
                i,
                Item {
                    prod,
                    dot: 0,
                    origin: i,
                },
            );
        }
        // Aycock–Horspool: a nullable nonterminal also advances its predictor in place.
        if grammar.is_nullable(nt) {
            self.add(
                grammar,
                i,
                Item {
                    dot: item.dot + 1,
                    ..item
                },
            );
        }
    }

    fn complete(&mut self, grammar: &Grammar, i: usize, item: Item) {
        let lhs = grammar.production(item.prod).lhs;
        let waiting: Vec<Item> = self.sets[item.origin]
            .iter()
            .copied()
            .filter(|w| {
                matches!(
                    grammar.production(w.prod).rhs.get(w.dot),
                    Some(e) if e.symbol == Symbol::NonTerminal(lhs)
                )
            })
            .collect();
        for w in waiting {
            self.add(
                grammar,
                i,
                Item {
                    dot: w.dot + 1,
                    ..w
                },
            );
        }
    }

    /// Productions of `nt` spanning `origin..end`, if any completed there.
    pub(crate) fn completed(&self, end: usize, nt: NtId, origin: usize) -> Option<&[ProdId]> {
        self.completed[end].get(&(nt, origin)).map(Vec::as_slice)
    }

    /// Whether the chart has a completed start production spanning the whole input.
    #[cfg_attr(not(test), expect(dead_code, reason = "used once derivations land"))]
    pub(crate) fn accepts(&self, grammar: &Grammar) -> bool {
        let n = self.sets.len() - 1;
        self.completed(n, grammar.start(), 0).is_some()
    }
}

/// Whether terminal `symbol` matches token `i` of `scan`.
pub(crate) fn terminal_matches(symbol: &Symbol, scan: &Scan, i: usize) -> bool {
    match symbol {
        Symbol::Literal(word) => scan.match_text(i) == word,
        Symbol::Token(class) => scan.tokens()[i].class == *class,
        Symbol::NonTerminal(_) => false,
    }
}

/// Whether `input` is in the language of `grammar`.
#[cfg(test)]
pub(crate) fn recognize(grammar: &Grammar, input: &str) -> bool {
    match grammar.tokenize(input) {
        Some(scan) => Chart::build(grammar, &scan).accepts(grammar),
        None => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::grammar::{GrammarBuilder, TokenClass, lit, nt, tok};

    /// A builder with the plain-words rules and the classes they got.
    fn words() -> (GrammarBuilder, TokenClass, TokenClass) {
        let mut b = GrammarBuilder::new();
        b.skip_token("whitespace", r"\s+");
        let number = b.token("number", "[0-9]+");
        let word = b.token("word", r"\S+");
        (b, number, word)
    }

    #[test]
    fn sequence_of_literals() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("a"), lit("b")]);
        let g = b.build().unwrap();
        assert!(recognize(&g, "a b"));
        assert!(!recognize(&g, "a"));
        assert!(!recognize(&g, "a b c"));
        assert!(!recognize(&g, "b a"));
    }

    #[test]
    fn alternation() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("a")]);
        b.production(s, [lit("b")]);
        let g = b.build().unwrap();
        assert!(recognize(&g, "a"));
        assert!(recognize(&g, "b"));
        assert!(!recognize(&g, "c"));
    }

    #[test]
    fn optional_prefix_through_a_nullable_nonterminal() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let opt = b.nonterminal("Opt");
        b.production(s, [nt(opt), lit("b")]);
        b.production(opt, [lit("a")]);
        b.production(opt, []);
        let g = b.build().unwrap();
        assert!(recognize(&g, "b"));
        assert!(recognize(&g, "a b"));
        assert!(!recognize(&g, "a"));
    }

    #[test]
    fn nullable_between_terminals() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let opt = b.nonterminal("Opt");
        b.production(s, [lit("a"), nt(opt), lit("b")]);
        b.production(opt, [lit("x")]);
        b.production(opt, []);
        let g = b.build().unwrap();
        assert!(recognize(&g, "a b"));
        assert!(recognize(&g, "a x b"));
        assert!(!recognize(&g, "a x x b"));
    }

    #[test]
    fn empty_input_needs_a_nullable_start() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let a = b.nonterminal("A");
        let c = b.nonterminal("C");
        b.production(s, [nt(a), nt(c)]);
        b.production(a, []);
        b.production(c, []);
        let g = b.build().unwrap();
        assert!(recognize(&g, ""));
        assert!(recognize(&g, "   "));
        assert!(!recognize(&g, "a"));

        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("a")]);
        let g = b.build().unwrap();
        assert!(!recognize(&g, ""));
    }

    #[test]
    fn left_recursion() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [nt(s), lit("a")]);
        b.production(s, [lit("a")]);
        let g = b.build().unwrap();
        assert!(recognize(&g, "a"));
        assert!(recognize(&g, "a a a"));
        assert!(!recognize(&g, "a b"));
    }

    #[test]
    fn right_recursion() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("a"), nt(s)]);
        b.production(s, [lit("a")]);
        let g = b.build().unwrap();
        assert!(recognize(&g, "a a a"));
        assert!(!recognize(&g, ""));
    }

    #[test]
    fn token_class_matches_any_token_of_that_class() {
        let (mut b, number, word) = words();
        let s = b.nonterminal("S");
        b.production(s, [tok(number), tok(word)]);
        let g = b.build().unwrap();
        assert!(recognize(&g, "3 apples"));
        assert!(recognize(&g, "3 four"));
        assert!(!recognize(&g, "three apples"));
    }

    #[test]
    fn literals_fold_case_when_asked() {
        let (mut b, _, _) = words();
        b.case_insensitive(true);
        let s = b.nonterminal("S");
        b.production(s, [lit("Look")]);
        let g = b.build().unwrap();
        assert!(recognize(&g, "LOOK"));
        assert!(recognize(&g, "look"));

        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("look")]);
        let g = b.build().unwrap();
        assert!(!recognize(&g, "LOOK"));
    }

    #[test]
    fn untokenizable_input_is_rejected() {
        let mut b = GrammarBuilder::new();
        let word = b.token("word", "[a-z]+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        let g = b.build().unwrap();
        assert!(!recognize(&g, "a!"));
    }
}
