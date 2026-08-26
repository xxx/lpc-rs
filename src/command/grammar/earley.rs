//! Earley recognition over a token stream, with nullable nonterminals handled
//! by Aycock–Horspool prediction, and lazy enumeration of every derivation
//! the chart admits.

use std::{
    collections::{HashMap, HashSet},
    iter,
    sync::{
        Arc,
        atomic::{AtomicUsize, Ordering},
    },
};

use super::{
    model::{Grammar, NtId, ProdId, Symbol},
    tokenizer::Scan,
    tree::{Child, Node, Parse},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Item {
    prod: ProdId,
    dot: usize,
    origin: usize,
}

/// The work one parse may do before it stops.
#[derive(Debug)]
pub(crate) struct Budget {
    limit: usize,
    used: AtomicUsize,
}

impl Budget {
    /// A budget of `limit` steps.
    pub(crate) fn new(limit: usize) -> Self {
        Budget {
            limit,
            used: AtomicUsize::new(0),
        }
    }

    /// Spend one step; `false` once the budget is exhausted.
    pub(crate) fn step(&self) -> bool {
        self.used.fetch_add(1, Ordering::Relaxed) < self.limit
    }

    /// Whether the budget has been spent.
    pub(crate) fn exhausted(&self) -> bool {
        self.used.load(Ordering::Relaxed) >= self.limit
    }

    /// Steps spent so far; test-only introspection.
    #[cfg(test)]
    pub(crate) fn used(&self) -> usize {
        self.used.load(Ordering::Relaxed)
    }
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
    /// Build the Earley chart for `scan` under `grammar`, spending one step
    /// of `budget` per item added; an exhausted budget stops construction.
    pub(crate) fn build(grammar: &Grammar, scan: &Scan, budget: &Budget) -> Chart {
        let n = scan.tokens().len();
        let mut chart = Chart {
            sets: vec![Vec::new(); n + 1],
            seen: vec![HashSet::new(); n + 1],
            completed: vec![HashMap::new(); n + 1],
        };

        for &prod in grammar.productions_of(grammar.start()) {
            chart.add(
                grammar,
                budget,
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
                    None => chart.complete(grammar, budget, i, item),
                    Some(Symbol::NonTerminal(nt)) => chart.predict(grammar, budget, i, item, *nt),
                    Some(terminal) => {
                        if i < n && terminal_matches(terminal, scan, i) {
                            chart.add(
                                grammar,
                                budget,
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

    fn add(&mut self, grammar: &Grammar, budget: &Budget, i: usize, item: Item) {
        if self.seen[i].contains(&item) || !budget.step() {
            return;
        }
        self.seen[i].insert(item);
        self.sets[i].push(item);
        let prod = grammar.production(item.prod);
        if item.dot == prod.rhs.len() {
            self.completed[i]
                .entry((prod.lhs, item.origin))
                .or_default()
                .push(item.prod);
        }
    }

    fn predict(&mut self, grammar: &Grammar, budget: &Budget, i: usize, item: Item, nt: NtId) {
        for &prod in grammar.productions_of(nt) {
            self.add(
                grammar,
                budget,
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
                budget,
                i,
                Item {
                    dot: item.dot + 1,
                    ..item
                },
            );
        }
    }

    fn complete(&mut self, grammar: &Grammar, budget: &Budget, i: usize, item: Item) {
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
                budget,
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
        Some(scan) => {
            let n = scan.tokens().len();
            Chart::build(grammar, &scan, &Budget::new(usize::MAX))
                .completed(n, grammar.start(), 0)
                .is_some()
        }
        None => false,
    }
}

/// What derivation enumeration walks: the grammar, its chart, the scan, and
/// the step budget shared across the whole derivation.
struct Ctx<'g> {
    grammar: &'g Grammar,
    chart: Chart,
    scan: Arc<Scan>,
    budget: Arc<Budget>,
}

/// The nonterminals being derived on the current path, innermost first.
struct PathNode {
    nt: NtId,
    span: (usize, usize),
    parent: Path,
}

type Path = Option<Arc<PathNode>>;

/// Whether `nt` over `span` is already being derived somewhere on `path`.
fn on_path(path: &Path, nt: NtId, span: (usize, usize)) -> bool {
    let mut cursor = path;
    while let Some(node) = cursor {
        if node.nt == nt && node.span == span {
            return true;
        }
        cursor = &node.parent;
    }
    false
}

// Recursive lazy enumeration cannot name its own iterator type, so each level is boxed.
type Nodes<'g> = Box<dyn Iterator<Item = Node> + Send + 'g>;
type Children<'g> = Box<dyn Iterator<Item = Vec<Child>> + Send + 'g>;

/// Derivations of `nt` over `start..end`: productions in id order, each
/// nonterminal child longest first. A nonterminal already on the path over
/// the same span is a cycle and is skipped.
fn derive<'g>(ctx: Arc<Ctx<'g>>, nt: NtId, start: usize, end: usize, path: Path) -> Nodes<'g> {
    if !ctx.budget.step() || on_path(&path, nt, (start, end)) {
        return Box::new(iter::empty());
    }
    let Some(prods) = ctx.chart.completed(end, nt, start) else {
        return Box::new(iter::empty());
    };
    // `completed` is in chart-discovery order, not id order.
    let mut prods = prods.to_vec();
    prods.sort_unstable_by_key(|p| p.0);
    let path: Path = Some(Arc::new(PathNode {
        nt,
        span: (start, end),
        parent: path,
    }));
    Box::new(prods.into_iter().flat_map(move |p| {
        derive_rhs(ctx.clone(), p, 0, start, end, path.clone()).map(move |children| Node {
            production: p,
            span: start..end,
            children,
        })
    }))
}

/// Ways to lay `rhs[k..]` of production `p` over `pos..end`.
fn derive_rhs<'g>(
    ctx: Arc<Ctx<'g>>,
    p: ProdId,
    k: usize,
    pos: usize,
    end: usize,
    path: Path,
) -> Children<'g> {
    let grammar = ctx.grammar;
    let rhs = &grammar.production(p).rhs;
    if k == rhs.len() {
        return if pos == end {
            Box::new(iter::once(Vec::new()))
        } else {
            Box::new(iter::empty())
        };
    }
    match &rhs[k].symbol {
        Symbol::NonTerminal(nt) => {
            let nt = *nt;
            // A nonterminal in the last rhs position can only span pos..end,
            // so enumerating shorter spans there is pure waste.
            let mids: Box<dyn Iterator<Item = usize> + Send> = if k + 1 == rhs.len() {
                Box::new(iter::once(end))
            } else {
                Box::new((pos..=end).rev())
            };
            Box::new(mids.flat_map(move |mid| {
                let ctx_rest = ctx.clone();
                let path_rest = path.clone();
                derive(ctx.clone(), nt, pos, mid, path.clone()).flat_map(move |node| {
                    derive_rhs(ctx_rest.clone(), p, k + 1, mid, end, path_rest.clone()).map(
                        move |mut rest| {
                            rest.insert(0, Child::Node(node.clone()));
                            rest
                        },
                    )
                })
            }))
        }
        terminal => {
            if pos < end && terminal_matches(terminal, &ctx.scan, pos) {
                Box::new(
                    derive_rhs(ctx, p, k + 1, pos + 1, end, path).map(move |mut rest| {
                        rest.insert(0, Child::Token(pos));
                        rest
                    }),
                )
            } else {
                Box::new(iter::empty())
            }
        }
    }
}

/// The derivations of one input, lazily, at most `Options::max_parses` of
/// them and within `Options::max_steps`.
pub struct Parses<'g> {
    inner: Box<dyn Iterator<Item = Parse<'g>> + Send + 'g>,
    budget: Arc<Budget>,
}

impl<'g> Iterator for Parses<'g> {
    type Item = Parse<'g>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}

impl Parses<'_> {
    /// Whether the step budget ran out — derivations may be missing.
    pub fn over_budget(&self) -> bool {
        self.budget.exhausted()
    }
}

/// Parses `input` under `grammar`; untokenizable input yields no parses.
pub fn parse<'g>(grammar: &'g Grammar, input: &str) -> Parses<'g> {
    let budget = Arc::new(Budget::new(grammar.options().max_steps));
    let Some(scan) = grammar.tokenize(input) else {
        return Parses {
            inner: Box::new(iter::empty()),
            budget,
        };
    };
    let scan = Arc::new(scan);
    let chart = Chart::build(grammar, &scan, &budget);
    let n = scan.tokens().len();
    let ctx = Arc::new(Ctx {
        grammar,
        chart,
        scan: scan.clone(),
        budget: budget.clone(),
    });
    let roots = derive(ctx, grammar.start(), 0, n, None).take(grammar.options().max_parses);
    Parses {
        inner: Box::new(roots.map(move |root| Parse::new(grammar, scan.clone(), root))),
        budget,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::grammar::{
        Child, GrammarBuilder, Label, Node, Options, Parse, Parses, TokenClass, lit, nt, parse, tok,
    };

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
        assert!(!recognize(&g, "3 4"));
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

    #[test]
    fn a_single_parse_has_the_expected_tree() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let bee = b.nonterminal("B");
        let p_s = b.production(s, [lit("a"), nt(bee)]);
        let p_b = b.production(bee, [lit("b")]);
        let g = b.build().unwrap();

        let parses: Vec<Parse> = parse(&g, "a b").collect();
        assert_eq!(parses.len(), 1);
        assert_eq!(
            *parses[0].root(),
            Node {
                production: p_s,
                span: 0..2,
                children: vec![
                    Child::Token(0),
                    Child::Node(Node {
                        production: p_b,
                        span: 1..2,
                        children: vec![Child::Token(1)],
                    }),
                ],
            }
        );
        assert_eq!(parses[0].tokens().len(), 2);
        assert_eq!(parses[0].token_text(1), "b");
        assert_eq!(parses[0].text(0..2), "a b");
        assert!(std::ptr::eq(parses[0].grammar(), &g));
    }

    #[test]
    fn productions_of_a_nonterminal_enumerate_in_id_order() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let a = b.nonterminal("A");
        let p0 = b.production(s, [nt(a)]);
        let p1 = b.production(s, [lit("a")]);
        b.production(a, [lit("a")]);
        let g = b.build().unwrap();

        let parses: Vec<Parse> = parse(&g, "a").collect();
        assert_eq!(parses[0].root().production, p0);
        assert_eq!(parses[1].root().production, p1);
    }

    #[test]
    fn captures_keep_the_input_spacing() {
        let (mut b, _, word) = words();
        let s = b.nonterminal("S");
        let rest = b.nonterminal("Rest");
        b.production(s, [lit("say"), nt(rest).labeled(Label(0))]);
        b.production(rest, [tok(word)]);
        b.production(rest, [nt(rest), tok(word)]);
        let g = b.build().unwrap();

        let p = parse(&g, "say hello   there").next().unwrap();
        assert_eq!(p.captures(), vec![(Label(0), "hello   there")]);
        assert_eq!(p.capture_spans(), vec![(Label(0), 1..3)]);
    }

    #[test]
    fn an_empty_capture_is_empty_text() {
        let (mut b, _, word) = words();
        let s = b.nonterminal("S");
        let star = b.nonterminal("Star");
        b.production(s, [lit("wait"), nt(star).labeled(Label(9))]);
        b.production(star, []);
        b.production(star, [nt(star), tok(word)]);
        let g = b.build().unwrap();

        let p = parse(&g, "wait").next().unwrap();
        assert_eq!(p.captures(), vec![(Label(9), "")]);
        assert_eq!(p.capture_spans(), vec![(Label(9), 1..1)]);
    }

    /// `E → E '+' E | 'n'`, the textbook ambiguous grammar.
    fn sums(max_parses: usize) -> crate::command::grammar::Grammar {
        let (mut b, _, _) = words();
        b.max_parses(max_parses);
        let e = b.nonterminal("E");
        b.production(e, [nt(e), lit("+"), nt(e)]);
        b.production(e, [lit("n")]);
        b.build().unwrap()
    }

    #[test]
    fn ambiguity_enumerates_every_derivation_longest_first() {
        let g = sums(32);
        let parses: Vec<Parse> = parse(&g, "n + n + n").collect();
        assert_eq!(parses.len(), 2);
        let left_spans: Vec<_> = parses.iter().map(|p| p.root().children[0].span()).collect();
        assert_eq!(left_spans, vec![0..3, 0..1]);
    }

    #[test]
    fn max_parses_caps_enumeration() {
        let g = sums(2);
        assert_eq!(parse(&g, "n + n + n + n").count(), 2);
        let g = sums(100);
        assert_eq!(parse(&g, "n + n + n + n").count(), 5);
    }

    #[test]
    fn enumeration_is_lazy() {
        // Catalan(19) derivations exist; only two are ever materialized.
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [nt(s), nt(s)]);
        b.production(s, [lit("a")]);
        b.max_parses(usize::MAX);
        let g = b.build().unwrap();
        let input = ["a"; 20].join(" ");
        assert_eq!(parse(&g, &input).take(2).count(), 2);
    }

    #[test]
    fn a_unit_cycle_yields_only_cycle_free_derivations() {
        let (mut b, _, _) = words();
        b.max_parses(1000);
        let s = b.nonterminal("S");
        b.production(s, [nt(s)]);
        b.production(s, [lit("a")]);
        let g = b.build().unwrap();
        assert_eq!(parse(&g, "a").count(), 1);
    }

    #[test]
    fn a_nullable_cycle_terminates() {
        let (mut b, _, _) = words();
        b.max_parses(1000);
        let s = b.nonterminal("S");
        let t = b.nonterminal("T");
        b.production(s, [nt(t)]);
        b.production(s, []);
        b.production(t, [nt(s)]);
        let g = b.build().unwrap();
        let count = parse(&g, "").count();
        assert_eq!(count, 1);
    }

    #[test]
    fn empty_input_parses_when_start_is_nullable() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, []);
        let g = b.build().unwrap();
        let p = parse(&g, "").next().unwrap();
        assert_eq!(p.root().span, 0..0);
        assert!(p.captures().is_empty());
    }

    #[test]
    fn unmatched_input_yields_no_parses() {
        let mut b = GrammarBuilder::new();
        let word = b.token("word", "[a-z]+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        let g = b.build().unwrap();
        assert_eq!(parse(&g, "a!").count(), 0);
        assert_eq!(parse(&g, "a b").count(), 0);
    }

    #[test]
    fn parses_are_send() {
        fn assert_send<T: Send>() {}
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send::<Parses<'static>>();
        assert_send::<Parse<'static>>();
        assert_send_sync::<Grammar>();
    }

    #[test]
    fn two_adjacent_word_lists_give_the_first_the_longest_span() {
        let (mut b, _, word) = words();
        let s = b.nonterminal("S");
        let rest = b.nonterminal("Rest");
        b.production(
            s,
            [
                lit("say"),
                nt(rest).labeled(Label(0)),
                nt(rest).labeled(Label(1)),
            ],
        );
        b.production(rest, [tok(word)]);
        b.production(rest, [nt(rest), tok(word)]);
        let g = b.build().unwrap();

        let p = parse(&g, "say a b c").next().unwrap();
        assert_eq!(p.captures(), vec![(Label(0), "a b"), (Label(1), "c")]);
    }

    /// `E → E E | word`, whose derivation count grows with the input.
    fn ambiguous(max_steps: usize) -> Grammar {
        let mut b = GrammarBuilder::new();
        let w = b.words_tokens();
        let e = b.nonterminal("E");
        b.production(e, [nt(e), nt(e)]);
        b.production(e, [tok(w.word)]);
        b.start(e);
        b.max_steps(max_steps);
        b.build().unwrap()
    }

    #[test]
    fn the_default_budget_is_unbounded() {
        assert_eq!(Options::default().max_steps, usize::MAX);
        let g = ambiguous(usize::MAX);
        let mut parses = parse(&g, "a b c d");
        assert_eq!(parses.by_ref().count(), 5);
        assert!(!parses.over_budget());
    }

    #[test]
    fn an_exhausted_budget_stops_chart_construction() {
        let g = ambiguous(3);
        let mut parses = parse(&g, "a b c d");
        assert_eq!(parses.by_ref().count(), 0);
        assert!(parses.over_budget());
    }

    /// A budget sized to exactly the steps the first derivation costs
    /// admits that one derivation and then runs out, proving a partial —
    /// not merely empty — enumeration.
    #[test]
    fn an_exhausted_budget_stops_enumeration() {
        let unbounded = ambiguous(usize::MAX);
        let mut parses = parse(&unbounded, "a b c d");
        parses.next().unwrap();
        let steps_to_first = parses.budget.used();

        let g = ambiguous(steps_to_first);
        let mut parses = parse(&g, "a b c d");
        let n = parses.by_ref().count();
        assert!((1..5).contains(&n), "{n}");
        assert!(parses.over_budget());
    }

    /// `"!"` matches no rule and there is no nomatch rule, so `parse` returns
    /// before the chart is built and no step is spent.
    #[test]
    fn untokenizable_input_is_not_over_budget() {
        let mut b = GrammarBuilder::new();
        let word = b.token("word", "[a-z]+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        b.max_steps(1);
        let g = b.build().unwrap();
        let mut parses = parse(&g, "!");
        assert_eq!(parses.by_ref().count(), 0);
        assert!(!parses.over_budget());
    }
}
