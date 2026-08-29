//! Earley recognition over a token stream, with nullable nonterminals handled
//! by Aycock–Horspool prediction, and lazy enumeration of every derivation
//! the chart admits.

use std::{
    iter,
    sync::{
        Arc,
        atomic::{AtomicU8, AtomicUsize, Ordering},
    },
};

use ahash::{AHashMap, AHashSet};

use super::{
    model::{Grammar, Limits, NtId, ProdId, Symbol},
    tokenizer::Scan,
    tree::{Child, Node, Parse},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Item {
    prod: ProdId,
    dot: usize,
    origin: usize,
}

/// How a parse stopped.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum Ending {
    /// The enumeration ran to its end within its limits, the parse cap
    /// included.
    Done = 0,
    /// The step budget refused a step; derivations may be missing.
    Exhausted = 1,
    /// A derivation nested deeper than `Limits::max_depth`; derivations may
    /// be missing.
    TooDeep = 2,
}

// Match patterns — an `as u8` cast is not one.
const EXHAUSTED: u8 = Ending::Exhausted as u8;
const TOO_DEEP: u8 = Ending::TooDeep as u8;

/// The step budget of one parse and how the parse ended.
#[derive(Debug)]
pub(crate) struct Budget {
    limit: usize,
    used: AtomicUsize,
    /// An `Ending` discriminant.
    ending: AtomicU8,
}

impl Budget {
    /// A budget of `limit` steps.
    pub(crate) fn new(limit: usize) -> Self {
        Budget {
            limit,
            used: AtomicUsize::new(0),
            ending: AtomicU8::new(Ending::Done as u8),
        }
    }

    /// Record that a derivation was refused for exceeding `max_depth`; every
    /// later step is refused too.
    pub(crate) fn refuse_depth(&self) {
        self.end(Ending::TooDeep);
    }

    /// Spend one step; `false` once the parse has ended.
    pub(crate) fn step(&self) -> bool {
        if self.ending() != Ending::Done {
            return false;
        }
        if self.used.fetch_add(1, Ordering::Relaxed) < self.limit {
            return true;
        }
        self.end(Ending::Exhausted);
        false
    }

    /// How the parse ended; `Done` while it is still within its limits.
    pub(crate) fn ending(&self) -> Ending {
        match self.ending.load(Ordering::Relaxed) {
            EXHAUSTED => Ending::Exhausted,
            TOO_DEEP => Ending::TooDeep,
            _ => Ending::Done,
        }
    }

    /// A later ending never replaces the first.
    fn end(&self, ending: Ending) {
        let _ = self.ending.compare_exchange(
            Ending::Done as u8,
            ending as u8,
            Ordering::Relaxed,
            Ordering::Relaxed,
        );
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
    /// Every `(set, item)` added, for dedup.
    seen: AHashSet<(usize, Item)>,
    /// `(set, nt)`: items of that set whose dot is before `nt`.
    waiting: AHashMap<(usize, NtId), Vec<Item>>,
    /// `(end, nt, origin)`: productions of `nt` completed over `origin..end`.
    completed: AHashMap<(usize, NtId, usize), Vec<ProdId>>,
}

impl Chart {
    /// Build the Earley chart for `scan` under `grammar`, spending one step
    /// of `budget` per item add attempted; an exhausted budget stops
    /// construction.
    pub(crate) fn build(grammar: &Grammar, scan: &Scan, budget: &Budget) -> Chart {
        let n = scan.tokens().len();
        let mut chart = Chart {
            sets: vec![Vec::new(); n + 1],
            seen: AHashSet::new(),
            waiting: AHashMap::new(),
            completed: AHashMap::new(),
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
                // Nothing more can be added.
                if budget.ending() != Ending::Done {
                    return chart;
                }
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

    /// Add `item` to set `i` unless it is already there; every attempt
    /// spends a step, duplicates included — the budget bounds work, not items.
    fn add(&mut self, grammar: &Grammar, budget: &Budget, i: usize, item: Item) {
        if !budget.step() || !self.seen.insert((i, item)) {
            return;
        }
        self.sets[i].push(item);
        let prod = grammar.production(item.prod);
        match prod.rhs.get(item.dot).map(|e| &e.symbol) {
            None => self
                .completed
                .entry((i, prod.lhs, item.origin))
                .or_default()
                .push(item.prod),
            Some(Symbol::NonTerminal(nt)) => {
                self.waiting.entry((i, *nt)).or_default().push(item);
            }
            Some(_) => {}
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
        // Taken out, not borrowed — `add` may grow this same entry when
        // `origin == i`.
        let key = (item.origin, lhs);
        let mut waiting = self
            .waiting
            .get_mut(&key)
            .map(std::mem::take)
            .unwrap_or_default();
        for &w in &waiting {
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
        if !waiting.is_empty() {
            let entry = self.waiting.entry(key).or_default();
            waiting.append(entry);
            *entry = waiting;
        }
    }

    /// Productions of `nt` spanning `origin..end`, if any completed there.
    pub(crate) fn completed(&self, end: usize, nt: NtId, origin: usize) -> Option<&[ProdId]> {
        self.completed.get(&(end, nt, origin)).map(Vec::as_slice)
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

/// One nonterminal on the derivation in hand: the productions it may use,
/// the one being tried, and the right-hand side laid so far.
struct Level {
    nt: NtId,
    start: usize,
    end: usize,
    /// Candidate productions, in id order.
    prods: Vec<ProdId>,
    prod_idx: usize,
    laid: Vec<Slot>,
    /// Where the nonterminal at `laid.len()` is next tried to end.
    cursor: Cursor,
    parent: Option<usize>,
    /// Levels on the path from the root, this one included.
    depth: usize,
}

/// One laid right-hand-side element.
#[derive(Clone, Copy)]
enum Slot {
    Token(usize),
    /// A nonterminal child: the index of its level and where it ends.
    Node {
        level: usize,
        end: usize,
    },
}

impl Slot {
    fn end(self) -> usize {
        match self {
            Slot::Token(i) => i + 1,
            Slot::Node { end, .. } => end,
        }
    }
}

/// The span ends still to try for a nonterminal element, longest first.
#[derive(Clone, Copy)]
enum Cursor {
    Fresh,
    Next(usize),
    Done,
}

impl Cursor {
    fn after(mid: usize) -> Cursor {
        mid.checked_sub(1).map_or(Cursor::Done, Cursor::Next)
    }
}

/// Lazy enumeration of every derivation the chart admits, as a depth-first
/// search over an explicit stack: productions in id order, each nonterminal
/// child longest first. Stack use is independent of the derivation's depth.
struct Derivations<'g> {
    grammar: &'g Grammar,
    chart: Chart,
    scan: Arc<Scan>,
    budget: Arc<Budget>,
    max_depth: usize,
    /// The derivation in hand, levels in pre-order.
    stack: Vec<Level>,
    /// The level whose right-hand side is being laid; `None` once the root
    /// is complete.
    active: Option<usize>,
    /// The root is complete and was handed out; the next call backtracks.
    yielded: bool,
}

impl<'g> Derivations<'g> {
    fn new(
        grammar: &'g Grammar,
        chart: Chart,
        scan: Arc<Scan>,
        budget: Arc<Budget>,
        max_depth: usize,
    ) -> Self {
        let n = scan.tokens().len();
        let mut derivations = Derivations {
            grammar,
            chart,
            scan,
            budget,
            max_depth,
            stack: Vec::new(),
            active: None,
            yielded: false,
        };
        if let Some(root) = derivations.level_for(grammar.start(), 0, n, None) {
            derivations.stack.push(root);
            derivations.active = Some(0);
        }
        derivations
    }

    /// A level deriving `nt` over `start..end` under `parent`; `None` when
    /// the budget refuses the step, the level would be too deep (terminal —
    /// pruning alone leaves every other span split to try), the same
    /// nonterminal over the same span is already being derived on the path
    /// (a cycle), or the chart never completed it.
    fn level_for(
        &self,
        nt: NtId,
        start: usize,
        end: usize,
        parent: Option<usize>,
    ) -> Option<Level> {
        if !self.budget.step() {
            return None;
        }
        let depth = parent.map_or(1, |p| self.stack[p].depth + 1);
        if depth > self.max_depth {
            self.budget.refuse_depth();
            return None;
        }
        let mut cursor = parent;
        while let Some(i) = cursor {
            let level = &self.stack[i];
            if level.nt == nt && level.start == start && level.end == end {
                return None;
            }
            cursor = level.parent;
        }
        // `completed` is in chart-discovery order, not id order.
        let mut prods = self.chart.completed(end, nt, start)?.to_vec();
        prods.sort_unstable_by_key(|p| p.0);
        Some(Level {
            nt,
            start,
            end,
            prods,
            prod_idx: 0,
            laid: Vec::new(),
            cursor: Cursor::Fresh,
            parent,
            depth,
        })
    }

    /// Undo the most recent choice on the derivation in hand: resume the
    /// last child level, else move the active level to its next
    /// production, else drop it and move its parent past that span end.
    /// `false` once no choice is left.
    fn backtrack(&mut self) -> bool {
        loop {
            let Some(a) = self.active else {
                return false;
            };
            match self.stack[a].laid.pop() {
                Some(Slot::Token(_)) => {}
                // Everything above the child is its own subtree: the levels
                // of later slots were dropped when those slots were retracted.
                Some(Slot::Node { level, .. }) => self.active = Some(level),
                None => {
                    let level = &mut self.stack[a];
                    level.prod_idx += 1;
                    level.cursor = Cursor::Fresh;
                    if level.prod_idx < level.prods.len() {
                        return true;
                    }
                    let (parent, end) = (level.parent, level.end);
                    self.stack.truncate(a);
                    self.active = parent;
                    let Some(p) = parent else {
                        return false;
                    };
                    self.stack[p].cursor = Cursor::after(end);
                    return true;
                }
            }
        }
    }

    /// The tree of the complete derivation on the stack.
    fn materialize(&self) -> Node {
        let mut built: Vec<Node> = Vec::new();
        for level in self.stack.iter().rev() {
            let children = level
                .laid
                .iter()
                .map(|slot| match slot {
                    Slot::Token(i) => Child::Token(*i),
                    Slot::Node { .. } => {
                        Child::Node(built.pop().expect("a child level after its parent"))
                    }
                })
                .collect();
            built.push(Node {
                production: level.prods[level.prod_idx],
                span: level.start..level.end,
                children,
            });
        }
        built.pop().expect("the root level")
    }
}

impl Iterator for Derivations<'_> {
    type Item = Node;

    fn next(&mut self) -> Option<Node> {
        if self.stack.is_empty() {
            return None;
        }
        if self.yielded {
            self.yielded = false;
            self.active = Some(0);
            if !self.backtrack() {
                self.stack.clear();
                return None;
            }
        }
        let grammar = self.grammar;
        loop {
            let Some(a) = self.active else {
                self.yielded = true;
                return Some(self.materialize());
            };
            let level = &self.stack[a];
            let (end, parent, cursor) = (level.end, level.parent, level.cursor);
            let rhs = &grammar.production(level.prods[level.prod_idx]).rhs;
            let k = level.laid.len();
            let pos = level.laid.last().map_or(level.start, |s| s.end());
            if k == rhs.len() {
                if pos == end {
                    self.active = parent;
                    if let Some(p) = parent {
                        self.stack[p].laid.push(Slot::Node { level: a, end });
                        self.stack[p].cursor = Cursor::Fresh;
                    }
                } else if !self.backtrack() {
                    break;
                }
                continue;
            }
            match &rhs[k].symbol {
                Symbol::NonTerminal(nt) => {
                    // A nonterminal in the last rhs position can only span
                    // pos..end, so enumerating shorter spans there is pure waste.
                    let lo = if k + 1 == rhs.len() { end } else { pos };
                    let mid = match cursor {
                        Cursor::Fresh => Some(end),
                        Cursor::Next(m) if m >= lo => Some(m),
                        Cursor::Next(_) | Cursor::Done => None,
                    };
                    let Some(mid) = mid else {
                        if !self.backtrack() {
                            break;
                        }
                        continue;
                    };
                    match self.level_for(*nt, pos, mid, Some(a)) {
                        Some(child) => {
                            self.stack.push(child);
                            self.active = Some(self.stack.len() - 1);
                        }
                        None => self.stack[a].cursor = Cursor::after(mid),
                    }
                }
                terminal => {
                    if pos < end && terminal_matches(terminal, &self.scan, pos) {
                        let level = &mut self.stack[a];
                        level.laid.push(Slot::Token(pos));
                        level.cursor = Cursor::Fresh;
                    } else if !self.backtrack() {
                        break;
                    }
                }
            }
        }
        self.stack.clear();
        None
    }
}

/// The derivations of one input, lazily, within `limits`.
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
    /// How the parse ended; `Done` until a step or depth limit ends it.
    pub fn ending(&self) -> Ending {
        self.budget.ending()
    }
}

/// Parses `input` under `grammar` within `limits`; untokenizable input
/// yields no parses.
pub fn parse<'g>(grammar: &'g Grammar, input: &str, limits: Limits) -> Parses<'g> {
    let budget = Arc::new(Budget::new(limits.max_steps));
    let Some(scan) = grammar.tokenize(input) else {
        return Parses {
            inner: Box::new(iter::empty()),
            budget,
        };
    };
    let scan = Arc::new(scan);
    let chart = Chart::build(grammar, &scan, &budget);
    let roots = Derivations::new(
        grammar,
        chart,
        scan.clone(),
        budget.clone(),
        limits.max_depth,
    )
    .take(limits.max_parses);
    Parses {
        inner: Box::new(roots.map(move |root| Parse::new(grammar, scan.clone(), root))),
        budget,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::command::grammar::{
        Child, DEFAULT_MAX_DEPTH, Ending, GrammarBuilder, Label, Limits, Node, Parse, Parses,
        TokenClass, lit, nt, parse, tok,
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
        let g = b.build(s).unwrap();
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
        let g = b.build(s).unwrap();
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
        let g = b.build(s).unwrap();
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
        let g = b.build(s).unwrap();
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
        let g = b.build(s).unwrap();
        assert!(recognize(&g, ""));
        assert!(recognize(&g, "   "));
        assert!(!recognize(&g, "a"));

        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("a")]);
        let g = b.build(s).unwrap();
        assert!(!recognize(&g, ""));
    }

    #[test]
    fn left_recursion() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [nt(s), lit("a")]);
        b.production(s, [lit("a")]);
        let g = b.build(s).unwrap();
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
        let g = b.build(s).unwrap();
        assert!(recognize(&g, "a a a"));
        assert!(!recognize(&g, ""));
    }

    #[test]
    fn token_class_matches_any_token_of_that_class() {
        let (mut b, number, word) = words();
        let s = b.nonterminal("S");
        b.production(s, [tok(number), tok(word)]);
        let g = b.build(s).unwrap();
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
        let g = b.build(s).unwrap();
        assert!(recognize(&g, "LOOK"));
        assert!(recognize(&g, "look"));

        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("look")]);
        let g = b.build(s).unwrap();
        assert!(!recognize(&g, "LOOK"));
    }

    #[test]
    fn untokenizable_input_is_rejected() {
        let mut b = GrammarBuilder::new();
        let word = b.token("word", "[a-z]+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        let g = b.build(s).unwrap();
        assert!(!recognize(&g, "a!"));
    }

    #[test]
    fn a_single_parse_has_the_expected_tree() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let bee = b.nonterminal("B");
        let p_s = b.production(s, [lit("a"), nt(bee)]);
        let p_b = b.production(bee, [lit("b")]);
        let g = b.build(s).unwrap();

        let parses: Vec<Parse> = parse(&g, "a b", Limits::default()).collect();
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
        let g = b.build(s).unwrap();

        let parses: Vec<Parse> = parse(&g, "a", Limits::default()).collect();
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
        let g = b.build(s).unwrap();

        let p = parse(&g, "say hello   there", Limits::default())
            .next()
            .unwrap();
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
        let g = b.build(s).unwrap();

        let p = parse(&g, "wait", Limits::default()).next().unwrap();
        assert_eq!(p.captures(), vec![(Label(9), "")]);
        assert_eq!(p.capture_spans(), vec![(Label(9), 1..1)]);
    }

    /// `E → E '+' E | 'n'`, the textbook ambiguous grammar.
    fn sums() -> crate::command::grammar::Grammar {
        let (mut b, _, _) = words();
        let e = b.nonterminal("E");
        b.production(e, [nt(e), lit("+"), nt(e)]);
        b.production(e, [lit("n")]);
        b.build(e).unwrap()
    }

    #[test]
    fn ambiguity_enumerates_every_derivation_longest_first() {
        let g = sums();
        let parses: Vec<Parse> = parse(&g, "n + n + n", Limits::default()).collect();
        assert_eq!(parses.len(), 2);
        let left_spans: Vec<_> = parses.iter().map(|p| p.root().children[0].span()).collect();
        assert_eq!(left_spans, vec![0..3, 0..1]);
    }

    #[test]
    fn max_parses_caps_enumeration() {
        let g = sums();
        let limits = Limits {
            max_parses: 2,
            ..Limits::default()
        };
        assert_eq!(parse(&g, "n + n + n + n", limits).count(), 2);
        let limits = Limits {
            max_parses: 100,
            ..Limits::default()
        };
        assert_eq!(parse(&g, "n + n + n + n", limits).count(), 5);
    }

    #[test]
    fn enumeration_is_lazy() {
        // Catalan(19) derivations exist; only two are ever materialized.
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [nt(s), nt(s)]);
        b.production(s, [lit("a")]);
        let g = b.build(s).unwrap();
        let input = ["a"; 20].join(" ");
        let limits = Limits {
            max_parses: usize::MAX,
            ..Limits::default()
        };
        assert_eq!(parse(&g, &input, limits).take(2).count(), 2);
    }

    #[test]
    fn a_unit_cycle_yields_only_cycle_free_derivations() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [nt(s)]);
        b.production(s, [lit("a")]);
        let g = b.build(s).unwrap();
        let limits = Limits {
            max_parses: 1000,
            ..Limits::default()
        };
        assert_eq!(parse(&g, "a", limits).count(), 1);
    }

    #[test]
    fn a_nullable_cycle_terminates() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        let t = b.nonterminal("T");
        b.production(s, [nt(t)]);
        b.production(s, []);
        b.production(t, [nt(s)]);
        let g = b.build(s).unwrap();
        let limits = Limits {
            max_parses: 1000,
            ..Limits::default()
        };
        let count = parse(&g, "", limits).count();
        assert_eq!(count, 1);
    }

    #[test]
    fn empty_input_parses_when_start_is_nullable() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, []);
        let g = b.build(s).unwrap();
        let p = parse(&g, "", Limits::default()).next().unwrap();
        assert_eq!(p.root().span, 0..0);
        assert!(p.captures().is_empty());
    }

    #[test]
    fn unmatched_input_yields_no_parses() {
        let mut b = GrammarBuilder::new();
        let word = b.token("word", "[a-z]+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        let g = b.build(s).unwrap();
        assert_eq!(parse(&g, "a!", Limits::default()).count(), 0);
        assert_eq!(parse(&g, "a b", Limits::default()).count(), 0);
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
        let g = b.build(s).unwrap();

        let p = parse(&g, "say a b c", Limits::default()).next().unwrap();
        assert_eq!(p.captures(), vec![(Label(0), "a b"), (Label(1), "c")]);
    }

    /// `E → E E | word`, whose derivation count grows with the input.
    fn ambiguous() -> Grammar {
        let mut b = GrammarBuilder::new();
        let w = b.words_tokens();
        let e = b.nonterminal("E");
        b.production(e, [nt(e), nt(e)]);
        b.production(e, [tok(w.word())]);
        b.build(e).unwrap()
    }

    #[test]
    fn a_budget_is_exhausted_only_once_a_step_is_refused() {
        let budget = Budget::new(2);
        assert!(budget.step());
        assert!(budget.step());
        assert_eq!(budget.ending(), Ending::Done);
        assert!(!budget.step());
        assert_eq!(budget.ending(), Ending::Exhausted);
    }

    #[test]
    fn the_default_budget_is_unbounded() {
        assert_eq!(Limits::default().max_steps, usize::MAX);
        let g = ambiguous();
        let mut parses = parse(&g, "a b c d", Limits::default());
        assert_eq!(parses.by_ref().count(), 5);
        assert_eq!(parses.ending(), Ending::Done);
    }

    #[test]
    fn an_exhausted_budget_stops_chart_construction() {
        let g = ambiguous();
        let limits = Limits {
            max_steps: 3,
            ..Limits::default()
        };
        let mut parses = parse(&g, "a b c d", limits);
        assert_eq!(parses.by_ref().count(), 0);
        assert_eq!(parses.ending(), Ending::Exhausted);
    }

    /// A budget sized to exactly the steps the first derivation costs
    /// admits that one derivation and then runs out, proving a partial —
    /// not merely empty — enumeration.
    #[test]
    fn an_exhausted_budget_stops_enumeration() {
        let unbounded = ambiguous();
        let mut parses = parse(&unbounded, "a b c d", Limits::default());
        parses.next().unwrap();
        let steps_to_first = parses.budget.used();

        let g = ambiguous();
        let limits = Limits {
            max_steps: steps_to_first,
            ..Limits::default()
        };
        let mut parses = parse(&g, "a b c d", limits);
        let n = parses.by_ref().count();
        assert!((1..5).contains(&n), "{n}");
        assert_eq!(parses.ending(), Ending::Exhausted);
    }

    /// `S: a S | a` (right) or `S: S a | a` (left) with the default depth.
    fn list(right: bool) -> Grammar {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        if right {
            b.production(s, [lit("a"), nt(s)]);
        } else {
            b.production(s, [nt(s), lit("a")]);
        }
        b.production(s, [lit("a")]);
        b.build(s).unwrap()
    }

    /// Run `f` on a thread with half the 2 MiB stack tokio's workers get.
    fn on_a_1_mib_stack<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
        std::thread::Builder::new()
            .stack_size(1 << 20)
            .spawn(f)
            .unwrap()
            .join()
            .unwrap()
    }

    /// Right recursion at a sixteenth of the depth: its chart costs about
    /// n²/2 items, and the STM soak test measures the whole process's RSS
    /// beside this one.
    #[test]
    fn a_list_at_the_default_depth_derives_within_a_1_mib_stack() {
        for right in [true, false] {
            let n = on_a_1_mib_stack(move || {
                let g = list(right);
                let items = if right {
                    DEFAULT_MAX_DEPTH / 16
                } else {
                    DEFAULT_MAX_DEPTH
                };
                let input = vec!["a"; items].join(" ");
                let mut parses = parse(&g, &input, Limits::default());
                let p = parses.next().unwrap();
                assert_eq!(parses.ending(), Ending::Done, "right={right}");
                // Captures walk the tree; the drop at the end unnests it.
                p.capture_spans().len()
            });
            assert_eq!(n, 0);
        }
    }

    #[test]
    fn a_list_one_past_the_default_depth_is_too_deep() {
        let (found, ending) = on_a_1_mib_stack(move || {
            let g = list(false);
            let input = vec!["a"; DEFAULT_MAX_DEPTH + 1].join(" ");
            let mut parses = parse(&g, &input, Limits::default());
            let found = parses.next().is_some();
            (found, parses.ending())
        });
        assert!(!found && ending == Ending::TooDeep);
    }

    #[test]
    fn max_depth_bounds_a_derivation_and_reports_it() {
        let (mut b, _, _) = words();
        let s = b.nonterminal("S");
        b.production(s, [lit("a"), nt(s)]);
        b.production(s, [lit("a")]);
        let g = b.build(s).unwrap();
        let limits = Limits {
            max_depth: 3,
            ..Limits::default()
        };
        let mut ok = parse(&g, "a a a", limits);
        assert!(ok.next().is_some());
        assert_eq!(ok.ending(), Ending::Done);
        let mut deep = parse(&g, "a a a a", limits);
        assert!(deep.next().is_none());
        assert_eq!(deep.ending(), Ending::TooDeep);
    }

    /// `"!"` matches no rule and there is no nomatch rule, so `parse` returns
    /// before the chart is built and no step is spent.
    #[test]
    fn untokenizable_input_ends_done() {
        let mut b = GrammarBuilder::new();
        let word = b.token("word", "[a-z]+");
        let s = b.nonterminal("S");
        b.production(s, [tok(word)]);
        let g = b.build(s).unwrap();
        let limits = Limits {
            max_steps: 1,
            ..Limits::default()
        };
        let mut parses = parse(&g, "!", limits);
        assert_eq!(parses.by_ref().count(), 0);
        assert_eq!(parses.ending(), Ending::Done);
    }

    #[test]
    fn the_first_ending_is_final() {
        let budget = Budget::new(1);
        assert!(budget.step());
        assert!(!budget.step());
        budget.refuse_depth();
        assert_eq!(budget.ending(), Ending::Exhausted);

        let budget = Budget::new(1);
        budget.refuse_depth();
        assert!(!budget.step());
        assert_eq!(budget.ending(), Ending::TooDeep);
        assert_eq!(budget.used(), 0);
    }
}
