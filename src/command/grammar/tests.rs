//! The engine's contract in its own vocabulary: the differential test against
//! a naive matcher, the `nomatch` class, builtin memoisation. Frontend shapes
//! are tested where they are built.

use super::{GrammarBuilder, Label, Limits, Parse, lit, nt, parse, tok};

fn first<'g>(g: &'g super::Grammar, input: &str) -> Option<Parse<'g>> {
    parse(g, input, Limits::default()).next()
}

fn captured(p: &Parse<'_>) -> Vec<(u32, String)> {
    p.captures()
        .into_iter()
        .map(|(Label(l), text)| (l, text.to_owned()))
        .collect()
}

#[test]
fn builtins_are_memoized_per_builder() {
    let mut b = GrammarBuilder::new();
    let w = b.words_tokens();
    assert_eq!(b.words_plus(&w), b.words_plus(&w));
    assert_eq!(b.words_star(&w), b.words_star(&w));
    assert_eq!(b.word_like(&w), b.word_like(&w));
    assert_ne!(b.optional(lit("a")), b.optional(lit("a")));
    assert_ne!(b.alternatives([lit("a")]), b.alternatives([lit("a")]));
    let s = b.nonterminal("S");
    let plus = b.words_plus(&w);
    b.production(s, [nt(plus)]);
    let g = b.build(s).unwrap();
    assert_eq!(parse(&g, "a b c", Limits::default()).count(), 1);
}

/// A pattern-frontend element, the shape the differential test generates.
#[derive(Clone, Copy, Debug)]
enum Elem {
    Lit(&'static str),
    WordLike,
    WordsPlus,
    WordsStar,
    Optional(&'static str),
}

const ALPHABET: [&str; 4] = ["a", "b", "c", "1"];

/// A deterministic 64-bit LCG; `rand` is not a dependency and the cases must replay.
struct Lcg(u64);

impl Lcg {
    fn next(&mut self, bound: usize) -> usize {
        self.0 = self
            .0
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1_442_695_040_888_963_407);
        ((self.0 >> 33) as usize) % bound
    }
}

fn random_elems(rng: &mut Lcg) -> Vec<Elem> {
    let len = 1 + rng.next(5);
    (0..len)
        .map(|_| match rng.next(5) {
            0 => Elem::Lit(ALPHABET[rng.next(4)]),
            1 => Elem::WordLike,
            2 => Elem::WordsPlus,
            3 => Elem::WordsStar,
            _ => Elem::Optional(ALPHABET[rng.next(4)]),
        })
        .collect()
}

fn random_tokens(rng: &mut Lcg) -> Vec<&'static str> {
    let len = rng.next(7);
    (0..len).map(|_| ALPHABET[rng.next(4)]).collect()
}

fn build(elems: &[Elem]) -> super::Grammar {
    let mut b = GrammarBuilder::new();
    let w = b.words_tokens();
    let mut rhs = Vec::new();
    for (i, e) in elems.iter().enumerate() {
        let label = Label(i as u32);
        let element = match e {
            Elem::Lit(s) => lit(s),
            Elem::WordLike => nt(b.word_like(&w)),
            Elem::WordsPlus => nt(b.words_plus(&w)),
            Elem::WordsStar => nt(b.words_star(&w)),
            Elem::Optional(s) => nt(b.optional(lit(s))),
        };
        rhs.push(element.labeled(label));
    }
    let s = b.nonterminal("S");
    b.production(s, rhs);
    b.build(s).unwrap()
}

/// Every way `elems` can cover `toks[pos..]`, as the span of each element.
fn naive(
    elems: &[Elem],
    pos: usize,
    toks: &[&str],
    acc: &mut Vec<(usize, usize)>,
    out: &mut Vec<Vec<(usize, usize)>>,
) {
    let Some((first, rest)) = elems.split_first() else {
        if pos == toks.len() {
            out.push(acc.clone());
        }
        return;
    };
    let ends: Vec<usize> = match first {
        Elem::Lit(w) => (pos < toks.len() && toks[pos] == *w)
            .then_some(pos + 1)
            .into_iter()
            .collect(),
        Elem::WordLike => (pos < toks.len()).then_some(pos + 1).into_iter().collect(),
        Elem::WordsPlus => (pos + 1..=toks.len()).collect(),
        Elem::WordsStar => (pos..=toks.len()).collect(),
        Elem::Optional(w) => {
            let mut ends = vec![pos];
            if pos < toks.len() && toks[pos] == *w {
                ends.push(pos + 1);
            }
            ends
        }
    };
    for end in ends {
        acc.push((pos, end));
        naive(rest, end, toks, acc, out);
        acc.pop();
    }
}

#[test]
fn earley_agrees_with_the_naive_matcher() {
    let mut rng = Lcg(0x5eed);
    let limits = Limits {
        max_parses: 100_000,
        ..Limits::default()
    };
    for _ in 0..500 {
        let elems = random_elems(&mut rng);
        let toks = random_tokens(&mut rng);
        let input = toks.join(" ");
        let g = build(&elems);

        let mut expected = Vec::new();
        naive(&elems, 0, &toks, &mut Vec::new(), &mut expected);
        expected.sort();

        let mut got: Vec<Vec<(usize, usize)>> = parse(&g, &input, limits)
            .map(|p| {
                p.capture_spans()
                    .into_iter()
                    .map(|(_, span)| (span.start, span.end))
                    .collect()
            })
            .collect();
        got.sort();

        assert_eq!(got, expected, "elems {elems:?} input {input:?}");
    }
}

#[test]
fn a_nomatch_class_is_a_production_symbol() {
    let mut b = GrammarBuilder::new();
    b.skip_token("whitespace", r"\s+");
    let word = b.token("word", "[a-z]+");
    let other = b.nomatch("other");
    let s = b.nonterminal("S");
    b.production(s, [tok(word), tok(other).labeled(Label(0))]);
    let g = b.build(s).unwrap();

    assert_eq!(
        captured(&first(&g, "ab !?").unwrap()),
        vec![(0, "!?".into())]
    );
    assert!(first(&g, "ab cd").is_none());
}
