//! The frontend-compilation table: each family's surface syntax built the way
//! its frontend will build it, parsed against the engine.

use super::{GrammarBuilder, Label, Parse, Words, lit, nt, parse, tok};

fn first<'g>(g: &'g super::Grammar, input: &str) -> Option<Parse<'g>> {
    parse(g, input).next()
}

fn captured(p: &Parse<'_>) -> Vec<(u32, String)> {
    p.captures()
        .into_iter()
        .map(|(Label(l), text)| (l, text.to_owned()))
        .collect()
}

#[test]
fn add_action_exact_verb() {
    let mut b = GrammarBuilder::new();
    b.case_insensitive(true);
    let w = b.words_tokens();
    let star = b.words_star(&w);
    let s = b.nonterminal("S");
    b.production(s, [lit("look"), nt(star).labeled(Label(0))]);
    b.start(s);
    let g = b.build().unwrap();

    assert_eq!(
        captured(&first(&g, "look at me").unwrap()),
        vec![(0, "at me".into())]
    );
    assert_eq!(captured(&first(&g, "LOOK").unwrap()), vec![(0, "".into())]);
    assert!(first(&g, "lookat me").is_none());
    assert!(first(&g, "say look").is_none());
}

#[test]
fn add_action_prefix_verb() {
    let mut b = GrammarBuilder::new();
    let whitespace = b.skip_token("whitespace", r"\s+");
    let verb = b.token("verb", r"'\S*");
    let number = b.token("number", "[0-9]+");
    let word = b.token("word", r"\S+");
    let w = Words {
        whitespace,
        number,
        word,
    };
    let star = b.words_star(&w);
    let s = b.nonterminal("S");
    b.production(s, [tok(verb).labeled(Label(0)), nt(star).labeled(Label(1))]);
    b.start(s);
    let g = b.build().unwrap();

    let p = first(&g, "'hello there").unwrap();
    assert_eq!(
        captured(&p),
        vec![(0, "'hello".into()), (1, "there".into())]
    );
    // The frontend slices the argument from the byte after the verb prefix.
    assert_eq!(p.tokens()[0].range, 0..6);
    assert_eq!(&"'hello there"[1..], "hello there");

    let p = first(&g, "' hello").unwrap();
    assert_eq!(captured(&p), vec![(0, "'".into()), (1, "hello".into())]);

    assert!(first(&g, "say hi").is_none());
}

#[test]
fn parse_command_pattern() {
    // 'get' / 'take' [the] %i 'from' %o
    let mut b = GrammarBuilder::new();
    b.case_insensitive(true);
    let w = b.words_tokens();
    let verb = b.alternatives([lit("get"), lit("take")]);
    let the = b.optional(lit("the"));
    let items = b.words_plus(&w);
    let s = b.nonterminal("S");
    b.production(
        s,
        [
            nt(verb),
            nt(the),
            nt(items).labeled(Label(0)),
            lit("from"),
            nt(items).labeled(Label(1)),
        ],
    );
    b.start(s);
    let g = b.build().unwrap();

    assert_eq!(
        captured(&first(&g, "take the red sword from bob").unwrap()),
        vec![(0, "red sword".into()), (1, "bob".into())]
    );
    assert_eq!(
        captured(&first(&g, "get sword from the chest").unwrap()),
        vec![(0, "sword".into()), (1, "the chest".into())]
    );
    assert!(first(&g, "take from bob").is_none());
    assert!(first(&g, "steal sword from bob").is_none());
}

#[test]
fn parse_command_word_number_and_string() {
    // 'give' %d %w 'to' %s
    let mut b = GrammarBuilder::new();
    b.case_insensitive(true);
    let w = b.words_tokens();
    let word_like = b.word_like(&w);
    let star = b.words_star(&w);
    let s = b.nonterminal("S");
    b.production(
        s,
        [
            lit("give"),
            tok(w.number).labeled(Label(0)),
            nt(word_like).labeled(Label(1)),
            lit("to"),
            nt(star).labeled(Label(2)),
        ],
    );
    b.start(s);
    let g = b.build().unwrap();

    assert_eq!(
        captured(&first(&g, "give 3 coins to bob the tall").unwrap()),
        vec![
            (0, "3".into()),
            (1, "coins".into()),
            (2, "bob the tall".into())
        ]
    );
    assert_eq!(
        captured(&first(&g, "give 3 4 to").unwrap()),
        vec![(0, "3".into()), (1, "4".into()), (2, "".into())]
    );
    assert!(first(&g, "give three coins to bob").is_none());
}

#[test]
fn parser_package_rule() {
    // parse_add_rule("throw", "OBJ at OBJ") with synonym "hurl"
    let mut b = GrammarBuilder::new();
    b.case_insensitive(true);
    let w = b.words_tokens();
    let verb = b.alternatives([lit("throw"), lit("hurl")]);
    let obj = b.words_plus(&w);
    let s = b.nonterminal("S");
    b.production(
        s,
        [
            nt(verb),
            nt(obj).labeled(Label(0)),
            lit("at"),
            nt(obj).labeled(Label(1)),
        ],
    );
    b.start(s);
    let g = b.build().unwrap();

    assert_eq!(
        captured(&first(&g, "hurl the ball at bob").unwrap()),
        vec![(0, "the ball".into()), (1, "bob".into())]
    );
    assert!(first(&g, "throw at bob").is_none());
}

#[test]
fn parser_package_bare_verb() {
    let mut b = GrammarBuilder::new();
    b.case_insensitive(true);
    let _w = b.words_tokens();
    let verb = b.alternatives([lit("inventory"), lit("i")]);
    let s = b.nonterminal("S");
    b.production(s, [nt(verb)]);
    b.start(s);
    let g = b.build().unwrap();

    assert!(first(&g, "i").unwrap().captures().is_empty());
    assert!(first(&g, "inventory").is_some());
    assert!(first(&g, "inventory now").is_none());
}

#[test]
fn dgd_grammar_builds_a_tree() {
    // whitespace = /[ \t]+/  num = /[0-9]+/  plus = /\+/
    // expr: expr '+' num    expr: num
    let mut b = GrammarBuilder::new();
    b.skip_token("whitespace", "[ \t]+");
    let num = b.token("num", "[0-9]+");
    b.token("plus", r"\+");
    let expr = b.nonterminal("expr");
    let p_sum = b.production(expr, [nt(expr), lit("+"), tok(num)]);
    let p_num = b.production(expr, [tok(num)]);
    let g = b.build().unwrap();

    let parses: Vec<Parse> = parse(&g, "1 + 2 + 3").collect();
    assert_eq!(parses.len(), 1);
    let root = parses[0].root();
    assert_eq!(root.production, p_sum);
    assert_eq!(root.children.len(), 3);
    assert_eq!(root.children[0].span(), 0..3);
    assert_eq!(parses[0].text(root.children[0].span()), "1 + 2");
    let super::Child::Node(left) = &root.children[0] else {
        panic!("left child is a node");
    };
    assert_eq!(left.production, p_sum);
    let super::Child::Node(leftmost) = &left.children[0] else {
        panic!("leftmost child is a node");
    };
    assert_eq!(leftmost.production, p_num);
}

#[test]
fn dgd_ambiguous_grammar_enumerates_alternatives() {
    // expr: expr '+' expr    expr: num
    let mut b = GrammarBuilder::new();
    b.skip_token("whitespace", "[ \t]+");
    let num = b.token("num", "[0-9]+");
    b.token("plus", r"\+");
    let expr = b.nonterminal("expr");
    b.production(expr, [nt(expr), lit("+"), nt(expr)]);
    b.production(expr, [tok(num)]);
    let g = b.build().unwrap();

    assert_eq!(parse(&g, "1 + 2 + 3").count(), 2);
    assert_eq!(parse(&g, "1 + 2 + 3 + 4").count(), 5);
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
    b.start(s);
    let g = b.build().unwrap();
    assert_eq!(parse(&g, "a b c").count(), 1);
}

#[test]
fn builtins_without_an_explicit_start_parse_the_first_builtin() {
    let mut b = GrammarBuilder::new();
    let w = b.words_tokens();
    b.word_like(&w);
    let s = b.nonterminal("S");
    b.production(s, [lit("a")]);
    let g = b.build().unwrap();
    assert_eq!(g.nonterminal_name(g.start()), "%word_like");
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
    b.max_parses(100_000);
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
    b.start(s);
    b.build().unwrap()
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
    for _ in 0..500 {
        let elems = random_elems(&mut rng);
        let toks = random_tokens(&mut rng);
        let input = toks.join(" ");
        let g = build(&elems);

        let mut expected = Vec::new();
        naive(&elems, 0, &toks, &mut Vec::new(), &mut expected);
        expected.sort();

        let mut got: Vec<Vec<(usize, usize)>> = parse(&g, &input)
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
