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
