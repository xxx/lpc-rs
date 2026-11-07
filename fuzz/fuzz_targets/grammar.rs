#![no_main]

use libfuzzer_sys::fuzz_target;
use lpc_rs::command::{
    frontend::{dgd, native, parser},
    grammar::{Grammar, GrammarBuilder, Label, Limits, lit, nt, parse},
};

#[derive(Clone, Copy)]
enum Element {
    Literal(&'static str),
    Word,
    Plus,
    Star,
    Optional(&'static str),
}

const WORDS: [&str; 4] = ["a", "b", "c", "1"];

fn exercise(grammar: &Grammar, input: &str) {
    let limits = Limits {
        max_parses: 32,
        max_steps: 20_000,
        max_depth: 64,
    };
    for result in parse(grammar, input, limits) {
        for token in result.tokens() {
            assert!(input.get(token.range.clone()).is_some());
        }
        for (_, span) in result.capture_spans() {
            assert!(span.start <= span.end && span.end <= result.tokens().len());
            let _ = result.text(span);
        }
    }
}

fn naive(
    elements: &[Element],
    tokens: &[&str],
    pos: usize,
    captures: &mut Vec<(usize, usize)>,
    results: &mut Vec<Vec<(usize, usize)>>,
) {
    let Some((first, rest)) = elements.split_first() else {
        if pos == tokens.len() {
            results.push(captures.clone());
        }
        return;
    };
    let ends: Vec<_> = match first {
        Element::Literal(word) => (pos < tokens.len() && tokens[pos] == *word)
            .then_some(pos + 1)
            .into_iter()
            .collect(),
        Element::Word => (pos < tokens.len())
            .then_some(pos + 1)
            .into_iter()
            .collect(),
        Element::Plus => (pos + 1..=tokens.len()).collect(),
        Element::Star => (pos..=tokens.len()).collect(),
        Element::Optional(word) => {
            let mut ends = vec![pos];
            if pos < tokens.len() && tokens[pos] == *word {
                ends.push(pos + 1);
            }
            ends
        }
    };
    for end in ends {
        captures.push((pos, end));
        naive(rest, tokens, end, captures, results);
        captures.pop();
    }
}

fn differential(data: &[u8]) {
    let Some((&length, data)) = data.split_first() else {
        return;
    };
    let count = (usize::from(length) % 5 + 1).min(data.len());
    let elements: Vec<_> = data[..count]
        .iter()
        .map(|&byte| match byte % 5 {
            0 => Element::Literal(WORDS[usize::from(byte / 5) % WORDS.len()]),
            1 => Element::Word,
            2 => Element::Plus,
            3 => Element::Star,
            _ => Element::Optional(WORDS[usize::from(byte / 5) % WORDS.len()]),
        })
        .collect();
    let tokens: Vec<_> = data[count..]
        .iter()
        .take(6)
        .map(|&byte| WORDS[usize::from(byte) % WORDS.len()])
        .collect();
    let input = tokens.join(" ");
    let mut builder = GrammarBuilder::new();
    let words = builder.words_tokens();
    let mut rhs = Vec::new();
    for (index, element) in elements.iter().enumerate() {
        let element = match element {
            Element::Literal(word) => lit(word),
            Element::Word => nt(builder.word_like(&words)),
            Element::Plus => nt(builder.words_plus(&words)),
            Element::Star => nt(builder.words_star(&words)),
            Element::Optional(word) => nt(builder.optional(lit(word))),
        };
        rhs.push(element.labeled(Label(index as u32)));
    }
    let start = builder.nonterminal("S");
    builder.production(start, rhs);
    let grammar = builder.build(start).unwrap();
    let limits = Limits {
        max_parses: 100_000,
        max_steps: 100_000,
        max_depth: 64,
    };
    let mut actual: Vec<_> = parse(&grammar, &input, limits)
        .map(|result| {
            result
                .capture_spans()
                .into_iter()
                .map(|(_, span)| (span.start, span.end))
                .collect::<Vec<_>>()
        })
        .collect();
    let mut expected = Vec::new();
    naive(&elements, &tokens, 0, &mut Vec::new(), &mut expected);
    actual.sort();
    expected.sort();
    assert_eq!(actual, expected);
}

fuzz_target!(|data: &[u8]| {
    differential(data);
    let text = String::from_utf8_lossy(data);
    let (pattern, input) = text.split_once("\n// fuzz-input\n").unwrap_or((&text, ""));
    if let Ok(compiled) = native::compile(pattern) {
        exercise(&compiled.grammar, input);
    }
    if let Ok(compiled) = native::compile_pattern(pattern) {
        exercise(&compiled.grammar, input);
    }
    if let Ok(compiled) = parser::compile("verb", pattern) {
        exercise(&compiled.compiled.grammar, input);
    }
    if let Ok(compiled) = dgd::compile(pattern) {
        exercise(&compiled.grammar, input);
    }
});
