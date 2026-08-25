//! Grammar-engine cost per rule: building a typical rule and parsing one
//! command line against it, hit and miss.

use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use lpc_rs::command::grammar::{Grammar, GrammarBuilder, Label, lit, nt, parse};

#[path = "support/profiler.rs"]
mod profiler;

/// `'give' / 'hand' %i 'to' %l`
fn give_rule() -> Grammar {
    let mut b = GrammarBuilder::new();
    b.case_insensitive(true);
    let w = b.words_tokens();
    let verb = b.alternatives([lit("give"), lit("hand")]);
    let items = b.words_plus(&w);
    let s = b.nonterminal("S");
    b.production(
        s,
        [
            nt(verb),
            nt(items).labeled(Label(0)),
            lit("to"),
            nt(items).labeled(Label(1)),
        ],
    );
    b.start(s);
    b.build().unwrap()
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("grammar/build/give_rule", |b| b.iter(give_rule));

    let g = give_rule();
    c.bench_function("grammar/parse_hit/give_rule", |b| {
        b.iter(|| {
            parse(&g, black_box("give the red sword to bob"))
                .next()
                .unwrap()
                .captures()
                .len()
        })
    });
    c.bench_function("grammar/parse_miss/give_rule", |b| {
        b.iter(|| parse(&g, black_box("look at the sword")).next().is_none())
    });
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = criterion_benchmark
}
criterion_main!(benches);
