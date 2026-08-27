//! Grammar-engine cost per rule: building a typical rule and parsing one
//! command line against it, hit and miss.

use std::hint::black_box;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use lpc_rs::command::frontend::dgd;
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

/// `[the] %i 'from' %o`, a `parse_command` pattern: two word runs whose
/// split the resolver may have to search.
fn items_pattern() -> Grammar {
    let mut b = GrammarBuilder::new();
    let w = b.words_tokens();
    let the = b.optional(lit("the"));
    let items = b.words_plus(&w);
    let s = b.nonterminal("S");
    b.production(
        s,
        [
            nt(the),
            nt(items).labeled(Label(0)),
            lit("from"),
            nt(items).labeled(Label(1)),
        ],
    );
    b.start(s);
    b.build().unwrap()
}

/// `doc/efun/parse_string.md`'s expression grammar, through the DGD frontend.
const DGD_EXPR: &str = "
    whitespace = /[ \t]+/
    number = /[0-9]+/
    Expr: Term
    Expr: Expr '+' Term ? add
    Expr: Expr '-' Term ? subtract
    Term: Factor
    Term: Term '*' Factor ? multiply
    Factor: number ? value
    Factor: '(' Expr ')' ? group
";

fn dgd_expr() -> Arc<Grammar> {
    dgd::compile(DGD_EXPR).unwrap().grammar
}

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("grammar/build/parser_rule", |b| {
        b.iter(|| lpc_rs::command::frontend::parser::compile("give", "OBJ to LIV").unwrap())
    });

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

    let p = items_pattern();
    c.bench_function("grammar/parse_all/items_pattern", |b| {
        b.iter(|| parse(&p, black_box("the red sword from the old bag")).count())
    });

    c.bench_function("grammar/build/dgd_expr", |b| b.iter(dgd_expr));

    let e = dgd_expr();
    c.bench_function("grammar/parse_all/dgd_expr", |b| {
        b.iter(|| parse(&e, black_box("2 + 3 * (4 - 1)")).count())
    });
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = criterion_benchmark
}
criterion_main!(benches);
