//! Grammar construction, command parsing, and visible-object traversal costs.

use std::hint::black_box;
use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use lpc_rs::command::frontend::dgd;
use lpc_rs::command::grammar::{Grammar, GrammarBuilder, Label, Limits, lit, nt, parse};

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
    b.build(s).unwrap()
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
    b.build(s).unwrap()
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
    let captured = parse(&g, "give the red sword to bob", Limits::default())
        .next()
        .unwrap();
    c.bench_function("grammar/capture_spans/give_rule", |b| {
        b.iter(|| black_box(&captured).capture_spans())
    });
    c.bench_function("grammar/parse_hit/give_rule", |b| {
        b.iter(|| {
            parse(
                &g,
                black_box("give the red sword to bob"),
                Limits::default(),
            )
            .next()
            .unwrap()
            .captures()
            .len()
        })
    });
    c.bench_function("grammar/parse_miss/give_rule", |b| {
        b.iter(|| {
            parse(&g, black_box("look at the sword"), Limits::default())
                .next()
                .is_none()
        })
    });

    let p = items_pattern();
    c.bench_function("grammar/parse_all/items_pattern", |b| {
        b.iter(|| {
            parse(
                &p,
                black_box("the red sword from the old bag"),
                Limits::default(),
            )
            .count()
        })
    });

    c.bench_function("grammar/build/dgd_expr", |b| b.iter(dgd_expr));

    let e = dgd_expr();
    c.bench_function("grammar/parse_all/dgd_expr", |b| {
        b.iter(|| parse(&e, black_box("2 + 3 * (4 - 1)"), Limits::default()).count())
    });

    let repeated = lpc_rs::command::frontend::parser::compile("cast", "STR at OBJ").unwrap();
    let input = (0..41)
        .map(|i| format!("w{i}"))
        .collect::<Vec<_>>()
        .join(" at ");
    c.bench_function("grammar/repeated_separator/first", |b| {
        b.iter(|| {
            parse(
                &repeated.compiled.grammar,
                black_box(&input),
                Limits::default(),
            )
            .next()
            .unwrap()
        })
    });
    c.bench_function("grammar/repeated_separator/all", |b| {
        b.iter(|| {
            parse(
                &repeated.compiled.grammar,
                black_box(&input),
                Limits {
                    max_parses: 100,
                    ..Limits::default()
                },
            )
            .count()
        })
    });
}

fn scope_walk(c: &mut Criterion) {
    use lpc_rs::interpreter::{
        lpc_ref::LpcRef,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    };
    use lpc_rs_utils::config::ConfigBuilder;

    const NAME: &str = "grammar/scope_walk/containers_64";
    if std::env::args().any(|arg| arg == "--list") {
        c.bench_function(NAME, |_| {});
        return;
    }
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let (vm, actor) = rt.block_on(async {
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.initialize_process_from_code(
            "/secure/master.c",
            "string *ids = ({}); string *parse_command_id_list() { return ids; }",
        )
        .await
        .unwrap();
        let actor = vm
            .initialize_process_from_code(
                "/actor.c",
                r#"
                void create() { enable_commands(); parse_init(); parse_add_rule("take", "OBJ"); }
                mixed can_take_obj(object ob, string word) { return 1; }
                void do_take_obj(object ob, string word) {}
                int run() { set_this_player(this_object()); return parse_sentence("take gem"); }
            "#,
            )
            .await
            .unwrap()
            .context
            .process;
        for index in 0..64 {
            let noun = if index == 63 { "gem" } else { "stone" };
            vm.initialize_process_from_code(
                format!("/box_{index}.c"),
                r#"
                string *ids = ({ "box" });
                void create() { move_object(find_object("/actor")); }
                string *parse_command_id_list() { return ids; }
                int inventory_visible() { return 1; }
                int inventory_accessible() { return 1; }
            "#,
            )
            .await
            .unwrap();
            vm.initialize_process_from_code(
                format!("/gem_{index}.c"),
                format!(
                    r#"
                    string *ids = ({{ "{noun}" }});
                    void create() {{ move_object(find_object("/box_{index}")); }}
                    string *parse_command_id_list() {{ return ids; }}
                    mixed direct_take_obj(object ob, string word) {{ return 1; }}
                "#
                ),
            )
            .await
            .unwrap();
        }
        (vm, actor)
    });
    let template = TaskTemplate::from(vm.global_state.clone());
    let before = rt
        .block_on(apply_function_by_name(
            "run",
            &[],
            actor.clone(),
            template.clone(),
            None,
        ))
        .unwrap()
        .unwrap();
    assert_eq!(before, LpcRef::from(1));
    c.bench_function(NAME, |b| {
        b.to_async(&rt).iter(|| async {
            black_box(
                apply_function_by_name("run", &[], actor.clone(), template.clone(), None)
                    .await
                    .unwrap()
                    .unwrap(),
            )
        });
    });
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = criterion_benchmark, scope_walk
}
criterion_main!(benches);
