use std::hint::black_box;

use criterion::{Criterion, Throughput, criterion_group, criterion_main};
use lpc_rs::compiler::Compiler;
use lpc_rs_core::register_counter::RegisterCounter;

#[path = "support/profiler.rs"]
mod profiler;

fn allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("register_allocation");
    group.throughput(Throughput::Elements(1024));
    group.bench_function("sequential", |b| {
        b.iter(|| {
            let mut counter = RegisterCounter::new(1);
            for _ in 0..1024 {
                black_box(black_box(&mut counter).next().unwrap());
            }
        });
    });
    group.bench_function("scoped", |b| {
        b.iter(|| {
            let mut counter = RegisterCounter::new(1);
            for _ in 0..32 {
                counter.push();
                for _ in 0..32 {
                    black_box(black_box(&mut counter).next().unwrap());
                }
                black_box(counter.pop());
            }
        });
    });
    group.finish();
}

fn compilation(c: &mut Criterion) {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let small =
        "int run(int value) { if (value < 2) return value; return value + run(value - 1); }";
    let temporaries = format!(
        "int run(int value) {{ {} return value; }}",
        "value += 1;\n".repeat(1024)
    );
    let globals = format!(
        "{} int run() {{ return value_0 + value_255; }}",
        (0..256)
            .map(|index| format!("int value_{index} = {index};\n"))
            .collect::<String>()
    );
    let closures = (0..64)
        .map(|index| format!("function make_{index}() {{ return (: $1 + $16 :); }}\n"))
        .collect::<String>();
    let mut group = c.benchmark_group("register_allocation_compile");
    for (name, source) in [
        ("small_function", small),
        ("temporaries", temporaries.as_str()),
        ("globals", globals.as_str()),
        ("closure_positionals", closures.as_str()),
    ] {
        group.bench_function(name, |b| {
            b.to_async(&runtime).iter(|| async {
                black_box(
                    Compiler::default()
                        .compile_string("/allocation.c", black_box(source))
                        .await
                        .unwrap(),
                )
            });
        });
    }
    group.finish();
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = allocation, compilation
}
criterion_main!(benches);
