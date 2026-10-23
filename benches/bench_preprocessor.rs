use std::sync::Arc;

use criterion::{Criterion, criterion_group, criterion_main};
use lpc_rs::compiler::{
    compilation_context::CompilationContextBuilder,
    preprocessor::{Preprocessor, definition_cache::DefinitionCache},
    source::CompilerSource,
};
use lpc_rs_utils::config::Config;

#[path = "support/profiler.rs"]
mod profiler;

fn definitions(c: &mut Criterion) {
    let runtime = tokio::runtime::Runtime::new().unwrap();
    let config = Arc::new(Config::default());
    let source = CompilerSource::new("/definitions.c", &config);
    let mut code = String::new();
    for i in 0..256 {
        code += &format!("#define VALUE_{i} ({i} + 1)\n#define LABEL_{i}(value) #value\n");
    }
    code += "VALUE_0 VALUE_255 LABEL_0(example)\n";
    let shared = Arc::new(DefinitionCache::default());
    let scan = |cache| {
        let context = CompilationContextBuilder::default()
            .source(source.clone())
            .config(config.clone())
            .definition_cache(cache)
            .build()
            .unwrap();
        async {
            Preprocessor::new(context)
                .scan("/definitions.c", &code)
                .await
                .unwrap()
        }
    };
    runtime.block_on(scan(shared.clone()));
    let mut group = c.benchmark_group("preprocessor_definitions");
    for reuse in [false, true] {
        group.bench_function(if reuse { "reused" } else { "fresh" }, |b| {
            b.to_async(&runtime).iter(|| {
                scan(if reuse {
                    shared.clone()
                } else {
                    Arc::default()
                })
            });
        });
    }
    group.finish();
}

criterion_group! { name = benches; config = profiler::profiled(); targets = definitions }
criterion_main!(benches);
