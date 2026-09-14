//! Retained allocations and insertion cost for fresh, unchanged, and edited sources.

use std::{fs, hint::black_box, path::Path};

use criterion::{BatchSize, Criterion, Throughput, criterion_group, criterion_main};
use jemalloc_ctl::thread::{allocatedp, deallocatedp};
// Link the driver's global jemalloc allocator into this standalone bench.
use lpc_rs as _;
use lpc_rs_errors::source_map::SourceMap;
use lpc_rs_utils::read_lpc_file;
use tokio::runtime::Runtime;

#[path = "support/profiler.rs"]
mod profiler;

struct Source {
    name: String,
    text: String,
    edited: String,
}

impl Source {
    fn new(name: String, text: String) -> Self {
        let edited = format!("{text}// edited source\n");
        Self { name, text, edited }
    }
}

fn read_directory(root: &Path, dir: &Path, rt: &Runtime, sources: &mut Vec<Source>) {
    let mut entries: Vec<_> = fs::read_dir(dir)
        .expect("read source directory")
        .map(|entry| entry.expect("read directory entry"))
        .collect();
    entries.sort_by_key(|entry| entry.file_name());
    for entry in entries {
        let kind = entry.file_type().expect("read source file type");
        let path = entry.path();
        if kind.is_dir() {
            read_directory(root, &path, rt, sources);
        } else if kind.is_file()
            && matches!(
                path.extension().and_then(|ext| ext.to_str()),
                Some("c" | "h")
            )
        {
            let name = format!("/{}", path.strip_prefix(root).unwrap().display());
            let text = rt
                .block_on(read_lpc_file(&path))
                .expect("read LPC source")
                .text;
            sources.push(Source::new(name, text));
        }
    }
}

fn inputs() -> Vec<Source> {
    if let Some(root) = std::env::var_os("LPC_SOURCE_CACHE_BENCH_DIR") {
        let root = Path::new(&root);
        let rt = Runtime::new().expect("source-reading runtime");
        let mut sources = Vec::new();
        read_directory(root, root, &rt, &mut sources);
        assert!(
            !sources.is_empty(),
            "source directory contains no .c or .h files"
        );
        sources
    } else {
        let text = "// A source line retained for compiler diagnostics.\n".repeat(128);
        (0..1024)
            .map(|i| Source::new(format!("/bench/file_{i}.c"), text.clone()))
            .collect()
    }
}

fn insert(sources: &mut SourceMap, inputs: &[Source], edited: bool) {
    for input in inputs {
        let text = if edited { &input.edited } else { &input.text };
        sources.add(input.name.clone(), text.clone());
    }
}

fn populated(inputs: &[Source]) -> SourceMap {
    let mut sources = SourceMap::default();
    insert(&mut sources, inputs, false);
    sources
}

fn report_allocations(inputs: &[Source]) {
    let allocated = allocatedp::read().expect("jemalloc thread.allocatedp");
    let deallocated = deallocatedp::read().expect("jemalloc thread.deallocatedp");
    let live = || i128::from(allocated.get()) - i128::from(deallocated.get());
    let mut sources = SourceMap::default();

    // Keep input construction, I/O, and printing outside the allocation window.
    let before = live();
    insert(&mut sources, inputs, false);
    let loaded_bytes = live() - before;
    let loaded_stats = sources.stats().unwrap();
    for _ in 0..10 {
        insert(&mut sources, inputs, false);
    }
    let unchanged_bytes = live() - before;
    let unchanged_stats = sources.stats().unwrap();
    insert(&mut sources, inputs, true);
    let edited_bytes = live() - before;
    let edited_stats = sources.stats().unwrap();
    drop(sources);
    let after_drop = live() - before;

    assert_eq!(
        unchanged_bytes, loaded_bytes,
        "unchanged sources must not grow the cache"
    );
    assert_eq!(unchanged_stats, loaded_stats);
    assert!(
        edited_bytes > loaded_bytes,
        "edited versions must be counted"
    );
    assert_eq!(edited_stats.file_versions, 2 * loaded_stats.file_versions);
    assert_eq!(edited_stats.unique_files, loaded_stats.unique_files);
    assert_eq!(
        after_drop, 0,
        "dropping the isolated map must free its allocations"
    );
    for (name, bytes, added, stats) in [
        ("fresh", loaded_bytes, loaded_bytes, loaded_stats),
        (
            "unchanged_x10",
            unchanged_bytes,
            unchanged_bytes - loaded_bytes,
            unchanged_stats,
        ),
        (
            "edited",
            edited_bytes,
            edited_bytes - unchanged_bytes,
            edited_stats,
        ),
    ] {
        println!(
            "[source_cache:{name}] retained_bytes={bytes} added_bytes={added} \
             file_versions={} unique_files={} source_bytes={} source_capacity_bytes={} \
             filename_capacity_bytes={} line_index_bytes={}",
            stats.file_versions,
            stats.unique_files,
            stats.source_bytes,
            stats.source_capacity_bytes,
            stats.filename_capacity_bytes,
            stats.line_index_bytes,
        );
    }
}

fn source_cache(c: &mut Criterion) {
    let listing = std::env::args().any(|arg| arg == "--list");
    let inputs = if listing { Vec::new() } else { inputs() };
    if !listing {
        report_allocations(&inputs);
    }
    let mut group = c.benchmark_group("source_cache");
    group.throughput(Throughput::Elements(inputs.len() as u64));
    group.bench_function("fresh", |b| {
        b.iter_batched(
            SourceMap::default,
            |mut sources| {
                insert(&mut sources, &inputs, false);
                black_box(sources)
            },
            BatchSize::PerIteration,
        );
    });
    group.bench_function("unchanged", |b| {
        let mut sources = populated(&inputs);
        b.iter(|| insert(black_box(&mut sources), &inputs, false));
    });
    group.bench_function("edited", |b| {
        b.iter_batched(
            || populated(&inputs),
            |mut sources| {
                insert(&mut sources, &inputs, true);
                black_box(sources)
            },
            BatchSize::PerIteration,
        );
    });
    group.finish();
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = source_cache
}
criterion_main!(benches);
