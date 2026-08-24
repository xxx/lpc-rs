//! Criterion profiler support: `cargo bench -- --profile-time <secs>` writes a
//! flamegraph per bench under `target/criterion/<id>/profile/`.

use std::{fs::File, os::raw::c_int, path::Path};

use criterion::{Criterion, profiler::Profiler};
use pprof::{ProfilerGuard, ProfilerGuardBuilder};

/// pprof's own criterion integration pins criterion ^0.5 and this repo is on
/// 0.8, so the two-hook `Profiler` trait is implemented here instead.
pub struct FlamegraphProfiler<'a> {
    frequency: c_int,
    active: Option<ProfilerGuard<'a>>,
}

/// A `Criterion` that emits a flamegraph SVG when run with `--profile-time`;
/// a plain bench run never starts the sampler.
pub fn profiled() -> Criterion {
    Criterion::default().with_profiler(FlamegraphProfiler {
        frequency: 999,
        active: None,
    })
}

impl Profiler for FlamegraphProfiler<'_> {
    fn start_profiling(&mut self, _benchmark_id: &str, _benchmark_dir: &Path) {
        let guard = ProfilerGuardBuilder::default()
            .frequency(self.frequency)
            // Sampling inside these libraries' signal frames deadlocks pprof (pprof README).
            .blocklist(&["libc", "libgcc", "pthread", "vdso"])
            .build()
            .expect("the pprof sampler failed to start");
        self.active = Some(guard);
    }

    fn stop_profiling(&mut self, _benchmark_id: &str, benchmark_dir: &Path) {
        let guard = self
            .active
            .take()
            .expect("stop_profiling without start_profiling");
        let report = guard
            .report()
            .build()
            .expect("the pprof report failed to build");
        std::fs::create_dir_all(benchmark_dir).expect("the profile dir could not be created");
        let file = File::create(benchmark_dir.join("flamegraph.svg"))
            .expect("the flamegraph file could not be created");
        report
            .flamegraph(file)
            .expect("the flamegraph failed to render");
    }
}
