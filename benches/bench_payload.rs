use std::hint::black_box;

use criterion::{Criterion, criterion_group, criterion_main};
use indexmap::IndexMap;
use lpc_rs::interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef};
use thin_vec::ThinVec;

#[path = "support/profiler.rs"]
mod profiler;

/// Common operations benchmarked for each candidate payload container.
///
/// `set` and `push` return a new container, which models the persistent
/// (copy-on-write) usage: the write produces the next version and the
/// original stays intact. `ThinVec` pays for that with a structural clone
/// per write, so its "index write / push" numbers are upper bounds for
/// in-place mutation, not a like-for-like comparison.
trait PayloadVector {
    fn from_slice(slice: &[LpcRef]) -> Self;
    fn index_read(&self, idx: usize) -> &LpcRef;
    fn index_write(&self, idx: usize) -> Self;
    fn push(&self) -> Self;
    fn len(&self) -> usize;
    fn iter(&self) -> impl Iterator<Item = &LpcRef>;
    fn clone_self(&self) -> Self;
    fn eq(&self, other: &Self) -> bool;
}

impl PayloadVector for ThinVec<LpcRef> {
    fn from_slice(slice: &[LpcRef]) -> Self {
        ThinVec::from(slice)
    }

    fn index_read(&self, idx: usize) -> &LpcRef {
        &self[idx]
    }

    fn index_write(&self, idx: usize) -> Self {
        let mut v = self.clone();
        v[idx] = LpcRef::from(42);
        v
    }

    fn push(&self) -> Self {
        let mut v = self.clone();
        // The trait's own zero-arg `push` shadows `ThinVec::push` inside
        // this impl, so the underlying push is qualified explicitly.
        ThinVec::push(&mut v, LpcRef::from(43));
        v
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn iter(&self) -> impl Iterator<Item = &LpcRef> {
        // `self.iter()` would resolve to this trait's own `iter` (ThinVec
        // has no inherent one; the slice's comes via `Deref`), so qualify
        // against the slice type where `iter` actually lives.
        <[_]>::iter(self)
    }

    fn clone_self(&self) -> Self {
        self.clone()
    }

    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl PayloadVector for imbl::Vector<LpcRef> {
    fn from_slice(slice: &[LpcRef]) -> Self {
        imbl::Vector::from(slice)
    }

    fn index_read(&self, idx: usize) -> &LpcRef {
        &self[idx]
    }

    // imbl's immutable write is `update`; `set` is the in-place `&mut`
    // variant, which is not what the persistent bench measures.
    fn index_write(&self, idx: usize) -> Self {
        self.update(idx, LpcRef::from(42))
    }

    // imbl has no immutable `push_back`; this is the persistent form of
    // it — clone is a shared spine with a fresh leaf chunk, so the cost
    // here is the promotion, not a deep copy.
    fn push(&self) -> Self {
        let mut next = self.clone();
        next.push_back(LpcRef::from(43));
        next
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn iter(&self) -> impl Iterator<Item = &LpcRef> {
        self.iter()
    }

    fn clone_self(&self) -> Self {
        self.clone()
    }

    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl PayloadVector for rpds::Vector<LpcRef> {
    fn from_slice(slice: &[LpcRef]) -> Self {
        // `rpds::Vector` has no `From<&[T]>`; `FromIterator` is the
        // documented construction path.
        slice.iter().cloned().collect()
    }

    fn index_read(&self, idx: usize) -> &LpcRef {
        &self[idx]
    }

    // `set` returns `Option`: `None` means the index was out of range.
    // The probe index is always in range, so `None` is a bug here.
    fn index_write(&self, idx: usize) -> Self {
        self.set(idx, LpcRef::from(42))
            .expect("probe index is always in range")
    }

    fn push(&self) -> Self {
        self.push_back(LpcRef::from(43))
    }

    fn len(&self) -> usize {
        self.len()
    }

    fn iter(&self) -> impl Iterator<Item = &LpcRef> {
        self.iter()
    }

    fn clone_self(&self) -> Self {
        self.clone()
    }

    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

/// Benchmarked containers use a cheap-to-clone case, so the measured time is the
/// container's own and not the elements' allocation.
fn elements(size: usize) -> Vec<LpcRef> {
    (0..size).map(|i| LpcRef::from(i as i64)).collect()
}

/// The index under test: not `0` (persistent vectors' head is a special
/// case), and small enough to sit in the tree body rather than the fast
/// tail chunk for every size benchmarked.
fn probe_index(size: usize) -> usize {
    (size % 3).max(1)
}

/// Bench one candidate container against the plan's operation set.
fn bench_vector<P>(c: &mut Criterion, name: &str)
where
    // No `Send` bound: criterion 0.8.2's `bench_function` runs the closure
    // on the calling thread, and `rpds::Vector`'s default `Rc` pointer kind
    // is not `Send`. The containers are never shared across threads here.
    P: PayloadVector,
{
    let mut group = c.benchmark_group(format!("payload/{name}"));

    for &size in &[4usize, 64, 1024] {
        let els = elements(size);
        let vec = P::from_slice(&els);
        let idx = probe_index(size);

        group.bench_function(format!("index_read/size_{size}"), |b| {
            b.iter(|| {
                P::index_read(black_box(&vec), idx);
                black_box(())
            })
        });
        group.bench_function(format!("index_write/size_{size}"), |b| {
            b.iter(|| {
                let next = P::index_write(black_box(&vec), idx);
                black_box(next.len());
            })
        });
        group.bench_function(format!("push/size_{size}"), |b| {
            b.iter(|| {
                let next = P::push(black_box(&vec));
                black_box(next.len());
            })
        });
        group.bench_function(format!("iterate/size_{size}"), |b| {
            b.iter(|| {
                let sum = vec
                    .iter()
                    .map(|e| e as *const LpcRef as usize)
                    .sum::<usize>();
                black_box(sum)
            })
        });
        // For the persistent vectors this is a refcount bump on the shared
        // spine (structural sharing), not a deep copy; `ThinVec` actually
        // allocates and copies.
        group.bench_function(format!("clone/size_{size}"), |b| {
            b.iter(|| {
                let next = P::clone_self(black_box(&vec));
                black_box(next.len());
            })
        });
        // The persistent vectors implement `PartialEq` structurally
        // (refcount bumps per element); `ThinVec` short-circuits by
        // length and pointer for a self-comparison.
        group.bench_function(format!("eq/size_{size}"), |b| {
            b.iter(|| P::eq(black_box(&vec), black_box(&vec)))
        });
    }

    group.finish();
}

/// Mappings keep `IndexMap` and are copied on write into the changeset, so
/// `clone()` at representative sizes _is_ the mapping story in full.
fn bench_mapping_clone(c: &mut Criterion) {
    let mut group = c.benchmark_group("payload/mapping");

    for &size in &[4usize, 64, 1024] {
        let mapping = (0..size)
            .map(|i| {
                (
                    LpcRef::from(LpcInt(i as i64)),
                    LpcRef::from(LpcInt((i + 1) as i64)),
                )
            })
            .collect::<IndexMap<LpcRef, LpcRef>>();

        group.bench_function(format!("indexmap_clone/size_{size}"), |b| {
            b.iter(|| {
                let copy = mapping.clone();
                black_box(copy.len());
            })
        });
    }

    group.finish();
}

fn criterion_benchmark(c: &mut Criterion) {
    bench_vector::<ThinVec<LpcRef>>(c, "thinvec");
    bench_vector::<imbl::Vector<LpcRef>>(c, "imbl_vector");
    bench_vector::<rpds::Vector<LpcRef>>(c, "rpds_vector");
    bench_mapping_clone(c);
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = criterion_benchmark
}
criterion_main!(benches);
