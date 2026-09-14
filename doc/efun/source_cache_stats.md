# source_cache_stats

`mapping source_cache_stats()`

Return a fresh mapping of integer measurements for the driver's diagnostic
source cache. No arguments or authorization are required. Values too large for
an LPC integer saturate at its maximum positive value.

| Key | Meaning |
| --- | --- |
| `file_versions` | Distinct filename/content pairs retained |
| `unique_files` | Distinct filenames across those versions |
| `source_bytes` | Uncompressed UTF-8 text bytes across all versions |
| `source_capacity_bytes` | Source string buffer capacities, including spare capacity |
| `filename_capacity_bytes` | Filename string buffer capacities across all versions |
| `line_index_bytes` | Line-start entries times the native pointer size, excluding spare capacity |

`source_bytes` is included in `source_capacity_bytes`; do not add them together.
The sum of `source_capacity_bytes`, `filename_capacity_bytes`, and
`line_index_bytes` is a lower bound on retained heap storage. It excludes the
file table, content lookup index, spare line-index capacity, and allocator
rounding and metadata. These fields do not measure resident pages or temporary
compiler buffers. The line index has an entry for an empty source and for the
empty line following a trailing newline.

Files retain their full text, including comments and whitespace. Identical text
under the same filename reuses an entry; edits retain another full version.
Reverting to previous contents reuses that version. Failed compilations also
register sources, and entries are not evicted or reclaimed when objects die.
`file_versions - unique_files` counts additional retained versions.

The sample covers the entire driver process, including every VM in that process,
and is independent of the calling transaction's snapshot. A read does not reset
the measurements. Fields are sampled under one read lock; the scan visits each
version and its line index without copying or scanning source text. Concurrent
compiles can change the next sample. Constructing the result mapping happens
after releasing the cache lock and participates in the caller's transaction.

```c
dump(source_cache_stats());
```

## Measuring the full allocation cost

Run the isolated jemalloc benchmark in release/bench mode:

```sh
cargo bench --bench bench_source_cache -- --test
```

It prints retained allocation bytes after loading 1,024 synthetic files, after
ten unchanged passes, and after editing every file once. `retained_bytes` is the
cumulative live allocation count; `added_bytes` is growth since the preceding
sample. The benchmark checks that unchanged passes retain no additional bytes
and that dropping the isolated source map frees all measured allocations.
Omit `--test` to also collect Criterion timings.

To use a mudlib's sources instead:

```sh
LPC_SOURCE_CACHE_BENCH_DIR=/path/to/mudlib \
  cargo bench --bench bench_source_cache -- --test
```

This reads all `.c` and `.h` files recursively, skips symlinks, and applies the
compiler's text decoding and trailing-newline handling. It registers every file
without compiling it. Results describe that corpus, not the subset or historical
versions currently loaded in a running driver.

The benchmark measures per-thread allocated bytes minus freed bytes, starting
before allocating the strings passed to the source map. Input fixtures, disk
reads, and output are outside the measurement. This captures retained string
buffers, internal vectors and tables, and allocator size rounding; it excludes
allocator metadata and unused resident pages. Shared allocator pages prevent
precise attribution of process RSS to the cache.

## See also

[`query_resident_memory`](query_resident_memory.md), [`dump`](dump.md)
