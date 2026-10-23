# query_resident_memory

`int query_resident_memory()`

Return jemalloc's resident-memory statistic in bytes for the driver process.
This includes allocator metadata and resident allocation pages; it is not the
OS's total process RSS or the number of live allocated bytes.

For peak process RSS reported by the operating system, use
[`rusage`](rusage.md)'s `maxrss` field, measured in KiB.

For retained diagnostic source storage, see
[`source_cache_stats`](source_cache_stats.md).

For estimated LPC object, program and data storage, see
[`object_memory_info`](object_memory_info.md).
