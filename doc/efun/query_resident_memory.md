# query_resident_memory

`int query_resident_memory()`

Return jemalloc's resident-memory statistic in bytes for the driver process.
This includes allocator metadata and resident allocation pages; it is not the
OS's total process RSS or the number of live allocated bytes.

For retained diagnostic source storage, see
[`source_cache_stats`](source_cache_stats.md).
