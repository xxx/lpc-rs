# Compiler caches

The compiler caches parsed syntax trees and prepared macro definitions to reuse
work while loading or recompiling objects with common ancestors and headers.
Neither cache stores a completed inheritance composition. Preprocessing, permission
checks and semantic checking still run against the current compilation context.
Already loaded objects execute without consulting these caches.

## Configuration

These settings are fixed when a VM or compiler is constructed. Set them in the
explicit environment file or process environment; the `LPC_` prefix takes
precedence over an unprefixed name.
Explicitly supplied shared cache instances retain their own construction settings.

| Setting | Default | Meaning |
|---|---:|---|
| `COMPILER_CACHE_IDLE_TIME` | 300 | Seconds since an entry's last insertion or successful lookup |
| `PARSER_CACHE_MAX_ENTRIES` | 256 | Maximum retained parsed programs |
| `PARSER_CACHE_MAX_BYTES` | 67108864 | Estimated retention budget for parsed syntax (64 MiB) |
| `DEFINITION_CACHE_MAX_ENTRIES` | 4096 | Maximum retained prepared macro definitions |
| `DEFINITION_CACHE_MAX_BYTES` | 8388608 | Estimated retention budget for prepared macros (8 MiB) |

Zero idle time disables both caches. A zero entry or byte limit disables the
corresponding cache. None of these settings makes retention unlimited. Invalid,
negative or overflowing numbers cause a configuration error.

Successful use refreshes only that entry's idle deadline. Unrelated compilation
and failed lookups do not refresh it. Expired entries cannot produce cache hits.
Insertion also removes expired entries and evicts least recently used entries
until both size limits are satisfied. An entry larger than its entire byte budget
is not retained.

The running VM sweeps the caches on a blocking worker every
`min(COMPILER_CACHE_IDLE_TIME, 30)` seconds, independently of GC and object cleanup.
Cold entries are therefore removed without waiting for another compilation. The
sweep runs when the main event loop is running; bootstrap and standalone compiler
instances also expire entries on access/insertion, and release their caches when
dropped. As with other periodic maintenance, scheduler delays can defer a sweep.

Expiration removes cache ownership. A compilation already using an entry may
finish using it safely. Compiled programs and game objects are unaffected; a later
cache miss parses or prepares the input again.

## Memory and diagnostics

Byte charges are conservative estimates of retained structures, strings and spare
capacity. They are not measurements of live heap, RSS, or memory returned to the
OS. Shared allocations and allocator page retention make those quantities differ.

Native embedding and diagnostic code can use `ObjectSpace::compiler_cache_stats()`
to read each cache's entry count, `estimated_bytes`, and cumulative `hits`/`misses`.
`ParserCache::stats()` and `DefinitionCache::stats()` provide the same counters
for standalone compilers. Reading statistics does not refresh entries or sweep
them. Counts can include expired entries awaiting maintenance; in-flight users
can retain allocations after an entry has been removed. These diagnostics do not
add an LPC efun.

Tune the budgets and idle time using the mudlib's boot, object-loading and
development workloads. Shorter idle times release memory sooner but can repeat
parsing after pauses; disabling a cache also removes reuse within a loading burst.
