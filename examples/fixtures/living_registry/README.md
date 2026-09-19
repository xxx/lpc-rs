# Living registry contention fixture

Run from the repository root:

```sh
cargo run --release --example living_registry_contention -- \
  --samples 3 --operations 8000 --names 128,1024 --workers 1,4,8 --writes 0,1,10
```

The example emits CSV on stdout and progress on stderr. `--variants` accepts
`scan,indexed,sharded`; `--work` adds that many LPC loop iterations after each
lookup, before the transaction commits. Operations are the total across all
workers, and must be a multiple of 100 times every selected worker count.
Supported worker counts divide eight, to keep the 800-operation warmup mix exact.

`scan.c` maps living names to arrays of objects. Registration scans every entry
to remove the object from its previous names, then adds it to the requested name.
Lookups filter out invalid or non-living objects.
`indexed.c` remembers each object's old name, replacing the whole-registry scan
with updates to the old and new entries. `sharded.c` uses the same implementation
with 64 fixed mappings for each of the name and reverse indexes.

Each case uses a fresh VM with actual simul-efuns and living objects. There are
`--names` stable lookup names and two alternating names per writer. Writers
rename their own object; lookups target only the stable names. This intentionally
isolates conflicts between logically unrelated entries. The registry key count
stays at `names + 2 * workers`.

The write percentage is exact over each worker's 100-operation cycles, with
phases staggered between workers. Cases run in a reproducibly shuffled order per
sample. Compilation, population, warmup, GC, and final verification are outside
timing. GC does not run during the measured batch. Every lookup checks its result;
final verification checks all registrations, removed aliases, and rename counts.

The workload uses NPCs, so it does not measure the `players` array,
`find_player`, connection changes, destruction, or duplicate-name lists. These
are synthetic saturation measurements, not observed live-MUD traffic.
