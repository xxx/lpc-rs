# Object inspection

## Summary

Dedicated efuns expose transaction measurements, global variables, and inheritance
for wizard tools and ad hoc debugging with `dump()`.

## Behavior

1. `mapping transaction_stats()` returns lifetime counters and current gauges
   documented in `doc/efun/transaction_stats.md`. Counters are observational,
   independent of the caller's world snapshot, and never reset by reading them.
   Attempt totals include only finished runner invocations, including nested
   applies, even when their outer transaction subsequently retries or fails.
   Elapsed times overlap across nested and concurrent invocations. Concurrent
   updates may straddle the sample; consumers must not assume exact identities
   between counters. Durations use integer nanoseconds, saturating at the largest
   LPC integer on conversion.
2. `mapping *variable_info(object ob)` returns every global declaration, including
   inherited, hidden, private, protected, and static variables, in storage order
   (ancestor blocks first, declarations in source order). A shared ancestor in a
   diamond appears once; unrelated declarations with the same name remain separate.
   Each entry has `name`, `program`, `type`, `flags`, and `value`. Program paths
   include `.c`; type and flags are descriptive strings.
3. Variable values observe the caller's transaction, including pending writes.
   The result array and entry mappings are fresh; replacing entry fields does not
   assign globals. Values retain normal LPC sharing: arrays/mappings, objects,
   and function pointers are not deep copies, and their usual operations remain
   available to an authorized inspector.
4. A live object may inspect itself. Inspection of another live object requires
   the master's `valid_variable_info(object caller, object target, string program)`
   to return a truthy value. `program` identifies the calling code, as in
   `valid_load`. Missing master/apply or refusal raises a permission error;
   an error from the apply propagates. The apply joins the caller's transaction
   and may rerun on retry. Target liveness is checked again after authorization.
5. `string *inherit_list(object ob)` returns direct parents in declaration order.
   `string *deep_inherit_list(object ob)` returns all ancestors, once each, in
   initialization order (ancestors before descendants), excluding the target's
   own program. Automatically inherited programs are included. Zero-global
   ancestors are included. Clones report their compiled program's inheritance.
   These efuns require no authorization and do not load programs.
6. All three object inspection efuns require one argument; zero or a destructed
   object returns an empty array, while other argument types raise an error.
   An object destructed during authorization likewise returns an empty array.
7. Existing `debug`, destruction, name resolution, global storage, and save/restore
   behavior remain unchanged.
