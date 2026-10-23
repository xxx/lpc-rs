# Maintaining the imbl dependency

lpc-rs uses [a small imbl fork](https://github.com/xxx/imbl/tree/lpc-rs-gc)
for `GenericHashMap::into_drop_chunks`, which transfers map ownership into opaque
chunks that can be destroyed independently. The dependency in `Cargo.toml` points
directly to an exact Git revision; commit `Cargo.lock` with every revision change.
Normal Cargo builds and downstream Git/path consumers need no preparation command,
root patch override, submodule or vendored source.

The initial pin is [`5fa322d043faee90d32c63430b51a52be3ad121d`](https://github.com/xxx/imbl/commit/5fa322d043faee90d32c63430b51a52be3ad121d),
based on upstream imbl `v7.0.2` at
[`834c67f1e08c63a4d662a4132425026a4cea7b25`](https://github.com/jneem/imbl/commit/834c67f1e08c63a4d662a4132425026a4cea7b25).
Its Rust source and original manifest match the published 7.0.2 archive. A separate
base correction changes the upstream lockfile's stale self-version from 7.1.0 to
7.0.2; the feature commit contains the API extension, tests and fork maintenance note.

## Ownership contract

A unique root transfers its entries without cloning keys/values or traversing
all descendants. A shared root remains one chunk. Dropping a chunk releases its
ownership while other snapshots retain shared nodes. The extension preserves
existing map layout, lookups, updates and ordinary destruction; it adds no unsafe
code, runtime dependency or worker policy.

The fork's `tests/drop_chunks.rs` and this workspace's
[`tests/persistent_map_test.rs`](../tests/persistent_map_test.rs) check empty and
unique maps, shared roots and descendants, collisions, exact destruction on two
threads, and values that must remain on their original thread. The workspace tests
keep these contracts in the normal CI gate when its dependency revision changes.

## Updating the pin

1. Fetch the intended upstream release in the fork checkout and create a new
   branch from its reviewed tag or commit. Keep earlier published commits
   available for consumers pinned to them.
2. Cherry-pick the owned-branch feature commit onto that base. Inspect upstream
   changes to root/node ownership even when the patch applies without conflicts.
   Update the fork maintenance note for the new base and any adaptations. Keep
   unrelated release corrections separate from the feature.
3. Run the fork's ownership tests with default and all features, its library tests,
   formatting and documentation checks. Review the diff against the upstream base.
4. Publish the tested branch in the fork and set the full commit SHA in this
   repository's direct `imbl` Git dependency. Update only the imbl lockfile entry;
   review any additional resolution changes before accepting them.
5. Run `cargo test --workspace --locked --test persistent_map_test`, the complete
   workspace tests and doctests, formatting, warning-free Clippy and documentation
   checks, and `cargo build --workspace --locked`. Commit the manifest and lockfile
   together with any necessary contract-test or maintenance-guide changes.

If upstream supplies equivalent ownership-preserving disposal, switch to a reviewed
registry release and run the same checks before removing the fork dependency.
Keep GC worker integration and policy in lpc-rs rather than adding them to imbl.
