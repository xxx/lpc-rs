# ulib implementation

## Context

At `9267aa90db274e1bf031daf81beb12508886f413`, the driver boots a configured
master and supports the necessary login, command, lifecycle and network hooks.
[`tests/fixtures/mudlib`](../../tests/fixtures/mudlib) exercises the protocol,
but its purpose and documentation do not make it a distributable starter lib.
[`doc/apply`](../../doc/apply) and the apply constants in
[`src/interpreter/mod.rs`](../../src/interpreter/mod.rs) define the coverage
inventory for [the product behaviour](PRODUCT.md).

## Implementation

- Keep all distributable material in `ulib/`: LPC, its environment file,
  licence and documentation. No driver runtime changes or embedded installer
  are needed. Make the root README's first server example use this directory.
- `secure/master.c` supplies boot, connection, security and diagnostic policy;
  `obj/player.c` is both the name prompt and connected body, so no `exec` is
  needed. `std/room.c` supplies room commands; `room/lounge.c` inherits it and
  sets the description. `include/ulib.h` holds the few shared constants.
- Name selection scans `users()` and reads each body's `query_name()` before
  assigning its own name. Those reads and the assignment share one transaction,
  so concurrent claims retry and recheck occupancy. No separate registry is
  needed. Room output uses `all_inventory`, `interactive` and `tell_object`;
  `catch_tell` terminates delivery with `write_socket`.
- Keep optional hooks in `#if 0` examples grouped by their destination object.
  These are valid LPC when enabled independently and do not clutter the active
  command path. The copied apply index documents every hook and return value,
  including distinctions between similarly named master and object hooks.
- Keep comments focused on hook contracts and LPC-specific behaviour. Longer
  lessons belong in the copied Markdown docs, with links back to actual code.
  Each function has adjacent reference documentation covering its role,
  arguments and result, including apply defaults and the example's rationale;
  the source must be understandable without first reading the separate index.

## Testing and validation

- Compile every live LPC object and every optional example with its disabled
  region enabled, using the library's root and include directory (behaviour 1,
  11, 13); the driver integration test reads the supplied configuration directly.
- Check the apply index and sample source against every page in `doc/apply`,
  including parser-handler families, so additions cannot silently leave the
  learning material incomplete (behaviour 11–12).
- Compare compiled prototypes from canonical signatures with all matching
  examples and the ulib index, including argument types/counts and return
  types; mixed results may be narrowed and create's ignored result may be void.
  Compare name sets in both directions to catch removed and renamed applies.
- Record reviewed SHA-256 digests of `doc/apply/*.md` recursively and the
  `parse_add_rule` reference in `tests/fixtures/ulib_apply_review.json`.
  A reference change fails the normal test run until the affected examples
  and prose are reviewed and the named paths are explicitly recorded with
  `tests/update_ulib_apply_review.py` (behaviour 14). This is a review reminder,
  not semantic verification; AGENTS.md requires driver changes to update the
  canonical reference and their behaviour tests as well.
- Boot a copied ulib outside the repository and connect real TCP clients to
  verify names, simultaneous duplicate claims, chat, commands, disconnects and
  shutdown (behaviour 1–10). Bind an ephemeral loopback port for automated tests.
  The subprocess test runs on Unix, where it sends SIGINT to exercise the same
  orderly shutdown as Ctrl-C; source and policy checks run on all platforms.
- Exercise permission hooks through LPC and assert expected allow/deny results
  without changing global driver behaviour (behaviour 13).
- Validate copied documentation links, root README instructions and source
  package inclusion, then run workspace build, tests, Clippy, fmt and rustdoc.

## Validation results

The workspace build and test suite pass, along with Clippy for all targets and
features, formatting, and `RUSTDOCFLAGS="-D warnings" cargo doc --workspace`.
The new tests cover the copied-directory startup, guest chat and connection
lifecycle, permission denials, compilation of enabled examples, apply coverage
and local documentation links. Cargo's source-package listing includes all
15 files under `ulib/`.

The command tutorial was also checked by copying its LPC snippets into a fresh
mudlib copy and verifying the wave messages through two real TCP clients.

Deliberately changed argument counts, argument types, return types, index
signatures, missing examples and removed applies were rejected by the contract
checks. Prose-only and dynamic parser-contract edits triggered the review check.
The review updater was exercised in a temporary copy for selective updates,
additions, deletions, invalid paths and CRLF normalisation.
