# Object inspection implementation

## Context

`Program::global_variables` is a name lookup table that omits hidden declarations.
`Program::layout` retains every program in initialization order but does not retain
direct inheritance edges. `GlobalState` already exposes committer statistics,
queue length, and attempt telemetry to Rust callers.

## Proposed changes

- Add a complete list of global declaration metadata and a list of direct parent
  paths to `Program`. Build both during `CodegenWalker::into_program`; preserve
  the current name lookup table. Relocate declaration slots alongside instruction
  operands and symbols when importing a parent. Deduplicate declarations by
  relocated slot so diamond inheritance retains one shared instance.
- Implement the inheritance efuns using direct parent paths and the existing
  layout. Resolve live object arguments transactionally, without loading files.
- Implement `variable_info` with `valid_apply` for cross-object access, followed
  by a liveness recheck and transaction reads of every global slot. Mint the
  result array and mappings in that same transaction.
- Implement `transaction_stats` as an asynchronous efun that reads the existing
  committer protocol and atomic telemetry, with separately named commit conflicts
  and completed-run conflicts. No new instrumentation on the execution path.
- Register all efuns in the shared prototype/dispatch table and document each
  efun and the new master apply.

## Testing and validation

- Exercise the LPC interfaces through the interpreter, covering hidden names,
  sibling slot relocation, diamonds, declaration flags/types, pending writes,
  normal value sharing, empty objects, and destroyed targets (behavior 2–3, 5–6).
- Verify direct versus transitive inheritance, zero-global ancestors, clones,
  automatic inheritance, and unchanged dispatch/save behavior (behavior 5, 7).
- Verify cross-object refusal, allowance, missing master/apply, apply errors,
  caller identity, and destruction during authorization (behavior 4).
- Compare efun stats to controlled runtime activity and forced retries, including
  read-only commits and a failed run; verify conversion saturation (behavior 1).
- Run workspace tests, clippy on all targets, formatting, and warning-free rustdoc.
