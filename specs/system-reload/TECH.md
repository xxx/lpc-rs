# Runtime system-object reload implementation

## Context

Before this change, `TaskContext` captured a physical simul-efun pointer and
master applies consulted `ObjectSpace` directly. `Task` retries keep their original context and entry seed.
The committer validates read/write conflicts; the attempt runner delivers deferred
effects. `compile_to_process` already separates compilation from placement.

## Proposed changes

- Resolve system objects through transactional path lookup; bootstrap initialization
  publishes their cells transactionally. Track a system revision for physical test
  fixtures before their first transactional publication.
- Supply the attempt's simul-efun process explicitly to in-game compilation.
  Nested contexts share an optional preparation view that pins the authorizing
  master while exposing newly prepared objects to compilation and initialization.
- Publish system-object physical projections inside the successful committer
  operation. Other effects retain their existing post-commit delivery.
- Give master driver entries and driver-fired simul-efun callbacks symbolic seeds
  resolved on every attempt; callbacks recheck their original owner. Bootstrap
  publication uses an explicit initializer seed that retired callbacks cannot use.
- Queue an authorized reload as a deferred VM operation. A dedicated AttemptBody
  compiles, checks compatibility, initializes, retires, and publishes the selected
  objects in one transaction. Any failure abandons that whole transaction.
- Expose request status through a bounded history owned by GlobalState. Preserve
  caller identity without rooting arbitrary LPC payloads in the queue.
- Validate process identity against its path cell so an old process cannot become
  live again when another process occupies its path.

## Testing and validation

- Exercise both LPC efuns, authorization and caller provenance, invalid targets,
  request aborts, status ownership, and history retention (behavior 1–3, 9).
- Exercise successful individual and paired reloads, fresh state, existing compiled
  calls/pointers, changed exports, and staged initialization (behavior 4, 6).
- Fail compilation and initialization after recording effects; force retries and
  confirm effects occur once (behavior 5).
- Hold dependent attempts across publication, verify their rejection and refreshed
  dispatch, and race reloads (behavior 7).
- Exercise stale references, local closures, and scheduled work (behavior 8).
- Run workspace build/tests, all-target clippy, formatting, and warning-free docs.
