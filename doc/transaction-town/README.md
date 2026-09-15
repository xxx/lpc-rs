# Transaction Town

An offline, interactive explanation of transactions in the LPC driver. Owning task A takes a snapshot, reads cells, stages private changes and output, then submits to the single committer. Another task B can commit in the meantime. A rejected attempt takes the return road through retry admission and starts again; an accepted attempt delivers its receipt.

The cart carries actual model state: one blue tile per tracked read, one gold tile per pending write or merge, and one violet tile per buffered effect. The live table compares A’s snapshot, its changes, and the committed world.

## Open it

Open [index.html](index.html) directly in a browser. It needs no build, server, network, or runtime dependencies.

For local HTTP viewing, serve the repository root so the source links work too:

```sh
python3 -m http.server 8000 --bind 127.0.0.1
```

Then visit `http://127.0.0.1:8000/doc/transaction-town/`. The site itself can also be hosted as a standalone directory; links into the Rust source require the surrounding repository.

## Experiments

With default settings, gold starts at 10. A reads 10 and proposes 14. B commits 13 before A submits. A’s read is invalidated, so A retries from 13, commits 17, and delivers one receipt.

| Change | Expected result |
|---|---|
| Uncheck **B writes gold** | B changes bells; A’s gold read remains valid. Gold ends at 14, bells at 3, with no retry. |
| Choose **Blind assignment** | A sets gold to 4 without reading it. B’s earlier write does not invalidate A, and A overwrites gold in commit order. |
| Choose **Unread merge add** | A queues `add(4)`. The committer folds it onto B’s 13, producing 17 without a read conflict. |
| Set **Competing writes** to 0 | B performs no action. A finishes in one attempt. |
| Raise **Competing writes** in Read + write mode | B can invalidate more attempts, including an admitted retry. A recomputes every time and still delivers only one receipt. |
| Move either amount slider | Future work uses the new amount; already staged changes retain their recorded value. |

B commits once per A attempt, up to the selected competing-write limit. When A succeeds, the run ends, even if that limit is not exhausted. Each B action is a fresh first attempt, so A’s retry turn does not block it. The operation selector begins a new run; the other model controls affect operations not yet performed. Tour speed changes animation and reading time only.

Unread merge mode selects the runtime operation directly. It does not claim every LPC increment compiles to a merge. Reading a pending merge would turn it into a tracked read and a private write.

## Controls and pacing

| Control | Action |
|---|---|
| Play / Space | Play or pause, including a reading stop. |
| Next / S | Advance exactly one station, then pause. |
| New run | Restart the example and keep which stations have been read. |
| Replay / R | Restart and clear reading history. |
| Follow cart / F | Follow A with the camera. |
| Labels / L | Toggle landmark labels. |
| Drag / wheel / pinch | Pan / zoom; dragging turns following off. |
| Fit / double-click | Show the whole town. |
| Station buttons / landmarks | Pause and read that district. Return to the live tour resumes. |
| About / Escape | Open / close the accuracy notes; opening pauses the tour. |
| Phone details handle / gear | Expand and pause for the explanation and live state / open experiment controls. |

First visits stop for `clamp(words / 3.8 + 3.5, 9, 26)` seconds, divided only by the speed slider. Repeat visits use a 1.8-second beat with the template’s repeat-visit acceleration. Reading history survives New run. The default first tour takes approximately five minutes at 1×. The progress bar displays the remaining stop time; pause holds indefinitely. Reduced-motion preferences start the tour paused.

## Stations and driver sources

| Place | Operation | Rust implementation |
|---|---|---|
| Snapshot Archive | Open an attempt on an immutable snapshot. | [snapshot.rs](../../src/interpreter/stm/snapshot.rs), [retry.rs](../../src/interpreter/stm/retry.rs) |
| Cell Library | Track snapshot reads; own writes answer first. | [changeset.rs](../../src/interpreter/stm/changeset.rs) |
| Private Workshop | Stage a write or an unread addition merge. | [mod.rs](../../src/interpreter/stm/mod.rs), [merge.rs](../../src/interpreter/stm/merge.rs) |
| Outbox Depot | Record an attempt-local effect. | [effects.rs](../../src/interpreter/stm/effects.rs), [mod.rs](../../src/interpreter/stm/mod.rs) |
| Neighbour Yard | Run and commit another owning task’s action. | [committer.rs](../../src/interpreter/stm/committer.rs) |
| Committer Hall | Check newer write history, fold merges, atomically publish or reject. | [committer.rs](../../src/interpreter/stm/committer.rs) |
| Return Depot | Show the discarded attempt’s empty buffers. | [retry.rs](../../src/interpreter/stm/retry.rs) |
| Retry Turnstile | Acquire a FIFO turn for the invalidated cell before the next snapshot. | [admission.rs](../../src/interpreter/stm/admission.rs), [retry.rs](../../src/interpreter/stm/retry.rs) |
| Delivery Office | Deliver successful output after commit and release of the retry turn. | [retry.rs](../../src/interpreter/stm/retry.rs), [effects.rs](../../src/interpreter/stm/effects.rs) |
| World Square | Inspect the completed action. | [transaction diagnostics](../transaction-diagnostics.md) |

The owning-task and nested-apply terminology follows [CONTEXT.md](../../CONTEXT.md). Nested applies share their owner’s transaction. The committer validates and publishes in one operation; the road from that hall to delivery does **not** split validation from publication. Blind writes have no separate write-write adjudication. Cell-specific retry admission reduces contention and does not replace the committer’s validation or block first attempts.

## How much of it is real

This ledger also appears in the page’s About dialog.

**Computed:** immutable snapshot values; read tracking including absent cells and reads of own writes; private changes; intersections with newer write history; commit-order blind writes; integer addition merges; atomic rejection; fresh-snapshot retries; FIFO cell-turn bookkeeping; and delivery of only the successful attempt’s buffered receipt. Every visible value comes from [js/model.js](js/model.js).

**Scaled down:** two named scalar cells, one followed owner, and at most three competing B actions stand in for the driver’s VarId-keyed world and many concurrent owning tasks. Versions start at v0 for each run. Only addition is demonstrated from the driver’s larger merge-operation family. Small exact JavaScript integers replace LPC’s integer width and wrapping arithmetic.

**Assumed:** gold begins at 10 and bells at 0. B commits before A at the Neighbour Yard for the selected number of attempts. Only A queues for a retry turn, so that queue has no waiting time. Receipts always deliver into an in-memory list. Changes to parameters affect operations not yet performed; the operation selector starts a fresh run.

**Illustrated / omitted:** this is a JavaScript teaching model, not the Rust VM or LPC execution. Road speed, buildings, and reading stops have no performance meaning. Snapshot maps are copied instead of structurally shared; all write history is retained. Worker scheduling, history reclamation, GC, timeouts, cancellation, backoff, general WorldValues, object lifecycles, and real I/O failures are not simulated. A successful state commit is separate from effect delivery; real delivery can fail, and output order across tasks is not guaranteed.

The standalone model also exercises reads of absence, removal, read-only commits without version increments, and merge type mismatch rejection in tests. These do not each have a separate animated scenario. The simple queue implements FIFO bookkeeping, but the tour has only one queued owner and does not simulate waiting or scheduler fairness.

## Files and architecture

| File | Responsibility |
|---|---|
| `index.html`, `css/styles.css` | Controls, responsive panels, accessible dialog, and fidelity ledger. |
| `js/iso.js` | Unchanged template projection, geometry, routes, and deterministic hashing. |
| `js/model.js` | Standalone transaction algorithm and example operations; no DOM or rendering. |
| `js/world.js` | Routes, waypoint-anchored stations, landmarks, narration, reading durations. |
| `js/sim.js` | Travel, reading stops, and branches driven by the commit result. |
| `js/render.js` | One sorted pass over landmark and vehicle footprints; a separate DPR-aware label pass. |
| `js/ui.js` | Live views of the model, experiments, transport, and dialog controls. |
| `js/main.js` | Template camera/input loop, with responsive framing and keyboard adaptations. |
| `tests/model.test.cjs` | Transaction invariants, all experiment combinations, stepping and pacing. |
| `tests/smoke.mjs` | Browser station coverage, branches, controls, offline use, and responsive screenshots. |

`World.stations` binds operations to waypoint distances; `Sim.advanceRoute()` owns every route branch. Model state changes only at stations, so travel cannot manufacture a result. All ground-footprint objects share one depth-sorted painter’s pass. Keep landmarks away from the road when editing the map: a building whose half-footprint exceeds its setback can hide the cart. Label coordinates retain the device-pixel-ratio transform and collisions are resolved before painting.

## Verify

From the repository root:

```sh
(for f in doc/transaction-town/js/*.js; do node --check "$f" || exit 1; done)
node --test doc/transaction-town/tests/model.test.cjs
```

Browser tests require Playwright and Chromium, only for testing. Reuse an available Playwright installation or install it in an isolated test directory:

```sh
mkdir -p local/transaction-town-tools
npm install --prefix local/transaction-town-tools playwright
local/transaction-town-tools/node_modules/.bin/playwright install chromium
NODE_PATH="$PWD/local/transaction-town-tools/node_modules" \
  node doc/transaction-town/tests/smoke.mjs --out local/transaction-town-smoke
NODE_PATH="$PWD/local/transaction-town-tools/node_modules" \
  node doc/transaction-town/tests/smoke.mjs --dpr 2 --out local/transaction-town-smoke-retina
```

The default test URL is `file://` for this explainer. Supply an HTTP URL as the first argument to test a server. Set `PLAYWRIGHT_CHROMIUM_EXECUTABLE` to reuse an existing Chromium binary if needed. Inspect the screenshots: label collisions and occlusion need visual review. The smoke harness adapts the skill’s station check to include the initial snapshot stop, which fires as the page starts, and exercises branches separately.

Template engine and pacing adapted from the MIT-licensed isometric-explainer template by Laurentiu Gabriel; its [license](LICENSE) is retained here.
