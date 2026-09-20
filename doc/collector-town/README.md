# Collector Town

An offline, interactive explanation of the garbage collector in lpc-rs. A collection cart gathers roots, passes a snapshot-pin check, and loops around a courtyard while an actual work stack traces the committed world. Unreachable payload cells disappear at the reclamation yard. A request made with live snapshots takes the refusal road and leaves the world intact.

Open [index.html](index.html) directly in a browser. There is no build, server, network access, or runtime dependency. To serve it over HTTP, run `python3 -m http.server 8000 --bind 127.0.0.1` from the repository root and visit `http://127.0.0.1:8000/doc/collector-town/`. The directory can also be hosted independently; links into the Rust source need the surrounding repository.

## Try it

The default synthetic world has 18 committed cells. Collection marks 13, retains one unmarked connection identity outside the sweep filter, and reclaims four: an abandoned capture, an abandoned image slot, and a two-array cycle.

| Change | Expected result |
|---|---|
| Untick **Global holds array** | The array/mapping/closure subgraph loses its root; 11 cells are reclaimed. |
| With that link cut, select **Queued call-out** | The queued function independently roots its captures, partial argument, and older image; reclamation returns to four cells. |
| Select **Input callback** or **Parser rule** | A live connection or rules cell exposes the function through another edge; the same subgraph survives. |
| Raise **Snapshot pins** above zero | The request is refused before tracing. Every cell remains; the reply records the live pin count. |
| Raise **Unrooted cycles** | Each added two-array cycle contributes two reclaimed cells to an admitted pass. A cycle cannot keep itself reachable. |

World controls start a fresh collection immediately and preserve reading history. If paused, the new request stays paused. This deliberately replaces the template's mid-run parameter edits: an admitted collection must observe one stable world. **Pace** affects travel and reading time only; it has no algorithmic or performance meaning.

The cart carries one gold tile per pending stack item and a separate tile for its selected item. Its teal gauge shows marked cells as a fraction of the original cell count. The numbered courtyard plinths correspond to the live world list. Marked cells turn teal; cells outside the sweep filter are blue until marked; reclaimed cells leave flat foundations. Color is accompanied by textual status in the panel.

## Controls and pacing

| Control | Action |
|---|---|
| Play / Space | Run or pause, holding a reading stop indefinitely. |
| Next stop / S | Advance exactly one station and pause. |
| New run | Restart with current settings; remember previously read stations. |
| Replay / R | Restart and forget reading history. |
| Pace | Scale travel and reading pauses from 0.4× to 8×. |
| Follow cart / F | Toggle camera following. |
| Labels / L | Toggle landmark labels; the cart's live readout stays visible. |
| Drag / wheel / pinch | Pan / zoom. Panning disables camera following. |
| Fit / double-click | Fit the full town in the visible area. |
| Landmarks / station buttons | Pause and browse a station's explanation. |
| Return to live tour | Restore the live notes and the previous play/pause state. |
| About / Escape | Open / close accuracy notes; opening pauses and closing restores playback. |
| Phone field-notes handle / gear | Expand the reading sheet / show experiment controls; opening either pauses. |

First visits pause for `clamp(words / 3.8 + 3.5, 9, 26)` seconds, divided only by Pace. Later visits use a 1.8-second beat, divided by 2.2 during repeated work-stack laps. Repeated laps travel at 2.4×; after all districts have been explained, travel has another 3× boost. These travel boosts never shorten first reading stops. The default first tour takes about five minutes at 1×. The progress bar shows remaining reading time. Reduced-motion preferences start the tour paused.

## Stations and source

The implementation was reviewed against repository revision `cec58e97` on 2026-09-19. Follow the source again if the runtime changes; this JavaScript model is not generated from Rust.

| Place | Operation | Source |
|---|---|---|
| Root Registry | Gather world cell IDs, live processes, and queued call-out functions. | [`GlobalState::gc`](../../src/interpreter/vm/global_state.rs), [`ObjectSpace::all_cell_ids`](../../src/interpreter/object_space.rs) |
| Quiet Gate | Sum live snapshot pins; refuse unless the sum is zero. | [`CommitProtocol::GcPass`](../../src/interpreter/stm/committer.rs) |
| Stack Depot | Seed the mark work vector from roots. | [`Committer::mark_world`](../../src/interpreter/stm/committer.rs) |
| Workstack Tower | Pop one Var, Ref, Process, or Image item; repeat while work remains. | [`Committer::mark_world`](../../src/interpreter/stm/committer.rs) |
| Reference Foundry | Mark cells and follow typed edges; deduplicate cells and image generations. | [`mark_world`, `mark_edges`, `mark_ref`](../../src/interpreter/stm/committer.rs), [process fields](../../src/interpreter/process/mod.rs), [image fields](../../src/interpreter/process/image.rs) |
| Reclamation Yard | Remove unmarked Ref, Array, Mapping, and Image world cells. | [`Committer::mark_world`](../../src/interpreter/stm/committer.rs) |
| Receipt House | Return `GcReport.reclaimed`, a cell count. | [`GcReport`](../../src/interpreter/stm/committer.rs) |
| Return Office | Report refusal with the live snapshot count; do no marking or sweeping. | [`GcRefused`](../../src/interpreter/stm/committer.rs), [`GlobalState::gc_when_quiet`](../../src/interpreter/vm/global_state.rs) |

The successful path skips Return Office; a refused request skips Stack Depot, the tracing loop, reclamation, and the successful receipt. Browser checks cover both paths separately.

Root gathering happens before the request is sent. Admission, all marking, and sweeping happen inside one committer message. The animated stations stretch that atomic operation for reading; they do not imply interleaving with another transaction. There is no separate upvalue-bank cull in the current implementation: captured cells use the same reachability walk and sweep. Stable identities and Rules cells are outside its removal filter. Object cleanup and destruction are separate mechanisms.

## How much of it is real

This ledger also appears in the page's About dialog.

**Computed.** Live-pin admission; a LIFO work stack with Var, Ref, Process, and Image work; marked-cell and image-generation sets; array edges; mapping keys and values; function captures, partial arguments, and retained images; connection and parser callbacks; the sweep filter; and the reclaimed-cell report. The cart tiles and world plinths show that state. All results are computed by [js/model.js](js/model.js).

**Scaled down.** One process, one closure, two reachable image generations, and 16–22 committed cells replace the driver's full world. The process has only image, connection, and rules fields; each image has one global. Other process fields and clone counters are omitted. JavaScript names replace VarIds and generation IDs.

**Assumed.** A synthetic object owns an array-to-mapping-to-closure graph. Its partial argument links back to the array; its capture reaches another array. Between 0 and 3 unrooted two-array cycles add garbage. Snapshot pins and callback owners are selected by the reader. Root order is deterministic here; production map iteration need not be. Every world-control change begins a fresh request.

**Illustrated / omitted.** This does not run the Rust VM, allocate an LPC heap, or simulate worker scheduling, reference-count destruction, history eviction, actual memory usage, or collection latency. The pauses split one atomic committer message for reading only. There is no separate upvalue-bank cull in the current source: captured cells use the same graph walk and sweep. Automatic retries, object cleanup/destruction, and their scheduling are outside this tour. No numerical result is pre-baked.

**The boundary that matters.** Unmarked Ref, Array, Mapping, and Image cells are removed. Process, Connection, and Rules cells are outside this filter. `GcReport` counts removed world variables, not bytes or game objects. Refusal does no marking or sweeping.

The standalone tests additionally exercise an absent world cell, the bootstrap initial-image fallback, image-generation deduplication, and object references that do not add payload edges. Those cases do not each have a dedicated animated scenario. Model reports use `{reclaimed: count}` or `{refused: pins}` to represent the Rust success/refusal result.

## Files and architecture

| File | Responsibility |
|---|---|
| `index.html`, `css/styles.css` | Responsive panels, controls, and the accuracy dialog. |
| `js/iso.js` | Unchanged template projection, geometry, deterministic hashing, and routes. |
| `js/model.js` | Standalone graph walk and synthetic world; no DOM or drawing. |
| `js/world.js` | Routes, waypoint-anchored stations, landmarks, narration, reading times. |
| `js/sim.js` | Station operations, reading stops, and branches controlled by model state. |
| `js/render.js` | One sorted painter's pass for ground-footprint objects; a DPR-aware label pass. |
| `js/ui.js` | Live state, controls, browsing, and modal focus management. |
| `js/main.js` | Template camera/input/frame loop, adapted for responsive framing and keyboard focus. |
| `tests/model.test.cjs` | Reachability, sweep/refusal invariants, all 128 control combinations, station sequence, and pacing. |
| `tests/smoke.mjs` | Browser station coverage, branches, controls, offline use, and responsive screenshots. |

`World.stations` anchors model operations to waypoint distances. `Sim.advanceRoute()` owns the refusal and work-loop branches. Travel changes no model values. The canvas and panel read the same model, so changing an input never interpolates between stored results. When editing the map, keep buildings back from roads: a building whose half-footprint exceeds its setback can obscure the cart. Ground-footprint objects share one depth sort; labels use CSS coordinates with the DPR transform and make room for the cart's readout.

## Verify

From the repository root:

```sh
(for f in doc/collector-town/js/*.js; do node --check "$f" || exit 1; done)
node --test doc/collector-town/tests/model.test.cjs
```

Browser tests need Playwright and Chromium only for development. Reuse an available installation or install test tooling in a gitignored directory:

```sh
mkdir -p local/collector-town-tools
npm install --prefix local/collector-town-tools playwright
local/collector-town-tools/node_modules/.bin/playwright install chromium
NODE_PATH="$PWD/local/collector-town-tools/node_modules" \
  node doc/collector-town/tests/smoke.mjs --out local/collector-town-smoke
NODE_PATH="$PWD/local/collector-town-tools/node_modules" \
  node doc/collector-town/tests/smoke.mjs --dpr 2 --out local/collector-town-smoke-retina
```

The default URL is `file://`; pass an HTTP URL as the first argument to test a server. Set `PLAYWRIGHT_CHROMIUM_EXECUTABLE` to reuse a browser binary. The harness adapts the skill's smoke check to the separate accepted/refused paths and the computed number of tracing laps. Inspect screenshots at both pixel ratios, portrait and landscape; then watch a full first tour at 1× to verify pacing and visibility.

Projection and pacing adapted from the MIT-licensed Learnscape template by Laurentiu Gabriel. Its [license](LICENSE) is retained.
