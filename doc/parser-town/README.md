# Parser Town

An offline, interactive explanation of the command parsing system in lpc-rs. Follow a rule and a sentence through one of three grammar frontends: native patterns, parser-package rules, or DGD-style grammars. Their roads converge on a shared Grammar, tokenizer, Earley chart, and derivation machinery, then separate for frontend-specific results.

The native and parser-package entrances meet even earlier, at the pattern group builder. The loop around Earley Yard processes one token boundary per visit. The cart carries the real teaching-model state: each box is a retained token, and the yard’s racks count chart items.

This explains player-command and string parsing. The LPC source compiler has a separate parser.

## Open it

Open [index.html](index.html) in a browser. The directory works offline, with no build, server, network requests, or runtime dependencies.

For HTTP viewing with working links into the Rust source, serve the repository root:

```sh
python3 -m http.server 8000 --bind 127.0.0.1
```

Visit `http://127.0.0.1:8000/doc/parser-town/`. The explainer can also be hosted as a standalone directory; source and efun-reference links need the surrounding repository.

## Follow one sentence

The initial sentence is `give red sword to bob`. Switch entrances to see how the same engine serves three different contracts:

| Entrance | Rule | Input seen by the engine | Result in this example |
|---|---|---|---|
| Native | `add_rule("'give' %o 'to' %L", "do_give")` | `give red sword to bob` | `do_give` receives the sword and bob objects. |
| Parser package | `parse_add_rule("give", "OBJ to LIV")` | `red sword to bob` | `can_`, object selection, all-filled rechecks, then `do_`; `parse_sentence` returns `1`. |
| DGD | `Command: 'give' Phrase 'to' Phrase ? accept` plus the displayed token and phrase rules | `give red sword to bob` | `parse_string` returns an array of strings from the tree. |

The compiler lowers syntax to Grammar; frontend metadata remains outside it. Labels identify native/package captures, parser slugs identify handler families, and DGD production IDs identify semantic actions. The shared engine does not know what an LPC object is.

The native family also provides [`parse_command`](../efun/parse_command.md), which takes an explicit scope and writes reference destinations on success. It uses the same pattern compiler without requiring a leading verb. [`add_action`](../efun/add_action.md) is another compatibility adapter over this engine, building a verb-and-remainder grammar; its API is not a fourth grammar dialect in this tour.

Ordinary command dispatch tries the actor’s `add_action` and `add_rule` registrations before consulting parser-package rules. [`parse_sentence`](../efun/parse_sentence.md) explicitly invokes the package alone. [`parse_string`](../efun/parse_string.md) is an independent efun call. The three entrances are alternatives for an example; they are not three consecutive stages of every command.

## Experiments

| Try | Watch for |
|---|---|
| Switch among the three frontend buttons | Native and package patterns share Group Junction. DGD bypasses it. The package strips the selected verb before tokenization. |
| Uncheck **Sword in scope** | Native/package syntax still succeeds, but noun resolution fails. DGD still returns strings because its grammar performs no object lookup. |
| Uncheck **Handler accepts** | The native handler returns `0`, the package’s `can_` refuses, or DGD’s `accept` returns `0` and rejects the derivation. These are separate result contracts. |
| Type `give blue sword to bob` | The grammar accepts the structure, but blue is not an adjective of the example sword. |
| Type `give red sword with bob` | Tokenization succeeds, but there is no full start-symbol derivation. No frontend handler runs. |
| Choose **Chat / empty text** and type just `say` | Native `%s` captures `""`. Package `STR` and this DGD grammar require at least one word. |
| In Chat, type `say hello   there` | Native/package captures retain internal spacing. DGD returns separate token strings. |
| In DGD Chat, type `say hello!` | Its small lowercase-letter tokenizer rejects `!`; the chart is never built. |
| Expand **Current chart’s dot rules** | Each item displays its production, progress dot, and origin column. The bars count deduplicated items. |

Changing a frontend, lesson, or checkbox starts a fresh run. Sentence edits apply with **Try** or Enter. Previous reading history survives those changes. Tour speed controls animation and dwell time only; it does not change parsing or its work counts.

The package example supplies accepting `direct_` and `indirect_` handlers, including their final rechecks. The checkbox controls `can_`, because a package `do_` return value is ignored. There is only one matching example object per slot, so this tour does not demonstrate ambiguity resolution among objects.

## Controls and pacing

| Control | Action |
|---|---|
| Play / Space | Play or pause, including a reading stop. |
| Next / S | Advance to exactly one station, then pause; at Earley Yard this advances one column. |
| New run | Restart while keeping reading history. |
| Replay / R | Restart and clear reading history. |
| Frontend buttons | Choose an entrance and begin a new run with the current sentence. |
| Follow / F | Follow the cart with the camera. |
| Labels / L | Toggle landmark labels; the live cart readout remains visible. |
| Drag / wheel / pinch | Pan / zoom; dragging turns following off. |
| Fit / double-click | Show the whole town. |
| Landmark / station button | Pause and read that district. Return to the live tour resumes. |
| About / Escape | Open / close the accuracy notes; opening pauses. |
| Phone details handle / gear | Expand and pause for the explanation and state / open experiments. |

First visits stop for `clamp(words / 3.8 + 3.5, 9, 26)` seconds, divided only by the speed setting. Repeated visits use a 1.8-second beat with the template’s repeat acceleration. The ring road speeds up after the first chart column. A new explanation still receives its full reading time. The initial native tour takes approximately four minutes at 1×; the progress bar shows the remaining stop time. Reduced-motion preferences start the tour paused.

## Stations and source map

| Place | Operation | Driver implementation |
|---|---|---|
| Pattern Studio | Native `add_rule` / `parse_command` pattern syntax. | [frontend/native/mod.rs](../../src/command/frontend/native/mod.rs) |
| Verb Office | Package tokens and handler metadata. | [frontend/parser.rs](../../src/command/frontend/parser.rs) |
| Grammar Studio | DGD token rules, productions, constants, actions. | [frontend/dgd.rs](../../src/command/frontend/dgd.rs) |
| Group Junction | Shared native/package group lowering. | [native builder](../../src/command/frontend/native/mod.rs), [grammar/builtins.rs](../../src/command/grammar/builtins.rs) |
| Grammar Foundry | Productions, symbols, labels, token rules, nullable analysis. | [grammar/model.rs](../../src/command/grammar/model.rs) |
| Token Works | Maximal munch and token source spans. | [grammar/tokenizer.rs](../../src/command/grammar/tokenizer.rs) |
| Earley Yard | Predict, scan, complete, nullable advancement, deduplication. | [grammar/earley.rs](../../src/command/grammar/earley.rs) |
| Derivation Grove | Complete spans become trees and labeled captures. | [grammar/earley.rs](../../src/command/grammar/earley.rs), [grammar/tree.rs](../../src/command/grammar/tree.rs) |
| Argument House | Resolve noun captures and construct native arguments. | [native/matcher.rs](../../src/command/frontend/native/matcher.rs), [resolve](../../src/command/resolve) |
| Permission Court | Package `can_`, selection, all-filled checks, `do_`. | [parser/attempt.rs](../../src/command/parser/attempt.rs), [handler contract](../apply/object/parser_handlers.md) |
| Action Workshop | Fold tree values through DGD semantic actions. | [parse_string.rs](../../src/interpreter/efun/parse_string.rs) |
| Result Square | Frontends expose their own success contracts. | [add_rule](../efun/add_rule.md), [parse_sentence](../efun/parse_sentence.md), [parse_string](../efun/parse_string.md) |
| Return Office | Lexical, syntax, resolution, and handler failures. | [trial.rs](../../src/command/trial.rs), [parser/lpc.rs](../../src/command/parser/lpc.rs) |

## How much of it is real

This ledger also appears in the page’s About dialog.

**Computed:** pattern-to-group lowering; group-to-grammar expansion; the printed DGD grammar subset; maximal-munch tokenization with rule-order ties and source spans; Earley prediction, scanning, completion, nullable advancement and item deduplication; derivation reconstruction in production order with longest spans first; capture extraction; noun matching against the example scope; and the demonstrated frontend call order. Every live count comes from [js/model.js](js/model.js).

**Scaled down:** two fixed lessons, one selected rule, two example objects, at most 120 input characters and 24 tokens. The browser caps work at 20,000 operations per chart or derivation pass, trees at 32 per span, and depth at 64. These are teaching limits, not driver defaults. Only the native and package capture forms printed in the lessons are exercised. DGD compilation accepts the displayed line-oriented subset and three simple regex forms.

**Assumed:** the sword has id sword and adjective red; bob is a living with id bob. Direct and indirect checks always accept. Handler accepts controls the native handler’s result, the package’s can_ result, or the DGD accept action, which returns its input array or 0. All examples use case-sensitive matching. Changing an experiment begins a fresh run.

**Illustrated / omitted:** this is JavaScript, not the Rust VM. Tokenization tests regexes separately instead of using a joint DFA; spans use JavaScript character offsets instead of UTF-8 byte offsets. The model eagerly materializes bounded derivations and skips same-span cycles; Rust enumerates lazily with different limits. It does not implement general LPC actions, action memoization, the full DGD dialect, many-object selection, complete noun vocabulary or scope traversal, multiple-rule dispatch, caches, STM or I/O. Buildings, travel time, and reading stops have no performance meaning. Trust the demonstrated mechanism and values within this subset, not the animation as a benchmark.

Grammar slabs count productions, token boxes count retained tokens, and Earley rack heights use 0.16 grid units per unique chart item. The inspector’s bars scale to the largest current column. The counts are real; those geometric scales are illustrative. Seed items are included in total item counts but separate from the predict/scan/complete/nullable counters. Only successfully scanned tokens turn blue.

DGD string constants enter the token-rule list ahead of regexes, so a constant wins a same-length tie. This means the three displayed grammars need not accept exactly the same language: for example, a DGD constant can take a token that would otherwise have matched `word`. Sharing the engine preserves these frontend-defined differences.

## Files and maintenance

| File | Responsibility |
|---|---|
| `index.html`, `css/styles.css` | Responsive controls, inspector, and the accuracy dialog. |
| `js/iso.js` | Unchanged template projection, solids, route geometry, deterministic hashing. |
| `js/model.js` | Standalone grammar compilers, tokenizer, Earley chart, trees, and frontend interpretation. |
| `js/world.js` | Routes, waypoint stations, meaningful landmarks, narration, reading times. |
| `js/sim.js` | Model operations at stations, route decisions, reading and stepping. |
| `js/render.js` | One depth-sorted footprint pass, model state on landmarks/cart, DPR-aware label pass. |
| `js/ui.js` | Controls, source links, live grammar/token/chart/tree/call views. |
| `js/main.js` | Template camera, pointer/keyboard input, responsive framing, animation loop. |
| `tests/model.test.cjs` | Grammar behavior, dialect distinctions, ordering, failure boundaries, route and pacing invariants. |
| `tests/smoke.mjs` | Browser coverage of all districts and frontend routes, controls, offline use, mobile, and screenshots. |

`World.stations` anchors operations to waypoint distances. `Sim.advanceRoute()` owns every branch, including the chart loop and rejection roads. All parse mutations happen at stations; distance traveled cannot manufacture a result. Renderer and inspector read the same model. Keep large landmarks back from the roads: a footprint that exceeds its setback can hide the cart. Label placement retains the device-pixel-ratio transform and resolves collisions separately.

When changing the driver’s grammar model, builtin productions, tokenizer, derivation order, capture rules, or parser-handler protocol, review the source map against `model.js`, update the affected behavior tests, and review both copies of the fidelity ledger. This is a separate teaching implementation, so passing its tests alone does not prove agreement with a changed driver. Public efun references remain authoritative for complete behavior.

## Verify

From the repository root:

```sh
(for f in doc/parser-town/js/*.js; do node --check "$f" || exit 1; done)
node --test doc/parser-town/tests/model.test.cjs
```

Browser tests need Node.js, Playwright, and Chromium only as test tools. Reuse an existing installation or install them in an isolated, ignored directory:

```sh
npm install --prefix local/parser-town-tools playwright
local/parser-town-tools/node_modules/.bin/playwright install chromium
NODE_PATH="$PWD/local/parser-town-tools/node_modules" \
  node doc/parser-town/tests/smoke.mjs --out local/parser-town-smoke
NODE_PATH="$PWD/local/parser-town-tools/node_modules" \
  node doc/parser-town/tests/smoke.mjs --dpr 2 --out local/parser-town-smoke-retina
```

The default test URL uses `file://`. Supply an HTTP URL as the first argument for a served copy. Set `PLAYWRIGHT_CHROMIUM_EXECUTABLE` to use an existing Chromium binary. Inspect screenshots at both pixel ratios, phone portrait and landscape; visual collisions cannot be detected by syntax tests. Also watch one untouched first tour at 1× after changing routes, camera framing, or narration.

The projection, camera and pacing derive from Laurentiu Gabriel’s MIT-licensed isometric-explainer template; its [license](LICENSE) is retained.
