# parse_sentence

`mixed parse_sentence(string line, [int debug = 0], [object *scope = 0], [mapping nicknames = ([])])`

Runs `line` for `this_player()` over the parser package's verb-attached
rules only — not `this_player()`'s own `add_action`/`add_rule` rules, no
`process_input` pre-hook, and no dispatcher fallback — and returns a result
code without delivering anything to anyone. `this_player()` must be alive
and have called `enable_commands()`; otherwise a runtime error. `debug` is
accepted and ignored (see Departures).

Every rule registered for `line`'s first word, on every live verb object, is
tried in registration order, exactly as the ordinary dispatcher tries them
(see `parse_add_rule`) — `parse_sentence` differs only in which rules it
tries and in delivering nothing itself.

### Result

| value | meaning |
|---|---|
| `1` | some rule's `do_` ran |
| `0` | no rule is registered for `line`'s first word |
| `-1` | a rule matched the verb, but no rule's grammar parsed the rest of `line` |
| `-2` | a rule's grammar matched, but a handler refused and the master gave no message |
| `-3` | a rule's grammar matched, but an object phrase did not resolve and the master gave no message |
| a string | `master->parser_error_message()`'s result for the failure |

When several of the verb's rules fail, the one reported is whichever got
furthest — most object slots chosen, ties to the one registered first (see
`parser_handlers`).

### scope

Without `scope`, candidates are the actor's ordinary walk: the actor, its
environment, both inventories, and — breadth-first — the contents of
anything reachable that answers `inventory_visible()` truthily (see
`inventory_visible`, `inventory_accessible`). Passing `scope` replaces that
walk entirely: every object it names is a candidate, and every one of them
is reachable, `inventory_accessible()` unconsulted. A nested array inside
`scope` contributes its own objects (to a nesting depth of 20 — deeper is a
runtime error); a destructed member, or one that is not an object, is
skipped. `LIV`/`LVS` phrases also resolve over the master's
`parse_command_users()` regardless of `scope`.

### nicknames

`nicknames` maps strings to objects: a key names its object as an extra id,
usable anywhere a noun phrase is, but only for an object that is already a
candidate — naming an object here does not itself add it to the scope.

### Errors

`parse_sentence: this_player() is not a living` — there is no
`this_player()`, or it has been destructed, or it has never called
`enable_commands()`; `parse_sentence: the scope must be an array of
objects` — `scope` given and not an array; `parse_sentence: the scope nests
deeper than 20`; `parse_sentence: nicknames must be a mapping from strings
to objects`. `parse_sentence` shares `command()`'s nesting limit — a rule's
handler running `parse_sentence` of its own, and any nested `command()`,
count together — at 16 deep, `parse_sentence: nesting deeper than 16`.

### Departures from MudOS/FluffOS

| MudOS | here | why |
|---|---|---|
| parser rules reached only by the lib calling `parse_sentence` | also by the dispatcher, after the actor's rules for the verb | ruled 2026-08-26 (one system) |
| default English messages when the master gives none | none: `-2`/`-3`, then the dispatcher's fallback | the driver holds no natural language |
| `a`, `any`, `my`, `the` recognised by the parser | the master's adjective list | same rule |
| `ERR_IS_NOT` vs `ERR_THERE_IS_NO` | only `THERE_IS_NO` | no game-wide vocabulary to know a noun "exists elsewhere" |
| `ERR_MANY_PATHS` cap | never raised | slots resolve one at a time; no combinatorial paths |
| `:c` "choose the first" rule modifier | not recognised | a documented mistake in MudOS's own notes |
| `parse_sentence` codes `1/0/-1/-2` | plus `-3` | distinguishes "nothing resolved" from "a handler refused" once there is no default message |
| `debug` argument | ignored | no debug tracer |
| `parse_refresh` invalidates cached names | no-op | nothing is cached across calls |
| nested scope arrays mean containment | flattened; containment is `environment()` | one scope shape |
| livingness by the package's own test | `enable_commands`, as `parse_command`'s `%l` | one definition |
| error reported: the last match's | the furthest parse's | deterministic and explainable |
| verb matched by MudOS's own rules | exact first word | the dispatcher's registry does the pre-filter |
| `parse_add_synonym(new, old)` | optional third argument, one rule | Lima's verb base calls it so |
| `parse_my_rules` shape unspecified | `"verb rule"` strings | FluffOS's page says strings |

### See also

`parse_add_rule`, `parse_add_synonym`, `parse_remove`, `parse_my_rules`,
`command`, [`parser_handlers`](../apply/object/parser_handlers.md),
[`parser_error_message`](../apply/master/parser_error_message.md),
[`parse_command_users`](../apply/master/parse_command_users.md)
