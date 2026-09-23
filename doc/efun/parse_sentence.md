# parse_sentence

`mixed parse_sentence(string line, [int debug = 0], [object *scope = 0], [mapping nicknames = ([])])`

Runs `line` for `this_player()` over the parser package's verb-attached
rules only — not `this_player()`'s own `add_action`/`add_rule` rules, no
`process_input` pre-hook, and no dispatcher fallback — and returns a result
code without delivering anything to anyone. `this_player()` must be alive
and have called `enable_commands()`; otherwise a runtime error. `debug` is
accepted and ignored.

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

Rules are tried in registration order; the first one whose `do_` runs ends
the search with `1`. Earlier failures do not prevent a later rule from
succeeding. For example, a refusal from `"STR"` does not block a later
`"at OBJ"` rule for `look at sword`. A successful earlier rule still wins,
even if a later rule is more specific.

Only when every rule fails does the driver call `parser_error_message`,
offering each reportable failure in registration order until the hook
returns a string. A successful search never calls that hook. If the hook
supplies no message, results combine by severity: `-3` beats `-2` beats `-1`.
A later silent refusal cannot downgrade an earlier unresolved object.

Within one rule's attempt at the line, which may parse it several ways,
the retained failure is whichever parse got furthest: most object slots
chosen, ties to the earliest (see `parser_handlers`). All handlers run in
the current transaction; declining a rule does not undo its effects.
Runtime exceptions propagate immediately.

### scope

Without `scope`, candidates are the actor's ordinary walk: the actor, its
environment, its own inventory, the environment's other contents, and —
breadth-first — the contents of any candidate answering
`inventory_visible()` truthily, reachable only while every container on its
path answers `inventory_accessible()` truthily. Passing `scope` replaces
that walk entirely: every object it names is a candidate, and every one of
them is reachable, `inventory_accessible()` unconsulted. A nested array inside
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

### See also

`parse_add_rule`, `parse_add_synonym`, `parse_remove`, `parse_my_rules`,
`command`, [`parser_handlers`](../apply/object/parser_handlers.md),
[`parser_error_message`](../apply/master/parser_error_message.md),
[`parse_command_users`](../apply/master/parse_command_users.md)
