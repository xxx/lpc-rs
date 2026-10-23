# parse_sentence

```text
mixed parse_sentence(string line, [int debug = 0],
                     [object *scope = nil], [mapping nicknames = nil],
                     [object actor = this_player()],
                     [object command_giver = actor])
```

Runs `line` for `actor` over the parser package's verb-attached rules and
returns a result code without delivering anything. The actor defaults to the
entering `this_player()` and must be alive and command-enabled. Its own
`add_action`/`add_rule` rules, the `process_input` pre-hook and dispatcher
fallback are outside this search. `debug` is accepted and ignored.

Registered verbs match leading whole words, with case and punctuation preserved
and Unicode whitespace treated as separators. The longest matching phrase is
tried first; its rules run in registration order across live owners. If none
succeeds, shorter matching phrases are tried. The ordinary dispatcher's parser
tier uses the same selection. See [`parse_add_rule`](parse_add_rule.md#the-verb).

### Result

| value | meaning |
|---|---|
| `1` | some rule's `do_` ran |
| `0` | no registered verb matches a leading phrase of `line` |
| `-1` | a rule matched the verb, but no rule's grammar parsed the rest of `line` |
| `-2` | a rule's grammar matched, but a handler refused and the master gave no message |
| `-3` | a rule's grammar matched, but an object phrase did not resolve and the master gave no message |
| a string | `master->parser_error_message()`'s result for the failure |

The first rule whose `do_` runs ends the search with `1`. Earlier failures do
not prevent a later rule or shorter verb phrase from succeeding, including when
a longer phrase's action is absent. Within one phrase, a successful earlier rule
wins even if a later rule is more specific. For example, a refusal from `"STR"`
does not block a later `"at OBJ"` rule for `look at sword`.

Only when every rule fails does the driver call `parser_error_message`,
offering retained failures with the most resolved object slots first until the
hook returns a string. Equal progress preserves phrase and registration trial
order. A successful search never calls that hook. If the hook
supplies no message, results combine by severity: `-3` beats `-2` beats `-1`.
A later silent refusal cannot downgrade an earlier unresolved object.

Within one rule, the driver tries up to 32 grammatical parses in
[longest-first capture order](parse_add_rule.md#text-captures-and-split-selection).
If those parses all fail,
the retained failure is whichever parse got furthest: most object slots
resolved, ties to the earliest (see `parser_handlers`). For pair checking,
both noun phrases count as resolved before permission checks begin. All handlers run in
the current transaction; declining a rule does not undo its effects.
Runtime exceptions propagate immediately.

During a parser trial, including scope and vocabulary callbacks, `query_verb()`
is the canonical matched phrase; `query_verb(1)` and generic handlers' typed-verb
argument contain its original input span, including internal whitespace.
`query_command()` retains the whole line. Each deferred error hook receives its
own failed phrase's context. Nested commands restore the surrounding context.

### Callback context

The actor selects the default scope and actor permission target. The command
giver supplies `this_player()` in scope, vocabulary, permission, action and error
callbacks. It defaults to the actor; an explicit `nil` clears it. A giver may be
any live object, including an unconnected user or NPC, and need not be living
or related to the actor. A destructed giver becomes null. Wrong non-null types
are errors, including integer zero in distinct-null mode. An explicitly null
or destructed actor is an error. Identities are validated before callbacks.

```lpc
parse_sentence(line, 0, nil, nil, body, user);
parse_sentence(line, 0, nil, nil, npc, nil);
```

Selection does not replace the task's interactive provenance: `this_interactive()`
keeps its existing entry-player and connection rules. Each callback starts with
the selected giver; its own `set_this_player()` affects its nested work without
changing later callbacks or the surrounding player. A nested parse selects its
own identities, with an omitted giver defaulting to its selected actor. Normal
return, refusal and exceptions restore the surrounding player and command frame.
Callback effects still follow the current transaction's catch and rollback rules.

Callbacks keep the actor as immediate `previous_object()` and report `"driver"`
from `origin()`. Behind the actor, the caller chain retains the object invoking this
efun and its external callers. For `main -> bridge -> owner -> parse_sentence`,
a callback sees `({ actor, owner, bridge, main })` from `previous_object(-1)`.
Equal adjacent objects are retained, including when the actor invokes the efun.

`calling_object`, `calling_function` and `calling_program` also retain local
and inherited frames. The actor occupies the first position without a calling
function or program; the captured invoking frames follow it. An efun pointer
adds its owner before the frame that fired it, with no function metadata for
the owner entry. Nested parser calls retain the outer callback's frames and
restore its context on return or exception. The same rules apply to
[`parse_my_rules`](parse_my_rules.md).

### scope

Without `scope`, candidates are the actor, its environment, its own inventory,
the environment's other contents, and — breadth-first — visible nested contents.
The actor and immediate environment contribute their direct inventories without
consulting their own inventory hooks. Other containers expose contents only when
`inventory_visible()` is truthy; reachability is inherited while
`inventory_accessible()` is truthy. Missing hooks count as true.

An explicit array replaces that walk. Top-level objects are reachable regardless
of their physical locations. An object immediately followed by an array supplies
that object's contents:

```lpc
mixed *scope = ({ room, ({ box, ({ gem }), book }) });
parse_sentence("look at gem", 0, scope);
```

Here `room` controls visibility and reachability of `box` and `book`; `box`
also controls `gem`. The named children need not actually be in those inventories.
A hidden parent's subtree is omitted. Visible children inherit reachability
from their parent and its `inventory_accessible()` result. Flat lists such as
`({ gem, book })` explicitly make those objects reachable without inventory hooks.

Each nested array must immediately follow a live object, even when empty or
inside a hidden subtree. A non-object or destructed member is skipped but breaks
that association; consecutive child arrays have no implicit shared parent.
Malformed structure is a runtime error. Nesting deeper than 20 is also an error,
including cycles that exceed that depth. Duplicate objects appear once, in first
visible occurrence order, and are reachable if any supplied visible path reaches
them. A flat occurrence can therefore override an inaccessible nested occurrence.

Array structure is snapshotted before parser callbacks. Each rule evaluates its
scope afresh, caching each parent's inventory hook result for that rule's walk.
Callbacks run in the current transaction with the selected command giver as
`this_player()`, the actor as `previous_object()`, and `"driver"` as `origin()`.
A callback exception propagates; refusing a rule does not undo callback effects.
Destructed objects are skipped, and a parent destructed during its inventory
hooks cannot expose children.

Unmodified object tokens require reachable matches; `OBJ:v`, `OBS:v`, `LIV:v`
and `LVS:v` also admit visible but unreachable matches. Normal permissions still
apply. `LIV`/`LVS`, with or without `:v`, also resolve over the master's
`parse_command_users()` regardless of `scope`.

### nicknames

`nicknames` maps strings to objects: a key names its object as an extra id,
usable anywhere a noun phrase is, but only for an object that is already a
candidate — naming an object here does not itself add it to the scope.

### Errors

`parse_sentence: this_player() is not a living` — there is no
`this_player()`, or it has been destructed, or it has never called
`enable_commands()`. An invalid explicit actor raises
`parse_sentence: the actor is not a living`. An invalid giver raises
`parse_sentence: the command giver must be an object or null`.
`parse_sentence: the scope must be an array of objects` — `scope` given and not
an array; `parse_sentence: the scope nests deeper than 20`;
`parse_sentence: a nested scope array must immediately follow a live object`; `parse_sentence: nicknames must be a mapping from strings
to objects`. `parse_sentence`, `parse_my_rules` and `command()` share a nesting
limit of 16; exceeding it here raises `parse_sentence: nesting deeper than 16`.

### See also

`parse_add_rule`, `parse_add_synonym`, `parse_remove`, `query_parser_rules`,
[`parse_my_rules`](parse_my_rules.md),
`command`, [`parser_handlers`](../apply/object/parser_handlers.md),
[`parser_error_message`](../apply/master/parser_error_message.md),
[`parse_command_users`](../apply/master/parse_command_users.md)
