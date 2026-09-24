# parse_my_rules

`mixed parse_my_rules(object actor, string line, [int execute = 0], [object command_giver = actor])`

Parses `line` using only `this_object()`'s registered parser rules, for the
explicit `actor`. The actor must be alive and have called `enable_commands()`;
it need not have called `parse_init()`. The caller owns the rules and need not
be the actor. Use [`query_parser_rules`](query_parser_rules.md) to list those
registrations without parsing a line.

With `execute` omitted or zero, the first parse that passes permissions returns
the arguments for a generic `do_verb_rule` call. No action handler is looked up
or called, and none needs to exist. With any nonzero integer, the parser finds
and calls the normal action handler, returning `1` regardless of its return
value. A missing action in execution mode leaves that parse unhandled and the
search continues.

### Returned arguments

Successful argument mode returns:

```lpc
({ base_verb, rule, captures..., typed_verb, source_strings... })
```

The layout is always the generic-rule layout, even if the owner defines a more
specific action. `base_verb` is the original verb for synonyms; `rule` is the
exact registered rule text. Captures are resolved objects, plural arrays, or
text strings. `typed_verb` retains the full matched phrase's input spelling and
internal whitespace. Each capture then contributes one source string; literals
contribute neither captures nor source strings. See
[`parser_handlers`](../apply/object/parser_handlers.md#arguments) for layouts
and plural refusal entries.

For example, an object registering `parse_add_rule("wave", "at LIV")` can expose:

```lpc
mixed match_wave(object actor, string line) {
    return parse_my_rules(actor, line);
}
```

For `wave at bob`, with Bob resolved to `B`, the result is
`({ "wave", "at LIV", B, "wave", "bob" })`. An empty rule returns
`({ base_verb, "", typed_verb })`.

### Selection and failures

Rules on other objects are excluded before scope or permission callbacks.
Among the caller's rules, the longest matching verb phrase runs first, then
registration order within that phrase. Refusals leave later text splits, rules
and shorter phrases eligible. Each rule tries at most 32 grammatical parses in
[longest-first capture order](parse_add_rule.md#text-captures-and-split-selection).
Registrations are snapshotted before callbacks; additions and removals affect
later calls, while destructed owners are skipped.

Both modes use the same failure results as `parse_sentence`:

| Value | Meaning |
|---|---|
| `0` | none of the caller's registered verbs matches a leading phrase |
| `-1` | a verb matched, but no rule's grammar parsed the remaining text |
| `-2` | a matching parse was refused, with no error message |
| `-3` | an object phrase could not resolve, with no error message |
| a string | the master's `parser_error_message` result |

Only after every eligible rule fails does the driver offer retained diagnostics
to [`parser_error_message`](../apply/master/parser_error_message.md), stopping at
the first string. If none supplies text, `-3` takes precedence over `-2`, then
`-1`. The efun never delivers an error message itself.

### Scope, context and effects

The explicit actor supplies the normal
[object scope](parse_sentence.md#scope), including remote living candidates from
`parse_command_users`. This entry point has no scope or nickname override.
Permissions and object selection use the normal handler protocol in both modes.

All parser callbacks, including scope, vocabulary, permissions, actions and
error reporting, see `command_giver` as `this_player()` and `actor` as immediate
`previous_object()`, with `origin()` equal to `"driver"`. Omitting the giver
selects the actor; explicit `nil` clears it. Any live object can be the giver,
including an unconnected user or NPC; a destructed giver becomes null. Wrong
non-null types are errors, including integer zero in distinct-null mode.
Selecting a giver preserves the task's interactive provenance. Each callback
starts with the selected giver, so callback-local `set_this_player()` changes
do not leak to later callbacks.

```lpc
mixed args = parse_my_rules(body, line, 0, user);
```

The rule owner remains the object executing the efun, independently of both
identities. Verb queries describe the current matched phrase; `query_command()`
holds the whole line. The surrounding player and command context is restored on return or exception, including nested parses
for a different actor.

The caller chain behind the actor retains the object invoking this efun and
its external callers; stack inspection also retains local and inherited
frames. See [`parse_sentence`'s callback context](parse_sentence.md#callback-context)
for the ordering, repeated objects and efun-pointer behavior.

Argument mode still runs permission and vocabulary code in the current
transaction. Refusals and caught exceptions do not undo callback effects.
Runtime exceptions propagate immediately. Nested calls share the depth limit
of 16 with `command()` and `parse_sentence()`.

### Errors

Invalid or nonliving actors raise `parse_my_rules: the actor is not a living`.
An invalid command giver raises
`parse_my_rules: the command giver must be an object or null`.
A non-string line raises `parse_my_rules: the line must be a string`; a
non-integer execution flag raises `parse_my_rules: execute must be an int`.
Excessive nesting raises `parse_my_rules: nesting deeper than 16`.

### See also

[`parse_add_rule`](parse_add_rule.md), [`parse_sentence`](parse_sentence.md),
[`query_parser_rules`](query_parser_rules.md),
[`parser_handlers`](../apply/object/parser_handlers.md)
