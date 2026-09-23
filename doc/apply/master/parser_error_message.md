# parser_error_message

`string parser_error_message(int kind, object ob, mixed arg, int flag)`

Absent `ob` and absent `arg` values described as `0` below use the selected
[null value](../../lpc/null.md): `nil` in distinct mode. Numeric counts,
kind codes, and flags remain integers.

Applied in the master only after every registered parser rule for the verb
has failed. A successful rule prevents this apply from being called for any
earlier failures. After total failure, the driver offers one retained
failure per rule in registration order, skipping failures with no diagnostic.
Within a rule, the retained failure comes from the parse that resolved the
most object slots, taking the earliest tie.

The first string result ends reporting: `parse_sentence` returns it, or
ordinary command dispatch delivers it to the actor. Any non-string result,
including `0` or distinct `nil`, leaves that failure silent and allows the
next retained failure to be reported. An absent apply supplies no message.
If every failure remains silent, `parse_sentence` combines their codes:
`-3` (unresolved object) over `-2` (handler refusal) over `-1` (no grammar
matched). Ordinary command dispatch proceeds to its usual `notify_fail` or
`command_not_found` fallback.

A handler that refuses outright — `0` or distinct `nil`, no reason string —
never reaches this apply; there is nothing to describe.

The apply runs in the current transaction, after all failed rules' handler
effects. `this_player()` and `previous_object()` are the actor, `origin()` is
`"driver"`, and `query_verb()` is the typed verb, including a synonym. Nested
commands or parses restore that outer verb when they return. Exceptions from
this apply propagate to the caller.

Diagnostic arguments retain the failure's phrase, reason, count, or candidate
references as recorded while trying its rule. Later handlers can change those
objects before reporting; references to destructed objects read as the selected
null value.

### Kinds

`ob` is `0` for every kind except 6, where it is the object that returned
the reason.

| `kind` | meaning | `arg` | `flag` |
|---|---|---|---|
| 2 | a `LIV`/`LVS` phrase matched only non-livings | the typed phrase | 1 if the slot is a many slot (`LVS`) |
| 3 | a match exists but is not reachable (`inventory_accessible`) | the typed phrase | 1 if the slot is a many slot (`OBS`/`LVS`) |
| 4 | a single slot has several equally good matches | the qualifying objects, as an array | 0 |
| 5 | an ordinal (`"third sword"`) went past how many qualified | the count that qualified | 0 |
| 6 | a `direct_`/`indirect_`/`can_` handler returned a reason string | the reason, with a leading `#` stripped | 0 |
| 7 | no object answers to the phrase, or every match was rejected without a reason | the typed phrase | 1 if the slot is a many slot (`OBS`/`LVS`) |
| 8 | `all`, or a plural, or a count on a slot that takes only one | 0 | 0 |

Kinds 1 and 9 are never raised. The driver defines no header for these numbers;
the mudlib may define named constants for the kinds it handles.

### Example

The driver holds no language of its own; this master's messages are English
purely as this example's own text.

```c
string parser_error_message(int kind, object ob, mixed arg, int flag) {
    switch (kind) {
        case 2: return "You don't see " + (flag ? "any " : "a ") + arg + " here to talk to.";
        case 3: return "You can't reach " + arg + ".";
        case 4: return "Which one do you mean? There are " + sizeof(arg) + ".";
        case 5: return "There are only " + arg + " of those.";
        case 6: return arg;
        case 7: return "You don't see " + (flag ? "any " : "a ") + arg + " here.";
        case 8: return "You'll have to do that one at a time.";
    }
    return 0;
}
```

### See also

[`parse_add_rule`](../../efun/parse_add_rule.md),
[`parse_sentence`](../../efun/parse_sentence.md),
[`parser_handlers`](../object/parser_handlers.md)
