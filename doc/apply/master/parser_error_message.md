# parser_error_message

`string parser_error_message(int kind, object ob, mixed arg, int flag)`

Absent `ob` and absent `arg` values described as `0` below use the selected
[null value](../../lpc/null.md): `nil` in distinct mode. Numeric counts,
kind codes, and flags remain integers.

Applied in the master only after every eligible rule for every matching parser
verb phrase has failed. `parse_my_rules` considers only its caller's rules.
A successful rule prevents this apply from being called for any
earlier failures. After total failure, the driver offers one retained
failure per rule, longest matched phrase first and registration order within
each phrase, skipping failures with no diagnostic.
Within a rule, the retained failure comes from the parse that resolved the
most object slots, taking the earliest tie.

The first string result ends reporting: `parse_sentence` or `parse_my_rules`
returns it, or ordinary command dispatch delivers it to the actor. Any non-string
result, including `0` or distinct `nil`, leaves that failure silent and allows the
next retained failure to be reported. An absent apply supplies no message.
If every failure remains silent, both parser efuns combine their codes:
`-3` (unresolved object) over `-2` (handler refusal) over `-1` (no grammar
matched). Ordinary command dispatch proceeds to its usual `notify_fail` or
`command_not_found` fallback.

A handler that refuses outright — `0` or distinct `nil`, no reason string —
never reaches this apply; there is nothing to describe.

The apply runs in the current transaction, after all failed rules' handler
effects. `this_player()` and `previous_object()` are the actor, `origin()` is
`"driver"`. `query_verb()` reports this failure's canonical matched phrase,
including a synonym; `query_verb(1)` preserves that phrase's raw input span,
including internal whitespace. `query_command()` remains the original line.
Nested commands or parses restore that outer context when they return.
Exceptions from this apply propagate to the caller.

For parser efun calls, the invoking LPC frames remain behind the actor even
during deferred reporting; see
[callback context](../../efun/parse_sentence.md#callback-context).

Diagnostic arguments retain the failure's phrase, reason, count, or candidate
references as recorded while trying its rule. Later handlers can change those
objects before reporting; references to destructed objects read as the selected
null value.

### Kinds

Valid diagnostic kinds are 2–8.

`ob` is `0` for every kind except 6, where it is the object that returned
the reason. An actor permission or owner fallback therefore identifies that
actor or owner, even when the permission concerns another noun object.

| `kind` | meaning | `arg` | `flag` |
|---|---|---|---|
| 2 | a `LIV`/`LVS` phrase matched only non-livings | the typed phrase | 1 if the slot is a many slot (`LVS`) |
| 3 | an unmodified object slot has a visible but unreachable match (`inventory_accessible`) | the typed phrase | 1 if the slot is a many slot (`OBS`/`LVS`) |
| 4 | a single slot has several equally good matches | the qualifying objects, as an array | 0 |
| 5 | an ordinal (`"third sword"`) went past the available matches | the available count, as described below | 0 |
| 6 | a `direct_`/`indirect_`/`can_` handler returned a reason string | the reason, with a leading `#` stripped | 0 |
| 7 | no object answers to the phrase, or aggregate filtering rejected every match without a reason | the typed phrase | 1 if the slot is a many slot (`OBS`/`LVS`) |
| 8 | `all`, or a plural, or a count on a slot that takes only one | 0 | 0 |

The driver defines no header for these numbers;
the mudlib may define named constants for the kinds it handles.

For two-object rules with at most one plural slot, ordinals count live noun
matches satisfying that slot's access requirement before permissions. A `:v`
slot includes visible but unreachable objects in that count and does not report
kind 3 solely for their inaccessibility. If both nouns resolve but every
pair is refused without a reason, the result is a silent `-2`, without this
hook. Single-object rules and rules with two plural slots retain aggregate
filtering, where ordinals count matches that passed the initial permission
check. See [parser handlers](../object/parser_handlers.md).

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
[`parse_my_rules`](../../efun/parse_my_rules.md),
[`parser_handlers`](../object/parser_handlers.md)
