# parser_error_message

`string parser_error_message(int kind, object ob, mixed arg, int flag)`

Applied in the master when a `parse_add_rule` rule's parse fails and the
failure has something worth describing. A string result becomes what
`parse_sentence` returns, or — on the dispatcher path — what is delivered to
the actor; `0`, or no such apply, leaves the failure silent: `-2` (a handler
refused) or `-3` (an object phrase did not resolve) from `parse_sentence`,
and on the dispatcher path the next rule for the verb, then the ordinary
`command_not_found` fallback.

A handler that refuses outright — `0`, no reason string — never reaches this
apply; there is nothing to describe.

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

Kinds 1 (`ERR_IS_NOT`) and 9 (`ERR_MANY_PATHS`) are MudOS's; neither is ever
raised here (see `parse_sentence`'s departures table). The numbers are
MudOS's own; the driver defines no header for them, so a lib including one
of MudOS's gets the same constants.

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

Departures from MudOS are listed in
[`parse_sentence`](../../efun/parse_sentence.md)'s departures table.

### See also

[`parse_add_rule`](../../efun/parse_add_rule.md),
[`parse_sentence`](../../efun/parse_sentence.md),
[`parser_handlers`](../object/parser_handlers.md)
