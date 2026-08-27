# parse_add_synonym

`void parse_add_synonym(string new_verb, string old_verb, [string rule = 0])`

For every rule `this_object()` has under `old_verb` — matched against that
rule's own verb, exactly as it was itself registered, whether by
`parse_add_rule` or by an earlier `parse_add_synonym` — or, when `rule` is
given, only the one whose text is `rule` — registers a sibling under
`new_verb`. A sibling shares its original's compiled grammar and handlers:
`can_`/`direct_`/`indirect_`/`do_` stay named for the *base* verb (the one
`parse_add_rule` registered), but `query_verb()` inside them reports
whichever verb the player actually typed. A sibling is `parse_remove`d
together with its base verb, not on its own (see `parse_remove`).

Matching `old_verb` against a rule's own verb, not its base, is what lets a
synonym chain from another synonym:

```c
parse_add_rule("give", "OBJ to LIV");
parse_add_synonym("g", "give");   // "g" is a sibling of "give"
parse_add_synonym("gv", "g");     // "gv" is a sibling of "g" — this looks
                                   // up "g", not "give"
```

All three verbs — `give`, `g`, `gv` — reach the same handlers, named for
`give`; `query_verb()` reports whichever of the three was typed.

### Errors

`parse_add_synonym: this_object() has no rules for 'old_verb'` — a runtime
error when `old_verb` (or `old_verb` with that `rule`) names none of
`this_object()`'s registrations.

Departures from MudOS are listed in [`parse_sentence`](parse_sentence.md)'s
departures table.

### See also

`parse_add_rule`, `parse_remove`, `parse_my_rules`, `query_verb`
