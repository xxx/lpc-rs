# parse_add_synonym

`void parse_add_synonym(string new_verb, string old_verb, [string rule = 0])`

For every rule `this_object()` registered under `old_verb` with
`parse_add_rule` — or, when `rule` is given, only the one whose text is
`rule` — registers a sibling under `new_verb`. A sibling shares its
original's compiled grammar and handlers: `can_`/`direct_`/`indirect_`/`do_`
stay named for `old_verb`, but `query_verb()` inside them reports whichever
verb the player actually typed. A sibling is `parse_remove`d together with
its base verb, not on its own (see `parse_remove`).

### Errors

`parse_add_synonym: this_object() has no rules for 'old_verb'` — a runtime
error when `old_verb` (or `old_verb` with that `rule`) names none of
`this_object()`'s registrations.

### See also

`parse_add_rule`, `parse_remove`, `parse_my_rules`, `query_verb`
