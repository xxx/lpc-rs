# parse_init

`void parse_init()`

Marks `this_object()` a verb object: `parse_add_rule` refuses to register a
rule until this has been called. Calling it again, or after rules are
already registered, is harmless.

A verb object's rules apply driver-wide, unlike `add_rule`'s: once
registered, a rule is tried whenever its verb is typed by any living,
wherever the verb object itself sits in the game.

### Errors

`parse_init: this_object() is not live` — `this_object()` has been
destructed.

### See also

`parse_add_rule`, `parse_add_synonym`, `parse_remove`, `parse_refresh`,
`parse_my_rules`, `parse_dump`
