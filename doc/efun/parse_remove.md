# parse_remove

`void parse_remove(string verb)`

Removes `this_object()`'s rules for `verb`. `verb` is matched against a
rule's *base* verb — the one `parse_add_rule` registered it under, not the
one a `parse_add_synonym` renamed it to — so removing a base verb removes
every synonym built from it in the same call; removing a synonym's own verb
name matches nothing. Removing a verb `this_object()` has no rules for is
silent.

As a side effect, `parse_remove` also purges every rule left in the registry
by any owner that has since been destructed, not only `this_object()`'s own
— destruct does the same. A dead owner's rules stop being tried immediately
regardless of either; the purge only reclaims the entry.

### See also

`parse_init`, `parse_add_rule`, `parse_add_synonym`, `parse_my_rules`,
`parse_dump`
