# query_parser_rules

`string *query_parser_rules()`

`this_object()`'s own registered rules, in registration order, each as
`"verb rule"` — a `parse_add_synonym` sibling reports the verb it was
registered under, not the base verb its handlers are named for. A bare-verb
rule (`rule` `""`) leaves a trailing space: `"look "`. Verb keys use canonical
spacing, including multiword keys; rule text retains its original whitespace.
This is registration order, not the longest-phrase order used for dispatch.

### See also

`parse_add_rule`, `parse_add_synonym`, `parse_remove`, `parse_dump`,
[`parse_my_rules`](parse_my_rules.md)
