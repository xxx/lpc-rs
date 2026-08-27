# parse_my_rules

`string *parse_my_rules()`

`this_object()`'s own registered rules, in registration order, each as
`"verb rule"` — a `parse_add_synonym` sibling reports the verb it was
registered under, not the base verb its handlers are named for. A bare-verb
rule (`rule` `""`) leaves a trailing space: `"look "`.

### See also

`parse_add_rule`, `parse_add_synonym`, `parse_remove`, `parse_dump`
