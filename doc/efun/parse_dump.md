# parse_dump

`string parse_dump()`

Every rule registered anywhere in the game by `parse_add_rule` or
`parse_add_synonym`, in registration order, as one string. Each registration is
formatted as `"verb rule  (owner)\n"` — `owner` the registering object's in-game
path. A bare-verb rule leaves nothing between the verb and the two spaces
before the parenthesis. Verb keys use canonical spacing, including multiword
keys; rule text is preserved exactly. This is registration order, not dispatch's
longest-phrase order. A rule whose owner has been destructed is left out.

Embedded newlines in rule text are emitted unchanged, so one registration can
span multiple physical lines. Joined verb/rule text does not preserve the boundary
between a multiword verb and its rule. Use
[`query_parser_registry`](query_parser_registry.md) for structured records.

### Example

```c
void create() {
    parse_init();
    parse_add_rule("look", "");
    parse_add_rule("look", "at OBJ");
    parse_add_synonym("examine", "look", "at OBJ");
}
```

registered in `/verbs/look.c`, `parse_dump()` returns:

    look   (/verbs/look)
    look at OBJ  (/verbs/look)
    examine at OBJ  (/verbs/look)

### See also

`query_parser_rules`, `parse_add_rule`, `parse_add_synonym`
