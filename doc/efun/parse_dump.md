# parse_dump

`string parse_dump()`

Every rule registered anywhere in the game by `parse_add_rule` or
`parse_add_synonym`, in registration order, as one string: one line per
rule, `"verb rule  (owner)\n"` — `owner` the registering object's in-game
path. A bare-verb rule leaves nothing between the verb and the two spaces
before the parenthesis. A rule whose owner has been destructed is left out.

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

Departures from MudOS are listed in [`parse_sentence`](parse_sentence.md)'s
departures table.

### See also

`parse_my_rules`, `parse_add_rule`, `parse_add_synonym`
