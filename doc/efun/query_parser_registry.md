# query_parser_registry

`mapping *query_parser_registry(object owner = nil)`

Returns parser-package registrations as structured records. Omit `owner` or pass
`nil` to inspect all registering objects. Pass a live object to inspect only its
registrations, including another object's. A destructed object returns an empty
array; it does not select the global registry.

Each mapping contains four fields:

| Key | Type | Value |
| --- | --- | --- |
| `verb` | string | Registered verb key, including multiword verbs and synonyms. |
| `rule` | string | Original rule text, preserving whitespace. |
| `owner` | object | Object that registered the rule. |
| `handler_verb` | string | Base verb used by the rule's handler protocol. |

Verb keys have canonical spacing and retain their spelling. For a normal
registration, `handler_verb` equals `verb`. A synonym, including a synonym of a
synonym, keeps the original handler verb. Bare-verb rules have an empty `rule`.
Keeping the fields separate distinguishes verb `go` with rule `to OBJ` from
verb `go to` with rule `OBJ`.

Results retain registration order, including duplicate registrations and synonym
entries. This is not the longest-phrase order used for dispatch. Only
`parse_add_rule` and `parse_add_synonym` registrations are included; actor-local
`add_action` and `add_rule` tables are separate. Rules whose owners are no longer
live in the current transaction are omitted.

The query sees this transaction's pending registrations, removals and destruction.
Its registry read participates in conflict detection. It calls no handlers or
apply hooks and emits no output. An empty query returns an empty array. Each call
creates fresh arrays and mappings; editing their fields or strings does not edit
the registry or another query result. The owner fields remain ordinary object
references, so an owner can be destructed after the query.

`nil` follows the configured null mode: integer zero selects all only in zero
mode. Other argument values raise an error. Known non-object argument types and
more than one argument are rejected by the compiler.

### Example

```c
void create() {
    parse_init();
    parse_add_rule("look", "at OBJ");
    parse_add_synonym("examine", "look", "at OBJ");
}

void show_rules() {
    foreach (mapping row : query_parser_registry(this_object())) {
        write(sprintf("%s: %s (handlers: %s)\n",
            row["verb"], row["rule"], row["handler_verb"]));
    }
}
```

`show_rules()` prints:

```text
look: at OBJ (handlers: look)
examine: at OBJ (handlers: look)
```

### See also

[`query_parser_rules`](query_parser_rules.md), [`parse_dump`](parse_dump.md),
[`parse_add_rule`](parse_add_rule.md), [`parse_add_synonym`](parse_add_synonym.md),
[`parse_remove`](parse_remove.md)
