# commands

`mixed *commands([object ob])`

The rules `ob` — the calling object by default — can command: one
`({ verb, flag, owner, function })` per rule, in the order they were
registered. `verb` is the registered verb; `flag` is the `add_action`
flag (0 for an `add_rule` or `parse_add_rule` rule); `owner` is the
object that registered the rule, 0 once it is destructed; `function` is
the handler's name — a closure literal's is its generated name,
`closure-0` — 0 for a parser rule, whose handlers are named by protocol.
An object that is not living, or has no rules, answers `({})`.
A string `ob` is loaded by path. A destructed or non-object argument is
an error.

### Examples

```c
string *verbs = map(commands(this_player()), (: $1[0] :));
```

### See also

`add_action`, `add_rule`, `parse_add_rule`, `remove_action`, `query_verb`
