# query_verb

`string query_verb([int full = 0])`

Returns the verb of the command in progress, or 0 outside a command.

| Context | `query_verb()` | `query_verb(1)` |
|---|---|---|
| Parser-package trial or its deferred error hook | The matched registered phrase, including a synonym, with canonical spacing | The complete matched phrase as typed, preserving internal whitespace |
| Native `add_rule` | The registered verb alternative that matched | The first word as typed |
| `add_action` | The registered verb, or the whole first word for `AA_SHORT` | The first word as typed |
| Ordinary unhandled-command fallback | The first word as typed | The first word as typed |

For a parser synonym `pick up` of `get`, input `"  pick\t up gem"` reports
`"pick up"` normally and `"pick\t up"` with `full` nonzero. Handler names and
generic base arguments still use `get`. The matched context also applies to
scope and noun-vocabulary hooks. Nested commands and parses restore the
surrounding context on return or exception. `query_command()` retains the whole
line, after any command preprocessing.

### See also

`add_action`, `add_rule`, `query_command`
