# add_action

`void add_action(string | function handler, string | string *verb, [int flag = 0])`

Registers `verb` on `this_player()`, which must be a living object: when that
player types a line starting with `verb`, `handler` is called with the rest of
the line as its one string argument. A string handler names a function of the
calling object (any visibility); a function pointer must have its receiver
bound, as `call_out` requires. An array registers one rule per verb.

Rules are tried most recently registered first; a handler returning 0 passes
the line to the next rule, and any other return handles it. In the handler,
`previous_object()` is the player.

`flag` selects how the verb matches the first word: `0` — the word equals the
verb; `1` / `AA_SHORT` — the word starts with the verb, `query_verb()` reports
the whole word, and the rest of the word joins the argument; `AA_NOSPACE` (2) —
as `AA_SHORT` but `query_verb()` reports the verb as registered;
`AA_IMM_ARGS` (3) — as `AA_NOSPACE` with only the rest of the word as the argument.

An empty verb (`""`) with a prefix flag matches every line, which is how a
mudlib registers a catch-all handler.

The driver defines no `AA_*` names; a mudlib's include file does
(`#define AA_SHORT 1` etc.).

Rules registered from `init()` last until the player leaves the registering
object's presence; `disable_commands()` and `destruct` remove them too.

### Examples

```c
void init() {
    add_action("do_look", "look");
    add_action("do_say", "'", AA_SHORT);
    add_action((: do_get :), ({ "get", "take" }));
}

int do_say(string text) {
    say(query_verb(1)[1..] + " " + text);
    return 1;
}
```

### See also

`remove_action`, `add_rule`, `command`, `query_verb`, `notify_fail`, `init`, `enable_commands`
