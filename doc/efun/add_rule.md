# add_rule

`int add_rule(string pattern, string | function handler)`

Registers `pattern` on `this_player()`, which must be a living object: when
that player types a line the pattern matches, `handler` is called with one
argument per capture, in pattern order. A string handler names a function of
the calling object (any visibility); a function pointer must have its
receiver bound, as `call_out` requires. Returns the rule's id, which
`remove_rule` takes. No living `this_player()` is a runtime error.

Rules are tried most recently registered first, native and `add_action` rules
alike; a handler returning 0 passes the line to the next rule, and any other
return handles it. A line the pattern does not match — or a `%d` too large
for an int — passes to the next rule the same way.

### The pattern

A pattern is a sequence of elements separated by spaces. The first element
must be a quoted verb; the rest describe the words after it.

| element | matches | argument |
|---|---|---|
| `'word'` | that word | — |
| `'get' / 'take'` | any one of the words | — |
| `[word]` | the word if present | — |
| `%w` | one word | string |
| `%s` | zero or more words | string, spacing intact, `""` when none |
| `%d` | a run of digits | int |

`%d` matches digits only: `-5` and spelled-out numbers do not, unlike
`parse_command` in other drivers.

Matching is by whole words and case-sensitive — unlike `parse_command` in
other drivers, and like `add_action` here. A quoted word cannot contain `'`.
`%s` is greedy: in `'say' %s 'to' %w`, the line `say hi to bob to sam` gives
`%s` the text `hi to bob` and `%w` the word `sam`. A `%d` too large for an int
does not match. A bare unquoted word, an unclosed quote or bracket, a quoted
phrase of several words, or a `/` not between two quoted words is a runtime
error naming the fault.

`%o`, `%l`, `%i`, and `%p` need the noun resolver, which this driver does not
have yet; a pattern using them is a runtime error.

`query_verb()` inside the handler reports the verb alternative the player
typed. Rules registered from `init()` last until the player leaves the
registering object's presence; `disable_commands()` and `destruct` remove
them too.

### Examples

```c
int give_id;

void init() {
    give_id = add_rule("'give' / 'hand' %w 'to' %w", "do_give");
    add_rule("'look' [at] %s", "do_look");
    add_rule("'buy' %d %w", (: do_buy :));
}

int do_give(string what, string whom) { ... return 1; }
int do_look(string at) { ... return 1; }
int do_buy(int count, string what) { ... return 1; }
```

### See also

`remove_rule`, `add_action`, `command`, `query_verb`, `notify_fail`, `init`,
`enable_commands`
