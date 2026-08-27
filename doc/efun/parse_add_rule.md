# parse_add_rule

`void parse_add_rule(string verb, string rule)`

Compiles `rule` and appends it, under `verb`, to the rules `this_object()`
owns — `this_object()` must have called `parse_init()` first. Several rules
may share a verb; when a typed line reaches the parser package (after the
dispatcher tries the actor's own `add_action`/`add_rule` rules first — see
`parse_sentence`), every registered rule for that verb, on every live verb
object, is tried in registration order until one runs its `do_`. The
dispatcher reads the registry only at that point, after the actor's own
rules have declined, so a rule one of those handlers registers or removes
for the same verb is already seen on that very line.

`parse_add_rule` is a pure append: it never reads the registry first, so
registering the same verb and rule text twice from the same object creates
two separate rules, not one. Both are tried in registration order, both
appear in `parse_my_rules`/`parse_dump`, and nothing merges or replaces the
earlier one. To replace a rule, `parse_remove` its verb first, then
register again.

### The rule

Tokens separated by spaces; `""` registers the bare verb alone.

| token | matches | handler argument |
|---|---|---|
| `OBJ` | words naming one object | the chosen object, or `0` |
| `OBS` | words naming objects, with a numeral | an array of the chosen objects, or `0` |
| `LIV` | words naming one living | the chosen living, or `0` |
| `LVS` | words naming livings, with a numeral | an array of the chosen livings, or `0` |
| `WRD` | one word | the word, as a string |
| `STR` | one or more words | the words, as a string |
| anything else | that word, literally, case-sensitive | — |

A rule may hold at most one `STR` and at most two object slots (`OBJ`,
`OBS`, `LIV`, `LVS`, in any combination) — the first is the *direct* object,
the second the *indirect* one. Extra whitespace between tokens is ignored.

### Handler names and slugs

Each object slot and `STR`/`WRD` token, in rule order, contributes a slug
word: `obj` for `OBJ`, `liv` for `LIV`, `wrd` for `WRD`, `str` for `STR`, and
— this is the one irregular case, kept from MudOS — `obj`/`liv` for `OBS`/
`LVS` too, except in `do_` names, where they contribute `obs`/`lvs`. A
literal word contributes itself, lowercased. The slug is these words joined
with `_`; a bare-verb rule has no slug. It names four applies:

| where | name | fallback |
|---|---|---|
| the verb object | `can_<verb>_<slug>` | `can_verb_rule(verb, rule, ...)` |
| the direct object's candidate | `direct_<verb>_<slug>` | `direct_verb_rule(verb, rule, ...)` |
| the indirect object's candidate | `indirect_<verb>_<slug>` | `indirect_verb_rule(verb, rule, ...)` |
| the verb object | `do_<verb>_<slug>` | `do_verb_rule(verb, rule, ...)` |

`<verb>` is always the *base* verb `parse_add_rule` registered — a
`parse_add_synonym` sibling keeps the original's handler names, even though
it was typed under another verb. A bare-verb rule gives `can_<verb>` and
`do_<verb>`, with no `direct_`/`indirect_` names (it has no object slots).
The full argument order, return values, `#` reasons, and the many-slot array
`do_` sees are described in
[`parser_handlers`](../apply/object/parser_handlers.md).

### A worked example

```c
void create() {
    parse_init();
    parse_add_rule("give", "OBJ to LIV");
}
```

Typing `give sword to bob` calls, in order (`OBJ(sword)` below stands for the
object resolved from the word `sword`; an object slot not yet chosen is
`0`):

- `can_give_obj_to_liv(0, 0, "sword", "bob")`
- `direct_give_obj_to_liv(OBJ(sword), 0, "sword", "bob")` — on each object
  named `sword`, to pick the one meant; for the duration of this call the
  candidate itself sits in its own (direct) slot, and the indirect slot is
  still unfilled
- `indirect_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` — on each
  living named `bob`, once the sword is chosen; the candidate now sits in
  its own (indirect) slot alongside the already-chosen direct object
- `direct_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` and
  `indirect_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` — the
  all-filled re-ask, once both slots are chosen
- `do_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")`

A many slot's (`OBS`/`LVS`) filtering candidate sits in its own slot the same
way — as a bare object, never as a one-element array; the array shape is
only for a slot already *chosen* (the all-filled re-ask and `do_`).

### Scope

An object slot resolves against the actor's scope: the actor, its
environment, both inventories, and — breadth-first — the contents of any
candidate answering `inventory_visible()` truthily, reachable only while
every container on its path answers `inventory_accessible()` truthily. A
container defining **neither** apply is transparent by default: visible and
reachable, the same as a plain object with nothing to hide.

### Errors

Raised as runtime errors, all prefixed `parse_add_rule: `:

| condition | text |
|---|---|
| `this_object()` has not called `parse_init()` | `parse_init() has not been called` |
| two `STR` tokens | `two STR tokens in 'rule'` |
| a token glued to other letters (`OBJect`) | `a token inside a word in 'rule'` |
| an all-caps word sharing a token's prefix (`LIVING`) | `a token inside a word in 'rule'` |
| a word containing `'` (`bob's`) | `a quote inside a word in 'rule'` |
| more than two object slots | `more than two object slots in 'rule'` |

### Departures

Listed in full, with the rest of the parser package's, in
[`parse_sentence`](parse_sentence.md)'s departures table.

### See also

`parse_init`, `parse_add_synonym`, `parse_remove`, `parse_sentence`,
[`parser_handlers`](../apply/object/parser_handlers.md),
[`inventory_visible`](../apply/object/inventory_visible.md),
[`inventory_accessible`](../apply/object/inventory_accessible.md)
