# parse_add_rule

`void parse_add_rule(string verb, string rule)`

Compiles `rule` and appends it, under `verb`, to the rules `this_object()`
owns — `this_object()` must have called `parse_init()` first. Several rules
may share a verb; when a typed line reaches the parser package (after the
dispatcher tries the actor's own `add_action`/`add_rule` rules first — see
`parse_sentence`), the longest matching verb phrase is tried first, with rules
for that phrase on every live verb object tried in registration order. If none
runs its `do_`, shorter matching phrases are tried in turn. The
dispatcher reads the registry only at that point, after the actor's own
rules have declined, so a rule one of those handlers registers or removes
for the same verb is already seen on that very line.

A failed rule, including one that supplies a reason string, leaves later
rules and shorter phrases eligible. Within one phrase, the first successful
rule wins even if a later rule is more specific. Only after all phrases fail
does the driver ask [`parser_error_message`](../apply/master/parser_error_message.md)
for text, in that same phrase and registration order, stopping at the first
string. A successful search never calls that hook.

[`parse_my_rules`](parse_my_rules.md) restricts this search to the caller's
registrations. Its default argument mode ends at the first parse that passes
permissions, returning generic action arguments without requiring or calling
any `do_` handler.

`parse_add_rule` is a pure append: it never reads the registry first, so
registering the same verb and rule text twice from the same object creates
two separate rules, not one. Both are tried in registration order, both
appear in `query_parser_rules`/`parse_dump`, and nothing merges or replaces the
earlier one. To replace a rule, `parse_remove` its verb first, then
register again.

### The verb

A verb may contain several words. Leading/trailing Unicode whitespace is removed
and whitespace between words becomes one space in the registered key. Matching
uses the same word boundaries, preserving case and punctuation. Empty or
whitespace-only names are errors. `pick up` does not match `pick upper`.

For example, `parse_add_synonym("pick up", "get")` makes `pick up gem` use
`get`'s handlers. A registered `pick up` is tried before `pick`, regardless of
registration order, with `pick` still eligible if the longer phrase fails.
The grammar receives only the text after the matched phrase, with source text
preserved. Registrations are snapshotted before parser callbacks; additions or
removals during a trial affect the next parse, while destructed owners are skipped.

Handler names retain the base verb literally. A multiword base therefore uses
the generic handler tiers; spaces are not converted to underscores. Use a
single-word base and a multiword synonym when specific handler names are wanted.

### The rule

Tokens separated by spaces; `""` registers the bare verb alone.

| token | matches | action (`do_`) argument |
|---|---|---|
| `OBJ` | words naming one object | the chosen object |
| `OBS` | words naming objects, with a numeral | an array of objects and plain refusal strings |
| `LIV` | words naming one living | the chosen living |
| `LVS` | words naming livings, with a numeral | an array of livings and plain refusal strings |
| `WRD` | one word | the word, as a string |
| `STR` | one or more words | the words, as a string |
| anything else | that word, literally, case-sensitive | — |

A rule may contain multiple `STR` captures, including adjacent ones such as
`"STR STR STR"`. Each must match at least one word. There may be at most two
object slots (`OBJ`, `OBS`, `LIV`, `LVS`, in any combination) — the first is
the *direct* object, the second the *indirect* one. Text captures do not count
towards that limit. Extra whitespace between tokens is ignored.

Append `:v` to an object token to require visibility without physical
reachability: `OBJ:v`, `OBS:v`, `LIV:v`, or `LVS:v`. For example,
`"OBJ at OBJ:v"` requires the first object to be reachable, while the target
only needs to be visible. The modifier does not reveal hidden contents or
bypass living checks, permission handlers, or action policy. It applies before
ordinal/count selection and also governs phrases such as `all`.

Only the single `:v` suffix is supported. Modifiers on `WRD`/`STR`, other
suffixes, and repeated/combined modifiers are errors.

### Text captures and split selection

For `parse_add_rule("cast", "STR at STR")`, the line
`cast spark at dusk at tower` admits these captures, tried in this order:

1. `"spark at dusk"`, `"tower"`.
2. `"spark"`, `"dusk at tower"`.

Earlier captures take the longest spans that let the remaining rule match.
A literal separator can also occur inside a capture. A refusal leaves later
splits eligible before trying the next registered rule; the first split to
reach `do_` (or return arguments in `parse_my_rules`) wins. Permission effects
remain visible during retries, and an exception stops the search.

At most 32 grammatical parses are tried for one rule and input, including
parses refused by handlers. Highly ambiguous text rules can have additional
valid splits beyond that cap. This is a result-count limit, not a work budget.
Capture strings retain their original case, punctuation and internal whitespace.

### Handler names and slugs

Each object slot and `STR`/`WRD` token, in rule order, contributes a slug
word: `obj` for `OBJ`, `liv` for `LIV`, `wrd` for `WRD`, and `str` for `STR`.
`OBS`/`LVS` contribute `obj`/`liv` too, except in `do_` names, where they
contribute `obs`/`lvs`. The `:v` suffix contributes nothing to handler names
or argument layouts; generic rule text still includes it exactly as registered.
A literal word contributes itself, lowercased. The
slug is these words joined with `_`; a bare-verb rule has no slug. It names
four apply families: `can_`, `direct_`, `indirect_`, and `do_`.

On each eligible target, names are tried in this order:

1. `<family>_<verb>_<slug>`.
2. The same name with every literal slug word replaced by `word`.
3. The second name with the base verb replaced by `verb`.
4. `<family>_verb_rule`.

For example, `read about STR from OBJ` gives
`do_read_about_str_from_obj`, `do_read_word_str_word_obj`,
`do_verb_word_str_word_obj`, then `do_verb_rule`. `WRD` remains `wrd`;
only literals become `word`. Duplicate names keep the earliest layout.

`<verb>` is always the registered base verb, including for synonyms. A bare
rule omits the slug, giving `can_<verb>`, `can_verb`, `can_verb_rule` and the
corresponding action names. It has no direct or indirect calls.

`can_` searches the actor then the rule's owner; object permissions search
the candidate then the owner; `do_` searches only the owner. All names on one
target precede the next target, and only absence triggers fallback.
The middle two tiers insert literal arguments. All tiers append source text
for every capture; generic tiers also include the typed verb before that tail.
See [parser handlers](../apply/object/parser_handlers.md) for exact argument
layouts, replies and selection timing.

### A worked example

```c
void create() {
    parse_init();
    parse_add_rule("give", "OBJ to LIV");
}
```

With one matching sword and one matching Bob, typing `give sword to bob`
calls these specific handlers when defined, in order (`OBJ(sword)` stands for
the resolved object; unresolved slots use the selected null value):

- `can_give_obj_to_liv(0, 0, "sword", "bob")` — on the actor, or owner if absent
- `direct_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` — on the sword
- `indirect_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` — on Bob,
  if the sword's direct check accepted
- `direct_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` and
  `indirect_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")` — the
  selected pair's recheck, in that order
- `do_give_obj_to_liv(OBJ(sword), OBJ(bob), "sword", "bob")`

When there are several matches, the parser checks compatible pairs before
selecting the objects. Bob's permission can therefore determine which sword
is meant, and the sword's permission can determine which Bob is meant.

With one plural slot, as in `OBS to LIV` or `OBJ with OBS`, permission checks
and rechecks still receive individual objects. The action receives an array
for the plural slot, with rejected members removed and plain refusal strings
appended. Explicit ordinals are pinned before permission checks; ordinary
counts are selected afterward. Single-object rules and rules with two plural
slots retain aggregate rechecks; see the full
[handler contract](../apply/object/parser_handlers.md).

### Scope

An object slot resolves against the actor's scope: the actor, its
environment, its own inventory, the environment's other contents, and -
breadth-first — the contents of any candidate answering
`inventory_visible()` truthily, reachable only while every container on its
path answers `inventory_accessible()` truthily. A container defining
**neither** apply is transparent by default: visible and reachable, the
same as a plain object with nothing to hide. The actor and its immediate
environment contribute their direct inventories without consulting their own
inventory hooks. A `:v` slot can select visible but unreachable contents; an
unmodified slot cannot. Inventory hooks govern contents, not the container's
own eligibility as a noun.

[`parse_sentence`](parse_sentence.md#scope) can replace this walk with an
explicit scope, including nested arrays describing supplied contents.

### Errors

Raised as runtime errors, all prefixed `parse_add_rule: `:

| condition | text |
|---|---|
| `this_object()` has not called `parse_init()` | `parse_init() has not been called` |
| empty or whitespace-only verb | `the verb must contain at least one word` |
| unsupported modifier, or a modifier on a text token | `unsupported parser modifier in 'token'; only OBJ:v, OBS:v, LIV:v and LVS:v are supported` |
| a token glued to other letters (`OBJect`) | `a token inside a word in 'rule'` |
| an all-caps word sharing a token's prefix (`LIVING`) | `a token inside a word in 'rule'` |
| more than two object slots | `more than two object slots in 'rule'` |

### See also

`parse_init`, `parse_add_synonym`, `parse_remove`, `parse_sentence`, `parse_my_rules`,
[`parser_handlers`](../apply/object/parser_handlers.md),
[`inventory_visible`](../apply/object/inventory_visible.md),
[`inventory_accessible`](../apply/object/inventory_accessible.md)
