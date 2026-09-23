# parser_handlers

Not one apply: the four families of applies a `parse_add_rule` rule calls,
once its grammar matches a typed line, to decide whether the line is
handled and what it does. `parse_add_rule.md` names the specific apply for
a given rule; this page is what it points to for the rest.

### The four families

| family | asked of | specific name | fallback |
|---|---|---|---|
| `can_` | the verb object | `can_<verb>_<slug>` | `can_verb_rule(verb, rule, ...)` |
| `direct_` | each candidate for the direct (first) object slot | `direct_<verb>_<slug>` | `direct_verb_rule(verb, rule, ...)` |
| `indirect_` | each candidate for the indirect (second) object slot | `indirect_<verb>_<slug>` | `indirect_verb_rule(verb, rule, ...)` |
| `do_` | the verb object | `do_<verb>_<slug>` | `do_verb_rule(verb, rule, ...)` |

`<verb>` is always the rule's *base* verb: a `parse_add_synonym` sibling
calls the same handlers as its original, whatever verb was typed. `<slug>`
is described in `parse_add_rule.md`; a bare-verb rule has no slug and no
`direct_`/`indirect_` name (`can_<verb>`, `do_<verb>` only). An object
defining neither the specific nor the generic name for a family is treated
as absent from it: absent `can_` counts as yes; absent `do_` leaves the
rule unhandled (as if every handler in it had refused); a candidate absent
from `direct_`/`indirect_` simply does not qualify.

The generic fallback names take two extra arguments before the ones below:
the base verb, then the rule text exactly as `parse_add_rule` received it
(`"OBJ to LIV"`).

### Arguments

Unchosen values described as `0` below use the selected [null value](../../lpc/null.md):
`0` in zero mode, `nil` in distinct mode.

One argument per capture token, in rule order; literal words add no argument:

- `WRD`/`STR` — the matched text, as a string.
- `OBJ`/`LIV` — an individual object, or `0` while unresolved.
- `OBS`/`LVS` — an individual object during pair permission checks;
  an array for `do_`. Single-object rules and rules with two plural slots
  retain the aggregate permission protocol described below.

Then, one more argument per object slot (`OBJ`/`OBS`/`LIV`/`LVS`, in rule
order), holding the words that slot's phrase was typed as — present in
every call, resolved or not.

`can_` is called once, with every object argument `0`. `do_` is called once
after selection and permission rechecks, with all slots filled. An `OBS` or
`LVS` action argument is always an array, even when only one object qualifies.

### Two-object rules with at most one plural slot

This covers two singular slots and either orientation of one singular and
one plural slot, such as `OBJ from OBJ`, `OBS from OBJ`, and `OBJ with OBS`.

1. Resolve both noun phrases to live, reachable matches. An unreachable match,
   per `inventory_accessible`, is never asked for permission.
2. Pin explicit ordinals to those matches in scope order, before permissions.
   `second sword` means the second reachable sword even if the first refuses.
   If the pinned object is incompatible, the rule fails without substituting
   another object. An out-of-range ordinal reports this pre-permission count.
3. Check each pair in scope order: the first direct match with each indirect
   match, then the next direct match. Call `direct_` on the direct object, then
   `indirect_` on the indirect object only if `direct_` accepts. Both arguments
   are individual objects in both calls, including a plural grammar slot.
   There are no provisional calls with a null partner. Missing either handler
   excludes that pair.
4. Select from pairs accepted by both handlers. A singular slot with several
   compatible objects is ambiguous. A plural slot takes all compatible objects,
   or up to its requested count, in scope order. A singular phrase in a plural
   slot takes the first compatible object. Plural counts cannot resolve an
   ambiguity in the singular partner.
5. Recheck each selected pair once, in the same order, with the same individual
   object arguments and direct-before-indirect ordering. A refused pair is
   removed; other accepted plural members remain eligible. Selection is not
   restarted, and a removed member is not replaced to fill a requested count.
6. Run `do_` if at least one selected pair survives. Build its plural array only
   now, as described below.

Objects destructed by an earlier hook are skipped, including destruction of
either endpoint between a pair's direct and indirect calls. Before `do_`, any
selected pair with a destructed endpoint is removed. Permission effects remain
visible to later checks; rechecking is one pass, not a search for a stable state.
Changes made by a later hook do not trigger another check of an earlier pair.

If noun resolution succeeds but no pair survives permissions, a reason string
can supply kind 6; without a reason this is a silent refusal (`-2`). Unresolved
nouns, inaccessible matches, invalid counts, ordinals, and ambiguity retain
their ordinary diagnostics and silent unresolved result (`-3`).

### Single-object rules and rules with two plural slots

These retain aggregate filtering and rechecks. Resolve each object slot in
order, ask each reachable match with its own slot holding that individual
object, and select that slot before proceeding to the next one. Earlier slots
hold their selected values; later slots remain `0`. Ordinals count matches
that passed this filtering.

After selection, ask each chosen direct object and then each chosen indirect
object again with all slots filled. A plural slot is the same object-only array
in every recheck of that slot. Any refusal at this stage rejects the whole rule.
Two plural slots therefore supply independent arrays, without a mapping of
approved pairs; pair filtering is not performed for those rules.

### Returns

`1`, or anything that is neither `0`, distinct `nil`, nor a string, means yes.
`0` and distinct `nil` mean no.
A string is a *reason*: it fails the same as `0`, but the text (its leading
`#`, if any, stripped) becomes `arg` of `parser_error_message`'s kind 6
(`ERR_ALLOCATED`). A leading `#` marks the reason *soft* — when several
candidates for one slot are asked and none qualifies, a plain reason beats a
soft one, and the earliest of whichever kind wins is what gets reported.
`do_`'s return value is not read at all: the rule counts as handled once
`do_` runs, however it answers.

Rejecting a rule leaves later registered rules eligible. The first rule to
reach `do_` wins, and no earlier failure is sent to `parser_error_message`.
Only after every rule fails are retained diagnostics offered to that hook,
in registration order, until it returns a string. Permission handlers and
`do_` run in the current transaction: a refusal does not undo earlier writes
or output. A runtime exception stops selection and propagates to the caller.

All four families run with `this_player()` and `previous_object()` set to the
actor and `origin()` set to `"driver"`. `query_verb()` is the typed verb, including
a synonym, while handler names and generic prefixes use the base verb.

### The many-slot array

An `OBS`/`LVS` action array contains the accepted objects in scope order,
followed by plain (non-`#`) refusal strings in that same order of rejected
matches. Soft reasons are omitted. Inspect entries before treating them as
objects.

For pair filtering, include refusals only for the selected singular partner.
Either permission hook can supply the reason for a rejected plural member.
A selected pair's final reply replaces its initial reply, so a late plain
refusal is also included. Destructed members contribute neither objects nor
reasons. `parser_error_message` is never called on a successful rule merely to
format these strings.

Under the aggregate protocol, the action array includes plain reasons from
initial filtering. Permission arrays contain only chosen objects; action
arrays are separate arrays and also include the reasons.

### See also

[`parse_add_rule`](../../efun/parse_add_rule.md),
[`parse_sentence`](../../efun/parse_sentence.md),
[`inventory_visible`](inventory_visible.md),
[`inventory_accessible`](inventory_accessible.md),
[`parser_error_message`](../master/parser_error_message.md)
