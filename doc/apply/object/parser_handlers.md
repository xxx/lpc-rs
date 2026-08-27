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

One argument per token, in rule order:

- `WRD`/`STR` — the matched text, as a string.
- `OBJ`/`LIV` — the object chosen for that slot, or `0` while it is
  unchosen. The one exception: for the duration of a candidate's own
  `direct_`/`indirect_` filtering call for that same slot, the argument is
  that candidate itself, not `0` — the slot reverts to `0` once the call
  returns, before the next candidate is asked.
- `OBS`/`LVS` — an array of the objects chosen for that slot, or `0` while
  unchosen. The same exception holds during filtering, but the candidate
  sits in its own slot as a bare object, never as a one-element array — the
  array shape is only for a slot already *chosen*.

Then, one more argument per object slot (`OBJ`/`OBS`/`LIV`/`LVS`, in rule
order), holding the words that slot's phrase was typed as — present in
every call, resolved or not.

`can_` is called once, with every object argument `0` (nothing is chosen
yet). `direct_`/`indirect_` are each called once per reachable candidate the
resolver found for their slot (an unreachable match, per
`inventory_accessible`, is never asked at all), with earlier slots' arguments
filled, the current slot holding that one candidate (bare object, even for a
many slot), and later slots still `0`; once every slot is chosen, `direct_`
and `indirect_` are each called once more on the objects finally chosen,
every argument filled — MudOS's guarantee that a handler eventually sees the
whole sentence. `do_` is called once, after that re-ask, with every slot
filled.

### Returns

`1`, or anything that is neither `0` nor a string, means yes. `0` means no.
A string is a *reason*: it fails the same as `0`, but the text (its leading
`#`, if any, stripped) becomes `arg` of `parser_error_message`'s kind 6
(`ERR_ALLOCATED`). A leading `#` marks the reason *soft* — when several
candidates for one slot are asked and none qualifies, a plain reason beats a
soft one, and the earliest of whichever kind wins is what gets reported.
`do_`'s return value is not read at all: the rule counts as handled once
`do_` runs, however it answers.

### The many-slot array

Everywhere but `do_` — `can_`, `direct_`/`indirect_`, and the all-filled
re-ask — an `OBS`/`LVS` slot's argument is the array of objects chosen for
it so far, and nothing else (a candidate filtering for that very slot is the
one exception described above: itself, as a bare object, not an array).
`do_` alone sees something wider for that same slot: the qualifying objects,
in candidate order, followed by every *plain* (non-`#`) reason a candidate
that did not qualify returned, as a string; soft reasons are dropped by this
point.

Departures from MudOS are listed in [`parse_sentence`](../../efun/parse_sentence.md)'s
departures table.

### See also

[`parse_add_rule`](../../efun/parse_add_rule.md),
[`parse_sentence`](../../efun/parse_sentence.md),
[`inventory_visible`](inventory_visible.md),
[`inventory_accessible`](inventory_accessible.md),
[`parser_error_message`](../master/parser_error_message.md)
