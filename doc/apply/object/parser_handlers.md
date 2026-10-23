# parser_handlers

Not one apply: the four families of applies a `parse_add_rule` rule calls,
once its grammar matches a typed line, to decide whether the line is
handled and what it does. `parse_add_rule.md` names the specific apply for
a given rule; this page is what it points to for the rest.

### Families and target order

| Family | Targets, in order |
|---|---|
| `can_` | actor, then the object registering the rule |
| `direct_` | candidate for the first object slot, then the rule's owner |
| `indirect_` | candidate for the second object slot, then the rule's owner |
| `do_` | rule's owner only |

On each target, try all four name tiers below before considering the next
object. An inherited handler counts as present. Identical targets are searched
once. A candidate's generic handler takes precedence over the owner's specific
handler; an actor's `can_` supplies the answer instead of the owner's check.

Only an absent function triggers fallback. A present handler's refusal or reason
ends that lookup, and an exception propagates. After all eligible targets have
been searched, absent `can_` permits the rule, absent object permissions exclude
the candidate or pair, and absent `do_` leaves the rule unhandled.

[`parse_my_rules`](../../efun/parse_my_rules.md) with its execution flag omitted
or zero runs the same permissions, then returns the generic-rule action
arguments. It never looks up `do_`, so an action handler is not required.
References to action execution below apply to ordinary dispatch,
`parse_sentence`, and `parse_my_rules` with a nonzero execution flag.

When an object permission runs on the owner, its arguments still name the
original noun candidates. A destructed candidate cannot be accepted through
owner fallback. All calls retain the actor as immediate caller and the selected
command giver as player.
For `parse_sentence` and `parse_my_rules`, the invoking LPC frames remain
behind the actor; see [callback context](../../efun/parse_sentence.md#callback-context).

### Name tiers

For base verb `read` and rule `about STR from OBJ`, search these names in order
on each target, replacing `do_` with the relevant family prefix:

| Tier | Name | Leading arguments |
|---|---|---|
| Specific | `do_read_about_str_from_obj` | capture values |
| Literal words | `do_read_word_str_word_obj` | literal words and capture values in rule order |
| Generic shape | `do_verb_word_str_word_obj` | base verb, then literal words and capture values |
| Generic rule | `do_verb_rule` | base verb, exact registered rule text, then capture values |

`WRD` contributes `wrd` to a name; `word` stands for a literal in the middle
two tiers. Only those tiers insert literal arguments, using their registered
spelling. [`parse_add_rule`](../../efun/parse_add_rule.md) defines capture slugs.
Permission names use `obj`/`liv` for plural slots; action names use `obs`/`lvs`.
The `:v` modifier changes neither names nor argument layouts; the generic rule
argument retains it as part of the exact registered text.

Names and generic prefixes always use the base verb, including for multiword
synonyms. A multiword base is inserted literally into names; it can use generic
tiers, but spaces are not converted to underscores for specific methods.
Duplicate generated names use their earliest tier's layout; parameter types or
counts do not select a different tier. For example, a literal named `word` can
make the first two names identical, in which case the specific layout wins.

An empty rule tries `can_<base>`, `can_verb`, `can_verb_rule`, and the corresponding
`do_` names. With no object slots, it makes no direct or indirect calls.

### Arguments

Unchosen object values use the selected [null value](../../lpc/null.md):
`0` in zero mode, `nil` in distinct mode.

Each capture contributes one semantic value:

- `WRD`/`STR` — the matched text, as a string.
- `OBJ`/`LIV` — an individual object, or the null value while unresolved.
- `OBS`/`LVS` — an individual object during pair permission checks;
  an array for `do_`. Single-object rules and rules with two plural slots
  retain the aggregate permission protocol described below.

After the leading arguments in the table, generic shape and generic rule
handlers receive the typed verb, including a synonym, as returned by
`query_verb(1)`: the full matched phrase with its original internal whitespace.
Every tier then receives one source string per capture in rule
order, including `WRD` and `STR`. These strings retain the matched source text.
Literals contribute no source-string suffix.

For `read about history from book`, with `B` the chosen book, action arguments
are:

| Name | Arguments |
|---|---|
| `do_read_about_str_from_obj` | `"history", B, "history", "book"` |
| `do_read_word_str_word_obj` | `"about", "history", "from", B, "history", "book"` |
| `do_verb_word_str_word_obj` | `"read", "about", "history", "from", B, "read", "history", "book"` |
| `do_verb_rule` | `"read", "about STR from OBJ", "history", B, "read", "history", "book"` |

Multiple `STR` tokens contribute separate values and source strings. For
`parse_add_rule("cast", "STR at STR")` and `cast spark at tower`, the layouts are:

| Name | Arguments |
|---|---|
| `do_cast_str_at_str` | `"spark", "tower", "spark", "tower"` |
| `do_cast_str_word_str` | `"spark", "at", "tower", "spark", "tower"` |
| `do_verb_str_word_str` | `"cast", "spark", "at", "tower", "cast", "spark", "tower"` |
| `do_verb_rule` | `"cast", "STR at STR", "spark", "tower", "cast", "spark", "tower"` |

The `can_` family receives the same text arguments. A text-only rule makes no
direct or indirect calls. Text captures interspersed with object captures keep
their rule positions in every family; only object slots select permission targets.

A synonym changes the later typed-verb argument, while the leading base verb
remains `"read"`. Empty-rule generic handlers receive `(base, typed)` or
`(base, "", typed)`. Fixed-arity functions may omit unused trailing parameters;
named variadic collectors receive the complete tail.

`can_` runs once per grammatical parse tried, with every object value unresolved.
Refusing one split leaves later splits eligible in
[longest-first order](../../efun/parse_add_rule.md#text-captures-and-split-selection).
`do_` runs once after
selection and permission rechecks, with all slots filled. An `OBS` or `LVS`
action argument is always an array, even when only one object qualifies.

### Visibility and reachability

Every object slot resolves within the visible scope. Unmodified slots also
require physical reachability. A `:v` slot accepts visible but unreachable
objects, subject to the same living and permission checks. Scope filtering
precedes the selection protocols below; modifiers apply independently to each
capture and do not change counts, ordinals, or plural action types. See
[`parse_sentence`](../../efun/parse_sentence.md#scope) for supplied scope trees.

### Two-object rules with at most one plural slot

This covers two singular slots and either orientation of one singular and
one plural slot, such as `OBJ from OBJ`, `OBS from OBJ`, and `OBJ with OBS`.

1. Resolve both noun phrases to live matches satisfying each slot's access
   requirement. An unreachable match is asked for permission only for a `:v` slot.
2. Pin explicit ordinals to those matches in scope order, before permissions.
   `second sword` means the second sword satisfying that slot's access requirement,
   even if the first refuses.
   If the pinned object is incompatible, the rule fails without substituting
   another object. An out-of-range ordinal reports this pre-permission count.
3. Check each pair in scope order: the first direct match with each indirect
   match, then the next direct match. Call `direct_` on the direct object, then
   `indirect_` on the indirect object only if `direct_` accepts. Both arguments
   are individual objects in both calls, including a plural grammar slot.
   There are no provisional calls with a null partner. Missing either handler
   on both the candidate and owner excludes that pair.
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
order, ask each match satisfying its slot's access requirement with that individual
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

The longest matching verb phrase is tried first, with registration order within
each phrase. Rejecting a rule leaves later rules and shorter phrases eligible.
The first rule to reach `do_` wins, and no earlier failure is sent to
`parser_error_message`. Only after every phrase fails are retained diagnostics
offered to that hook with the most resolved object slots first, preserving
trial order for ties, until it returns a string. A `can_` refusal has zero
progress; pair permissions begin after both nouns resolve. Permission handlers and
`do_` run in the current transaction: a refusal does not undo earlier writes
or output. A runtime exception stops selection and propagates to the caller.

All four families run with the selected command giver as `this_player()`, the
actor as `previous_object()`, and `"driver"` as `origin()`. The giver defaults to
the actor; parser efuns can select another object or explicit null.
`query_verb()` is the canonical matched phrase, including a synonym; `query_verb(1)` preserves its raw input span.
Handler names and generic prefixes use the base verb. Scope and vocabulary hooks
see the same matched context, and each deferred error hook sees its own failed
phrase. Nested commands restore the surrounding context on return or exception.
`parse_sentence` defaults the actor to the entering player and accepts an
explicit override; `parse_my_rules` requires the actor argument. The actor
continues to determine scope and actor permission lookup independently of the
giver. Each callback starts with the selected giver, and selecting a giver does
not replace the task's interactive provenance.

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
[`parse_my_rules`](../../efun/parse_my_rules.md),
[`inventory_visible`](inventory_visible.md),
[`inventory_accessible`](inventory_accessible.md),
[`parser_error_message`](../master/parser_error_message.md)
