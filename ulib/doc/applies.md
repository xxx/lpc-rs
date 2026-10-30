# Apply reference

An **apply** is a function the driver calls by name on a particular LPC object.
It is not an efun you call to ask the driver to do something. This index covers
every apply documented by the accompanying lpc-rs distribution, including the
four families of grammar handlers. It describes this driver's contracts;
other LPC drivers may use different signatures or defaults.

Each entry points to a live implementation or an optional example. The files
under `examples/` use `#if 0`: copy a selected function into its proper owner to
enable it. Do not inherit those files wholesale. An ordinary helper such as
`query_name()`, `announce()` or `do_help()` is not an apply.

Applies belong to object roles, not fixed source filenames. The configuration
selects the master object, and the master's `connect()` selects the initial
connection body; `exec()` can later transfer the connection to another body.
Paths such as `/secure/master.c` and `/obj/player.c` identify ulib's choices.
Your mudlib can use other paths while preserving the required apply names and
contracts. When renaming or moving ulib objects, update the configuration,
object-path constants and any affected load or inheritance permissions too.

Every source function has adjacent documentation describing its role, inputs
and result. Apply comments also explain omission defaults and why ulib makes
its particular choice; parser examples identify which object owns each hook.

The default `NULL_MODE=zero` uses `0` as the absent value for LPC strings,
objects and arrays. With `NULL_MODE=distinct`, absence uses `nil`; omitted
parameters use `0` for int, `0.0` for float and `nil` otherwise. The zero
absence descriptions below assume the default mode; numeric status codes
and parser flags stay numeric in both modes. Unless an
entry says otherwise, an optional missing hook performs no mudlib action.

## Master applies

These belong on the object configured by `LPC_MASTER_OBJECT`.
Live implementations are in [secure/master.c](../secure/master.c); optional
ones are in [examples/master.c](../examples/master.c).

### Lifecycle, input and diagnostics

| Apply and arguments | Result and behaviour | Example |
|---|---|---|
| `connect(string remote_ip_addr, int remote_port)` | Return the body to attach, or a string to send before disconnecting; anything else fails login. Required for a usable server. There is no attached `this_player()` yet. | Live: clone a player. |
| `epilog(int load_empty)` | Return paths for startup preloading. `load_empty` is currently always `0`; missing or `0` means no preloads. | Live: return the lounge path. |
| `preload(string file)` | Load each requested file yourself; the driver calls this once per `epilog()` entry. A missing hook does not load it. | Live: `load_object(file)`. |
| `compile_object(string path, string func, object caller, string program)` | For a missing source file, return an existing blueprint's path to create a virtual object. `path` has a leading slash and no `.c`. Returning `0` or omitting the hook falls back to the ordinary load, which may fail. | Optional: decline with `0`. |
| `modify_command(string line, object player)` | Return the command string to dispatch; **any non-string, including `0`, consumes it**. Omission passes the original through. Runs before the player's `process_input()`. | Optional: return `line`. |
| `command_not_found(object player, string line)` | Return a message for an unhandled command, or `0` for silence. A handler's `notify_fail()` message takes precedence. Omission gives the driver's `What?` message or its hint for an unimplemented living. | Live: suggest `help`. |
| `error_handler(mapping error)` | Handle a runtime error in a fresh transaction. Keys: `error`, `location`, `object`, `diagnostic`; optional `frames` and `frames_omitted` provide bounded root-to-leaf source and rendered argument/local snapshots, rebuilt on handler retries. Missing frame keys mean no snapshot; caught errors expose none. The driver always logs the diagnostic; `this_interactive()` identifies the affected connection for commands and input callbacks. Without this hook the driver also sends its diagnostic to that player. | Live: send a short notice. |
| `warning_handler(mapping warning)` | Handle a compiler warning in the compiling transaction. Keys: `message`, `location`, `file`, `diagnostic`. Omission logs it; implementing this hook makes logging your responsibility. | Live: write the diagnostic to the debug log. |
| `get_mud_stats()` | Return an MSSP mapping. Custom fields override defaults; omit `PLAYERS` to retain the driver's live connection count. Custom values are cached for each connection. | Live: set `NAME` and `FAMILY`. |
| `shutdown()` | Last LPC hook during an orderly stop; client output can still be delivered, but new commands, call-outs and raw TCP callbacks do not run. Raw TCP workers stop before the hook. | Live: broadcast a farewell. |

### Policy

These hooks return an integer: nonzero permits, `0` denies. Omission denies
the gated operation except for `valid_override`, whose omission allows explicit
efun calls. They run in the requesting operation's transaction.
`program` identifies the source defining the code that asked, which can differ
from the caller object's name through inheritance or virtual objects.

| Apply and arguments | What it gates | ulib policy |
|---|---|---|
| `valid_load(string path, string func, object caller, string program)` | Loading source not already resident, or a `compile_string()`/`syntax_tree()` source identity after `valid_write()`. `path` is the canonical source filename with `.c`. Ordinary calls to resident objects and cloning an existing prototype do not ask again. | `/obj/`, `/room/`, `/std/` only. |
| `valid_inherit(string path, string from)` | Explicit source inheritance, with canonical filenames. Configured auto-inheritance is exempt. | `/std/` only. |
| `valid_override(string source, string name, string main_file)` | Explicit `efun::` calls during LPC-triggered compilation. Source and main paths retain extensions; macros name their invocation site. Boot compilation is exempt. Omitting the hook allows calls; existing compiled code is not revoked. | Deny; ordinary efun calls remain available. |
| `valid_read(string path, string func, object caller, string program)` | File reads and compilation includes. An include uses `func == "include"`, `caller == 0`, and the including source as `program`. | Only includes under `/include/`. |
| `valid_write(string path, string func, object caller, string program)` | File mutation and named source for `compile_string()` or `syntax_tree()`. | Deny. |
| `valid_destruct(object caller, object target, string program)` | Object destruction, including self-destruction. | Only `caller == target`. |
| `valid_exec(string program, object new_body, object old_body)` | Moving a connection between bodies with `exec()`. `program` has a leading slash and `.c`; the driver's initial attachment after `connect()` does not need this grant. | Deny. |
| `valid_snoop(object caller, object observer, object target, string operation, string program)` | Start, stop or inspect a snoop relationship. Operation selects the endpoint meanings; runs in the requesting transaction with its player context. Missing policy denies even self-inspection. | Deny. |
| `valid_socket(object caller, string operation, mixed *info)` | Authorize raw TCP creation, mutation, offers or inspection. The five info slots describe handle, owner, endpoint and recipient; state is rechecked after policy returns. Runs in the requesting transaction with its player context. | Deny; ulib uses player Telnet sessions only. |
| `valid_variable_info(object caller, object target, string program)` | Inspecting another object's globals, including private ones; inspecting self does not ask. | Deny. |
| `valid_store_variable(object caller, object target, string name, string declaring_program, mixed value, string program)` | Replacing another object's named global. The requested source selector may be nil; declaration lookup and type checks follow authorization. Self-writes do not ask. | Deny. |
| `valid_recompile(object prototype, object caller, string program)` | In-place upgrades of a prototype and its current clones, including system prototypes; checked when requested and before compilation, under old system policy. Also permits other objects to query a job's status using the querying caller, program and command giver; a missing target is 0. The original requester reads its own status directly. | Deny. |
| `valid_shutdown(object caller, string program)` | The LPC shutdown efun, not the host's Ctrl-C or termination signal. | Deny. |
| `query_allow_shadow(object target)` | Shadowing `target`; `previous_object()` is the would-be shadow. The driver rechecks liveness and structural restrictions after an allow. A target's `no_shadow` pragma can also forbid it. | Deny. |

### Grammar vocabulary

These optional examples are unnecessary for ulib's `add_action()` commands.
They support the driver's grammar facilities (`parse_command()`,
`parse_sentence()` and `parse_my_rules()`). Install only the vocabulary your game
needs.

In callbacks from `parse_sentence` and `parse_my_rules`, `this_player()` is the
selected command giver, defaulting to the actor. `previous_object()` is the actor,
followed by the object invoking the efun and its external callers.
Stack inspection also retains local and inherited frames, with no function
metadata for the actor's driver entry. This applies to vocabulary, inventory,
permission, action and error callbacks; equal adjacent callers are retained.

For `command(line, actor)`, both native patterns and parser rules run vocabulary
with the actor as `this_player()` and immediate caller, just like permissions
and actions. The surrounding player is restored on return or exception;
selecting an actor does not replace the task's interactive provenance. A direct
`parse_command()` call instead keeps the entering player and uses the object
invoking that efun as its immediate caller.

| Apply and arguments | Result and omission | Optional example |
|---|---|---|
| `parse_command_id_list()` | String array of shared singular names; absent means none. | `({ "thing" })` |
| `parse_command_plural_id_list()` | String array of shared plural names; absent means none. | `({ "things" })` |
| `parse_command_adjectiv_id_list()` | String array of shared adjectives; the spelling **adjectiv** is part of the API. | Empty array. |
| `parse_command_prepos_list()` | String array of prepositions, including multiword phrases if needed; absent means none. | `in`, `on`, `to`, `from`, `with`. |
| `parse_command_all_word()` | String meaning “all”; absent means no such word. | `"all"` |
| `parse_command_numeral(string word)` | Positive count, negative ordinal, or `0` for unrecognised; absence recognises nothing. | `one`, `two`, `first`, `second`. |
| `parse_command_pluralize(string *singulars)` | Corresponding plural at each array index, for an object whose plural hook supplies no array. Missing means no derived plurals. | Append `s`; deliberately incomplete English. |
| `parse_command_users()` | Extra living candidates for `LIV`/`LVS` slots, beyond local scope. Missing adds none. | Named guests from `users()`. |
| `parser_error_message(int kind, object ob, mixed arg, int flag)` | Called only after all matching parser phrases fail, with the most resolved object slots first until it returns a string; ties keep longest phrase first and registration order within each. Its query context identifies the failed phrase. Return failure text, or `0`/`nil` to try the next retained failure. Omission supplies no message; dispatch then uses its ordinary unhandled-command fallback. `kind` selects the failure and the meaning of `arg`; `ob` is populated only for a handler refusal. | Preserve refusal text, otherwise a generic message. |

Valid parser failure kinds are `2` non-living, `3` inaccessible, `4` ambiguous
(`arg` is the candidate array), `5` ordinal out of range (`arg` is the count),
`6` a handler's refusal (`arg` is its reason), `7` an unknown phrase and `8`
an invalid plural selection.

## Object applies

These belong on the affected game object. Live examples are in
[std/room.c](../std/room.c), [room/lounge.c](../room/lounge.c) and
[obj/player.c](../obj/player.c); optional vocabulary and container hooks are in
[examples/object.c](../examples/object.c).

| Apply and arguments | Result and behaviour | Example |
|---|---|---|
| `create(...)` | Initialises an object after all global initialisers; its return value is ignored. Each object chooses its parameters: `clone_object` forwards extra arguments, while prototype loads supply none. Omitted parameters use defaults or `0`; excess arguments require `...` and `argv` to use them. Call `::create(...)` yourself when an inherited constructor is needed. | Live: room descriptions. Optional: box material argument. |
| `init()` | Offers actions when a living enters an object's presence. `this_player()` is the living receiving them; `previous_object()` is that living too. | Live: register room actions. |
| `receive_snoop(string text, object target, string direction)` | Asynchronous decoded input or accepted output; hidden input and protocol operations are excluded. Fresh driver task with nil player/interactive and no previous object. Failure or overflow retires observation. | Disabled: explicitly forward text to the observer’s own socket; the live master denies snooping. |
| `clean_up(int references)` | Idle cleanup hook. Nonzero asks for later checks; `0` stops checks and **does not destruct the object**. Clones receive `0`; a prototype receives `1 + its clone count`, not a general reference count. | Live: keep occupied rooms or prototypes with clones, explicitly destruct other idle rooms. |
| `id(string phrase)` | Match a name; the grammar parser uses this only when `parse_command_id_list()` is absent. This fallback must handle any adjectives/plurals itself; the master's shared ids do not apply. | Live: a guest's name. |
| `parse_command_id_list()` | String array of this object's singular names. Defining it disables `id()` fallback even when it returns no array. | Optional: `box`. |
| `parse_command_plural_id_list()` | String array of plural names; a missing/non-array result asks the master's `parse_command_pluralize()`. | Optional: `boxes`. |
| `parse_command_adjectiv_id_list()` | String array of adjectives for this object. | Optional: `wooden`, `small`. |
| `inventory_visible()` | Nonzero exposes contents in the default walk or a supplied child array; it does not control the object's own name. Missing defaults to true. | Optional: an open box. |
| `inventory_accessible()` | Nonzero lets the parser reach visible contents. Missing defaults to true; `:v` slots waive reachability but still require permission. | Optional: an open box. |
| `parser_handlers` | Four dynamic families described below; with actor, noun-candidate and verb-owner lookup. | [examples/verb.c](../examples/verb.c) |

Inventory hooks run before noun permissions, inside the current transaction,
with the selected command giver as `this_player()`, the actor as
`previous_object()`, and `origin()` set to `"driver"`. Exceptions propagate; refusals do not undo effects. Explicit scope
structure is snapshotted before callbacks, and each rule walk reuses a parent's
hook results. Later rules and parses ask afresh. A parent destructed by a hook
cannot expose children.

`parse_sentence` accepts a hierarchy such as `({ room, ({ box, ({ item }) }) })`:
each child array belongs to its immediately preceding live object. Hidden
subtrees are excluded; visible children inherit accessibility down the path.
Other entries are skipped but break the parent association. Orphan/consecutive
arrays and nesting beyond 20 are errors. Duplicate objects keep their first
visible position and are reachable through any reachable visible occurrence.
A flat explicit list, `({ item })`, bypasses inventory hooks and marks the object
reachable. The default walk exempts the actor and immediate room's direct
inventories from those roots' hooks.

### Grammar handler families

For `parse_add_rule("give", "OBJ to LIV")`, the example names are:

| Family | Target order | Specific hook |
|---|---|---|
| `can_` | Actor, then rule owner | `can_give_obj_to_liv` |
| `direct_` | Candidate for the first object slot, then rule owner | `direct_give_obj_to_liv` |
| `indirect_` | Candidate for the second object slot, then rule owner | `indirect_give_obj_to_liv` |
| `do_` | Rule owner only | `do_give_obj_to_liv` |

Each target tries four tiers: specific, literal-word, generic-shape, and
generic-rule. For the action these are `do_give_obj_to_liv`,
`do_give_obj_word_liv`, `do_verb_obj_word_liv`, and `do_verb_rule`.
All tiers on one target precede the next target. Only absence triggers fallback:
an actor's can handler supplies the answer instead of the owner's check, and a
candidate's generic permission takes precedence over the owner's specific one.

The specific hook takes
`(object item, object recipient, string item_words, string recipient_words)`.
The literal-word tier inserts `string prep` between item and recipient, holding
`"to"`. The generic-shape tier adds the base verb before those semantic values
and the typed verb before the two source phrases. The generic-rule tier receives
`(base_verb, rule_text, item, recipient, typed_verb, item_words, recipient_words)`.
It omits literal arguments. A synonym preserves the base verb and changes the
typed-verb argument. The rule text retains its original whitespace and any `:v` modifiers.

The example also registers `hand over` as a synonym of `give`. Its handlers stay
named for `give`; generic base arguments remain `"give"`. For input
`"hand\t over sword to bob"`, `query_verb()` returns `"hand over"`, while
`query_verb(1)` and generic
typed-verb arguments preserve `"hand\t over"`. The whole command line remains
available through `query_command()`. Nested parses restore the enclosing context.
Scope and vocabulary hooks see the matched phrase, and each deferred error hook
sees its own failure's phrase. Ordinary unhandled-command fallback sees the first
word again.

Parser verb names normalize Unicode whitespace to single spaces and reject
empty names; case and punctuation remain exact. Matching requires whole words.
Aliases copy current registrations, with exact rule-text filters; later base
additions do not extend them. Removing the base removes its aliases too.
A multiword base can use generic methods; spaces are not converted to underscores
in specific names. The single-word base plus multiword alias avoids that issue.

Every capture, including `WRD` and `STR`, contributes a source string after the
semantic values (and typed verb for generic tiers). The middle two tiers also
insert literal semantic arguments in rule order. `WRD` is named `wrd`, while a
literal becomes `word` in those tiers. Empty rules omit the slug; their
generic forms receive `(base, typed)` or `(base, "", typed)`. Duplicate
generated names use the earliest layout.

The text example registers `cast` with `STR at STR`. Its `can_cast_str_at_str`
and `do_cast_str_at_str` receive `(spell, target, spell_words, target_words)`:
four strings, with each capture repeated as source text. Multiple `STR` tokens
are allowed, including adjacent captures; each requires one or more words.
Earlier captures take longer spans first. The example's permission accepts only
`"spark"`, so `cast spark at dusk at tower` first refuses `"spark at dusk"`,
then acts with `"spark"` and `"dusk at tower"`. Text-only rules call no object
permissions. With mixed text/object rules, all captures keep their rule positions
and the first/second object slots still select direct/indirect targets.

At most 32 grammatical parses are tried per rule/input; a refusal permits the
next split within that cap, keeping earlier effects. The first action wins;
a runtime exception stops the search. A literal separator inside a capture is
ordinary text. The driver's `parse_add_rule` reference has the complete
split-selection contract.

`OBJ:v`, `OBS:v`, `LIV:v`, and `LVS:v` permit visible but unreachable objects.
Unmodified slots require reachability. Each capture keeps its own policy, so
`OBJ at OBJ:v` still requires the first object to be reachable. The suffix changes
neither names nor argument layouts, and never bypasses living or permission
checks. Other modifiers, including combined/repeated suffixes, are unsupported.

`can_` sees unresolved object slots as `0` (`nil` in distinct mode). For this
two-object rule, `direct_` and `indirect_` receive both individual objects.
The parser checks pairs in scope order, asking direct before indirect and
skipping indirect if direct refuses. Both nouns resolve before these calls;
permission checks can therefore disambiguate either object. Selected pairs
are checked once more before the action. Destructed objects are skipped.
Return `1` to permit, `0` to reject, or a reason string to reject with an
explanation. A leading `#` makes the reason soft, so a plain reason from
another candidate takes precedence. Without `can_`, permission defaults to
yes; without an object permission on both the candidate and owner, that
candidate is excluded. Owner permission arguments still name the noun objects;
kind-6 diagnostics identify the object that actually returned the reason.

When executing a rule, `do_` runs once after acceptance and its return is ignored.
Without `do_`, the rule is unhandled. The disabled example refuses gifts in
`indirect_`, and its `do_` only prints a teaching message; it is not an item-transfer implementation.
The hooks need to be distributed to their stated owners, named objects added,
and the verb object loaded before this grammar can be used. Registered grammar
rules are tried after the living's own actions decline; `parse_sentence()` can
also invoke the parser explicitly. Longest matching verb phrases run first,
with registration order within each phrase. If a phrase's rules fail, shorter
matching phrases remain eligible. The first rule to reach `do_` wins, even if
an earlier rule supplied a refusal string. `parser_error_message()` runs only
when every phrase fails, reporting failures with the most resolved object slots
first and preserving trial order for ties. Refusing a
rule does not undo its handler effects, and runtime exceptions propagate immediately.

`parse_my_rules(actor, line)` uses only the calling object's registrations and
returns generic action arguments after permissions accept, without looking up
or calling any `do_`. The `match_text` helper in `examples/verb.c` demonstrates
this for text captures. The result is `({ base, rule, captures..., typed_verb,
source_strings... })`; failures use the same status codes or error messages as
`parse_sentence`, without delivering them. A third argument of nonzero executes
normally and returns 1. The explicit living actor supplies scope, actor
permission lookup and immediate caller identity. A fourth argument selects the
command giver returned by callback `this_player()`; omission uses the actor and
explicit nil clears it. The example's `match_text(actor, line, command_giver)`
keeps the owner, actor and command giver separate.
`parse_sentence(line, 0, scope, nicknames, actor, command_giver)` accepts the same
separation, with an omitted actor defaulting to the entering player. The actor
must be living; the giver may be any live object or null. A destructed giver
becomes null, and wrong non-null types are errors. Selection does not replace
the task's interactive provenance. Callback-local player changes do not affect
later callbacks, and nested calls restore the surrounding player and command
frame. The invoking LPC caller chain remains behind the actor in every callback.
Returning arguments still runs permissions and retains their effects.

Two-object rules with one `OBS`/`LVS` slot use the same individual-object
permission arguments in both orientations. Only `do_` receives an array for
the plural slot: accepted objects followed by plain refusal strings for that
partner. A late refusal removes that member while preserving the others.
Ordinals pin a noun match satisfying its slot's access requirement before permissions; ordinary counts select
from compatible matches afterward. A late rejection does not substitute a new
object or restart selection.

Single-object rules and rules with two plural slots retain aggregate filtering
and rechecks: each initial check fills its own slot with one object, earlier
slots hold selected values, and final rechecks receive arrays for plural slots.
Their action arrays also include plain initial refusal strings. Use appropriately
typed arguments and inspect entries before treating them as objects; the simple
`OBJ to LIV` example avoids array arguments.

## Living applies

Both live in [obj/player.c](../obj/player.c). `catch_tell()` can also be useful
on non-living objects that receive messages.

| Apply and arguments | Result and behaviour | Example |
|---|---|---|
| `catch_tell(string message)` | Receives text sent to the object. Without it, delivery falls back to a connection or the debug log. Its return is ignored. | Live: use `write_socket()` to avoid recursion. |
| `process_input(string line)` | A string replaces the line; `0` continues with the original; other non-string values consume it. Missing also continues. | Live: expand `'text` to `say text`. |

## Special applies

These belong on a connection's body. Live examples are in
[obj/player.c](../obj/player.c); optional protocol hooks are in
[examples/player.c](../examples/player.c).

| Apply and arguments | Result and behaviour | Example |
|---|---|---|
| `logon(string remote_ip_addr, int remote_port)` | Called after attaching a body; return nonzero to keep the connection, `0` to disconnect. Required for successful login. Output and input registrations are delivered in call order after commit; typed replies wait until this delivery finishes. Enabling commands is the mudlib's responsibility. | Live: prompt for a name with `input_to()`. |
| `write_prompt()` | Return prompt text, optionally writing output too. Skipped while `input_to()` or `get_char()` waits. Without the hook there is no automatic prompt or prompt marker. | Live: `alice> ` after naming. |
| `net_dead()` | Unexpected disconnect, with the connection already unbound and `interactive()` false. Not called for `quit`/destruction, `exec`, failed `logon` or orderly shutdown. Missing leaves cleanup to the mudlib. | Live: announce departure and destruct the body. |
| `gmcp(string package, string payload)` | Negotiated GMCP from the client; `payload` is raw JSON text and may arrive before login finishes. Its return is ignored. | Optional: empty hook with a validation reminder. |
| `window_size(int cols, int rows)` | Client NAWS dimensions, including known dimensions on attachment. `query_connection()` already holds them. Its return is ignored. | Optional: empty hook for future layout changes. |
