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

`0` is also the absent value for LPC strings, objects and arrays. Unless an
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
| `error_handler(mapping error)` | Handle a runtime error in a fresh transaction. Keys: `error`, `location`, `object`, `diagnostic`. The driver always logs the diagnostic; `this_interactive()` identifies the affected connection when present. Without this hook the driver also sends its diagnostic to that player. | Live: send a short notice. |
| `warning_handler(mapping warning)` | Handle a compiler warning in the compiling transaction. Keys: `message`, `location`, `file`, `diagnostic`. Omission logs it; implementing this hook makes logging your responsibility. | Live: write the diagnostic to the debug log. |
| `get_mud_stats()` | Return an MSSP mapping. Custom fields override defaults; omit `PLAYERS` to retain the driver's live connection count. Custom values are cached for each connection. | Live: set `NAME` and `FAMILY`. |
| `shutdown()` | Last LPC hook during an orderly stop; client output can still be delivered, but new commands and call-outs do not run. | Live: broadcast a farewell. |

### Policy

These hooks return an integer: nonzero permits, `0` denies. Omission denies
the gated operation. They run in the requesting operation's transaction.
`program` identifies the source defining the code that asked, which can differ
from the caller object's name through inheritance or virtual objects.

| Apply and arguments | What it gates | ulib policy |
|---|---|---|
| `valid_load(string path, string func, object caller, string program)` | Loading source not already resident. `path` is the canonical source filename with `.c`. Calls to resident objects and cloning an existing prototype do not ask again. | `/obj/`, `/room/`, `/std/` only. |
| `valid_inherit(string path, string from)` | Explicit source inheritance, with canonical filenames. Configured auto-inheritance is exempt. | `/std/` only. |
| `valid_read(string path, string func, object caller, string program)` | File reads and compilation includes. An include uses `func == "include"`, `caller == 0`, and the including source as `program`. | Only includes under `/include/`. |
| `valid_write(string path, string func, object caller, string program)` | File mutation, including the virtual source path given to `compile_string()`. | Deny. |
| `valid_destruct(object caller, object target, string program)` | Object destruction, including self-destruction. | Only `caller == target`. |
| `valid_exec(string program, object new_body, object old_body)` | Moving a connection between bodies with `exec()`. Here `program` has no leading slash; the driver's initial attachment after `connect()` does not need this grant. | Deny. |
| `valid_variable_info(object caller, object target, string program)` | Inspecting another object's globals, including private ones; inspecting self does not ask. | Deny. |
| `valid_reload(string target, object caller, string program)` | System restart and in-place recompile requests for `master`, `simul_efun` or `both`; system recompilation also needs `valid_recompile`. | Deny. |
| `valid_recompile(object prototype, object caller, string program)` | In-place upgrades of a prototype and its current clones, including system prototypes; checked when requested and before compilation, under old system policy. | Deny. |
| `valid_shutdown(object caller, string program)` | The LPC shutdown efun, not the host's Ctrl-C or termination signal. | Deny. |
| `query_allow_shadow(object target)` | Shadowing `target`; `previous_object()` is the would-be shadow. A target's `no_shadow` pragma can also forbid it. | Deny. |

### Grammar vocabulary

These optional examples are unnecessary for ulib's `add_action()` commands.
They support the driver's grammar facilities (`parse_command()` and
`parse_sentence()`). Install only the vocabulary your game needs.

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
| `parser_error_message(int kind, object ob, mixed arg, int flag)` | Return parser failure text, or `0` for silence. Omission also supplies no message; dispatch may try other rules and its ordinary unhandled-command fallback. `kind` selects the failure and the meaning of `arg`; `ob` is populated only for a handler refusal. | Preserve refusal text, otherwise a generic message. |

Parser failure kinds are `2` non-living, `3` inaccessible, `4` ambiguous
(`arg` is the candidate array), `5` ordinal out of range (`arg` is the count),
`6` a handler's refusal (`arg` is its reason), `7` an unknown phrase and `8`
an invalid plural selection. Kinds `1` and `9` are unused in this implementation.

## Object applies

These belong on the affected game object. Live examples are in
[std/room.c](../std/room.c), [room/lounge.c](../room/lounge.c) and
[obj/player.c](../obj/player.c); optional vocabulary and container hooks are in
[examples/object.c](../examples/object.c).

| Apply and arguments | Result and behaviour | Example |
|---|---|---|
| `create()` | Initialises a newly created object after its global initialisers; its return value is ignored. Call `::create()` yourself when an inherited initialiser is needed. | Live: room descriptions. |
| `init()` | Offers actions when a living enters an object's presence. `this_player()` is the living receiving them; `previous_object()` is that living too. | Live: register room actions. |
| `clean_up(int references)` | Idle cleanup hook. Nonzero asks for later checks; `0` stops checks and **does not destruct the object**. Clones receive `0`; a prototype receives `1 + its clone count`, not a general reference count. | Live: keep occupied rooms or prototypes with clones, explicitly destruct other idle rooms. |
| `id(string phrase)` | Match a name; the grammar parser uses this only when `parse_command_id_list()` is absent. This fallback must handle any adjectives/plurals itself; the master's shared ids do not apply. | Live: a guest's name. |
| `parse_command_id_list()` | String array of this object's singular names. Defining it disables `id()` fallback even when it returns no array. | Optional: `box`. |
| `parse_command_plural_id_list()` | String array of plural names; a missing/non-array result asks the master's `parse_command_pluralize()`. | Optional: `boxes`. |
| `parse_command_adjectiv_id_list()` | String array of adjectives for this object. | Optional: `wooden`, `small`. |
| `inventory_visible()` | Nonzero lets the grammar parser search the object's contents. Missing defaults to true. | Optional: an open box. |
| `inventory_accessible()` | Nonzero lets the grammar parser reach the object's contents. Missing defaults to true; this is parser reachability, not a security permission. | Optional: an open box. |
| `parser_handlers` | Four dynamic families described below; each belongs to the verb, direct object or indirect object. | [examples/verb.c](../examples/verb.c) |

### Grammar handler families

For `parse_add_rule("give", "OBJ to LIV")`, the example names are:

| Family | Owner | Specific hook | Generic fallback |
|---|---|---|---|
| `can_` | Object registering the verb | `can_give_obj_to_liv` | `can_verb_rule` |
| `direct_` | Candidate for the first object slot | `direct_give_obj_to_liv` | `direct_verb_rule` |
| `indirect_` | Candidate for the second object slot | `indirect_give_obj_to_liv` | `indirect_verb_rule` |
| `do_` | Object registering the verb | `do_give_obj_to_liv` | `do_verb_rule` |

Each specific hook in this example takes
`(object item, object recipient, string item_words, string recipient_words)`.
The generic fallback adds `string verb, string rule` before those arguments.
The specific name wins when present; a synonym uses the original verb's name.
Other rules have different arguments: one per grammar token, then one phrase
string per object slot. The rule's literal `to` contributes no argument.

`can_` sees unresolved object slots as `0`. During candidate filtering,
`direct_` and `indirect_` see their own candidate filled in; other slots may
still be `0`. They are asked again after resolution with all slots filled.
Return `1` to permit, `0` to reject, or a reason string to reject with an
explanation. A leading `#` makes the reason soft, so a plain reason from
another candidate takes precedence. Without `can_`, permission defaults to
yes; without a candidate's `direct_` or `indirect_`, that candidate is excluded.

`do_` runs once after acceptance and its return is ignored. Without `do_`, the
rule is unhandled. The disabled example refuses gifts in `indirect_`, and its
`do_` only prints a teaching message; it is not an item-transfer implementation.
The hooks need to be distributed to their stated owners, named objects added,
and the verb object loaded before this grammar can be used. Registered grammar
rules are tried after the living's own actions decline; `parse_sentence()` can
also invoke the parser explicitly.

For many-object `OBS`/`LVS` slots, candidate filtering passes the candidate as a
bare object in its own slot. Elsewhere, resolved many-object slots are arrays.
In `do_`, those arrays also include plain refusal strings from rejected
candidates. Use appropriately typed arguments and inspect entries before
treating them as objects; the simple `OBJ to LIV` example avoids this case.

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
| `logon(string remote_ip_addr, int remote_port)` | Called after attaching a body; return nonzero to keep the connection, `0` to disconnect. Required for successful login. Enabling commands is the mudlib's responsibility. | Live: prompt for a name with `input_to()`. |
| `write_prompt()` | Return prompt text, optionally writing output too. Skipped while `input_to()` waits. Without the hook there is no automatic prompt or prompt marker. | Live: `alice> ` after naming. |
| `net_dead()` | Unexpected disconnect, with the connection already unbound and `interactive()` false. Not called for `quit`/destruction, `exec`, failed `logon` or orderly shutdown. Missing leaves cleanup to the mudlib. | Live: announce departure and destruct the body. |
| `gmcp(string package, string payload)` | Negotiated GMCP from the client; `payload` is raw JSON text and may arrive before login finishes. Its return is ignored. | Optional: empty hook with a validation reminder. |
| `window_size(int cols, int rows)` | Client NAWS dimensions, including known dimensions on attachment. `query_connection()` already holds them. Its return is ignored. | Optional: empty hook for future layout changes. |
