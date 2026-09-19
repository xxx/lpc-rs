# How ulib works

ulib has four live LPC source files. The driver provides compilation, network
connections, scheduling and object storage; these files supply the world's
behaviour. An **efun** is a built-in operation LPC calls in the driver. An
**apply** is a named function the driver calls in an LPC object.

## From connection to conversation

At startup the driver loads [the master](../secure/master.c). Its `epilog()`
returns the starting room's path, then `preload()` loads that room. The room's
`create()` initialises its descriptions; `::create()` calls the inherited
room implementation before the specific room sets its own text.

For each connection, the driver calls the master's `connect()`, which clones
[the player](../obj/player.c). Clones share compiled code but have separate
variables. The driver attaches the socket to this clone and calls its `logon()`.
Returning `1` keeps the connection. `input_to(receive_name)` directs the next
line to a name-selection function instead of the command system.

The player validates the name and checks `users()` for a duplicate. `users()`
includes bodies still choosing names, so their `query_name()` returns `0`.
After claiming a name, the player calls `enable_commands()`, registers personal
commands and moves to the lounge.

Movement calls the [room's `init()`](../std/room.c) with the arriving living as
`this_player()`. The room registers `look`, `say` and `emote` for that player.
These actions belong to the room's presence: moving away removes them.
The player registers `help`, `who` and `quit` on itself, so they travel with it.

`process_input()` expands the apostrophe shortcut. An `add_action()` handler
receives the text after the command verb, or `0` when there is no argument.
Returning `1` marks the command handled. After dispatch, `write_prompt()`
provides the next prompt. While `input_to()` is waiting, the driver skips that
hook; `logon()` and the name callback write their own `Name:` prompt.

The room sends chat with `tell_object()` to named, interactive occupants.
The player's `catch_tell()` forwards it through `write_socket()`. Using
`write()` inside `catch_tell()` would recurse through the same hook.

## A name is connection state

The name scan and assignment run in one transaction. If two guests claim the
same name concurrently, the driver retries the conflicting transaction against
the committed state. The retried scan sees the name already taken. No separate
name registry or lock is needed, and output from a discarded attempt is not
sent twice.

`quit` announces departure and destructs the player. An unexpected socket close
calls `net_dead()` after detaching the connection; it performs the same cleanup.
An unfinished login is also removed. Nothing saves the name or authenticates
its next user, so reconnecting with a familiar name does not prove identity.

When the host stops the driver with Ctrl-C, the master's `shutdown()` sends a
farewell before connections close. This is separate from `net_dead()`, which is
not called for orderly server shutdown.

## Policy

The master allows runtime source loads from `/obj/`, `/room/` and `/std/`,
inheritance from `/std/`, and compilation includes from `/include/`. Paths such
as `/obj/player` are inside the mudlib; they are not host filesystem paths.
`LPC_LIB_DIR` determines their filesystem root.

The policy permits self-destruction for logout and empty-room cleanup. It
denies LPC file writes, ordinary file reads, connection transfer, inspection
of another object's globals, system reload, LPC-requested shutdown and shadows.
The master and player also use pragmas to forbid inheritance and shadowing.

These checks are specific driver gates, not a general sandbox for arbitrary
LPC. In particular, `valid_load()` controls loading missing programs, not calls
to already resident objects; `valid_inherit()` controls which code may become
part of another program. All source in the allowed directories is trusted code
installed by the operator. Guests have no command for compiling or evaluating
source. When adding a privileged operation, validate who may invoke that
operation instead of relying only on which directory holds its code.

## Why optional applies are disabled

Most hooks are unnecessary for a one-room chat world. A missing optional hook
has a driver-defined default; a placeholder with the wrong return value can
change that behaviour. For example, `process_input()` returns `0` to continue
dispatch, but `modify_command()` must return the original string to continue.

The [examples](../examples/) keep optional hooks inside `#if 0` so they can be
read and copied deliberately. The [apply reference](applies.md) explains where
each hook belongs, what the driver expects, and what omission means. Grammar
parser examples are separate because this mudlib uses the smaller
`add_action()` command system.

The driver repository's tests compile the live files and the enabled examples,
then boot a copied mudlib and exercise it through real network connections.
