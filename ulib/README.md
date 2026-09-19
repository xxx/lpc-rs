# ulib — microlib

A tiny LPC mudlib for lpc-rs: choose a guest name, enter one room and chat.
It is a runnable example and a starting point for your own mudlib.

Names last until disconnect, and a name cannot be used by two connected guests.
There are no passwords, saved accounts, transcripts, administrator commands or
simulated efuns. Restarting starts an empty world. The whole mudlib, including
this guide and its MIT licence, lives in this directory.

## Start your first chat world

You need the `lpc-rs-driver` executable and a Telnet or MUD client. From an
lpc-rs source checkout, install the driver if you have not already done so:

```sh
cargo install --locked --path lpc-rs-driver
```

Ensure Cargo's binary directory (normally `~/.cargo/bin`) is on your `PATH`.
The `ulib/` directory ships with the source distribution; installing the binary
alone does not copy it. Copy it from the checkout to a new location:

```sh
cp -R ulib ~/my-mud
cd ~/my-mud
lpc-rs-driver --env driver.env
```

Run the driver **from the copied directory**: `LPC_LIB_DIR=.` in
[driver.env](driver.env) makes it the root of the game. No file outside this
directory is needed once the driver is installed. The server should report
that it is listening on `127.0.0.1:24960`.

In another terminal, connect:

```sh
telnet 127.0.0.1 24960
```

At `Name:`, enter `alice`. Names contain 3–16 ASCII letters; case does not
distinguish names, and `quit` is reserved for disconnecting. You should see a
welcome, **The Common Room**, its description and an `alice> ` prompt.

Open another connection and choose `bob`. On Alice's connection, type:

```text
say Hello, Bob!
```

Alice sees `You say: Hello, Bob!`; Bob sees `Alice says: Hello, Bob!`.
Try `who` on either connection to see `Online: Alice, Bob.`.
Type `quit` to disconnect, or stop the server with Ctrl-C in its terminal.
Connected guests receive a farewell during an orderly shutdown.

## Commands

| Command | Result |
|---|---|
| `help` | List the commands. |
| `look` | Read the room description. |
| `who` | List named, connected guests, alphabetically. |
| `say <text>` | Speak to everyone in the room. `'Hello` is shorthand for `say Hello`. |
| `emote <text>` | Send an action, such as `Alice waves.` for `emote waves.` |
| `quit` | Disconnect and release your name; also works at the name prompt. |

Chat accepts non-blank text up to 400 characters. Control characters and Unicode
line separators are refused; text is sent literally, without evaluating LPC,
colour tokens or markup. Guests choosing a name do not appear in `who` or
receive room chat. An idle connection is closed after 15 minutes.

## Configure and extend

| Task | Where to start |
|---|---|
| Change the game name or chat limit | [include/ulib.h](include/ulib.h) |
| Change the room description | [room/lounge.c](room/lounge.c) |
| Add your first command | [Add a command](doc/add-a-command.md) |
| Understand login, objects and transactions | [How ulib works](doc/how-it-works.md) |
| Look up a driver callback or its example | [Apply reference](doc/applies.md) |
| Change permissions | [secure/master.c](secure/master.c) and the policy explanation in [How ulib works](doc/how-it-works.md#policy) |

Edit [driver.env](driver.env) for lasting configuration changes. A variable in
the process environment overrides the same variable in the file; `LPC_` names
take precedence over unprefixed names. For example, use another local port:

```sh
LPC_PORT=24961 lpc-rs-driver --env driver.env
```

By default, only clients on the same machine can connect. To accept remote
connections, set `LPC_BIND_ADDRESS` to the desired interface (for example,
`0.0.0.0` for all IPv4 interfaces) and allow the port through your firewall.
The supplied configuration uses plaintext Telnet and temporary names, with no
authentication or moderation. Set up those features before using it as a public
community server.

Restart after editing LPC to load the new source. If you also install
`lpc-rs-lpcc` from its directory in the driver checkout, inspect a file without
executing its hooks, from your copied mudlib directory:

```sh
lpc-rs-lpcc -S --config driver.env obj/player.c
```

## Files

```text
driver.env          Configuration for running from this directory
include/ulib.h      Shared names, paths and limits
secure/master.c    Login factory, startup and driver policy
obj/player.c       Guest name, personal commands and connection lifecycle
std/room.c         Room descriptions, chat commands and idle cleanup
room/lounge.c      The only room, inheriting std/room.c
examples/          Disabled examples of optional applies
doc/               Learning guide and complete apply index
LICENSE            MIT licence; copy and adapt this mudlib freely
```

The `examples/` objects are not loaded by the game. Their `#if 0` blocks are
commented examples to copy into the appropriate live object, not another set
of required services.

## Troubleshooting

- **The driver command is missing:** add Cargo's binary directory to `PATH`, or
  use the absolute path to the installed executable.
- **The master or an include cannot be found:** change into the copied directory
  before starting, and check for shell variables overriding `LPC_LIB_DIR` or
  `LPC_SYSTEM_INCLUDE_DIRS`.
- **The address is already in use:** stop the previous server or choose another
  port, then connect your client to that port too.
- **An LPC error appears:** read the diagnostic in the server terminal; both
  server and debug logs go there in the supplied configuration.
- **An edit has no effect:** restart the driver; loaded objects keep their
  compiled code until they are replaced or the server stops.
