# lpc-rs

An LPC compiler, virtual machine and gamedriver for multi-user dungeons (MUDs),
written in Rust.

LPC is a language for building persistent, multiplayer text worlds. You write
rooms, creatures, player commands and game rules in LPC; the driver compiles and
runs those objects, manages their lifetimes and connects them to players. The
collection of LPC code that defines a game is its **mudlib**.

lpc-rs combines that model with a concurrent, transactional runtime. Player
commands and scheduled callbacks can execute in parallel, while the runtime
commits their game-state changes atomically and retries conflicts.

The project is under active development. It has a working compiler and networked
server, but its LPC dialect and interfaces are still evolving. Existing mudlibs
may need changes; the language and efun documentation describe the behaviour
implemented here.

## What's here

- **LPC compilation and execution:** preprocessing, type checking, multiple
  inheritance, closures, function pointers, arrays, mappings and byte strings,
  with diagnostics tied to source locations.
- **Object lifecycle and world structure:** loading and cloning, environments and
  inventories, scheduled call-outs, garbage collection, object cleanup and
  save/restore support. Objects can be loaded into the running game.
- **Player interaction:** login and input hooks, actions and grammar-based
  command rules, text formatting, terminal colour and Markdown rendering.
- **MUD networking:** Telnet, GMCP, MXP, MSSP and terminal capability negotiation;
  optional TLS connections and a certificate helper for Certbot-based renewal.
- **Development tools:** a command-line compiler/runner and a language server
  with editor diagnostics, built-in function completion and hover documentation.

## Build and install

You need a current stable Rust toolchain and your platform's native build tools
(a C compiler, linker and `make`). CI builds and tests on Linux. The commands
below use a POSIX shell.

Clone the repository:

```sh
git clone https://github.com/xxx/lpc-rs.git
cd lpc-rs
```

Install the gamedriver and command-line compiler from the checkout:

```sh
cargo install --locked --path lpc-rs-driver
cargo install --locked --path lpc-rs-lpcc
```

`cargo install` builds optimised release binaries by default and installs them
in Cargo's binary directory, normally `~/.cargo/bin`. Ensure that directory is
on your `PATH`. The examples below use these installed commands.

Install the editor and certificate tools if you need them:

```sh
cargo install --locked --path lpc-rs-lsp
cargo install --locked --path lpc-rs-cert
```

The certificate helper requires a separate Certbot installation for certificate
issuance and renewal; see [secure connections](doc/secure-connections.md).

To build the whole workspace without installing it:

```sh
cargo build --release --workspace --locked
```

The executables are in `target/release/` and can be run directly, for example
`./target/release/lpc-rs-driver --env /path/to/mud.env`. Installed binaries can
run from any directory; use absolute paths in your configuration when running
outside the checkout. Your mudlib and configuration are managed separately
from the executables.

## Run your first LPC object

From the repository root, create a directory for your LPC code:

```sh
mkdir -p lib
```

Create `lib/hello.c`:

```c
void create() {
    dump("hello, world!\n");
}
```

Compile and run it from the repository root:

```sh
LPC_LIB_DIR=./lib lpc-rs-lpcc lib/hello.c
```

You should see:

```text
hello, world!
```

The runner initialises the object and calls its `create()` hook. With no player
attached, `dump()` writes to the debug log, which defaults to standard output.
Change the greeting and run the command again to compile and execute your edit.

Check the same file without running its `create()`:

```sh
LPC_LIB_DIR=./lib lpc-rs-lpcc --check lib/hello.c
```

This should finish successfully without printing the greeting. The runner exits
after initialisation; use the gamedriver for a running world, network connections
and scheduled callbacks.

To inspect the compiled instructions without executing LPC, use `-S` (or
`--emit-asm`):

```sh
LPC_LIB_DIR=./lib lpc-rs-lpcc -S lib/hello.c
```

The assembly listing goes to stdout, so you can redirect it with `> hello.asm`.
Warnings and errors go to stderr. Assembly mode also leaves configured simulated
efuns uninitialised, and cannot be combined with `--check`.

## Connect to a running world

The distribution includes [ulib (microlib)](ulib/README.md), a tiny mudlib for
guest login and chat. Its documented source and apply examples provide a
starting point for writing your own mudlib.

From the repository root, copy it somewhere you can edit and start it:

```sh
cp -R ulib ~/my-mud
cd ~/my-mud
lpc-rs-driver --env driver.env
```

In another terminal, connect with a Telnet client:

```sh
telnet 127.0.0.1 24960
```

Enter a guest name when prompted. You should receive a welcome message,
`The Common Room` and a prompt bearing your name. Connect a second client with
another name and try `say Hello!`, `who` and `emote waves.`. Use `help` for the
command list and `quit` to disconnect. Stop the driver with Ctrl-C in its terminal.

Run from the copied directory so its relative configuration paths resolve.
It listens on loopback by default. Names last for the connection; no accounts
or conversations are saved. `ulib/` ships with the source distribution, and
is copied separately from the installed driver binary. Its
[guide](ulib/README.md) covers configuration and adding commands, and its
[apply reference](ulib/doc/applies.md) maps every driver hook to an implementation
or disabled example.

## Run your own mudlib

Adapt [ulib](ulib/README.md) or supply your own master object for driver policy
and login, along with the
objects that make up the game. Start with the
[master hooks](doc/apply/master), especially
[`connect`](doc/apply/master/connect.md), and the player object's
[`logon`](doc/apply/special/logon.md) hook.

Copy the configuration example:

```sh
cp default.env .env
```

Edit `LIB_DIR` to point to your mudlib and `MASTER_OBJECT` to name its master
object. `LIB_DIR` is a host filesystem path; `MASTER_OBJECT` and other in-game
paths are rooted inside it. Set `BIND_ADDRESS=127.0.0.1` for local development;
the default listens on all interfaces.

Then start the driver with that file:

```sh
lpc-rs-driver --env .env
```

Configuration comes from environment variables, optionally supplemented by an
explicitly selected dotenv file. No `.env` file is loaded automatically.
Process variables override file entries with the same name; `LPC_`-prefixed
settings take precedence over their unprefixed forms. The compiler accepts
`--config .env` for the same purpose.

See [default.env](default.env) for configuration options, including include
paths, simulated efuns, execution limits and logging. The
[secure connections guide](doc/secure-connections.md) covers TLS certificates
and renewal, and the [language server guide](lpc-rs-lsp/README.md) covers editor
setup.

## How execution works

A player command or scheduled callback starts a transaction. It reads a
snapshot of the world and stages its changes privately; calls into other LPC
objects share the same transaction. A single committer validates the reads and
publishes accepted changes atomically. Conflicting attempts are discarded and
run again against a fresh snapshot, within the task's execution allowance.

Player output and other deferred effects are delivered after a successful
commit, so a discarded attempt does not send its messages to players. External
I/O delivery is separate from the state commit and can still fail.

[Transaction Town](doc/transaction-town/README.md) is an interactive explanation
of snapshots, conflicts, retries and committed output. Open
[`doc/transaction-town/index.html`](doc/transaction-town/index.html) locally in
a browser; it works offline without a build or server. For runtime measurements
and troubleshooting, see [transaction diagnostics](doc/transaction-diagnostics.md).

## Documentation

| If you want to… | Start here |
|---|---|
| Run and adapt a tiny mudlib | [ulib guide](ulib/README.md), [add a command](ulib/doc/add-a-command.md), [apply examples](ulib/doc/applies.md) |
| Learn the project's LPC terminology | [Glossary](doc/glossary.md) |
| Understand values and callable functions | [Types](doc/lpc/types.md), [functions and closures](doc/lpc/function_type.md), [references](doc/lpc/references.md), [argument spreading](doc/lpc/argument_spreading.md) |
| Look up a built-in function | [Efun reference](doc/efun) |
| Implement callbacks the driver calls | [Master hooks](doc/apply/master), [object hooks](doc/apply/object), [living hooks](doc/apply/living), [connection hooks](doc/apply/special) |
| Add player commands | [`add_action`](doc/efun/add_action.md), [`add_rule`](doc/efun/add_rule.md), [`parse_sentence`](doc/efun/parse_sentence.md) |
| See how the parsing frontends share one engine | [Parser Town](doc/parser-town/README.md) · open [the interactive tour](doc/parser-town/index.html) locally in a browser |
| Save and restore game data | [`save_object`](doc/efun/save_object.md), [`restore_object`](doc/efun/restore_object.md), [save-file format](doc/save-format.md) |
| Replace the master or simulated efuns at runtime | [`request_system_reload`](doc/efun/request_system_reload.md) |

## Work on the driver

The repository is a Cargo workspace. Its executable crates are:

| Crate / binary | Purpose |
|---|---|
| `lpc-rs-lpcc` | Run an LPC file, check it with `--check`, or print instructions with `-S` |
| `lpc-rs-driver` | Run the game server |
| `lpc-rs-lsp` | Run the language server over stdio |
| `lpc-rs-cert` | Manage TLS certificates, with issuance and renewal via Certbot |

The root `lpc-rs` crate contains the [compiler](src/compiler),
[interpreter](src/interpreter), [transactional runtime](src/interpreter/stm),
[efuns](src/interpreter/efun) and [command system](src/command).
Supporting crates provide core types, bytecode instructions, function metadata,
errors, configuration and Telnet protocol handling.

During development, Cargo can rebuild and run a binary from the checkout:

```sh
cargo run -p lpc-rs-driver -- --env .env
```

This uses the debug profile by default; add `--release` before `--` to run an
optimised build.

Run the workspace checks from the repository root:

```sh
cargo build --workspace
cargo test --workspace
cargo clippy --workspace --all-targets -- -D warnings
cargo fmt --check
RUSTDOCFLAGS="-D warnings" cargo doc --workspace
```

Use `--workspace` for builds, tests and Clippy: the root package alone does not
check the sibling crates. The formatting configuration uses nightly-only
options; stable rustfmt's warnings about those options are expected.

Tests live alongside the Rust code and in [tests](tests); performance workloads
live in [benches](benches) and [examples](examples). See [AGENTS.md](AGENTS.md)
for repository conventions and profiling commands.

Apply changes also update ulib's implementations, examples and source comments;
see [keeping ulib aligned with applies](doc/maintaining-ulib.md) for the checks
and review process.

## Licence

[MIT](LICENSE).
