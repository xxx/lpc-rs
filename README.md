# lpc-rs

An LPC compiler, virtual machine and gamedriver for multi-user dungeons (MUDs).

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

Start with [Learn LPC](doc/lpc/README.md) if you have not written the language
before: run a small program, then build objects and commands in a live game.

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

Install the gamedriver from the checkout:

```sh
cargo install --locked --path lpc-rs-driver
```

`cargo install` builds optimised release binaries by default and installs them
in Cargo's binary directory, normally `~/.cargo/bin`. Ensure that directory is
on your `PATH`. The game examples below use the installed driver.

The installed driver can run from any directory. Your mudlib and configuration
are managed separately from the executable; the next section uses the bundled
ulib to start a game.

## Run a game

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
and login, along with the objects that make up the game. Start with the
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
settings take precedence over their unprefixed forms. Use absolute paths in
your configuration when starting the driver from another directory.

See [default.env](default.env) for configuration options, including include
paths, simulated efuns, execution limits and logging.
The [compiler cache guide](doc/compiler-caches.md) covers retention budgets,
idle expiration and native usage counters.

`NULL_MODE=zero` preserves zero-as-null behavior; `NULL_MODE=distinct` enables
a separate `nil` value. See [null values](doc/lpc/null.md) for defaults, type
checks, and save-file behavior. Examples assume zero mode unless stated otherwise.

For TLS certificate issuance and renewal, install the optional certificate helper
from the repository checkout:

```sh
cargo install --locked --path lpc-rs-cert
```

The helper requires a separate Certbot installation; see the
[secure connections guide](doc/secure-connections.md) for setup.

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
| Write LPC for the first time | [Learn LPC](doc/lpc/README.md): a first program, then objects and player commands |
| Run and adapt a tiny mudlib | [ulib guide](ulib/README.md), [add a command](ulib/doc/add-a-command.md), [apply examples](ulib/doc/applies.md) |
| Compare LPC drivers | [Driver comparison and porting notes](doc/driver-comparison.md) |
| Port Lima | [Lima mudlib guide](doc/mudlibs/lima.md) and [port history](doc/mudlibs/lima-port-history.md) |
| Learn the project's LPC terminology | [Glossary](doc/glossary.md) |
| Look up compiler pragmas and their scope | [Pragma reference](doc/lpc/pragmas.md) |
| Understand values and callable functions | [Types](doc/lpc/types.md), [functions and closures](doc/lpc/function_type.md), [references](doc/lpc/references.md), [argument spreading](doc/lpc/argument_spreading.md) |
| Look up a built-in function | [Efun reference](doc/efun) |
| Inspect or replace named globals | [`variable_info`](doc/efun/variable_info.md), [`store_variable`](doc/efun/store_variable.md) |
| Implement callbacks the driver calls | [Master hooks](doc/apply/master), [object hooks](doc/apply/object), [living hooks](doc/apply/living), [connection hooks](doc/apply/special) |
| Add player commands | [`add_action`](doc/efun/add_action.md), [`add_rule`](doc/efun/add_rule.md), [`parse_sentence`](doc/efun/parse_sentence.md) |
| See how the parsing frontends share one engine | [Parser Town](doc/parser-town/README.md) · open [the interactive tour](doc/parser-town/index.html) locally in a browser |
| Explore garbage collection, roots, and cycles | [Collector Town](doc/collector-town/README.md) · open [the interactive tour](doc/collector-town/index.html) locally in a browser |
| Save and restore game data | [`save_object`](doc/efun/save_object.md), [`restore_object`](doc/efun/restore_object.md), [`save_value`](doc/efun/save_value.md), [`restore_value`](doc/efun/restore_value.md), [save format](doc/save-format.md) |
| Upgrade live objects, clones, or the master and simulated efuns | [`request_object_recompile`](doc/efun/request_object_recompile.md) |

## Develop and debug LPC

The command-line compiler helps you check LPC files, inspect their compiled
instructions and run small examples while developing a mudlib. The language
server adds editor diagnostics, completion and hover documentation. Install
either tool as needed from the repository checkout:

```sh
cargo install --locked --path lpc-rs-lpcc
cargo install --locked --path lpc-rs-lsp
```

The [compiler guide](lpc-rs-lpcc/README.md) covers running a single object,
checking files with `--check` and inspecting instructions with `-S`. Use
`--config /path/to/driver.env` to load your mudlib's configuration. See the
[language server guide](lpc-rs-lsp/README.md) for editor setup.

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

To build the whole workspace without installing it:

```sh
cargo build --release --workspace --locked
```

The executables are in `target/release/` and can be run directly, for example
`./target/release/lpc-rs-driver --env /path/to/mud.env`.

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

### Mutation testing

The reviewed scope includes:

- [Core types](lpc-rs-core/src/lib.rs): registers, types and unions, paths and
  confinement, flags, nominal classes, call metadata, null modes and escapes.
- [Function support](lpc-rs-function-support/src/lib.rs): prototypes, constants,
  symbol scope, instruction metadata and register renaming.
- [Byte helpers](lpc-rs-utils/src/lpc_bytes.rs): literal encoding and capped
  concatenation.
- [String helpers](lpc-rs-utils/src/string.rs): repetition limits, concatenation
  and closure argument numbers.
- [`LpcString`](lpc-rs-utils/src/lpc_string.rs): static/dynamic contents, Unicode
  lengths, comparison/hash consistency, borrowed views and concatenation.
- [Source decoding](lpc-rs-utils/src/lib.rs): UTF-8/Latin-1 contents and trailing
  newline preservation.
- [`DebugLog`](lpc-rs-utils/src/debug_log.rs): queued writes, flush barriers and
  runtime-safe shutdown/drop behavior.
- [Configuration](lpc-rs-utils/src/config.rs): defaults, environment precedence,
  validation, listener combinations and log destinations.
- [TLS helpers](lpc-rs-utils/src/tls.rs): certificate identity/lifetime checks,
  settings and private-material path validation.
- [Assembly addresses](lpc-rs-asm/src/address.rs): program-counter conversions,
  equality, offsets and hexadecimal display.
- [Instructions](lpc-rs-asm/src/instruction.rs): operand mapping, branch targets,
  comparison semantics, range directions and opcode metadata.
- [Diagnostic spans](lpc-rs-errors/src/span.rs): packed locations, warning policy,
  source extraction and file-position rendering.
- [Error frames](lpc-rs-errors/src/error_frames.rs): rendered byte accounting,
  trace merging, omissions and snapshot budgets.
- [Source maps](lpc-rs-errors/src/source_map.rs): registry identity, version reuse,
  hash-collision fallback, retained storage and diagnostic lookup.
  [Line indexes](lpc-rs-errors/src/source_map/source_file.rs) cover compact/wide
  offsets and parity with codespan's reference locations.
- [Diagnostics](lpc-rs-errors/src/lib.rs): error kinds, attached errors/frames,
  catch rendering and terminal output.
- [`abs()`](src/interpreter/efun/abs.rs): numeric results, integer overflow,
  float zero and invalid input.
- [`allocate()`](src/interpreter/efun/allocate.rs): count/layout validation,
  platform-width conversion and shared initial values.
- [Integer values](src/interpreter/lpc_int.rs): full-width conversions, truth
  values, comparison with integers and decimal display.
- [Float values](src/interpreter/lpc_float.rs): comparison with primitive floats
  and numeric display.
- [Mapping values](src/interpreter/lpc_mapping.rs): display/debug contents and
  separators, entry comparisons and owned iteration order.
- [Array values](src/interpreter/lpc_array.rs): comparison lengths and contents,
  construction, capacity, formatting, mutable access, iteration and concatenation.
- [Telnet protocol](lpc-rs-telnet/src/lib.rs): option-byte conversions, input
  framing, split feeds, malformed subnegotiations, outbound escaping and newline
  handling, reservation growth, option negotiation states, terminal reports,
  input batching, mode changes and session events.
- [Certificate helper](lpc-rs-cert/src/main.rs): hostname/configuration validation,
  issuance and renewal arguments, atomic publication, locking and offline status.
- [Driver entry point](lpc-rs-driver/src/main.rs): installed-command startup,
  shutdown and process exit status.
- [Compiler CLI](lpc-rs-lpcc/src/main.rs): execution/check/assembly modes,
  simulated-efun targets, diagnostics and output flushing.
- [Language server](lpc-rs-lsp/src/main.rs): stdio startup, initialization,
  capabilities, shutdown, overlays, file URIs, UTF-16 positions and diagnostic
  metadata, completion/hover metadata, full-sync edit validation, cancellation
  generations and lifecycle behavior.
- [Additional efun modules](src/interpreter/efun/mod.rs): `add_rule`,
  `all_environment`, `all_inventory`, `call_inherited`, `case`, `clone_object`,
  `commands`, `compile_string`, `copy_class`, `ctime`, `deep_inventory`,
  `destruct`, `disable_commands`, `disassemble`, `enable_commands`,
  `environment`, `evaluate`, `exec`, `explode`, `file_size`, `filter`,
  `filter_map`, `find_object`, `function_name`, `function_object`,
  `function_signature`, `get_char`, `get_dir`, `has_key`, `hash_string`,
  `implode`, `inherit_list`, `input_to`, `interactive`, `json_decode`,
  `json_encode`, `keys_values`, `living`, `load_object`, `m_delete`, `mkdir`,
  `move_object`, `object_clones`, `object_memory_info`, `object_time`,
  `objects`, `origin`, `papplyv`, `query_call_out`, `query_connection`,
  `query_ip_number`, `remove_action`, `remove_call_out`, `remove_rule`,
  `restore_value`, `rm`, `rmdir`, `save_map`, `save_value`, `send_gmcp`,
  `send_mxp`, `shutdown`, `sizeof`, `source_cache_stats`, `strindex`,
  `syntax_tree`, `tell_object`, `this_interactive`, `throw`, `time`,
  `transaction_stats`, `type_of`, `uptime`, `users`.

[cargo-mutants](https://mutants.rs/) changes the code and checks whether the tests
detect each change. The file allowlist in
[`.cargo/mutants.toml`](.cargo/mutants.toml) keeps the workload small.

Install the version used by CI, then run from the repository root:

```sh
cargo install --locked cargo-mutants --version 27.1.0
cargo install --locked cargo-nextest
cargo mutants --workspace --list
CARGO_TARGET_DIR="$PWD/target" cargo mutants --workspace --jobs 1 --output /tmp/lpc-rs-mutants
```

The run first checks the unmodified tests, then tests each mutation in a temporary
source copy. Mutation tests use nextest for process isolation, as normal CI does;
doctests remain part of normal workspace validation. `CARGO_TARGET_DIR` keeps build
output in the workspace's shared `target/`. The allowlist limits which files are
mutated, even with `--workspace`; only the mutated crate's tests run. Continue
using the workspace checks above for normal validation. Reports and per-mutation
logs are written under
`/tmp/lpc-rs-mutants/mutants.out/`.

Review `missed.txt` for changes the tests did not detect, and add tests for the
missing observable behaviour. Check `timeout.txt` and `unviable.txt` separately:
a timeout or compilation failure does not establish that an assertion caught the
change. Reviewed exclusions have specific regexes and explanations:
`<` and `<=` assign the same value at equality in `RegisterCounter::set`; OR and
XOR set the same warning flag after `Span::file_id` clears that bit.
Telnet framing permits different linear reservation hints while preserving wire
bytes; a growth check catches the quadratic reservation mutation.

The [Mutation testing workflow](.github/workflows/mutation.yml) uses `--in-diff`
on pull requests and pushes to `master`. Pull requests compare the checked-out
merge commit with the PR's base commit; pushes compare with the commit before the
push. Manual runs compare with `base_ref`, which defaults to `HEAD^`.

CI counts mutations on changed production lines and splits them across at most
four matrix jobs, skipping the test jobs when the diff selects no mutations.
Every shard uses the same diff and runs only its share of mutations, with one
mutation at a time, with incremental compilation enabled to reuse build artifacts.
Each shard has a 10-minute job limit and a 30-second test timeout; preparation has
a five-minute limit. Hitting a limit fails the run. Reports are uploaded
separately for each shard even when mutations survive. The workflow is independent
of the required `ci` check.

Diff mode selects mutations in production code, so changes confined to tests or
configuration can select none. Use the local command above to review the whole
allowlist after changing tests or expanding coverage.

Expand the allowlist one module at a time, reviewing survivors and measuring run
time before adding more. Use `--test-workspace=true` when downstream tests are
needed, or select all relevant crates explicitly with repeated `--test-package`
options. Keep exclusions specific to reviewed equivalent mutations.

## Licence

[MIT](LICENSE).
