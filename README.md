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
- [Timer queues](src/interpreter/call_outs.rs): deferred scheduling,
  cancellation, repeat retirement, clock limits and remaining time.
- [Callback dispatch](src/interpreter/vm/prioritize_call_out.rs): receiver
  resolution, cancellation, runtime errors and repeat lifecycle.
- [Efun registry](src/interpreter/efun/mod.rs): registration, signatures and dispatch.
- [Additional efun modules](src/interpreter/efun/mod.rs): `add_action`,
  `add_rule`, `all_environment`, `all_inventory`, `bits`, `bytes`,
  `call_inherited`, `call_out`, `callback`, `calling`, `case`, `chars`,
  `clone_object`, `command`, `commands`, `compile_string`, `compose`,
  `conversions`, `copy_class`, `crypt`, `ctime`, `debug`, `deep_copy`,
  `deep_inventory`, `destruct`, `disable_commands`, `disassemble`, `dump`,
  `efun_context`, `enable_commands`, `environment`, `evaluate`, `exec`,
  `explode`, `file_access`, `file_name`, `file_size`, `file_time`, `filter`,
  `filter_map`, `find_object`, `function_description`, `function_exists`,
  `function_info`, `function_name`, `function_object`, `function_signature`,
  `functions`, `get_char`, `get_dir`, `has_key`, `hash_string`, `implode`,
  `inherit_list`, `input_to`, `interactive`, `json_decode`, `json_encode`,
  `keys_values`, `living`, `load_object`, `localtime`, `m_delete`, `map`,
  `math`, `member_array`, `min_max`, `mkdir`, `move_object`, `notify_fail`,
  `object_clones`, `object_memory_info`, `object_time`, `object_update`,
  `objects`, `origin`, `papplyv`, `parse_add_rule`, `parse_add_synonym`,
  `parse_command`, `parse_dump`, `parse_init`, `parse_my_rules`, `parse_remove`,
  `parse_sentence`, `parse_string`, `parser_context`, `present`,
  `previous_object`, `query_call_out`, `query_call_outs`, `query_command`,
  `query_connection`, `query_ip_number`, `query_notify_fail`,
  `query_parser_registry`, `query_parser_rules`, `query_resident_memory`,
  `query_verb`, `random`, `read_file`, `reduce`, `regexp`, `regmatch`,
  `remove_action`, `remove_call_out`, `remove_rule`, `rename`, `replace_string`,
  `request_clean_up`, `restore_map`, `restore_object`, `restore_value`, `rm`,
  `rmdir`, `rusage`, `save_map`, `save_object`, `save_value`, `send_gmcp`,
  `send_mxp`, `set_this_player`, `shadow`, `shutdown`, `sizeof`, `snoop`,
  `sockets`, `sort_array`, `source_cache_stats`, `sscanf`, `store_variable`,
  `strindex`, `syntax_tree`, `tell_object`, `text_encoding`, `this_interactive`,
  `this_object`, `this_player`, `throw`, `time`, `transaction_stats`, `trim`,
  `type_of`, `type_predicates`, `unique_array`, `uptime`, `users`,
  `variable_info`, `wildmatch`, `word_wrap`, `write`, `write_file`,
  `write_socket`.
- [Runtime entry](src/runtime.rs): worker execution and returned values.
- [Memo cache](src/util/memo.rs): bounded reuse and concurrent construction.
- [Simul-efun lookup](src/util/mod.rs): configured resident objects.
- [VM regex cache](src/interpreter/vm/regex_cache.rs): option keys, reuse and eviction.
- [Local symbol scope](src/compiler/semantic/local_scope.rs): insertion, lookup and display.
- [Array syntax nodes](src/compiler/ast/array_node.rs): source spans, rendering and traversal.
- [Spread syntax nodes](src/compiler/ast/spread_node.rs): source spans, rendering and traversal.
- [Assignment syntax nodes](src/compiler/ast/assignment_node.rs): operation spellings, source spans and traversal.
- [Markdown rendering efun](src/interpreter/efun/render_markdown/mod.rs): argument defaults and recipient terminal capabilities.
- [Closure syntax nodes](src/compiler/ast/closure_node.rs): capture scopes, source spans and rendering.
- [Variable initializer nodes](src/compiler/ast/var_init_node.rs): types, source spans, rendering and symbol metadata.
- [Operator syntax nodes](src/compiler/ast/operator_node.rs): arity, closure source ranges and rendering.
- [Variable syntax nodes](src/compiler/ast/var_node.rs): reference identity, source ranges and rendering.
- [Explicit efun call authorization](src/compiler/codegen/efun_override_walker.rs): source attribution, retained warnings and dependency traversal.
- [Predefined preprocessor values](src/compiler/preprocessor/predefined.rs): source names and invocation positions.
- [Unary constant folding](src/compiler/parser/collapse_unary_op.rs): numeric and bitwise operands.
- [Transaction retry admission](src/interpreter/stm/admission.rs): turn order, cancellation and queue cleanup.
- [World snapshots](src/interpreter/stm/snapshot.rs): retained cells, object identity and snapshot metadata.
- [VM login initiation](src/interpreter/vm/initiate_login.rs): connect decisions, logon results and prompt scheduling.
- [VM idle cleanup](src/interpreter/vm/clean_up.rs): scheduling, stale objects and transactional callbacks.
- [VM boot preloads](src/interpreter/vm/preload.rs): ordered files, diagnostics and missing applies.
- [Runtime compile gate](src/interpreter/compile_gate.rs): dependency policy arguments and optional efun override policy.
- [Telnet listener](src/telnet/listener.rs): plaintext acceptance, retry backoff, shutdown and TLS reloads.
- [Benchmark workload support](src/bench_support.rs): workload setup and concurrent apply execution.
- [Compiler permission defaults](src/compiler/compile_gate.rs): dependency-only gates permit explicit efun calls.
- [Captured frame cells](src/interpreter/call_frame/cells.rs): conversion, compact storage, identities and debug output.
- [World-state reclamation](src/interpreter/stm/reclaimer.rs): worker thresholds, owned chunks and disposal completion.
- [VM lifecycle](src/interpreter/vm/mod.rs): boot, subsystem startup, committed exits and connection draining.
- [Call origins](src/interpreter/call_origin.rs): stable LPC spellings for local calls, callbacks and driver dispatch.
- [Callback continuations](src/interpreter/continuation.rs): frame cloning and socket ownership offers.
- [Environment iteration](src/interpreter/process/util.rs): transactional container traversal and iterator termination.
- [Compiler source reads](src/compiler/source_reader.rs): file contents and filesystem kind probes.
- [Command vocabulary](src/command/resolve/vocabulary.rs): default visibility for objects in the actor’s surroundings.
- [Class declarations](src/compiler/ast/class_def_node.rs): AST dispatch through compiler tree walkers.
- [Compiler callees](src/compiler/callee.rs): prototype access for local, simul-efun and efun resolution.
- [Preprocessor expressions](src/compiler/preprocessor/preprocessor_node.rs): readable conditional expressions in compiler diagnostics.
- [Apply diagnostics](src/interpreter/apply/diagnostics.rs): missing hooks, dispatch events and resolved outcomes.
- [Driver callbacks](src/interpreter/task/driver_callback.rs): target liveness, initialization and execution outcomes.
- [Prepared programs](src/compiler/prepared_program.rs): authorized inheritance, depth limits and receiver analysis.
- [Function scopes](src/interpreter/program/function_scope.rs): inherited lookup, namespaces, privacy and shared storage accounting.
- [VM operations](src/interpreter/vm/vm_op.rs): equality of login, call-out, shutdown and update messages.
- [Command numerals](src/command/resolve/numeral.rs): digit counts, the all word and mudlib ordinal resolution.
- [Program images](src/interpreter/process/image.rs): retained global identities, migration aliases and GC roots.
- [Compiler inputs](src/compiler/source.rs): source identities, relative-path bases and safe diagnostic names.
- [Task locations](src/interpreter/task/location.rs): frame reads, writes and integer increments.
- [Task templates](src/interpreter/task/task_template.rs): player context, captured cells and transaction sharing.
- [Switch collection](src/compiler/codegen/case_collector.rs): labels for the current switch without crossing nested functions.
- [Closure captures](src/compiler/codegen/capture_walker.rs): creation-scope expressions and occurrence diagnostics.
- [Control-flow AST nodes](src/compiler/ast/mod.rs): traversal and balanced diagnostic formatting for `do`, `if`, `switch` and `while`.
- [Parsed parameters](src/compiler/parser/parameter.rs): required names, variadic types and collector constraints.
- [Statement AST nodes](src/compiler/ast/mod.rs): traversal and formatting for blocks, catches, declarations, `for`, labeled statements and programs.
- [Literal AST nodes](src/compiler/ast/mod.rs): integer, float, string and nil traversal, source ranges and formatting.
- [Jump AST nodes](src/compiler/ast/mod.rs): `break`, `continue` and `return` traversal, source ranges and formatting.
- [Expression AST nodes](src/compiler/ast/mod.rs): capture, cast, comma and ternary traversal, source ranges and formatting.
- [Additional expression AST nodes](src/compiler/ast/mod.rs): range, mapping, unary and reference traversal, source ranges and formatting.
- [Declaration and label AST nodes](src/compiler/ast/mod.rs): function, inheritance and switch-label traversal, source ranges and formatting.
- [Call AST nodes](src/compiler/ast/mod.rs): call and pointer traversal, source ranges, receiver formatting and namespaces.
- [Compiler AST support](src/compiler/ast/mod.rs): foreach initialization, class expressions, parsed types and binary operations.
- [AST wrappers](src/compiler/ast/mod.rs): generic node conversion, source locations, diagnostics and assignment ownership.
- [Compiler support](src/compiler/mod.rs): diagnostics, inheritance interfaces, conditional compilation and macro-cache reuse and retention limits.
- [Runtime support](src/interpreter/mod.rs): STM merges, pending calls, cleanup scheduling and bounded file reads.
- [Command support](src/command/mod.rs): parser dispatch, shared grammar productions, ordered rule snapshots and transactional command execution.
- [Compiler-runtime integration](src/interpreter/mod.rs): JSON values, staged-file metadata, process compilation and native command matching.
- [Formatting and supplied scopes](src/interpreter/efun/sprintf/mod.rs): format parsing, multiline layout and caller-supplied parser scope traversal.
- [World values and register banks](src/interpreter/bank.rs): typed world payloads, register initialization, storage operations and formatting.
- [Client output buffering](src/telnet/outbox.rs): bounded pending bytes, truncation and recovery after writes.
- [Pending directory views](src/interpreter/file_view/directory.rs): staged membership, recreation and rename dependencies.
- [Command verb frontend](src/command/frontend/add_action.rs): literal and prefix grammars, argument spans, reported verbs and shared grammar caching.
- [Compiled-program storage](src/interpreter/program/code_pool.rs): validated code sharing, weak-cache cleanup and retained path and class memory accounting.
- [Parser nesting](src/compiler/parser/nesting.rs): AST child traversal, depth boundaries and nearest source locations.
- [Command phrases](src/command/resolve/phrase.rs): singular and plural nouns, shared vocabulary and adjective coverage.
- [Actor command rules](src/command/registry/actor.rs): transactional registration, owner-scoped removal, visibility and departure cleanup.
- [Class type resolution](src/compiler/codegen/class_walker.rs): nominal identities, inherited visibility, field layouts and type annotations.
- [Compiler entry points](src/compiler/mod.rs): source-extension fallback, I/O errors, preprocessing, analysis and compiled warning groups.
- [External call handling](src/interpreter/task/handle_call_other.rs): resident and loaded receivers, collection results and argument propagation.
- [Function pointer dispatch](src/interpreter/task/handle_call_fp.rs): resident and loading calls, partial application, visibility and argument checks.
- [Function pointer values](src/interpreter/function_type/function_ptr.rs): partial bindings, call classification, receiver liveness, diagnostic formatting and retained buffers.
- [Runtime apply helpers](src/interpreter/apply.rs): nested caller/player context, master decisions, message delivery and warning reports.
- [Parser protocol handlers](src/command/parser/handlers.rs): handler lookup tiers, typed words, replies and failure selection.
- [String element writes](src/interpreter/task/assign_index.rs): scalar indexing, UTF-8 replacement, bounds and final byte limits.
- [Inheritance compilation](src/compiler/codegen/inheritance_walker.rs): namespaces, loading decisions, inherited warnings and variable restrictions.
- [Include traversal](src/compiler/preprocessor/include.rs): search order, per-directive permission checks, memoized sources, include-once and depth limits.
- [Object initialization APIs](src/interpreter/vm/object_initializers.rs): source and path initialization, simul-efun bootstrap and compile-only behavior.
- [Grammar tokenization](src/command/grammar/tokenizer.rs): longest matches, progress, UTF-8 byte offsets, skipped rules and folded token text.
- [Inherited program linking](src/interpreter/program/linker.rs): global relocation, shared ancestor regions, packed views and inherited visibility.
- [Parser rule frontend](src/command/frontend/parser.rs): verb normalization, capture modifiers, handler slugs, argument layouts and pattern caching.
- [Transaction diagnostics](src/interpreter/stm/diagnostics.rs): conflict provenance, readable cell aliases and nested compilation timing.
- [Committed object updates](src/interpreter/vm/object_update.rs): request identities, status retention, caller context and transaction execution.
- [Command dispatch](src/command/dispatch.rs): pre-hooks, actor context, handler selection and failure messages.
- [LPC parser adapter](src/command/parser/lpc.rs): handler values, per-slot arrays, parse completion, liveness and capture access.
- [Function prototype collection](src/compiler/codegen/function_prototype_walker.rs): defaults, closure positionals, reference metadata and inherited arity.
- [Transactional file content](src/interpreter/file_view.rs): pending reads, byte and character replacement, file kinds and compiler input visibility.
- [Command rule trials](src/command/trial.rs): command frames, handler precedence, parser outcomes and deferred failures.
- [Function application](src/interpreter/task/apply_function.rs): retained results, named and master applies, seeded arguments and runtime error delivery.
- [System export compatibility](src/interpreter/vm/system_recompile.rs): selector names and configuration, public export preservation and independent signature changes.
- [Parsed syntax cache](src/compiler/parser_cache.rs): literal-sensitive keys, pristine syntax reuse, retention charges, expiration counts and per-use authorization.
- [Call stacks](src/interpreter/call_stack.rs): frame capacity, activity tracking, LIFO removal, external callers, program names and stack traces.
- [Function addresses](src/interpreter/function_type/function_address.rs): compatible binding, retained images, structural equality, names and diagnostics after receiver retirement.
- [Program dispatch and lookup](src/interpreter/program.rs): override targets, inherited namespaces, function presence, global aliases and assembly listings.
- [Object vocabulary](src/command/resolve/lpc.rs): live and remote scope entries, explicit plural lists, nickname fallback and nested apply context.
- [Observer delivery](src/interpreter/snooping/delivery.rs): registration identity, budget leases, activation validation, delivery order and retirement.
- [Grammar derivation trees](src/command/grammar/tree.rs): token spans, preserved source spacing, capture order and iterative cleanup of deep trees.
- [Socket callbacks](src/interpreter/sockets/callbacks.rs): runtime lifecycle, callback budgets and provenance, transactional acknowledgement and stale-event cleanup.
- [Presence hooks](src/command/presence.rs): movement and destruction cleanup, living arrival order, reentrant meetings and transaction read dependencies.
- [Macro definitions](src/compiler/preprocessor/define.rs): keyword identifiers, object and function forms, source spelling and redefinition spacing.
- [Observer relationships](src/interpreter/snooping/mod.rs): handler compatibility, endpoint validation, cycle prevention, matching retirement and live registration identity.
- [Compiler scope trees](src/compiler/semantic/scope_tree.rs): lexical and function scope lookup, navigation, mutable symbols and captured-cell layout.
- [Object recompilation](src/interpreter/vm/object_recompile.rs): live initialized prototypes, authorization, clone-group migration, retained globals and status targets.
- [Shadow chains](src/interpreter/process/shadow.rs): transactional links, unshadowed entry, public dispatch, caller boundaries and chain queries.
- [Relational parser attempts](src/command/parser/attempt/relations.rs): living noun selection, pair liveness and permissions, plural partners, rechecks and refusal priority.
- [Connection binding](src/interpreter/vm/binding.rs): registry identity, transactional handovers, stale owners, snoop retirement and conflict diagnostics.
- [Noun resolver](src/command/resolve/mod.rs): scope order, living and remote filters, numerals, prepositions and cached vocabulary.
- [Terminal colour](src/interpreter/efun/terminal_colour/mod.rs): input and wrapping limits, markup expansion, resolver tokens and recipient colour depth.
- [Class instances](src/interpreter/lpc_class.rs): nominal fields, retained views, save validation, graph copying, memory charges and read-only member access.
- [Shared compiler cache](src/compiler/cache.rs): disabled limits, exact byte budgets, LRU eviction, replacement accounting and idle expiration.
- [Range replacement](src/interpreter/task/replace_range.rs): inclusive and reverse bounds, Unicode scalar slices, checked sizes and retained result capacity.
- [Object space](src/interpreter/object_space.rs): bootstrap publication, stable cell identities, compiler caches, root enumeration, clone placement and master identity.
- [Sprintf display units](src/interpreter/efun/sprintf/display.rs): terminal columns, grapheme boundaries, SGR ownership, precision and custom padding.
- [Connection state](src/telnet/connection.rs): callback and address identity, cached hostnames, input lifecycle, observer controls, snapshots and flags.
- [Data instructions](src/interpreter/task/handle_data.rs): staged arrays and mappings, pointer receiver loading and captures, reverse indexes and element access.
- [File-entry overlay](src/interpreter/file_view/entry.rs): provisional entry kinds, failed operations, rename dependencies, symlinks and metadata errors.
- [Memory accounting](src/interpreter/memory.rs): scalar instance slots, shared and interned payloads, captured values, cycles, bounded traversal and overflow.
- [Command scopes](src/command/scope.rs): weak membership, destination neighborhoods, actor and room boundaries, visible contents and callback context.
- [Saved class records](src/interpreter/save_format/class.rs): canonical identities, numeric metadata, field descriptors, retained views and nesting limits.
- [Contention backoff](src/interpreter/stm/backoff.rs): bounded jitter, yield and sleep ladders, commit wakeups and elapsed-time accounting.
- [Grammar model](src/command/grammar/model.rs): builder validation, token and nonterminal identities, diagnostics, case folding and bounded nullable propagation.
- [Command rule registry](src/command/registry.rs): registration identity, flags, raw phrase slices, synonyms, ownership and transactional edits.
- [Committed effects](src/interpreter/stm/effects.rs): deferred delivery, pending file changes, diagnostics and call-out owner identity.
- [Transaction changesets](src/interpreter/stm/changeset.rs): tracked reads, own writes, merge ordering, container allocation reuse, publication and conflict search.
- [Binary literal folding](src/compiler/parser/collapse_binary_op.rs): wrapping integer arithmetic, signed shift counts, zero-divisor deferral, bitwise operations and bounded strings.
- [Function bindings](src/interpreter/program/function.rs): native frame compatibility, inherited visibility, shared code, canonical global projection and equality.
- [Parser attempts](src/command/parser/attempt.rs): selection, permission checks, liveness, handler arguments, rechecks and refusal diagnostics.
- [Native patterns](src/command/frontend/native/mod.rs): advancing UTF-8 capture scanning, diagnostics, dialect caches, grammar construction and ordered captures.
- [Register packing](src/compiler/codegen/register_packing.rs): liveness convergence, temporary reuse, parameter allocation bounds, in-place operands, exception paths and jump boundaries.
- [Scope analysis](src/compiler/codegen/scope_walker.rs): closure storage, reference outputs, callable visibility, foreign calls and diagnostic aggregation.
- [Compilation context](src/compiler/compilation_context.rs): source configuration, pragma boundaries, synthetic names, parent namespaces and function visibility.
- [Peephole optimization](src/compiler/codegen/peephole.rs): copy coalescing, control-flow pruning, address remapping and debug convergence.
- [Runtime error frames](src/interpreter/error_frames.rs): UTF-8 text bounds, frame and variable budgets, argument metadata, transactional object liveness and fresh diagnostic mappings.
- [Terminal colour models](src/interpreter/efun/terminal_text/colour.rs): palette conversion, named tokens, SGR parsing, style resets and hexadecimal validation.
- [Terminal text formatting](src/interpreter/efun/terminal_text/format.rs): cursor progress, expansion limits, style offsets, grapheme wrapping and escape handling.
- [Sprintf rendering](src/interpreter/efun/sprintf/render.rs): numeric signs and precision, nonfinite floats, field padding, wide-character wrapping and argument diagnostics.
- [Markdown rendering](src/interpreter/efun/render_markdown/render.rs): parser invariants, table styling and fit, tight-list spacing, and cumulative expansion and output bounds.
- [Save-file literals](src/interpreter/save_format.rs): signed-zero floats, scalar grammar, string and byte escapes, object references, container depth and debug cursor progress.
- [Compile-time stack configuration](src/compile_time_config.rs): driver worker stacks support compilation at the language nesting limit.
- [Call frames](src/interpreter/call_frame.rs): receiver identity, instruction diagnostics, inherited globals, argument layout, captured defaults, nominal class bindings, transactional addition and integer updates.
- [Opcode counters](src/interpreter/opcode_profile.rs): direct dispatch recording and counter snapshots in ordinary test builds.
- [TCP transport](src/interpreter/sockets/transport.rs): committed delivery, bounded stream input, output draining, worker cleanup, observer disconnection and close-control reasons.
- [Function calls](src/interpreter/task/handle_call.rs): entry history, inherited and constructor calls, argument types, reference and spread binding, register budgets and call-site diagnostics.
- [Compiler tree walking](src/compiler/codegen/tree_walker.rs): child traversal order, lexical scope hooks, error propagation and pass diagnostic policy.
- [Object state](src/interpreter/process/mod.rs): names, transactional images and liveness, movement, living inventories, diagnostic cell names, GC roots and stored environment links.
- [Shared VM state](src/interpreter/vm/global_state.rs): committer lifecycle, queue metrics, GC retry limits, callback initialization and named target refresh.
- [Macro expansion](src/compiler/preprocessor/expand.rs): construction and stringification budgets, recursive hide sets, argument substitution, synthesized escaping and source spans.
- [Language lexer](src/compiler/lexer/mod.rs): cursor progress, directive comment and quote boundaries, numeric magnitudes, keyword spelling and token spans.
- [Socket events](src/interpreter/sockets/events.rs): input credits, ordered delivery, accept admission, callback acknowledgement and drained wakeup rearming.
- [Socket registry](src/interpreter/sockets/mod.rs): handle identity, commit-ordered publication, write admission, ownership transfer and lifecycle cleanup.
- [Task context](src/interpreter/task_context.rs): optional results, caller chains, transactional loading, nested entry, player state and call-out views.
- [DGD grammar frontend](src/command/frontend/dgd.rs): grammar tokenization, rule diagnostics, reachable productions, regex translation, actions and caching.
- [Transactions](src/interpreter/stm/mod.rs): slot and payload views, object placement, deferred effects, context sharing, provenance and compilation accounting.
- [Committer](src/interpreter/stm/committer.rs): protocol replies, snapshot pins, conflict history, publication, GC reachability, reclamation strategy and timing units.
- [Attempt runner](src/interpreter/stm/retry.rs): retry deadlines, admission, telemetry, diagnostics, committer requests and committed readers.
- [Semantic checking pass](src/compiler/codegen/semantic_check_walker.rs): scopes, jump and label boundaries, reference and spread arguments, class validation and diagnostic locations.
- [Semantic type utilities](src/compiler/semantic/semantic_checks.rs): operator compatibility, null-sensitive casts, collection inference, expression types and conversion advice.
- [Telnet connection loop](src/telnet/mod.rs): hostname enrichment, input callbacks, protocol events, prompt suppression, stalled writes, departure cleanup and reconnect diagnostics.
- [Instruction execution](src/interpreter/task/eval_loop.rs): integer batching, fallback and yield budgets, bytecode errors, indexed stores and slice bounds.

[cargo-mutants](https://mutants.rs/) changes the code and checks whether the tests
detect each change. The file allowlist in
[`.cargo/mutants.toml`](.cargo/mutants.toml) limits mutations to reviewed modules.

Install the version used by CI, then run from the repository root:

```sh
cargo install --locked cargo-mutants --version 27.1.0
cargo install --locked cargo-nextest
cargo mutants --workspace --list
CARGO_TARGET_DIR="$PWD/target" NEXTEST_TEST_THREADS=4 cargo mutants --workspace --jobs 1 --output /tmp/lpc-rs-mutants
```

The run first checks the unmodified tests, then tests each mutation in a temporary
source copy. The command runs one mutation at a time and up to four test processes.
Mutation runs use `--max-fail=1:immediate` to terminate other test processes after
a failure. The committer shutdown and GC retry probes run early through
[Nextest priorities](.config/nextest.toml), catching lifecycle failures before
other workers can remain blocked.
Mutation tests use nextest for process isolation, as normal CI does;
doctests remain part of normal workspace validation. `CARGO_TARGET_DIR` keeps build
output in the workspace's shared `target/`. The allowlist limits which files are
mutated, even with `--workspace`; only the mutated crate's tests run. Continue
using the workspace checks above for normal validation. Reports and per-mutation
logs are written under
`/tmp/lpc-rs-mutants/mutants.out/`.

Review `missed.txt` for changes the tests did not detect, and add tests for the
missing observable behaviour. Check `timeout.txt` and `unviable.txt` separately:
a timeout or compilation failure does not establish that an assertion caught the
change. Struct-update field deletions can bypass name regex filters in
cargo-mutants 27.1.0; check `mutants.json` after narrowing a local run. CI's diff
and shard filters apply after discovery.

Register packing checks its convergence bound in debug builds, including mutation
CI. The release loop is unchanged.

Seven constructor mutations for `write_range` are excluded because cargo-mutants
27.1.0 generates constructors that `std::ops::Range<usize>` does not provide.
Compile probes confirm that all seven fail; skipping them avoids repeated failed
builds in CI.

Reviewed exclusions have specific regexes and explanations:
Token and production definitions receive distinct rule numbers. The pinned DGD
redefinition comparison differs only at equality, which the rule parser cannot produce.

Eight generated channel replacements in the socket registry refer to unimported
`Receiver` and `Sender` names. Compile probes confirm that all eight fail; the
filters match only these replacements and avoid repeated slow compiler diagnostics.

Sixty-four generated tuple replacements for `spawn_committer` are excluded
because they use unsupported `JoinHandle` constructors. Small compile probes
confirm all four handle forms fail; the filter matches only these combinations.
Program reference parameters have indices below their declared arity; builtin
reference tails use the separate efun path. At equal argument counts, the later
missing-reference check is false, preserving the pinned call-frame comparison.
The TCP manager stops when its stop sender closes, including during ordinary
field destruction. Each handle publishes one explicit `Close` command before
retirement uses `Retire`; the committer preserves their order. Tests cover these
cleanup and command-stream invariants behind the two pinned TCP exclusions.
Call-frame string addition uses the same concatenation helper and records the
same cell reads through its general path. Replacing an int register through
the general assignment stores the same scalar without allocation or drop work.
The two pinned call-frame exclusions preserve these behaviours.
CommonMark extension flags are disjoint. Balanced blocks already flush before
paragraph or HTML starts, and parsed tables contain columns and a header row.
The three pinned Markdown exclusions preserve these parser invariants.
Numeric fields contain ASCII text; at equal widths, the pinned zero-padding
comparison adds no characters and preserves the original field.
Terminal text consumers accept smaller literal chunks with intact escape atoms;
streaming retains CRLF normalization, styles and graphemes. Leading delimiters
are already handled before literal scanning, and zero wrap width adds no padding.
The three pinned formatting exclusions preserve these behaviours.
ANSI indices within the selected palette map back to their unique entries,
so disabling the pinned colour guard produces the same SGR codes.
The peephole's non-r0 and operand-count checks already rule out a deleted
copy definer. Its pinned `i - 1` to `i / 1` deletion check therefore leaves
the selected rewrites unchanged.
Compilation clears prepared parent interfaces before linking concrete parents.
The pinned `&&` to `||` lookup change is equivalent for these disjoint states;
with neither kind of parent, both branches return no declaration.
Register packing filters arguments before its occupancy check, so the pinned
`>` to `>=` change is equivalent. Its in-place writes are already read operands,
and have no retargetable destination; omitting their definition preserves both
liveness and slot occupancy.
Native pattern compilation checks the leading verb in both `group` and `build`.
Disabling the pinned `group` guard still returns the same `NoVerb` error from
`build`; the empty-pattern check remains distinct because it returns `Empty`.
`choose` rejects zero for a single object before checking for a negative numeral.
Only that later guard's pinned `<` to `<=` mutation is equivalent.
`Function::inherited` clears the packed visibility bits before inserting the
restricted visibility. OR and XOR therefore produce the same binding at that
pinned expression; tests cover both declared and inherited protection.
`Changeset::conflicting_read` may scan either collection to find any conflicting
read. Reversing its pinned size comparison changes only the scan order; tests
check intersection membership across both scan sizes and multiple conflicts.
`<` and `<=` assign the same value at equality in `RegisterCounter::set`; OR and
XOR set the same warning flag after `Span::file_id` clears that bit.
Telnet framing permits different linear reservation hints while preserving wire
bytes; a growth check catches the quadratic reservation mutation.
Composition can reuse the inner function's register for its result after the call
returns; that arithmetic exclusion is pinned to the expression's source position.
The positional-argument maximum in `FunctionPrototypeWalker::visit_var` stays
unchanged when assigned at equality; that single pinned comparison is excluded.
Regex option bits are disjoint, so OR and XOR build the same supported flag
mask; that exclusion is pinned to the mask's six operators and guarded by a
test that the flags remain disjoint.
In `EfunContext::build`, equality extends an empty argument-padding range.
The scalar integer result store and its fallback assignment produce the same
LPC value; these two exclusions are pinned to their expression positions.
The macro-definition cache budgets at least one expression node per body token.
Changing its extra `2 * size_of::<PreprocessorNode>()` allowance to
`2 + size_of::<PreprocessorNode>()` still covers that bound; only this pinned
margin mutation is excluded. Parameter strings and expression storage have
independent lower-bound checks.
The parser cache's two pinned node allowances still cover the larger of
`AstNode` and `ExpressionNode` per token. Tests check that layout bound and
retained token and syntax-vector storage independently.
Equal `sprintf` continuation offsets append zero spaces and assign the same
column value; that single padding comparison is excluded at its source position.
In `file_view::merge_pending`, resizing a byte vector to its existing length
changes neither its bytes nor capacity; only that pinned equality check is excluded.
In `compatible_exports`, `init-program` is stored separately from the function map
and `init-globals` is private. Replacing the pinned initializer-name OR with AND
therefore preserves the public export set; a compiler test checks those invariants.

Named collection calls suspend through pending calls; invalid names fail before
the slow receiver match. The single collection-guard branch deletion in
`Task::handle_call_other` is excluded at its source position; review it again if
call routing changes.

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
