# lpc-rs-lpcc

Compile, inspect or run a single LPC file. See the root
[README](../README.md#build-and-install) for installation instructions.

## Usage

Set `LPC_LIB_DIR` to your mudlib directory. The filename is a host filesystem
path, relative to the working directory when not absolute:

```sh
LPC_LIB_DIR=./lib lpc-rs-lpcc lib/hello.c
LPC_LIB_DIR=./lib lpc-rs-lpcc --check lib/hello.c
LPC_LIB_DIR=./lib lpc-rs-lpcc -S lib/hello.c > hello.asm
```

| Mode | Behaviour |
|---|---|
| No mode flag | Compile and initialise the object, including its `create()` hook, then exit |
| `--check` | Compile without initialising the target object; configured simulated efuns are still initialised |
| `-S`, `--emit-asm` | Print the compiled instruction listing without executing LPC |

`--emit-asm` and `--check` are mutually exclusive. Assembly output goes to stdout;
compilation warnings and errors go to stderr. Compilation errors return a nonzero
exit status without emitting a listing.

The listing includes function headers, constants, argument lists, labels and
instructions, including inherited functions and global initialisation code.
Configured simulated efuns are compiled to resolve their declarations, but their
initialisers and `create()` hooks do not run in assembly mode. Only the target
program's listing is printed.

## Configuration

Use `-c <path>` / `--config <path>` to load an environment file explicitly:

```sh
lpc-rs-lpcc --config mud.env --emit-asm /path/to/mudlib/object.c
```

No `.env` file is loaded automatically. See [default.env](../default.env) for
configuration options. Include paths, automatic includes and inheritance, and
simulated efuns apply in assembly mode as they do during compilation for execution.

For network connections and scheduled callbacks, use `lpc-rs-driver`.
