# lpc-rs-lsp

A stdio language server for the lpc-rs dialect of LPC.

## Running

Build with `cargo build --workspace`, then configure your editor's LSP client to
launch:

```sh
/path/to/lpc-rs/target/debug/lpc-rs-lsp --lib-dir /path/to/mudlib
```

Pass `--config /path/to/driver.env` to load the same explicit environment file as
the compiler. Environment variables override values in that file; `--lib-dir`
overrides `LIB_DIR` / `LPC_LIB_DIR`. Without a configured root, the first editor
workspace folder is the mudlib root, falling back to the server's working directory.
The environment file is not loaded automatically.

The compiler uses the existing `SYSTEM_INCLUDE_DIRS` (colon-separated),
`AUTO_INCLUDE_FILE`, `AUTO_INHERIT_FILE`, `SIMUL_EFUN_FILE`, and
`MAX_INHERIT_DEPTH` settings, including their `LPC_`-prefixed forms.

Associate your LPC files with a distinct `lpc` filetype in the editor so that `.c`
does not select its C language server. The launch command above runs the server;
editor-specific client configuration is still required.

### Zed

Zed requires a registered language-server adapter; naming `LPC` and `lpc-rs-lsp`
in settings does not register them. Until an LPC extension supplies an adapter,
use the existing C adapter with a binary override in the **mudlib project's**
`.zed/settings.json`:

```json
{
  "languages": { "C": { "language_servers": ["clangd"] } },
  "lsp": {
    "clangd": {
      "binary": {
        "path": "/absolute/path/to/lpc-rs/target/debug/lpc-rs-lsp",
        "arguments": [
          "--lib-dir", "/absolute/path/to/mudlib",
          "--config", "/absolute/path/to/driver.env"
        ]
      }
    }
  }
}
```

Omit the `--config` pair if no environment file is needed. The adapter is named
`clangd`, but the process it launches is `lpc-rs-lsp`; the explicit server list
also excludes other C servers such as SourceKit. Keep this override local to the
LPC project so ordinary C projects retain their C server. Syntax highlighting
still uses Zed's C grammar. Run **editor: restart language server** after changing
the configuration if the old server remains attached.

See [Zed's language-server configuration documentation](https://zed.dev/docs/configuring-languages#configuring-language-servers)
for binary overrides.

## Behavior

- Opening or saving a file checks all open `.c` objects in the configured mudlib.
- Checks read an immutable snapshot of open buffers, including unsaved headers
  and inherited objects, falling back to disk for other files.
- Errors and warnings appear at their source locations, including included and
  inherited files. Secondary compiler labels become related diagnostic locations.
- Edits clear existing diagnostics immediately; fresh checks run on the next
  save. Results from an older set of buffers are discarded. Closing a file clears
  its results and rechecks remaining objects against the updated set of buffers.
- Completion suggests built-in efuns matching the identifier prefix at the cursor.
  Hover shows their signatures and bundled Markdown documentation, even when the
  surrounding program has syntax errors. Hover is explicitly an efun reference;
  it does not resolve local functions that shadow an efun.
- Requests use UTF-16 positions and full document text synchronization.

The server checks source without executing LPC or starting a gamedriver.
Configured simul-efuns are compiled for their declarations; their `create()` does
not run. The edited object's analysis stops before code generation, while
inherited objects currently compile fully.

## Current limits

This version supports one mudlib configuration per server process. Headers are
checked through open `.c` objects, not as standalone translation units. It does
not index unopened objects, watch external disk changes, or provide navigation,
rename, local-symbol completion, formatting, or parser recovery. Save an open
object after changing a dependency outside the editor to refresh its diagnostics.

Diagnostic source snapshots are released after each batch of checks. The existing
compiler and driver continue using their default diagnostic storage.

## Verification

`cargo test --workspace` covers the protocol lifecycle, document versions, efun
help, Unicode positions, and checking unsaved includes and inherits, alongside
the existing compiler and runtime tests.
