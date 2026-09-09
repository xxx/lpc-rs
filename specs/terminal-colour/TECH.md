# Terminal colour implementation

## Context

At lpc-rs `1f00499516304e955e94111d7aaddee76da4ea03`, TTYPE exists in Opt but
session policy refuses it. Connection::Snapshot mirrors negotiated state before
input events are dispatched. query_connection constructs a fresh STM mapping.
Efuns are appended to the registry to preserve CallEfun indices. LPC callbacks
use Continuation rather than nesting host-language execution.

## Changes

- Add terminal capability state and ColourDepth to lpc-rs-telnet. A bounded
  TTYPE exchange reuses the RFC 1143 table and wire framing. No new socket path
  or LPC callback is needed for negotiation.
- Mirror terminal information in Connection::Snapshot and expose it through
  query_connection. The formatter snapshots the chosen profile once per call.
  Player overrides are explicit efun arguments, not mutable driver preferences.
- Implement a synchronous efun with a continuation only when custom LPC
  resolvers are supplied. Read mappings through the transaction; release the
  transaction borrow before callbacks. Snapshot mapping values for the call.
- Separate token expansion, concrete color/SGR parsing and quantization, and
  Unicode layout within the terminal_colour module. Bound strings before
  appending; never recursively expand caller mappings. Parse escapes as units.
- Use unicode-segmentation and unicode-width for grapheme-safe column layout.
  Existing break_string behavior has different indentation and long-word rules,
  so retain it unchanged; a later refactor can unify lower-level layout when
  there is demonstrated reuse.
- Preserve the current output path: a mudlib formatter can run in catch_tell,
  call terminal_colour with the recipient explicitly, and call write_socket.
  The resulting bytes remain a deferred commit effect. No theme implementation
  or compatibility simul-efuns are part of this change.

## Testing and validation

- Token/efun tests exercise PRODUCT 1–5, invalid types, callback errors,
  mapping immutability, finite expansion, and STM rollback of callbacks.
- Color tests pin RGB and indexed foreground/background output for every
  profile, fixed palette/grayscale conversion, raw SGR, and plain/reset behavior
  (PRODUCT 6–8).
- Layout tests cover word/long-word wrapping, negative-width padding, indent,
  explicit lines, tabs, CJK, combining marks, emoji, and styles around wrapping
  (PRODUCT 9–10).
- Session tests cover fragmented negotiation, refusal, unsolicited/malformed
  responses, repeated WILL, repeated/cycling terminal names, the exchange cap,
  MTTS precedence, unknown terminals, and renegotiation (PRODUCT 11–12).
- Interpreter tests connect clients reporting different depths and verify
  per-recipient rendering, explicit overrides, and query_connection fields.
- Complete cargo test --workspace, cargo clippy --workspace --all-targets,
  cargo fmt --check, and warning-free cargo doc --workspace.
