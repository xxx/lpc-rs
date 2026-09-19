# ulib: a tiny learning mudlib

## Summary

Ship a self-contained `ulib/` (microlib) directory that someone can copy, run
with an installed lpc-rs driver, and use as the start of their own mudlib.
It provides guest login and one shared chat room, with teaching material for
every apply supported by the driver.

## Behaviour

1. Copying the whole directory preserves everything needed to run it and read
   its documentation; it has no runtime dependencies on repository fixtures,
   generated files, external services or additional mudlibs.
2. From the copied directory, `lpc-rs-driver --env driver.env` starts on
   `127.0.0.1:24960`. The documented configuration lets the operator choose a
   different address or port. Installing the driver binary alone does not
   install the source mudlib.
3. A connection is invited to choose a guest name of 3–16 ASCII letters.
   Names are case-insensitive; invalid or occupied names get an explanation
   and another prompt. `quit` at that prompt disconnects. Names are temporary,
   with no passwords, saved accounts or administrative privileges.
4. Two concurrent logins cannot acquire the same name. Unnamed connections
   are excluded from the online list and from chat delivery.
5. A successful login enters a shared room, shows its description and a short
   command hint, and announces the arrival to other occupants.
6. `say <text>` broadcasts speech to the room; the speaker receives an
   acknowledgement. A leading apostrophe is shorthand for `say`.
   `emote <text>` broadcasts an action including the speaker's name.
7. Chat accepts up to 400 characters of nonblank text and refuses terminal
   control characters and line separators. Text is displayed literally,
   without interpreting markup, colour tokens or LPC code.
8. `help` lists commands, `look` describes the room, and `who` lists named
   connected guests alphabetically. Unknown commands explain how to get help.
9. `quit` sends a farewell, announces departure and releases the guest name.
   An unexpected disconnect also removes the body and announces departure.
   A disconnect during name selection leaves no body behind. The supplied
   configuration disconnects a connection after 15 minutes without a line.
10. An orderly driver shutdown tells connected clients that the server is
    closing. Guest names and chat are not saved across restarts.
11. The runnable source demonstrates boot, login, commands, movement, output
    and lifecycle hooks. Every other current apply has a documented disabled
    example, including all four parser-handler families. Absence and return
    semantics are explained rather than presenting every hook as required.
12. Documentation includes a first-run tutorial, a guide to adding a command,
    an explanation of the object and transaction model, and a complete apply
    index pointing to the relevant source examples inside the copied tree.
    Every apply and helper is documented beside its declaration with its role,
    arguments and result; applies also explain omission defaults and why the
    example chooses its behaviour, including where parser handlers belong.
13. The example allows loading game objects and inheriting the shared room,
    permits objects to destroy themselves, and refuses unnecessary privileged
    operations. Players cannot evaluate source, edit files or administer the
    driver through the chat commands.
14. Apply changes update the canonical reference, ulib examples and adjacent
    documentation together. CI checks names and documented signatures, and
    changes to the apply references require an explicit ulib review record;
    semantic changes also require review and behaviour tests even when the
    existing examples continue to compile.
