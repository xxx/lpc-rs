# inventory_accessible

`int inventory_accessible()`

Applied by the parser package's scope walk alongside `inventory_visible()`,
on a candidate whose contents just joined the scope, to decide whether those
contents can actually be reached — a closed but glass-fronted box is visible
without being accessible. Undefined counts as truthy; together with the same
default for `inventory_visible()`, a container defining **neither** apply is
fully transparent: visible and reachable.

A match that is visible but not reachable — because it, or a container on
its path, answered `inventory_accessible()` falsy — still counts as
*existing*: it is reported to `parser_error_message` as kind 3 (not kind 7,
"no such thing"), and is never offered `direct_`/`indirect_`.

Departures from MudOS are listed in [`parse_sentence`](../../efun/parse_sentence.md)'s
departures table.

### See also

[`inventory_visible`](inventory_visible.md),
[`parser_handlers`](parser_handlers.md),
[`parser_error_message`](../master/parser_error_message.md)
