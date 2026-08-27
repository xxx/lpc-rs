# inventory_visible

`int inventory_visible()`

Applied by the parser package's scope walk on each candidate it reaches, to
decide whether that candidate's own inventory joins the scope too — an open
box, a transparent case, the room's obvious contents. Undefined counts as
truthy: an object that says nothing about it hides nothing.

Departures from MudOS are listed in [`parse_sentence`](../../efun/parse_sentence.md)'s
departures table.

### See also

[`inventory_accessible`](inventory_accessible.md),
[`parser_handlers`](parser_handlers.md),
[`parse_sentence`](../../efun/parse_sentence.md)
