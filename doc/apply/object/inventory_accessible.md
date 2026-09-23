# inventory_accessible

`int inventory_accessible()`

Controls whether the parser package can reach this object's visible contents.
Return a truthy value to allow reachability or a falsy value to make those
contents inaccessible. Omission counts as true. The result governs the children,
not the container's own eligibility as a noun; an inaccessible ancestor keeps
all its descendants inaccessible even through an open inner container.

The default walk and explicit scope trees ask this after `inventory_visible()`
permits descent and while the parent is reachable. The default walk exempts the
actor and immediate environment for their direct inventories. Flat explicit
scope entries are reachable without consulting either hook.

For an unmodified object token, visible but unreachable matches are excluded
before permission checks; if no reachable match qualifies, kind 3 can report
inaccessibility rather than kind 7 (no matching noun). A `:v` token allows those
visible matches to proceed to normal permissions. It does not reveal hidden
contents or grant permission to perform the action.

Called in the current transaction with the actor as `this_player()` and
`previous_object()`, and `origin()` set to `"driver"`. Explicit scope structure
is snapshotted before callbacks; a parent's hook result is reused within each
rule's walk. Later rules and parses ask afresh. Exceptions propagate and hook
effects remain if a rule refuses. In an explicit scope walk, a parent destructed
during the hook cannot expose its supplied children.

### See also

[`inventory_visible`](inventory_visible.md),
[`parser_handlers`](parser_handlers.md),
[`parser_error_message`](../master/parser_error_message.md),
[`parse_sentence`](../../efun/parse_sentence.md#scope)
