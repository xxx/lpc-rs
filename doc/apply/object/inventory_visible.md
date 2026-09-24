# inventory_visible

`int inventory_visible()`

Controls whether the parser package includes this object's contents in its
visible scope. Return a truthy value to include them or a falsy value to hide
them. Omission counts as true. This does not control the object's own noun
eligibility; supply its vocabulary separately.

The default walk asks nested containers, exempting the actor and immediate
environment when including their direct inventories. Explicit scopes ask a
parent when evaluating its supplied child array. A flat explicit object needs
no inventory hook. `:v` tokens do not expose hidden subtrees.

Called in the current transaction with the actor as `this_player()` and
`previous_object()`, and `origin()` set to `"driver"`, before noun permission
handlers. Explicit scope arrays are snapshotted before callbacks; each rule's
walk reuses a parent's result. Later rules and later parses ask afresh.
Exceptions propagate; refusing a rule does not undo hook effects. In an explicit
scope walk, destruction of the parent during the hook prevents descent.

Parser efun calls retain the invoking LPC frames behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

[`inventory_accessible`](inventory_accessible.md),
[`parser_handlers`](parser_handlers.md),
[`parse_sentence`](../../efun/parse_sentence.md#scope)
