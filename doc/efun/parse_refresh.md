# parse_refresh

`void parse_refresh()`

Accepted, and does nothing. MudOS's `parse_refresh()` invalidates names the
parser cached for `this_object()`; this driver caches no resolution across
calls — every `parse_add_rule` rule, and every noun phrase, is read and
resolved fresh each time — so there is nothing to invalidate.

### See also

`parse_init`, `parse_add_rule`, `parse_sentence`
