# parse_command_plural_id_list

`string *parse_command_plural_id_list()`

The plural names; when this returns no array the master's
`parse_command_pluralize()` derives them from the singular ids.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, [the master apply](../master/parse_command_plural_id_list.md)
