# parse_command_id_list

`string *parse_command_id_list()`

Singular ids every object answers to, such as `it` and `thing`. Optional; no
list when undefined. The same name on an object gives that object's own ids.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, [the object apply](../object/parse_command_id_list.md)
