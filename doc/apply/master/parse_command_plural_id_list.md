# parse_command_plural_id_list

`string *parse_command_plural_id_list()`

Plural ids every object answers to, such as `them`. Optional; no list when
undefined. The same name on an object gives that object's own plural ids.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, [the object apply](../object/parse_command_plural_id_list.md)
