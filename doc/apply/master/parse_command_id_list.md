# parse_command_id_list

`string *parse_command_id_list()`

Singular ids every object answers to, such as `it` and `thing`. Optional; no
list when undefined. The same name on an object gives that object's own ids.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, `this_player()` is the
selected command giver (defaulting to the actor), and the invoking LPC frames
remain behind the actor's immediate caller entry; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, [the object apply](../object/parse_command_id_list.md)
