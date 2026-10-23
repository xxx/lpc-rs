# parse_command_plural_id_list

`string *parse_command_plural_id_list()`

The plural names; when this returns no array the master's
`parse_command_pluralize()` derives them from the singular ids.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, `this_player()` is the
selected command giver (defaulting to the actor), and the invoking LPC frames
remain behind the actor's immediate caller entry; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, [the master apply](../master/parse_command_plural_id_list.md)
