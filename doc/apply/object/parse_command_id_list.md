# parse_command_id_list

`string *parse_command_id_list()`

The singular names this object answers to; an entry may be several words
(`long sword`). An object defining this but returning no array has no names
of its own; the master's shared ids (`it`, `thing`) can still name it. An
object not defining it is asked `id()` instead.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, `id`, [the master apply](../master/parse_command_id_list.md)
