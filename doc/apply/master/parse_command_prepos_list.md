# parse_command_prepos_list

`string *parse_command_prepos_list()`

The list `%p` matches against when the caller's destination holds no array;
entries may be several words, such as `in front of`. Optional; no list when
undefined.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`
