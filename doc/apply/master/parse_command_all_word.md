# parse_command_all_word

`string parse_command_all_word()`

The word meaning every candidate, such as `all`; `%i`/`%l` report numeral 0
for it. Undefined means no such word.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, `this_player()` is the
selected command giver (defaulting to the actor), and the invoking LPC frames
remain behind the actor's immediate caller entry; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`
