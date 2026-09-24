# parse_command_all_word

`string parse_command_all_word()`

The word meaning every candidate, such as `all`; `%i`/`%l` report numeral 0
for it. Undefined means no such word.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`
