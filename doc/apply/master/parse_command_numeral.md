# parse_command_numeral

`int parse_command_numeral(string word)`

Applied for the first word of a noun phrase when it is neither a run of
digits nor the all word. Return a count as a positive int (`two` → 2), an
ordinal as a negative int (`second` → -2, `2nd` → -2), and 0 for a word that
is no numeral. Undefined means no words are numerals.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, `this_player()` is the
selected command giver (defaulting to the actor), and the invoking LPC frames
remain behind the actor's immediate caller entry; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`
