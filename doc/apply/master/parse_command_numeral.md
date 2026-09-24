# parse_command_numeral

`int parse_command_numeral(string word)`

Applied for the first word of a noun phrase when it is neither a run of
digits nor the all word. Return a count as a positive int (`two` → 2), an
ordinal as a negative int (`second` → -2, `2nd` → -2), and 0 for a word that
is no numeral. Undefined means no words are numerals.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`
