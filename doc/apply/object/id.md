# id

`int id(string phrase)`

Applied by the noun resolver, for an object that defines no
`parse_command_id_list()`, with the phrase after any numeral; nonzero means
the phrase names this object. The object handles adjectives and plurals
itself. The master's shared ids (`it`, `thing`) do not apply to an object
resolved through `id()`.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, `this_player()` is the
selected command giver (defaulting to the actor), and the invoking LPC frames
remain behind the actor's immediate caller entry; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, `parse_command_id_list`
