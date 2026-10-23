# parse_command_pluralize

`string *parse_command_pluralize(string *singulars)`

Applied once for an object that defines `parse_command_id_list()` but
returns no array from `parse_command_plural_id_list()`; returns the plural
of each `singulars[i]` at `[i]` (a non-string entry means no plural).
Undefined means such objects have no plural forms.

During command dispatch, vocabulary uses the actor as player and immediate
caller; see [command callback context](../../efun/command.md#callback-context).
A direct `parse_command` call retains its entering player.

For calls from `parse_sentence` or `parse_my_rules`, `this_player()` is the
selected command giver (defaulting to the actor), and the invoking LPC frames
remain behind the actor's immediate caller entry; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`
