# parse_command_adjectiv_id_list

`string *parse_command_adjectiv_id_list()`

Adjectives every object accepts before its name, such as `that`. Optional; no
list when undefined. The same name on an object gives that object's own
adjectives.

For calls from `parse_sentence` or `parse_my_rules`, the invoking LPC frames
remain behind the actor; see
[callback context](../../efun/parse_sentence.md#callback-context).

### See also

`parse_command`, [the object apply](../object/parse_command_adjectiv_id_list.md)
