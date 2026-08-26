# parse_command_id_list

`string *parse_command_id_list()`

The singular names this object answers to; an entry may be several words
(`long sword`). An object defining this but returning no array has no names
of its own; the master's shared ids (`it`, `thing`) can still name it. An
object not defining it is asked `id()` instead.

### See also

`parse_command`, `id`, [the master apply](../master/parse_command_id_list.md)
