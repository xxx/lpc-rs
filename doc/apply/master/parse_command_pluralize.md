# parse_command_pluralize

`string *parse_command_pluralize(string *singulars)`

Applied once for an object that defines `parse_command_id_list()` but
returns no array from `parse_command_plural_id_list()`; returns the plural
of each `singulars[i]` at `[i]` (a non-string entry means no plural).
Undefined means such objects have no plural forms.

### See also

`parse_command`
