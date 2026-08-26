# id

`int id(string phrase)`

Applied by the noun resolver, for an object that defines no
`parse_command_id_list()`, with the phrase after any numeral; nonzero means
the phrase names this object. The LPMud convention; adjectives and plurals
are the object's own affair here. The master's shared ids (`it`, `thing`)
do not apply to an object resolved through `id()`.

### See also

`parse_command`, `parse_command_id_list`
