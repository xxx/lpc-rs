# parse_command

`int parse_command(string cmd, object | object *scope, string pattern, ref ...)`

Matches `cmd` against `pattern`, resolves the pattern's noun captures against
`scope`, and — when some parse of the whole line succeeds — writes each capture
into the destination argument in its position and returns 1. On 0 no
destination is written. Every argument after `pattern` is passed by reference
implicitly, as `sscanf`'s are.

`scope` is either an object, whose candidates are itself and its deep
inventory, or an array of objects taken in order (non-objects and destructed
members are skipped). `cmd` or `pattern` empty or not a string returns 0.

### The pattern

The dialect of `add_rule` without its leading verb: elements separated by
spaces, any of

| element | matches | destination |
|---|---|---|
| `'word'` | that word | — |
| `'get' / 'take'` | any one of the words | — |
| `[word]` | the word if present | — |
| `%w` | one word | string |
| `%s` | zero or more words | string, spacing intact, `""` when none |
| `%d` | a run of digits | int |
| `%o` | words naming one object | object: the first in scope order |
| `%i` | words naming objects, with a numeral | `({ numeral, ob... })` |
| `%l` | as `%i`, over the livings in scope | `({ numeral, ob... })` |
| `%p` | a preposition | string; see below |

Matching is case-sensitive. `%s` is greedy: when several splits of the line
fit the pattern, the earlier `%s` takes the most words it can, and a split is
abandoned only when one of its noun captures names nothing.

### Nouns

A noun phrase is `[numeral] adjective... noun`. The noun is a singular or
plural id of the object — from its `parse_command_id_list()` and
`parse_command_plural_id_list()`, or the master's shared lists — and each
word before it is an adjective from `parse_command_adjectiv_id_list()` (the
object's or the master's). Ids and adjectives may be several words. An object
that defines no `parse_command_id_list()` is asked `id(phrase)` instead, with
the phrase after the numeral and no adjective handling.

The numeral is written first in `%i`/`%l`'s array: `> 0` for a count (`3`,
`three`), `< 0` for an ordinal (`2nd`, `second`), `0` for the master's all
word or for a bare plural (`swords`). A count or the all word makes the phrase
plural, so only the plural ids are tried. The efun makes no use of the
numeral beyond reporting it: `second sword` returns every sword, and the
caller decides what second means. The driver recognizes only digit runs by
itself; every other numeral, the all word, and plural forms come from the
master's `parse_command_numeral()`, `parse_command_all_word()`, and
`parse_command_pluralize()`, so a lib in any language needs no driver change.

`%p` matches an entry of a preposition list, which may be several words. When
its destination already holds an array of strings, that array is the list and
the destination receives a new array with the matched entry swapped into
`[0]`; otherwise the master's `parse_command_prepos_list()` is the list and
the destination receives the matched entry. When several `%p` captures have
array destinations, the first one's array is the list used to match all of
them.

### Errors

`parse_command: the scope must be an object or an array of objects`;
`parse_command: too few arguments for the pattern` (raised before parsing);
`parse_command: ` followed by the pattern fault, as `add_rule` reports it.

### Example

```c
mixed *items;
string *preps = ({ "in", "on", "under" });

if (parse_command(str, environment(this_player()), "[the] %i %p [the] %o", items, preps, container))
    // items = ({ numeral, ob... }), preps[0] is the preposition typed, container an object
```

### A master in English

The driver holds no language. This master gives an English mud CD's
behaviour; adapt the tables for another language.

```c
string parse_command_all_word() { return "all"; }

string *parse_command_id_list() { return ({ "it", "thing" }); }
string *parse_command_plural_id_list() { return ({ "them", "things" }); }
string *parse_command_adjectiv_id_list() { return ({ "that" }); }
string *parse_command_prepos_list() { return ({ "in", "on", "under", "in front of" }); }

int parse_command_numeral(string word)
{
    string *ones = ({ "one", "two", "three", "four", "five", "six", "seven",
        "eight", "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
        "fifteen", "sixteen", "seventeen", "eighteen", "nineteen" });
    string *ordinal_ones = ({ "first", "second", "third", "fourth", "fifth",
        "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth",
        "thirteenth", "fourteenth", "fifteenth", "sixteenth", "seventeenth",
        "eighteenth", "nineteenth" });
    string *tens = ({ "twenty", "thirty", "forty", "fifty", "sixty", "seventy",
        "eighty", "ninety" });
    string *ordinal_tens = ({ "twentieth", "thirtieth", "fortieth", "fiftieth",
        "sixtieth", "seventieth", "eightieth", "ninetieth" });
    int n, i, j;
    string suffix;

    if (sscanf(word, "%d%s", n, suffix) == 2 && n > 0) {
        if (suffix == "")
            return n;
        if (n % 100 / 10 == 1)
            return suffix == "th" ? -n : 0;
        if (n % 10 == 1) return suffix == "st" ? -n : 0;
        if (n % 10 == 2) return suffix == "nd" ? -n : 0;
        if (n % 10 == 3) return suffix == "rd" ? -n : 0;
        return suffix == "th" ? -n : 0;
    }
    for (i = 0; i < 19; i++) {
        if (word == ones[i]) return i + 1;
        if (word == ordinal_ones[i]) return -(i + 1);
    }
    for (i = 0; i < 8; i++) {
        if (word == tens[i]) return (i + 2) * 10;
        if (word == ordinal_tens[i]) return -((i + 2) * 10);
        for (j = 0; j < 9; j++) {
            if (word == tens[i] + ones[j] || word == tens[i] + "-" + ones[j])
                return (i + 2) * 10 + j + 1;
            if (word == tens[i] + ordinal_ones[j] || word == tens[i] + "-" + ordinal_ones[j])
                return -((i + 2) * 10 + j + 1);
        }
    }
    return 0;
}

string pluralize_word(string word)
{
    mapping irregular = ([ "ox": "oxen", "tooth": "teeth", "foot": "feet",
        "man": "men", "woman": "women", "child": "children", "goose": "geese",
        "mouse": "mice", "deer": "deer", "moose": "moose", "sheep": "sheep" ]);
    mixed last = word[-1..];
    mixed last2 = word[-2..];
    mixed before = word[-2..-2];

    if (irregular[word]) return irregular[word];
    if (last2 == "ch" || last2 == "sh" || last == "s" || last == "x")
        return word + "es";
    if (last2 == "fe") { mixed head = word[..-3]; return head + "ves"; }
    if (last == "f") { mixed head = word[..-2]; return head + "ves"; }
    if (last == "y" && before != "a" && before != "e" && before != "i" && before != "o" && before != "u") {
        mixed head = word[..-2];
        return head + "ies";
    }
    return word + "s";
}

string pluralize_phrase(string phrase)
{
    string *words = explode(phrase, " ");
    int i;
    for (i = 0; i < sizeof(words); i++)
        if (i == sizeof(words) - 1 || words[i + 1] == "of")
            words[i] = pluralize_word(words[i]);
    return implode(words, " ");
}

string *parse_command_pluralize(string *singulars)
{
    string *plurals = ({});
    int i;
    for (i = 0; i < sizeof(singulars); i++)
        plurals += ({ pluralize_phrase(singulars[i]) });
    return plurals;
}
```

This compiler currently types a range-indexed string (`word[-2..]`) as
`string *`, not `string`, so `pluralize_word`'s locals holding such slices are
declared `mixed` above until that is fixed.

### Departures from CD's parse_command

`%s` is greedy (CD tries the following element at each word); `%d` is a digit
run (CD's `sscanf` accepts `5x` as 5); destinations are written only on success
(CD writes as it matches); a malformed pattern and too few destinations are
errors (CD warns, or discards); `%l` has no `find_living` fallback; there is
no built-in all word, numeral table, or pluralizer; a phrase's adjectives must
all be listed; the id must end the phrase.

### See also

`add_rule`, `sscanf`, `parse_command_id_list`, `parse_command_numeral`,
`parse_command_pluralize`, `id`
