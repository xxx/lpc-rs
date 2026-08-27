//! `parse_command`, end to end: the dialect, every capture kind, the
//! scope forms, the applies, and the errors.

use indoc::indoc;

use super::{fails, run, s};
use crate::interpreter::lpc_ref::LpcRef;

const SWORD: (&str, &str) = (
    "/sword.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "sword" }); }
    string *parse_command_plural_id_list() { return ({ "swords" }); }
    string *parse_command_adjectiv_id_list() { return ({ "red" }); }
    void go(object dest) { move_object(dest); }
"# },
);

const KNIFE: (&str, &str) = (
    "/knife.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "knife" }); }
    void go(object dest) { move_object(dest); }
"# },
);

const BAG: (&str, &str) = (
    "/bag.c",
    indoc! { r#"
    string *parse_command_id_list() { return ({ "bag" }); }
    string *parse_command_adjectiv_id_list() { return ({ "old" }); }
    void go(object dest) { move_object(dest); }
"# },
);

const BOB: (&str, &str) = (
    "/bob.c",
    indoc! { r#"
    void create() { enable_commands(); }
    string *parse_command_id_list() { return ({ "bob" }); }
"# },
);

/// `doc/efun/parse_command.md`'s example master, verbatim.
const ENGLISH_MASTER: &str = indoc! { r#"
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
        mapping irregular = ([ "child": "children", "fish": "fish", "foot": "feet",
            "goose": "geese", "louse": "lice", "man": "men", "mouse": "mice", "ox": "oxen",
            "person": "people", "sheep": "sheep", "tooth": "teeth", "woman": "women" ]);
        string last = word[-1..];
        string last2 = word[-2..];
        string before = word[-2..-2];

        if (irregular[word]) return irregular[word];
        if (last2 == "ch" || last2 == "sh" || last == "s" || last == "x")
            return word + "es";
        if (last2 == "fe") { string head = word[..-3]; return head + "ves"; }
        if (last == "f") { string head = word[..-2]; return head + "ves"; }
        if (last == "y" && before != "a" && before != "e" && before != "i" && before != "o" && before != "u") {
            string head = word[..-2];
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
"# };

#[tokio::test]
async fn plain_captures_write_their_destinations() {
    let r = run("", &[], indoc! { r#"
        mixed *create() { string a; string b; int n; int r = parse_command("give sword to bob 3", ({}), "'give' %w 'to' %w %d", a, b, n); return ({ r, a, b, n }); }
    "# }).await;
    assert_eq!(
        r,
        vec![LpcRef::from(1), s("sword"), s("bob"), LpcRef::from(3)]
    );
}

#[tokio::test]
async fn a_plain_only_pattern_makes_no_master_apply() {
    let master = indoc! { r#"
        int calls;
        string *parse_command_id_list() { calls++; return ({}); }
        int query_calls() { return calls; }
    "# };
    let r = run(
        master,
        &[],
        indoc! { r#"
        mixed *create() {
            string a; string b; int n;
            int r = parse_command("give sword to bob 3", ({}), "'give' %w 'to' %w %d", a, b, n);
            int after_plain = "/secure/master"->query_calls();
            mixed *items;
            parse_command("sword", ({}), "%i", items);
            int after_noun = "/secure/master"->query_calls();
            return ({ r, after_plain, after_noun });
        }
    "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(0), LpcRef::from(1)]);
}

#[tokio::test]
async fn items_writes_a_numeral_and_the_objects() {
    let r = run("", &[SWORD], indoc! { r#"
        mixed *create() { mixed *items; int r = parse_command("red sword", ({ find_object("/sword") }), "%i", items); return ({ r, sizeof(items), items[0], items[1] == find_object("/sword") }); }
    "# }).await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            LpcRef::from(2),
            LpcRef::from(1),
            LpcRef::from(1)
        ]
    );
}

#[tokio::test]
async fn object_writes_the_first_match_and_living_only_livings() {
    let r = run(
        "",
        &[SWORD],
        indoc! { r#"
        string *parse_command_id_list() { return ({ "me" }); }
        mixed *create() {
            object ob; mixed *who; mixed *none;
            enable_commands();
            object *scope = ({ this_object(), find_object("/sword") });
            int r1 = parse_command("sword", scope, "%o", ob);
            int r2 = parse_command("me", scope, "%l", who);
            int r3 = parse_command("sword", scope, "%l", none);
            return ({ r1, ob == find_object("/sword"), r2, who[1] == this_object(), r3, none });
        }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(0),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn a_single_living_capture_writes_the_matched_object() {
    let r = run(
        "",
        &[BOB],
        indoc! { r#"
        mixed *create() {
            object ob;
            int r = parse_command("bob", ({ find_object("/bob") }), "%L", ob);
            return ({ r, ob == find_object("/bob") });
        }
    "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(1)]);
}

#[tokio::test]
async fn a_preposition_array_destination_gets_the_match_swapped_to_the_front() {
    let r = run("", &[], indoc! { r#"
        mixed *create() { string *preps = ({ "in", "on", "in front of" }); string rest; int r = parse_command("in front of box", ({}), "%p %s", preps, rest); return ({ r, preps[0], preps[1], preps[2], rest }); }
    "# }).await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("in front of"),
            s("on"),
            s("in"),
            s("box")
        ]
    );
}

#[tokio::test]
async fn an_extra_destination_is_left_untouched() {
    let r = run("", &[], indoc! { r#"
        mixed *create() { string a; string keep = "kept"; int r = parse_command("give sword", ({}), "'give' %w", a, keep); return ({ r, a, keep }); }
    "# }).await;
    assert_eq!(r, vec![LpcRef::from(1), s("sword"), s("kept")]);
}

#[tokio::test]
async fn two_preposition_captures_share_the_first_array_list() {
    let r = run(
        "",
        &[],
        indoc! { r#"
        mixed *create() {
            string *first = ({ "in", "on" });
            string *second = ({ "under", "over" });
            string w;
            int r = parse_command("on box in", ({}), "%p %w %p", first, w, second);
            return ({ r, first[0], first[1], w, second[0], second[1] });
        }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            s("on"),
            s("in"),
            s("box"),
            s("in"),
            s("on")
        ]
    );
}

#[tokio::test]
async fn a_preposition_without_an_array_destination_uses_the_masters_list() {
    let master = r#"string *parse_command_prepos_list() { return ({ "under" }); }"#;
    let r = run(master, &[], indoc! { r#"
        mixed *create() { string p; string w; int r = parse_command("under bed", ({}), "%p %w", p, w); int miss = parse_command("over bed", ({}), "%p %w", p, w); return ({ r, p, w, miss }); }
    "# }).await;
    assert_eq!(
        r,
        vec![LpcRef::from(1), s("under"), s("bed"), LpcRef::from(0)]
    );
}

#[tokio::test]
async fn an_object_scope_is_the_object_and_its_deep_inventory() {
    let r = run(
        "",
        &[SWORD, BAG],
        indoc! { r#"
        mixed *create() {
            object ob;
            "/bag"->go(this_object());
            "/sword"->go(find_object("/bag"));
            int r = parse_command("sword", this_object(), "%o", ob);
            return ({ r, ob == find_object("/sword") });
        }
    "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(1)]);
}

#[tokio::test]
async fn a_failed_match_writes_nothing() {
    let r = run(
        "",
        &[SWORD],
        indoc! { r#"
        mixed *create() {
            string a = "keep"; mixed *items = ({ 9 });
            int r1 = parse_command("take sword", ({}), "'give' %w", a);
            int r2 = parse_command("axe", ({ find_object("/sword") }), "%i", items);
            return ({ r1, a, r2, items[0] });
        }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![LpcRef::from(0), s("keep"), LpcRef::from(0), LpcRef::from(9)]
    );
}

#[tokio::test]
async fn the_next_parse_is_tried_when_a_phrase_fails() {
    let r = run("", &[SWORD, BAG], indoc! { r#"
        mixed *create() {
            mixed *a; string w; mixed *b;
            int r = parse_command("red sword in old bag", ({ find_object("/sword"), find_object("/bag") }), "%i %w %i", a, w, b);
            return ({ r, a[1] == find_object("/sword"), w, b[1] == find_object("/bag") });
        }
    "# }).await;
    assert_eq!(
        r,
        vec![LpcRef::from(1), LpcRef::from(1), s("in"), LpcRef::from(1)]
    );
}

#[tokio::test]
async fn an_empty_command_or_a_non_string_returns_zero() {
    let r = run("", &[], indoc! { r#"
        mixed *create() {
            string a; mixed five = 5;
            return ({ parse_command("", ({}), "%w", a), parse_command("x", ({}), "", a), parse_command(five, ({}), "%w", a), parse_command("x", ({}), five, a) });
        }
    "# }).await;
    assert_eq!(r, vec![LpcRef::from(0); 4]);
}

#[tokio::test]
async fn every_error_names_its_cause() {
    let bad_scope = fails(
        "",
        &[],
        r#"mixed *create() { string a; mixed bad = 5; parse_command("x", bad, "%w", a); }"#,
    )
    .await;
    assert!(
        bad_scope.contains("parse_command: the scope must be an object or an array of objects"),
        "{bad_scope}"
    );
    let too_few = fails(
        "",
        &[],
        r#"mixed *create() { string a; parse_command("x y", ({}), "%w %w", a); }"#,
    )
    .await;
    assert!(
        too_few.contains("parse_command: too few arguments for the pattern"),
        "{too_few}"
    );
    let malformed = fails(
        "",
        &[],
        r#"mixed *create() { string a; parse_command("x", ({}), "look %w", a); }"#,
    )
    .await;
    assert!(
        malformed.contains("parse_command: `look` must be quoted: 'look'"),
        "{malformed}"
    );
}

#[tokio::test]
async fn the_english_example_master_resolves_numerals_and_plurals() {
    let r = run(ENGLISH_MASTER, &[SWORD, KNIFE], indoc! { r#"
        mixed *create() {
            object *scope = ({ find_object("/sword"), find_object("/knife") });
            mixed *a; mixed *b; mixed *c; mixed *d; mixed *e; mixed *f;
            parse_command("2nd sword", scope, "%i", a);
            parse_command("twentyone swords", scope, "%i", b);
            parse_command("all", scope, "%i", c);
            parse_command("knives", scope, "%i", d);
            parse_command("that thing", scope, "%i", e);
            parse_command("twelfth sword", scope, "%i", f);
            return ({ a[0], b[0], c[0], sizeof(c), d[0], d[1] == find_object("/knife"), sizeof(e), f[0] });
        }
    "# }).await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(-2),
            LpcRef::from(21),
            LpcRef::from(0),
            LpcRef::from(3),
            LpcRef::from(0),
            LpcRef::from(1),
            LpcRef::from(3),
            LpcRef::from(-12),
        ]
    );
}

#[tokio::test]
async fn a_master_without_the_language_applies_still_resolves_digits() {
    let r = run(
        "",
        &[SWORD],
        indoc! { r#"
        mixed *create() {
            object *scope = ({ find_object("/sword") });
            mixed *a; mixed *b; mixed *c;
            int r1 = parse_command("3 swords", scope, "%i", a);
            int r2 = parse_command("sword", scope, "%i", b);
            int r3 = parse_command("all", scope, "%i", c);
            return ({ r1, a[0], r2, b[0], r3 });
        }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            LpcRef::from(1),
            LpcRef::from(3),
            LpcRef::from(1),
            LpcRef::from(1),
            LpcRef::from(0)
        ]
    );
}

#[tokio::test]
async fn an_object_without_lists_is_asked_id() {
    let rock = ("/rock.c", r#"int id(string s) { return s == "rock"; }"#);
    let r = run("", &[rock], indoc! { r#"
        mixed *create() { mixed *a; mixed *b; int r1 = parse_command("rock", ({ find_object("/rock") }), "%i", a); int r2 = parse_command("big rock", ({ find_object("/rock") }), "%i", b); return ({ r1, a[1] == find_object("/rock"), r2 }); }
    "# }).await;
    assert_eq!(r, vec![LpcRef::from(1), LpcRef::from(1), LpcRef::from(0)]);
}
