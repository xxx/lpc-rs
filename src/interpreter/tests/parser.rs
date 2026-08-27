//! The parser package, end to end: registration, the handler protocol,
//! parse_sentence's codes, and the master's messages.

use indoc::indoc;

use super::{fails, run, s};
use crate::interpreter::lpc_ref::LpcRef;

const VERB: (&str, &str) = (
    "/verbs/look.c",
    indoc! { r#"
        void create() {
            parse_init();
            parse_add_rule("look", "");
            parse_add_rule("look", "at OBJ");
            parse_add_rule("look", "at OBS with OBJ");
            parse_add_synonym("examine", "look", "at OBJ");
        }
        void drop() { parse_remove("look"); }
        string *rules() { return parse_my_rules(); }
    "# },
);

#[tokio::test]
async fn my_rules_lists_verb_and_rule_in_registration_order() {
    // `mixed`, not `mixed *`: a call_other's static type is always the
    // scalar `mixed` wildcard, which `mixed *` never matches.
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed create() { return "/verbs/look"->rules(); }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![
            s("look "),
            s("look at OBJ"),
            s("look at OBS with OBJ"),
            s("examine at OBJ")
        ]
    );
}

#[tokio::test]
async fn parse_dump_lists_every_rule_with_its_owner() {
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed *create() { return ({ parse_dump() }); }
    "# },
    )
    .await;
    assert_eq!(
        r,
        vec![s(
            "look   (/verbs/look)\nlook at OBJ  (/verbs/look)\nlook at OBS with OBJ  (/verbs/look)\nexamine at OBJ  (/verbs/look)\n"
        )]
    );
}

#[tokio::test]
async fn parse_remove_drops_a_verb_and_its_synonyms() {
    // `mixed`, not `mixed *`: see the previous test.
    let r = run(
        "",
        &[VERB],
        indoc! { r#"
        mixed create() { "/verbs/look"->drop(); return "/verbs/look"->rules(); }
    "# },
    )
    .await;
    assert_eq!(r, Vec::<LpcRef>::new());
}

#[tokio::test]
async fn add_rule_without_init_and_bad_rules_are_errors() {
    assert!(
        fails(
            "",
            &[],
            r#"mixed *create() { parse_add_rule("x", "OBJ"); return ({}); }"#
        )
        .await
        .contains("parse_add_rule: parse_init() has not been called")
    );
    assert!(
        fails(
            "",
            &[],
            r#"mixed *create() { parse_init(); parse_add_rule("x", "STR STR"); return ({}); }"#
        )
        .await
        .contains("parse_add_rule: two STR tokens in 'STR STR'")
    );
    assert!(
        fails(
            "",
            &[],
            r#"mixed *create() { parse_init(); parse_add_synonym("y", "x"); return ({}); }"#
        )
        .await
        .contains("parse_add_synonym: this_object() has no rules for 'x'")
    );
}

#[tokio::test]
async fn parse_refresh_is_accepted() {
    let r = run(
        "",
        &[],
        r#"mixed *create() { parse_refresh(); return ({ 1 }); }"#,
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(1)]);
}
