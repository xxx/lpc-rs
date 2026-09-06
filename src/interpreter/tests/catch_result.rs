//! `catch`'s answer when nothing is raised, once the packer has given its
//! register away to an earlier temp.

use indoc::indoc;

use super::run;
use crate::interpreter::lpc_ref::LpcRef;

const X: (&str, &str) = (
    "/x.c",
    "int tag(string s) { return 1; } int quiet() { return 1; }",
);

#[tokio::test]
async fn a_catch_that_raises_nothing_answers_zero_after_a_string_temp() {
    let r = run(
        "",
        &[X],
        indoc! { r#"
            mixed *create() {
                object ob = find_object("/x");
                string who = "wiz";
                string err;
                ob->tag("root:" + who);
                err = (string)catch(ob->quiet());
                return ({ err });
            }
        "# },
    )
    .await;
    assert_eq!(r, vec![LpcRef::from(0)]);
}
