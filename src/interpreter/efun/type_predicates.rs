//! The type predicates (`intp`, `floatp`, ...): 1 when the one argument is
//! that variant, else 0.

use lpc_rs_core::RegisterSize;
use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

fn type_predicate<const N: usize>(
    context: &mut EfunContext<'_, N>,
    is: fn(&LpcRef) -> bool,
) -> Result<()> {
    let result = is(context.resolve_local_register(1 as RegisterSize));
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

/// `arrayp`: 1 if the argument is an array.
pub fn arrayp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Array(_)))
}

/// `floatp`: 1 if the argument is a float.
pub fn floatp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Float(_)))
}

/// `functionp`: 1 if the argument is a function pointer.
pub fn functionp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Function(_)))
}

/// `intp`: 1 if the argument is an int (a null object is one).
pub fn intp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Int(_)))
}

/// `mappingp`: 1 if the argument is a mapping.
pub fn mappingp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Mapping(_)))
}

/// `objectp`: 1 if the argument is a live object.
pub fn objectp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let live = context
        .resolve_local_register(1 as RegisterSize)
        .live_object(context.txn())
        .is_some();
    context.return_efun_result(LpcRef::from(live));
    Ok(())
}

/// `stringp`: 1 if the argument is a string.
pub fn stringp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::String(_)))
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{interpreter::vm::Vm, test_support::test_config};

    /// Every variant in a typed and a `mixed` slot, plus a null object in
    /// each; `PRED` is the predicate under test.
    const FIXTURE: &str = indoc! { r#"
        int a = 123;
        float b = 123.456;
        string c = "hello";
        object d = this_object();
        object dd = find_object("/non-existent");
        int *e = ({ 1, 2, 3 });
        mapping f = ([ "foo": "bar" ]);
        function g = (: 1 :);

        mixed h = 123;
        mixed i = 123.456;
        mixed j = "hello";
        mixed k = this_object();
        mixed kk = find_object("/non-existent");
        mixed *l = ({ 1, 2, 3 });
        mixed m = ([ "foo": "bar" ]);
        mixed n = (: 1 :);

        int *create() {
            return ({
                PRED(a), PRED(b), PRED(c), PRED(d), PRED(dd), PRED(e), PRED(f), PRED(g),
                PRED(h), PRED(i), PRED(j), PRED(k), PRED(kk), PRED(l), PRED(m), PRED(n),
            });
        }
    "# };

    /// Each predicate is 1 on exactly its variant; a null object counts as
    /// an int.
    #[tokio::test]
    async fn each_predicate_matches_only_its_variant() {
        const CASES: &[(&str, [i64; 16])] = &[
            ("intp", [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0]),
            ("floatp", [0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0]),
            ("stringp", [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0]),
            ("objectp", [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0]),
            ("arrayp", [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0]),
            ("mappingp", [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0]),
            (
                "functionp",
                [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1],
            ),
        ];

        for (predicate, expected) in CASES {
            let master = FIXTURE.replace("PRED", predicate);
            let vm = Vm::new(test_config());
            let master_proc = vm
                .initialize_process_from_code("master.c", &master)
                .await
                .unwrap();

            let result = master_proc.result().unwrap();
            result
                .with_array(master_proc.context.txn(), |arr| {
                    assert_eq!(arr, expected.as_slice(), "{predicate}");
                })
                .unwrap();
        }
    }
}
