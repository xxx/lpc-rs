//! The type predicates (`intp`, `floatp`, ...): 1 when the one argument is
//! that type or configured null value, else 0.

use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

fn type_predicate<const N: usize>(
    context: &mut EfunContext<'_, N>,
    is: fn(&LpcRef) -> bool,
) -> Result<()> {
    let value = context.arg(0);
    let result = if matches!(value, LpcRef::Object(_)) && value.live_object(context.txn()).is_none()
    {
        is(&context.txn().nil())
    } else {
        is(value)
    };
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

/// `arrayp`: 1 if the argument is an array.
pub fn arrayp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Array(_)))
}

/// `bytesp`: 1 if the argument is a `bytes`.
pub fn bytesp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Bytes(_)))
}

/// `floatp`: 1 if the argument is a float.
pub fn floatp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Float(_)))
}

/// `classp`: 1 if the argument is a class instance, including an empty class.
pub fn classp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Class(_)))
}

/// `functionp`: 1 if the argument is a function pointer.
pub fn functionp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Function(_)))
}

/// `intp`: 1 if the argument is an int, including null objects in zero mode.
pub fn intp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Int(_)))
}

/// `mappingp`: 1 if the argument is a mapping.
pub fn mappingp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    type_predicate(context, |r| matches!(r, LpcRef::Mapping(_)))
}

/// `nilp`: 1 if the argument is the configured null value or a destructed object.
pub fn nilp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let result = context.arg(0).is_null_in(context.txn());
    context.return_efun_result(LpcRef::from(result));
    Ok(())
}

/// `objectp`: 1 if the argument is a live object.
pub fn objectp<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let live = context.arg(0).live_object(context.txn()).is_some();
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
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, test_config},
    };

    #[tokio::test]
    async fn nilp_observes_null_mode_without_accepting_other_false_or_empty_values() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = TempLib::new("nilp-values");
            let vm = Vm::new(
                ConfigBuilder::default()
                    .lib_dir(root.to_str().unwrap())
                    .null_mode(mode)
                    .build()
                    .unwrap(),
            );
            let result = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                class empty {}
                int *create() {
                    function predicate = nilp;
                    return ({ nilp(nil), nilp(0), nilp(0.0), nilp(-0.0), nilp(1),
                        nilp(""), nilp(({})), nilp(([])), nilp(to_bytes("", "utf8")),
                        nilp(new(class empty)), nilp(this_object()), nilp(predicate),
                        predicate(nil), evaluate(nilp, nil), predicate(0) });
                }
            "#,
                )
                .await
                .unwrap();
            let zero = i64::from(mode == NullMode::Zero);
            result
                .result()
                .unwrap()
                .with_array(result.context.txn(), |values| {
                    assert_eq!(
                        values,
                        &[1, zero, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, zero][..],
                        "{mode}"
                    );
                })
                .unwrap();
        }
    }

    #[tokio::test]
    async fn nilp_tests_values_independently_of_membership_and_argument_presence() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = TempLib::new("nilp-absence");
            let vm = Vm::new(
                ConfigBuilder::default()
                    .lib_dir(root.to_str().unwrap())
                    .null_mode(mode)
                    .build()
                    .unwrap(),
            );
            let code = r#"
                mixed defaulted(mixed value = "default") { return value; }
                int supplied(mixed *args...) { return sizeof(args); }
                varargs int omitted(mixed value) { return nilp(value); }
                int create() {
                    mapping m = ([ "nil": nil, "zero": 0 ]);
                    mixed value;
                    return nilp(value) && nilp(find_object("/missing"))
                        && nilp(m["missing"]) && nilp(m["nil"])
                        && nilp(m["zero"]) == ZERO
                        && !has_key(m, "missing") && has_key(m, "nil") && has_key(m, "zero")
                        && defaulted() == "default" && nilp(defaulted(nil))
                        && nilp(defaulted(0)) == ZERO && omitted()
                        && supplied() == 0 && supplied(nil) == 1 && supplied(0) == 1;
                }
            "#
            .replace("ZERO", if mode == NullMode::Zero { "1" } else { "0" });
            let result = vm
                .initialize_process_from_code("/test.c", &code)
                .await
                .unwrap();
            assert_eq!(result.result(), Some(LpcRef::from(1)), "{mode}");
        }
    }

    #[tokio::test]
    async fn nilp_observes_transactional_destruction_rollback_commit_and_gc() {
        use crate::interpreter::task::{
            apply_function::apply_function_by_name, task_template::TaskTemplate,
        };

        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = TempLib::new("nilp-destruct");
            let vm = Vm::new(
                ConfigBuilder::default()
                    .lib_dir(root.to_str().unwrap())
                    .null_mode(mode)
                    .build()
                    .unwrap(),
            );
            vm.initialize_process_from_code(
                "/secure/master.c",
                "int valid_destruct(object o, object c, string p) { return 1; }",
            )
            .await
            .unwrap();
            vm.initialize_process_from_code("/victim.c", "void create() {}")
                .await
                .unwrap();
            let result = vm
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                object victim = find_object("/victim");
                void abort_destruct() { destruct(victim); throw(nilp(victim) ? "dead" : "live"); }
                int check() { return nilp(victim) && !objectp(victim); }
                int create() { return !nilp(victim) && objectp(victim); }
                int destroy() { destruct(victim); return check(); }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(result.result(), Some(LpcRef::from(1)), "{mode}");
            let aborted = apply_function_by_name(
                "abort_destruct",
                &[],
                result.context.process.clone(),
                TaskTemplate::from(vm.global_state.clone()),
                Some(5000),
            )
            .await
            .unwrap()
            .unwrap_err();
            assert!(aborted.to_string().contains("dead"), "{aborted}");
            for function in ["create", "destroy"] {
                let checked = apply_function_by_name(
                    function,
                    &[],
                    result.context.process.clone(),
                    TaskTemplate::from(vm.global_state.clone()),
                    Some(5000),
                )
                .await
                .unwrap()
                .unwrap();
                assert_eq!(checked, LpcRef::from(1), "{mode}, {function}");
            }
            for collect in [false, true] {
                if collect {
                    vm.global_state.gc().await.unwrap().unwrap();
                }
                let checked = apply_function_by_name(
                    "check",
                    &[],
                    result.context.process.clone(),
                    TaskTemplate::from(vm.global_state.clone()),
                    Some(5000),
                )
                .await
                .unwrap()
                .unwrap();
                assert_eq!(checked, LpcRef::from(1), "{mode}, gc={collect}");
            }
        }
    }

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
        bytes o = to_bytes("x", "utf8");
        mixed p = to_bytes("x", "utf8");
        class record { int n; }
        class record q = new(class record);
        mixed r = q;

        int *create() {
            return ({
                PRED(a), PRED(b), PRED(c), PRED(d), PRED(dd), PRED(e), PRED(f), PRED(g),
                PRED(h), PRED(i), PRED(j), PRED(k), PRED(kk), PRED(l), PRED(m), PRED(n),
                PRED(o), PRED(p),
                PRED(q), PRED(r),
            });
        }
    "# };

    /// Each predicate is 1 on exactly its variant; a null object counts as
    /// an int.
    #[tokio::test]
    async fn each_predicate_matches_only_its_variant() {
        const CASES: &[(&str, [i64; 18])] = &[
            (
                "intp",
                [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0],
            ),
            (
                "floatp",
                [0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
            ),
            (
                "stringp",
                [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0],
            ),
            (
                "objectp",
                [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0],
            ),
            (
                "arrayp",
                [0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
            ),
            (
                "mappingp",
                [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0],
            ),
            (
                "functionp",
                [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0],
            ),
            (
                "bytesp",
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1],
            ),
            ("classp", [0; 18]),
        ];

        for (predicate, expected) in CASES {
            let mut expected = expected.to_vec();
            expected.extend([i64::from(*predicate == "classp"); 2]);
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

    #[tokio::test]
    async fn classp_distinguishes_empty_instances_from_null_and_class_arrays() {
        use crate::{interpreter::lpc_ref::LpcRef, test_support::run_prog};
        let result = run_prog(
            r#"
            class empty {}
            int create() {
                class empty absent;
                class empty value = new(class empty);
                class empty *values = ({ value });
                return classp(value) && !classp(absent) && !classp(values) && !classp(0);
            }
        "#,
        )
        .await;
        assert_eq!(result.result(), Some(LpcRef::from(1)));
    }
}
