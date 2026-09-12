use lpc_rs_errors::Result;

use crate::interpreter::efun::{callback::function_arg, efun_context::EfunContext};

/// Return the stored function name without inspecting its receiver or arguments.
pub fn function_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let ptr = function_arg(context, "function_name", 0)?;
    context.return_efun_result(ptr.name().into());
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, tests::run},
        test_support::{strings_of, try_run_prog},
    };

    #[tokio::test]
    async fn named_pointers_return_only_the_function_name() {
        let code = indoc! { r#"
            inherit "/grandparent";
            void foo(int a, int b) { throw("must not run"); }
            mixed *create() {
                function f = foo;
                return map(({ f, &foo(), &foo(10, ), write, &write("secret"),
                    grandparent_method, simul_efun, &simul_efun("secret"),
                    &function_name(), &function_description() }), function_name);
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            [
                "foo",
                "foo",
                "foo",
                "write",
                "write",
                "grandparent_method",
                "simul_efun",
                "simul_efun",
                "function_name",
                "function_description"
            ]
        );
    }

    #[tokio::test]
    async fn names_survive_receiver_and_owner_destruction() {
        let results = run(
            "",
            &[(
                "/target.c",
                indoc! { r#"
            void method() { throw("must not run"); }
            function pointer() { return method; }
            function efun_pointer() { return write; }
        "# },
            )],
            indoc! { r#"
            mixed *create() {
                object target = find_object("/target");
                function explicit = &target->method();
                function local = target->pointer();
                function ef = target->efun_pointer();
                function dynamic = papplyv(&->method(), ({ target }));
                destruct(target);
                return map(({ explicit, local, ef, dynamic }), function_name);
            }
        "# },
        )
        .await;
        assert_eq!(
            results,
            ["method", "method", "write", "method"].map(LpcRef::from)
        );
    }

    #[tokio::test]
    async fn dynamic_names_do_not_depend_on_receiver_resolution() {
        let code = indoc! { r#"
            mixed *create() {
                function f = &->missing(10, );
                return ({ function_name(f),
                    function_name(papplyv(f, ({ "/not_a_real_object" }))),
                    function_name(papplyv(f, ({ 42 }))),
                    find_object("/not_a_real_object") });
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            ["missing", "missing", "missing", "0"]
        );
    }

    #[tokio::test]
    async fn unavailable_simul_efun_targets_still_have_names() {
        let code = indoc! { r#"
            mixed *create() {
                function f = &simul_efun("secret");
                destruct(function_object(f));
                return ({ function_name(f), function_description(f) });
            }
        "# };
        assert_eq!(strings_of(code).await, ["simul_efun", "0"]);
    }

    #[tokio::test]
    async fn generated_names_match_the_metadata_without_calling_the_pointer() {
        let code = indoc! { r#"
            mixed *create() {
                function f = (: throw("must not run") :);
                function *pointers = ({ f, &operator(+)(10), f @ &write() });
                mixed *names = map(pointers, function_name);
                return ({ names[0] == function_info(pointers[0])["name"],
                    names[1] == function_info(pointers[1])["name"],
                    names[2] == function_info(pointers[2])["name"],
                    wildmatch("*->*", names[0]) == 0,
                    wildmatch("*->*", names[1]) == 0,
                    wildmatch("*->*", names[2]) == 0 });
            }
        "# };
        assert_eq!(strings_of(code).await, ["1"; 6]);
    }

    #[tokio::test]
    async fn large_argument_lists_do_not_affect_the_name() {
        let code =
            "mixed *create() { return ({ function_name(papplyv(write, allocate(8192))) }); }";
        assert_eq!(strings_of(code).await, ["write"]);
    }

    #[tokio::test]
    async fn non_function_arguments_are_runtime_errors() {
        let code = "mixed create() { mixed f = 0; return function_name(f); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("function_name: int is not a function"),
            "{err}"
        );
    }
}
