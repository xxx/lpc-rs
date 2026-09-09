//! Names of function pointers without invoking them or resolving dynamic targets.

use lpc_rs_errors::Result;
use lpc_rs_utils::string::MAX_STRING_LENGTH;

use crate::interpreter::{
    efun::{callback::function_arg, efun_context::EfunContext},
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    lpc_ref::{LpcRef, NULL},
};

/// `function_name(f)`: a closure's printable name, or 0 for a destructed target.
pub fn function_name<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let ptr = function_arg(context, "function_name", 0)?;
    let Some(mut name) = target_name(context, &ptr) else {
        context.return_efun_result(NULL);
        return Ok(());
    };
    let args = if matches!(ptr.address, FunctionAddress::Dynamic(_)) {
        ptr.partial_args().get(1..).unwrap_or_default()
    } else {
        ptr.partial_args()
    };
    let suffix_len = if args.is_empty() {
        0
    } else {
        2 * args.len() + 2
    };
    if name.len() + suffix_len > MAX_STRING_LENGTH {
        return Err(context.runtime_error("function_name: result exceeds maximum string length"));
    }
    if !args.is_empty() {
        name.reserve(suffix_len);
        name.insert(0, '&');
        name.push('(');
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                name.push(',');
            }
            name.push(if arg.is_some() { '?' } else { '_' });
        }
        name.push(')');
    }
    context.return_efun_result(name.into());
    Ok(())
}

fn target_name<const N: usize>(context: &EfunContext<'_, N>, ptr: &FunctionPtr) -> Option<String> {
    let target = match &ptr.address {
        FunctionAddress::Local(target, _) => target.upgrade()?,
        FunctionAddress::SimulEfun(name) => {
            let target = context.task_context().simul_efuns()?;
            target.program.lookup_function(name)?;
            target.clone()
        }
        FunctionAddress::Efun(name) => return Some(name.to_string()),
        FunctionAddress::Dynamic(name) => {
            return match ptr.partial_args().first() {
                Some(Some(receiver @ LpcRef::Object(_))) => {
                    let target = receiver.live_object(context.txn())?;
                    Some(format!(
                        "\"{}\"->{name}",
                        target.in_game_name(context.config().paths())
                    ))
                }
                Some(Some(LpcRef::String(path))) => Some(format!("\"{path}\"->{name}")),
                Some(Some(_)) => None,
                Some(None) | None => Some(format!("->{name}")),
            };
        }
    };
    target.is_live(context.txn()).then(|| {
        format!(
            "\"{}\"->{}",
            target.in_game_name(context.config().paths()),
            ptr.name()
        )
    })
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, vm::Vm},
        test_support::{run_prog, strings_of, test_config, try_run_prog},
    };

    #[tokio::test]
    async fn local_names_match_the_mudlibs_keep_property_check() {
        let code = indoc! { r#"
            int keep_obj_m_no_sell() { throw("must not run"); }
            mixed *create() {
                function f = keep_obj_m_no_sell;
                return ({
                    function_name(f),
                    wildmatch("*->keep_obj_m_no_sell", function_name(f))
                });
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            [r#""/my_file"->keep_obj_m_no_sell"#, "1"]
        );
    }

    #[tokio::test]
    async fn inherited_functions_name_the_receiving_object() {
        let code = indoc! { r#"
            inherit "/grandparent";
            string create() { return function_name(&grandparent_method()); }
        "# };
        assert_eq!(
            run_prog(code).await.result().unwrap().as_str(),
            Some(r#""/my_file"->grandparent_method"#)
        );
    }

    #[tokio::test]
    async fn bound_arguments_are_masked_and_holes_are_distinguished() {
        let code = indoc! { r#"
            int add(int a, int b, int c) { return a + b + c; }
            mixed *create() {
                return ({
                    function_name(&add(1, , 3)),
                    function_name(&write("secret")),
                    function_name(&add(, , ))
                });
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            [
                r#"&"/my_file"->add(?,_,?)"#,
                "&write(?)",
                r#"&"/my_file"->add(_,_,_)"#
            ]
        );
    }

    #[tokio::test]
    async fn efun_names_work_through_a_function_name_pointer() {
        let code = indoc! { r#"
            mixed *create() {
                return map(({ &write(), &reduce(), &function_name() }), &function_name());
            }
        "# };
        assert_eq!(strings_of(code).await, ["write", "reduce", "function_name"]);
    }

    #[tokio::test]
    async fn dynamic_names_do_not_resolve_or_load_the_receiver() {
        let code = indoc! { r#"
            mixed *create() {
                return ({
                    function_name(&->absent()),
                    function_name(&->absent(1)),
                    function_name(papplyv(&->absent(1), ({ "/not_a_real_object" }))),
                    find_object("/not_a_real_object")
                });
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            [
                "->absent",
                "&->absent(?)",
                r#"&"/not_a_real_object"->absent(?)"#,
                "0"
            ]
        );
    }

    #[tokio::test]
    async fn closures_expose_their_generated_unmangled_name() {
        let code = r#"string create() { return function_name((: throw("must not run") :)); }"#;
        assert_eq!(
            run_prog(code).await.result().unwrap().as_str(),
            Some(r#""/my_file"->closure-0"#)
        );
    }

    #[tokio::test]
    async fn clone_names_include_the_receivers_clone_id() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code(
            "/target.c",
            "void method() {} function pointer() { return &method(); }",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/user.c",
                indoc! { r#"
            int create() {
                object target = clone_object("/target");
                return function_name(target->pointer()) == "\"" + file_name(target) + "\"->method";
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn destructed_receivers_return_zero_within_the_transaction() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code(
            "/target.c",
            "void method() {} function pointer() { return &method(); }",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/user.c",
                indoc! { r#"
            int create() {
                object target = find_object("/target");
                function local = target->pointer();
                function dynamic = papplyv(&->method(), ({ target }));
                destruct(target);
                return function_name(local) == 0 && function_name(dynamic) == 0;
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
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

    #[tokio::test]
    async fn long_bound_argument_lists_respect_the_string_limit() {
        let code = "string create() { return function_name(papplyv(&write(), allocate(8192))); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("function_name: result exceeds maximum string length"),
            "{err}"
        );
    }
}
