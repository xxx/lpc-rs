//! Function metadata without invoking pointers or loading their receivers.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{
        callback::function_arg,
        compose::{COMPOSE_EXECUTOR, COMPOSE_RECEIVER_EXECUTOR},
        efun_context::EfunContext,
    },
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtr},
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
};

/// The public argument slots exclude composition operands and dynamic receivers.
fn argument_offset(ptr: &FunctionPtr) -> usize {
    match &ptr.address {
        FunctionAddress::Dynamic(_) => 1,
        FunctionAddress::Local(_, function) if Arc::ptr_eq(function, &COMPOSE_EXECUTOR) => 2,
        FunctionAddress::Local(_, function)
            if Arc::ptr_eq(function, &COMPOSE_RECEIVER_EXECUTOR) =>
        {
            3
        }
        _ => 0,
    }
}

/// A stored receiver, preserving paths and observing this transaction's destructs.
pub(super) fn receiver<const N: usize>(
    context: &EfunContext<'_, N>,
    mut ptr: &FunctionPtr,
) -> LpcRef {
    loop {
        let value = match &ptr.address {
            FunctionAddress::Local(_, function) if Arc::ptr_eq(function, &COMPOSE_EXECUTOR) => {
                let Some(Some(LpcRef::Function(outer))) = ptr.partial_args().first() else {
                    return NULL;
                };
                ptr = outer;
                continue;
            }
            FunctionAddress::Local(_, function)
                if Arc::ptr_eq(function, &COMPOSE_RECEIVER_EXECUTOR) =>
            {
                ptr.partial_args().get(2).and_then(Option::as_ref).cloned()
            }
            FunctionAddress::Local(target, _) => Some(target.clone().into()),
            FunctionAddress::Dynamic(_) => {
                ptr.partial_args().first().and_then(Option::as_ref).cloned()
            }
            FunctionAddress::Efun(_) => Some(ptr.owner.clone().into()),
            FunctionAddress::SimulEfun(name) => {
                context.task_context().simul_efuns().and_then(|target| {
                    target.program.lookup_function(name)?;
                    Some(Arc::downgrade(target).into())
                })
            }
        };
        return match value {
            Some(value @ LpcRef::String(_)) => value,
            Some(value @ LpcRef::Object(_)) if value.live_object(context.txn()).is_some() => value,
            _ => NULL,
        };
    }
}

/// Inspect a pointer's owner, receiver, bound arguments, and composition operands.
pub fn function_info<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let ptr = function_arg(context, "function_info", 0)?;
    let offset = argument_offset(&ptr);
    let kind = match &ptr.address {
        FunctionAddress::Local(_, _) if offset > 0 => "composed",
        FunctionAddress::Local(_, _) => "local",
        FunctionAddress::Dynamic(_) => "dynamic",
        FunctionAddress::Efun(_) => "efun",
        FunctionAddress::SimulEfun(_) => "simul_efun",
    };
    let owner = ptr
        .owner
        .upgrade()
        .filter(|owner| owner.is_live(context.txn()));
    let args = ptr.partial_args().get(offset..).unwrap_or_default();
    let arguments = context.mint_array(args.iter().map(|arg| arg.clone().unwrap_or(NULL)));
    let bound = context.mint_array(args.iter().map(|arg| LpcRef::from(arg.is_some())));
    let components = context.mint_array(
        ptr.partial_args()
            .iter()
            .take(if offset >= 2 { 2 } else { 0 })
            .map(|arg| arg.clone().unwrap_or(NULL)),
    );
    let fields = [
        ("name", ptr.name().into()),
        ("kind", kind.into()),
        (
            "owner",
            owner.map_or(NULL, |owner| Arc::downgrade(&owner).into()),
        ),
        ("receiver", receiver(context, &ptr)),
        ("receiver_bound", ptr.receiver_bound().into()),
        ("arguments", arguments),
        ("bound", bound),
        ("components", components),
    ];
    context.return_mapping(LpcMapping::new(
        fields
            .into_iter()
            .map(|(key, value)| (key.into(), value))
            .collect(),
    ));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{lpc_ref::LpcRef, tests::run},
        test_support::{run_prog, strings_of, try_run_prog},
    };

    #[tokio::test]
    async fn bare_local_and_efun_pointers_use_their_owner_as_receiver() {
        let code = indoc! { r#"
            void foo() { throw("must not run"); }
            mixed *create() {
                function *pointers = ({ foo, &foo(), write, &write(), (: throw("must not run") :) });
                mixed *results = ({});
                foreach (function f : pointers) {
                    mapping info = function_info(f);
                    results += ({
                        info["owner"] == this_object(),
                        info["receiver"] == this_object(),
                        function_object(f) == this_object(),
                        info["receiver_bound"],
                        sizeof(info["arguments"]) == 0,
                        sizeof(info["bound"]) == 0,
                        sizeof(info["components"]) == 0
                    });
                }
                return results;
            }
        "# };
        assert_eq!(strings_of(code).await, vec!["1"; 35]);
    }

    #[tokio::test]
    async fn pointers_keep_their_original_owner_when_inspected_elsewhere() {
        let results = run(
            "",
            &[(
                "/maker.c",
                indoc! { r#"
            void foo() { throw("must not run"); }
            function local_pointer() { return foo; }
            function efun_pointer() { return write; }
        "# },
            )],
            indoc! { r#"
            mixed *create() {
                object maker = find_object("/maker");
                function local = maker->local_pointer();
                function ef = maker->efun_pointer();
                return ({
                    function_info(local)["owner"] == maker,
                    function_info(ef)["owner"] == maker,
                    function_object(local) == maker,
                    function_object(ef) == maker,
                    function_name(local) == "\"/maker\"->foo",
                    function_name(ef) == "\"/maker\"->write"
                });
            }
        "# },
        )
        .await;
        assert_eq!(results, vec![LpcRef::from(1); 6]);
    }

    #[tokio::test]
    async fn explicit_receivers_are_distinct_from_the_pointer_owner() {
        let results = run(
            "",
            &[("/target.c", "void foo() { throw(\"must not run\"); }")],
            indoc! { r#"
            mixed *create() {
                object target = find_object("/target");
                function f = &target->foo();
                mapping info = function_info(f);
                return ({ info["owner"] == this_object(), info["receiver"] == target,
                    function_object(f) == target, info["name"] == "foo", info["kind"] == "local" });
            }
        "# },
        )
        .await;
        assert_eq!(results, vec![LpcRef::from(1); 5]);
    }

    #[tokio::test]
    async fn inherited_and_simulated_functions_report_the_actual_receiver() {
        let code = indoc! { r#"
            inherit "/grandparent";
            mixed *create() {
                mapping local = function_info(grandparent_method);
                mapping simul = function_info(simul_efun);
                return ({
                    local["receiver"] == this_object(),
                    local["owner"] == this_object(),
                    simul["owner"] == this_object(),
                    file_name(simul["receiver"]),
                    function_object(simul_efun) == simul["receiver"],
                    simul["kind"]
                });
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            ["1", "1", "1", "/secure/simul_efuns", "1", "simul_efun"]
        );
    }

    #[tokio::test]
    async fn unavailable_simulated_receivers_return_zero() {
        let code = indoc! { r#"
            mixed *create() {
                function f = &simul_efun("value");
                destruct(function_object(f));
                mapping info = function_info(f);
                return ({ function_object(f) == 0, info["receiver"] == 0,
                    info["owner"] == this_object(), info["arguments"][0] == "value" });
            }
        "# };
        assert_eq!(strings_of(code).await, ["1", "1", "1", "1"]);
    }

    #[tokio::test]
    async fn argument_values_preserve_holes_and_bound_zeroes() {
        let code = indoc! { r#"
            int foo(int a, int b, int c) { return a + b + c; }
            mixed *create() {
                function f = &foo(10, , 0);
                mapping info = function_info(f);
                mapping filled = function_info(papplyv(f, ({ 20 })));
                return info["arguments"] + info["bound"] + filled["arguments"] + filled["bound"];
            }
        "# };
        assert_eq!(
            strings_of(code).await,
            [
                "10", "0", "0", "1", "0", "1", "10", "20", "0", "1", "1", "1"
            ]
        );
    }

    #[tokio::test]
    async fn inspection_returns_fresh_containers_with_normal_value_references() {
        let code = indoc! { r#"
            mixed foo(mixed a, mixed b) { return ({ a, b }); }
            mixed *create() {
                mixed *shared = ({ 7 });
                function f = &foo(10, shared);
                mapping info = function_info(f);
                info["arguments"][0] = 99;
                info["bound"][0] = 0;
                info["arguments"][1][0] = 8;
                mapping again = function_info(f);
                return ({ f()[0], again["arguments"][0], again["bound"][0], f()[1][0] });
            }
        "# };
        assert_eq!(strings_of(code).await, ["10", "10", "1", "8"]);
    }

    #[tokio::test]
    async fn dynamic_receivers_are_separate_from_arguments_and_never_loaded() {
        let code = indoc! { r#"
            mixed *create() {
                function open = &->absent(10, , 0);
                function path = papplyv(open, ({ "/not_a_real_object" }));
                function bound = papplyv(open, ({ this_object() }));
                function invalid = papplyv(open, ({ 42 }));
                mapping info = function_info(path);
                return ({
                    function_info(open)["receiver"] == 0,
                    function_info(open)["receiver_bound"] == 0,
                    function_info(bound)["receiver"] == this_object(),
                    function_object(bound) == this_object(),
                    info["receiver"] == "/not_a_real_object",
                    info["receiver_bound"] == 1,
                    info["kind"] == "dynamic",
                    sizeof(info["arguments"]) == 3,
                    info["arguments"][0] == 10,
                    info["bound"][1] == 0,
                    function_object(path) == 0,
                    find_object("/not_a_real_object") == 0,
                    function_object(open) == 0,
                    function_object(invalid) == 0
                });
            }
        "# };
        assert_eq!(strings_of(code).await, vec!["1"; 14]);
    }

    #[tokio::test]
    async fn composition_exposes_components_separately_from_applied_arguments() {
        let code = indoc! { r#"
            int inc(int n) { return n + 1; }
            int add(int a, int b) { return a + b; }
            mixed *create() {
                function outer = inc;
                function inner = &add(10);
                function f = papplyv(outer @ inner, ({ 20 }));
                mapping info = function_info(f);
                mapping nested = function_info(outer @ f);
                return ({
                    info["kind"] == "composed",
                    info["components"][0] == outer,
                    info["components"][1] == inner,
                    sizeof(info["arguments"]) == 1,
                    info["arguments"][0] == 20,
                    info["bound"][0] == 1,
                    function_object(f) == this_object(),
                    nested["receiver"] == this_object(),
                    sizeof(nested["arguments"]) == 0,
                    f() == 31
                });
            }
        "# };
        assert_eq!(strings_of(code).await, vec!["1"; 10]);
    }

    #[tokio::test]
    async fn compositions_preserve_explicit_and_dynamic_receivers() {
        let results = run(
            "",
            &[("/target.c", "void foo() { throw(\"must not run\"); }")],
            indoc! { r#"
            mixed *create() {
                object target = find_object("/target");
                function explicit = &target->foo() @ &write();
                function open = &->foo() @ &write();
                function nested = open @ &write();
                function bound = papplyv(nested, ({ target, "value" }));
                mapping info = function_info(bound);
                return ({
                    function_object(explicit) == target,
                    function_info(explicit)["owner"] == this_object(),
                    function_info(open)["receiver"] == 0,
                    function_info(open)["receiver_bound"] == 0,
                    function_object(nested) == 0,
                    function_object(bound) == target,
                    info["receiver"] == target,
                    info["receiver_bound"] == 1,
                    sizeof(info["arguments"]) == 1,
                    info["arguments"][0] == "value",
                    sizeof(info["components"]) == 2
                });
            }
        "# },
        )
        .await;
        assert_eq!(results, vec![LpcRef::from(1); 11]);
    }

    #[tokio::test]
    async fn introspection_efuns_work_as_function_pointers() {
        let code = indoc! { r#"
            void foo() {}
            mixed *create() {
                mixed *info = map(({ foo, write }), function_info);
                mixed *objects = map(({ foo, write }), function_object);
                return ({ info[0]["name"] == "foo", info[1]["name"] == "write",
                    objects[0] == this_object(), objects[1] == this_object() });
            }
        "# };
        assert_eq!(strings_of(code).await, ["1", "1", "1", "1"]);
    }

    #[tokio::test]
    async fn non_function_arguments_are_runtime_errors() {
        for name in ["function_info", "function_object"] {
            let code = format!("mixed create() {{ mixed f = 0; return {name}(f); }}");
            let err = try_run_prog(&code).await.unwrap_err().to_string();
            assert!(
                err.contains(&format!("{name}: int is not a function")),
                "{err}"
            );
        }
    }

    #[tokio::test]
    async fn destroyed_owners_are_zero_but_metadata_remains_available() {
        let code = indoc! { r#"
            void foo() {}
            int create() {
                function f = &foo(10);
                destruct(this_object());
                mapping info = function_info(f);
                return info["owner"] == 0 && info["receiver"] == 0
                    && function_object(f) == 0 && info["arguments"][0] == 10;
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }
}
