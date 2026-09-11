use std::sync::{Arc, LazyLock};

use lpc_rs_asm::instruction::{Arg, ArgList, Instruction};
use lpc_rs_core::{
    RegisterSize, function_arity::FunctionArity, function_flags::FunctionFlags, lpc_type::LpcType,
    register::Register,
};
use lpc_rs_errors::Result;
use lpc_rs_function_support::{
    function_prototype::FunctionPrototypeBuilder,
    program_function::{ProgramFunction, ProgramFunctionBuilder},
};
use thin_vec::thin_vec;

use crate::interpreter::{
    efun::{EFUN_PROTOTYPES, efun_context::EfunContext},
    function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
    lpc_ref::LpcRef,
};

/// Calls `f(g(args...))` with both functions bound in the pointer.
pub static COMPOSE_EXECUTOR: LazyLock<Arc<ProgramFunction>> =
    LazyLock::new(|| compose_executor(false));

/// Calls `f(receiver, g(args...))`, reserving the third parameter for the receiver.
pub static COMPOSE_RECEIVER_EXECUTOR: LazyLock<Arc<ProgramFunction>> =
    LazyLock::new(|| compose_executor(true));

fn compose_executor(dynamic_receiver: bool) -> Arc<ProgramFunction> {
    let num_args = if dynamic_receiver { 3 } else { 2 };
    let local = |index: RegisterSize| Register(index).as_local();
    let argv = local(num_args + 1);
    let inner = local(num_args + 2);
    let result = local(num_args + 3);
    let mut arg_types = vec![LpcType::Function(false), LpcType::Function(false)];
    if dynamic_receiver {
        arg_types.push(LpcType::Mixed(false));
    }
    let prototype = FunctionPrototypeBuilder::default()
        .name(if dynamic_receiver {
            "compose-receiver-executor"
        } else {
            "compose-executor"
        })
        .filename(Arc::new(Default::default()))
        .return_type(LpcType::Mixed(false))
        .arity(FunctionArity::new(num_args))
        .arg_types(arg_types)
        .flags(FunctionFlags::default().with_ellipsis(true))
        .build()
        .unwrap();

    let instructions = vec![
        Instruction::PopulateArgv(argv, num_args, 4),
        Instruction::CallEfun(
            u8::try_from(EFUN_PROTOTYPES.get_index_of("papplyv").unwrap()).unwrap(),
            ArgList(0),
        ),
        Instruction::Copy(local(0), inner),
        Instruction::CallFp(inner, ArgList(1)),
        Instruction::Copy(local(0), result),
        Instruction::CallFp(local(1), ArgList(2)),
        Instruction::Ret,
    ];

    let mut outer_args = Vec::new();
    if dynamic_receiver {
        outer_args.push(Arg::Value(local(3)));
    }
    outer_args.push(Arg::Value(result));
    let arg_lists = vec![
        vec![Arg::Value(local(2)), Arg::Value(argv)],
        vec![],
        outer_args,
    ];
    let debug_spans = vec![None; instructions.len()];

    let func = ProgramFunctionBuilder::default()
        .prototype(prototype)
        .num_locals(4)
        .num_upvalues(0)
        .arg_lists(arg_lists)
        .instructions(instructions)
        .debug_spans(debug_spans)
        .labels(Default::default())
        .local_variables(vec![])
        .arg_locations((1..=num_args).map(local).collect::<Vec<_>>())
        .build()
        .unwrap();

    Arc::new(func)
}

/// `compose`, an efun for composing two functions into a new function.
pub fn compose<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let a = context.arg(0).clone();
    let LpcRef::Function(outer) = &a else {
        return Err(context.runtime_error("non-function sent as first argument to `compose`"));
    };
    let dynamic_receiver = !outer.receiver_bound();

    let b = context.arg(1).clone();
    if !matches!(b, LpcRef::Function(_)) {
        return Err(context.runtime_error("non-function sent as second argument to `compose`"));
    };

    let mut partial_args = thin_vec![Some(a), Some(b)];
    let executor = if dynamic_receiver {
        partial_args.push(None);
        COMPOSE_RECEIVER_EXECUTOR.clone()
    } else {
        COMPOSE_EXECUTOR.clone()
    };

    let ptr = FunctionPtrBuilder::default()
        .owner(Arc::downgrade(context.process()))
        .address(FunctionAddress::Local(
            Arc::downgrade(context.process()),
            executor,
        ))
        .partial_args(partial_args)
        .build()
        .unwrap();

    let lpc_ref = ptr.into();

    context.return_efun_result(lpc_ref);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            tests::{fails, run, run_with},
        },
        test_support::{lib_holding, temp_lib_config},
    };

    #[tokio::test]
    async fn composition_keeps_the_dynamic_receiver_separate_from_the_inner_result() {
        let result = run(
            "",
            &[],
            indoc! { r#"
                int move(object destination) { return destination == this_object(); }
                mixed *create() {
                    function f = &->move() @ &this_object();
                    return ({ f(this_object()) });
                }
            "# },
        )
        .await;

        assert_eq!(result, vec![LpcRef::from(1)]);
    }

    #[tokio::test]
    async fn composition_moves_the_passed_object_to_the_present_result() {
        let result = run(
            "",
            &[
                ("/room.c", ""),
                (
                    "/waferquay.c",
                    indoc! { r#"
                        int id(string name) { return name == "waferquay"; }
                        void create() { move_object("/room"); }
                    "# },
                ),
                (
                    "/mover.c",
                    "int move(object destination) { move_object(destination); return 42; }",
                ),
            ],
            indoc! { r#"
                #define NAME_WAFERQUAY "waferquay"
                mixed *create() {
                    move_object("/room");
                    function f = &->move() @ &present(NAME_WAFERQUAY);
                    object mover = find_object("/mover");
                    return ({
                        f(mover),
                        environment(mover) == find_object("/waferquay")
                    });
                }
            "# },
        )
        .await;

        assert_eq!(result, vec![LpcRef::from(42), LpcRef::from(1)]);
    }

    #[tokio::test]
    async fn composition_passes_arguments_to_the_inner_function() {
        let result = run(
            "",
            &[],
            indoc! { r#"
                int add(int a, int b, int c) { return a + 10 * b + 100 * c; }
                mixed *create() {
                    function f = (: $1 + 1 :) @ &add(, 2);
                    return ({ f(1, 3), f(4, 5) });
                }
            "# },
        )
        .await;

        assert_eq!(result, vec![LpcRef::from(322), LpcRef::from(525)]);
    }

    #[tokio::test]
    async fn dynamic_composition_forwards_remaining_arguments_and_fills_method_holes() {
        let result = run(
            "",
            &[],
            indoc! { r#"
                int move(int a, int b, int c) { return a + 10 * b + 100 * c; }
                mixed *create() {
                    function f = &->move(1, , 3) @ (: $1 * 2 + $2 :);
                    return ({ f(this_object(), 8, 2), f("/main", 4, 2) });
                }
            "# },
        )
        .await;

        assert_eq!(result, vec![LpcRef::from(481), LpcRef::from(401)]);
    }

    #[tokio::test]
    async fn composition_with_a_bound_receiver_passes_all_arguments_to_the_inner_function() {
        let result = run(
            "",
            &[],
            indoc! { r#"
                int move(int a, int b, int c) { return a + 10 * b + 100 * c; }
                mixed *create() {
                    function bound = papplyv(&->move(1, , 3), ({ this_object() }));
                    function f = bound @ (: $1 * 2 + $2 :);
                    function g = &(this_object())->move(1, , 3) @ (: $1 * 2 + $2 :);
                    return ({ f(8, 2), g(8, 2) });
                }
            "# },
        )
        .await;

        assert_eq!(result, vec![LpcRef::from(481); 2]);
    }

    #[tokio::test]
    async fn nested_compositions_preserve_the_receiver_and_allow_it_to_be_bound() {
        let result = run(
            "",
            &[],
            indoc! { r#"
                int move(int n) { return n + 1; }
                mixed *create() {
                    function f = (&->move() @ (: $1 + 1 :)) @ (: $1 * 2 :);
                    function g = &->move() @ (: $1 + 1 :) @ (: $1 * 2 :);
                    function bound = papplyv(f, ({ this_object() }));
                    function h = bound @ (: $1 + 1 :);
                    return ({ f(this_object(), 20), g(this_object(), 20), bound(20), h(19) });
                }
            "# },
        )
        .await;

        assert_eq!(result, vec![LpcRef::from(42); 4]);
    }

    #[tokio::test]
    async fn composed_callbacks_load_receivers_and_keep_missing_methods_as_zero() {
        let root = lib_holding(
            "compose-receiver",
            &[
                ("mover.c", "int move(int n) { return n * 2; }"),
                ("other.c", ""),
            ],
        );
        let result = run_with(
            temp_lib_config(&root),
            "int valid_load(string p, string f, object c, string g) { return 1; }",
            &[],
            indoc! { r#"
                mixed *create() {
                    function f = &->move() @ (: 21 :);
                    return map(({ "/mover", "/other", "/mover" }), f);
                }
            "# },
        )
        .await;

        assert_eq!(
            result,
            vec![LpcRef::from(42), LpcRef::from(0), LpcRef::from(42)]
        );
    }

    #[tokio::test]
    async fn a_composition_cannot_use_the_inner_result_as_a_missing_receiver() {
        let error = fails(
            "",
            &[],
            indoc! { r#"
                int move(object destination) { return 1; }
                mixed *create() {
                    function f = &->move() @ &this_object();
                    return ({ f() });
                }
            "# },
        )
        .await;

        assert!(
            error.contains("needs an object or path as its receiver, got `0`"),
            "{error}"
        );
    }

    #[tokio::test]
    async fn call_out_requires_a_compositions_receiver_to_be_bound() {
        let error = fails(
            "",
            &[],
            indoc! { r#"
                mixed *create() {
                    function f = &->move() @ (: 21 :);
                    call_out(f, 100);
                    return ({});
                }
            "# },
        )
        .await;

        assert_eq!(
            error,
            "runtime error: `call_out` needs the receiver of a dynamic function pointer bound"
        );
    }
}
