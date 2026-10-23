use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    compile_time_config::MAX_TASK_CHAIN,
    interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, process::Process},
};

async fn load_prototype<const N: usize>(
    context: &mut EfunContext<'_, N>,
    path: &str,
) -> Result<Arc<Process>> {
    let full_path = context.in_game_path(path);

    if full_path.is_clone() {
        return Err(context.runtime_error(format!("Cannot clone a clone: {}", full_path)));
    }

    context.load_object(path).await
}

/// `clone_object`, the efun for creating new object instances.
pub async fn clone_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg = context.arg(0);

    let path = arg.with_string(|s| s.to_string())?;

    let prototype = load_prototype(context, &path).await?;

    debug_assert!(!prototype.is_clone(), "prototype cannot be a clone");

    {
        if prototype.program(context.txn()).pragmas.no_clone() {
            return Err(context.runtime_error(format!(
                "{} has `#pragma no_clone` enabled, and so cannot be cloned.",
                prototype.program(context.txn()).filename
            )));
        }
    }

    // The message only; `TaskContext::nested` refuses at the same bound —
    // checked before creating anything, so a refusal here leaves nothing to
    // roll back.
    if context.chain_count() >= MAX_TASK_CHAIN {
        return Err(context.runtime_error("infinite clone recursion detected"));
    }

    let new_prog = prototype.program(context.txn());
    let clone_process = context.object_space().create_clone_process(new_prog);
    debug_assert!(clone_process.is_clone(), "new_clone must be a clone");
    let args: Vec<_> = (1..context.arg_count())
        .map(|i| context.arg(i).clone())
        .collect();
    context
        .task_context()
        .insert_and_initialize_with_args(Some(context.chain()), &clone_process, &args)
        .await?;

    let result = LpcRef::from(Arc::downgrade(&clone_process));
    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_core::{RegisterSize, register::RegisterVariant};
    use lpc_rs_utils::config::Config;

    use super::*;
    use crate::{
        assert_regex,
        interpreter::{
            CommittedReader,
            lpc_ref::{LpcRef, NULL},
            program::Program,
            task::Task,
            task_context::TaskContext,
            vm::{Vm, global_state::GlobalState, vm_op::VmOp},
        },
        test_support::{
            compile_prog, lib_holding, permissive_master, temp_lib_config, test_config,
        },
    };

    /// Committed global values by name, read through the committer.
    fn committed_globals_by_name(
        gs: &Arc<GlobalState>,
        proc: &Process,
    ) -> std::collections::HashMap<String, LpcRef> {
        proc.initial_program()
            .global_variables
            .iter()
            .filter_map(|(name, sym)| {
                let RegisterVariant::Global(reg) = sym.location? else {
                    return None;
                };
                Some((name.clone(), gs.committed_global(proc, reg.index())))
            })
            .collect()
    }

    async fn task_context_fixture(
        program: Program,
        config: Arc<Config>,
        tx: tokio::sync::mpsc::Sender<VmOp>,
    ) -> TaskContext {
        let process = Process::new(program);
        let global_state = Arc::new(GlobalState::new(config, tx));
        permissive_master(&global_state.object_space).await;

        TaskContext::new(global_state, process, None)
    }

    #[tokio::test]
    async fn does_not_create_multiple_prototype_objects() {
        let prog = indoc! { r#"
            object foo = clone_object("./example");
        "# };

        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let context = task_context_fixture(program, config, tx).await;

        let mut task = Task::<10>::new(context.clone());
        task.timed_eval(func.clone(), &[], 300)
            .await
            .expect("first task failed");

        let mut task = Task::<10>::new(context);
        task.timed_eval(func, &[], 300)
            .await
            .expect("second task failed");

        // procs are the master, /example, /example#0, /example#1
        assert_eq!(task.context.object_space().len(), 4);
    }

    #[tokio::test]
    async fn returns_error_if_no_clone() {
        let prog = indoc! { r#"
            object foo = clone_object("./no_clone.c");
        "# };

        let (program, config, _) = compile_prog(prog).await;
        let func = program.initializer.clone().expect("no init found?");
        let (tx, _rx) = tokio::sync::mpsc::channel(128);

        let context = task_context_fixture(program, config, tx).await;
        let mut task = Task::<10>::new(context);

        let result = task.timed_eval(func, &[], 300).await;

        assert_regex!(
            result.as_ref().unwrap_err().message(),
            r"no_clone\.c has `#pragma no_clone` enabled, and so cannot be cloned\."
        );
    }

    #[tokio::test]
    async fn initializes_clone_of_an_uninitialized_prototype() {
        let cloned = indoc! { r#"
            int i = 123;
        "# };

        let cloner = indoc! { r#"
            object foo = clone_object("cloned");
        "# };

        let vm = Vm::new(test_config());
        let cloned_proc = vm
            .create_process_from_code("cloned.c", cloned)
            .await
            .unwrap();

        assert_eq!(
            committed_globals_by_name(&vm.global_state, &cloned_proc)
                .get("i")
                .unwrap(),
            &NULL
        );
        assert!(!vm.global_state.is_initialized(&cloned_proc));

        let cloner_proc = vm
            .initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap()
            .context
            .process;
        assert!(vm.global_state.is_initialized(&cloner_proc));

        assert_eq!(
            committed_globals_by_name(&vm.global_state, &cloned_proc)
                .get("i")
                .unwrap(),
            &NULL
        );
        let LpcRef::Object(foo) = committed_globals_by_name(&vm.global_state, &cloner_proc)
            .get("foo")
            .unwrap()
            .clone()
        else {
            panic!("foo is not an object");
        };

        let foo = foo.upgrade().unwrap();
        assert!(vm.global_state.is_initialized(&foo));

        let foo_i = committed_globals_by_name(&vm.global_state, &foo)
            .get("i")
            .unwrap()
            .clone();
        assert_eq!(foo_i, LpcRef::from(123));
    }

    #[tokio::test]
    async fn initializes_clone_of_an_initialized_prototype() {
        let cloned = indoc! { r#"
            int i = 123;
        "# };

        let cloner = indoc! { r#"
            object foo = clone_object("cloned");
        "# };

        let vm = Vm::new(test_config());
        let cloned_proc = vm
            .initialize_process_from_code("cloned.c", cloned)
            .await
            .unwrap()
            .context
            .process;
        assert!(vm.global_state.is_initialized(&cloned_proc));

        let cloner_proc = vm
            .initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap()
            .context
            .process;
        let LpcRef::Object(foo) = committed_globals_by_name(&vm.global_state, &cloner_proc)
            .get("foo")
            .unwrap()
            .clone()
        else {
            panic!("foo is not an object");
        };
        let foo = foo.upgrade().unwrap();

        assert!(vm.global_state.is_initialized(&foo));
        assert_eq!(
            committed_globals_by_name(&vm.global_state, &foo)
                .get("i")
                .unwrap(),
            &LpcRef::from(123)
        );
    }

    #[tokio::test]
    async fn initializes_a_missing_prototype_and_its_clone() {
        let cloner = indoc! { r#"
            object foo = clone_object("/clone_target");
        "# };

        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code("cloner.c", cloner)
            .await
            .unwrap();

        let object_space = &vm.global_state.object_space;
        for key in ["/clone_target", "/clone_target#0"] {
            let process = object_space.lookup(key).unwrap();
            assert!(vm.global_state.is_initialized(&process), "{key}");
            assert_eq!(
                committed_globals_by_name(&vm.global_state, &process)
                    .get("i")
                    .unwrap(),
                &LpcRef::from(123),
                "{key}"
            );
        }
    }

    #[tokio::test]
    async fn forwards_arguments_only_to_the_clone_after_inherited_globals() {
        let root = lib_holding(
            "clone-constructor-inherited",
            &[
                (
                    "base.c",
                    r#"
                    int value = 10;
                    int observed;
                    void create(int amount, int bonus = 7) {
                        observed = value + this_object()->query_child() + amount + bonus;
                    }
                "#,
                ),
                (
                    "child.c",
                    r#"
                    inherit "/base";
                    int child = 100;
                    int query_child() { return child; }
                "#,
                ),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            "caller.c",
            r#"
            object copy = clone_object("/child", 20);
        "#,
        )
        .await
        .unwrap();

        let prototype = vm.global_state.object_space.lookup("/child").unwrap();
        let clone = vm.global_state.object_space.lookup("/child#0").unwrap();
        for (process, expected) in [(&prototype, 117), (&clone, 137)] {
            assert!(vm.global_state.is_initialized(process));
            assert_eq!(
                committed_globals_by_name(&vm.global_state, process)["observed"],
                LpcRef::from(expected)
            );
        }
        assert!(Arc::ptr_eq(
            prototype.initial_program(),
            clone.initial_program()
        ));
    }

    #[tokio::test]
    async fn forwards_spread_values_and_preserves_context_and_collection_identity() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code(
            "target.c",
            r#"
            object caller;
            object player;
            object owner;
            int result;
            int extras;
            void create(object who, int *items, mapping data, function callback, ...) {
                caller = previous_object();
                player = this_player();
                owner = who;
                items[0] = 42;
                data["value"] = 99;
                result = callback();
                extras = argv[0] + argv[1];
            }
        "#,
        )
        .await
        .unwrap();
        let caller = vm
            .initialize_process_from_code(
                "caller.c",
                r#"
            int *items = ({ 1 });
            mapping data = ([ "value": 2 ]);
            object copy;
            int array_value;
            int mapping_value;
            void create() {
                enable_commands();
                set_this_player(this_object());
                mixed *args = ({ this_object(), items, data, (: 123 :), 5, 6 });
                copy = clone_object("/target", args...);
                array_value = items[0];
                mapping_value = data["value"];
            }
        "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let clone = vm.global_state.object_space.lookup("/target#0").unwrap();
        let values = committed_globals_by_name(&vm.global_state, &clone);
        for name in ["caller", "player", "owner"] {
            assert_eq!(
                values[name],
                LpcRef::from(Arc::downgrade(&caller)),
                "{name}"
            );
        }
        assert_eq!(values["result"], LpcRef::from(123));
        assert_eq!(values["extras"], LpcRef::from(11));
        let values = committed_globals_by_name(&vm.global_state, &caller);
        assert_eq!(values["array_value"], LpcRef::from(42));
        assert_eq!(values["mapping_value"], LpcRef::from(99));
    }

    #[tokio::test]
    async fn applies_constructor_defaults_and_ignores_unused_arguments() {
        let root = lib_holding(
            "clone-constructor-defaults",
            &[
                (
                    "target.c",
                    "int value; void create(int amount = 7) { value = amount; }",
                ),
                ("empty.c", "int value = 3;"),
                ("noargs.c", "int value; void create() { value = 4; }"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        vm.initialize_process_from_code(
            "caller.c",
            r#"
            object a = clone_object("/target");
            object b = clone_object("/target", 0);
            object c = clone_object("/target", 12, "unused");
            object d = clone_object("/empty", "unused");
            object e = clone_object("/noargs", "unused");
        "#,
        )
        .await
        .unwrap();
        for (name, expected) in [
            ("/target", 7),
            ("/target#0", 7),
            ("/target#1", 0),
            ("/target#2", 12),
            ("/empty#3", 3),
            ("/noargs#4", 4),
        ] {
            let process = vm.global_state.object_space.lookup(name).unwrap();
            assert_eq!(
                committed_globals_by_name(&vm.global_state, &process)["value"],
                LpcRef::from(expected),
                "{name}"
            );
        }
    }

    #[tokio::test]
    async fn rejects_constructor_type_mismatches_without_leaving_a_clone() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code(
            "target.c",
            r#"
            void create(int amount) {}
        "#,
        )
        .await
        .unwrap();
        let caller = vm
            .initialize_process_from_code(
                "caller.c",
                r#"
            string failure;
            object copy;
            void create() {
                failure = catch(clone_object("/target", "wrong type"));
                copy = clone_object("/target", 42);
            }
        "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let values = committed_globals_by_name(&vm.global_state, &caller);
        assert!(
            values["failure"]
                .with_string(|s| s.to_str().contains("unexpected argument type to `create`"))
                .unwrap()
        );
        assert!(vm.global_state.object_space.lookup("/target#0").is_none());
        assert!(vm.global_state.object_space.lookup("/target#1").is_some());
    }

    #[tokio::test]
    async fn rejects_reference_parameters_and_oversized_constructor_spreads() {
        for (target, arguments, message) in [
            (
                "void create(int ref value) {}",
                "",
                "must be passed by reference",
            ),
            (
                "void create(int ref value) {}",
                ", 1",
                "must be passed by reference",
            ),
            (
                "void create(...) {}",
                ", allocate(65535)...",
                "cannot pass 65535 arguments",
            ),
        ] {
            let vm = Vm::new(test_config());
            vm.create_process_from_code("target.c", target)
                .await
                .unwrap();
            let caller = vm
                .initialize_process_from_code(
                    "caller.c",
                    format!("string failure = catch(clone_object(\"/target\"{arguments}));"),
                )
                .await
                .unwrap()
                .context
                .process;
            let values = committed_globals_by_name(&vm.global_state, &caller);
            let failure = values["failure"].with_string(|s| s.to_string()).unwrap();
            assert!(failure.contains(message), "{failure}");
            assert!(vm.global_state.object_space.lookup("/target#0").is_none());
        }
    }

    #[tokio::test]
    async fn rolls_back_constructor_effects_and_loaded_objects_on_uncaught_error() {
        let root = lib_holding(
            "clone-constructor-rollback",
            &[(
                "target.c",
                r#"
            void create(int fail) {
                if (fail) {
                    "/state"->set_value(42);
                    throw("constructor failed");
                }
            }
        "#,
            )],
        );
        let vm = Vm::new(temp_lib_config(&root));
        permissive_master(&vm.global_state.object_space).await;
        let state = vm
            .initialize_process_from_code(
                "state.c",
                r#"
            int value = 7;
            void set_value(int amount) { value = amount; }
        "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let error = vm
            .initialize_process_from_code(
                "caller.c",
                r#"
            object copy = clone_object("/target", 1);
        "#,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("constructor failed"), "{error}");
        assert_eq!(
            committed_globals_by_name(&vm.global_state, &state)["value"],
            LpcRef::from(7)
        );
        assert!(vm.global_state.object_space.lookup("/target").is_none());
        assert!(vm.global_state.object_space.lookup("/target#0").is_none());
    }

    #[tokio::test]
    async fn clones_its_own_prototype() {
        let caller = indoc! { r#"
            object copy = "/self_copy"->copy();
        "# };

        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let caller_proc = vm
            .initialize_process_from_code("caller.c", caller)
            .await
            .unwrap()
            .context
            .process;

        let LpcRef::Object(copy) = committed_globals_by_name(&vm.global_state, &caller_proc)
            .get("copy")
            .unwrap()
            .clone()
        else {
            panic!("copy is not an object");
        };
        let copy = copy.upgrade().unwrap();

        assert_eq!(copy.filename(), "/self_copy#0");
        assert!(vm.global_state.is_initialized(&copy));
        assert_eq!(
            committed_globals_by_name(&vm.global_state, &copy)
                .get("i")
                .unwrap(),
            &LpcRef::from(123)
        );
    }

    #[tokio::test]
    async fn handles_clone_self_recursion() {
        // Each clone's initializer clones the same path again.
        let prototype = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        let self_clone = indoc! { r#"
            object foo = clone_object("self_clone");
        "# };

        let vm = Vm::new(test_config());
        let _self_clone_proc = vm
            .create_process_from_code("self_clone.c", self_clone)
            .await
            .unwrap();

        let prototype_proc = vm
            .initialize_process_from_code("prototype.c", prototype)
            .await;

        assert!(
            prototype_proc
                .unwrap_err()
                .to_string()
                .contains("infinite clone recursion detected")
        );
    }

    #[tokio::test]
    async fn empties_vars_before_initialization() {
        let prototype = indoc! { r#"
            void create() {
                "/clone"->set_name("proto foo");

                // The clone starts from its own initializer, not from a
                // copy of the prototype's data.
                object student = clone_object("/clone");
            }
        "# };

        let clone = indoc! { r#"
            string name;

            void set_name(string new_name) {
                name = new_name;
            }
        "# };

        let vm = Vm::new(test_config());
        let _clone_proc = vm
            .create_process_from_code("/clone.c", clone)
            .await
            .unwrap();

        let _prototype_proc = vm
            .initialize_process_from_code("/prototype.c", prototype)
            .await
            .unwrap();

        let student = vm.global_state.object_space.lookup("/clone#0").unwrap();

        let slots = vm.global_state.global_slot_count(&student);
        for i in 0..slots {
            assert!(
                vm.global_state
                    .committed_global(&student, i as RegisterSize)
                    .is_null()
            );
        }
    }
}
