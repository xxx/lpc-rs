use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::interpreter::{
    efun::{callback::function_arg, efun_context::EfunContext, function_info::receiver},
    lpc_ref::{LpcRef, NULL},
    task_context::ObjectLookup,
};

/// Return a pointer's live receiver without loading or initializing an object.
pub fn function_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let ptr = function_arg(context, "function_object", 0)?;
    let value = receiver(context, &ptr);
    let result = if let LpcRef::String(path) = &value {
        match context
            .task_context()
            .object_path(path.to_str(), "/", "function_object")
        {
            Ok(path) => match context.find_object(&path) {
                ObjectLookup::Found(target) => Arc::downgrade(&target).into(),
                ObjectLookup::Removed | ObjectLookup::NotCreated => NULL,
            },
            Err(_) => NULL,
        }
    } else {
        value
    };
    context.return_efun_result(result);
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, tests::run, vm::Vm},
        test_support::{permissive_master, test_config},
    };

    #[tokio::test]
    async fn finds_transactional_clones_and_observes_their_destruction() {
        let results = run(
            "",
            &[("/target.c", "void foo() {}")],
            indoc! { r#"
                mixed *create() {
                    object target = clone_object("/target");
                    function local = &target->foo();
                    function dynamic = papplyv(&->foo(), ({ target }));
                    function path = papplyv(&->foo(), ({ file_name(target) }));
                    int found = function_object(local) == target
                        && function_object(dynamic) == target && function_object(path) == target;
                    destruct(target);
                    return ({ found, function_object(local) == 0, function_object(dynamic) == 0,
                        function_object(path) == 0, function_info(local)["receiver"] == 0,
                        function_info(local)["owner"] == this_object() });
                }
            "# },
        )
        .await;
        assert_eq!(results, vec![LpcRef::from(1); 6]);
    }

    #[tokio::test]
    async fn an_existing_path_receiver_is_found_without_initializing_it() {
        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let target = vm
            .create_process_from_code(
                "/target.c",
                "void create() { throw(\"must not initialize\"); } void foo() {}",
            )
            .await
            .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/d/inspector.c",
                indoc! { r#"
            int create() {
                function f = papplyv(&->foo(), ({ "target.c" }));
                mapping info = function_info(f);
                return info["receiver"] == "target.c"
                    && function_object(f) == find_object("/target");
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!vm.global_state.is_initialized(&target));
    }

    #[tokio::test]
    async fn a_destroyed_owner_does_not_replace_a_live_explicit_receiver() {
        let results = run(
            "",
            &[
                ("/target.c", "void foo() {}"),
                (
                    "/maker.c",
                    indoc! { r#"
            function pointer() { return &(find_object("/target"))->foo(); }
            function efun_pointer() { return write; }
        "# },
                ),
            ],
            indoc! { r#"
            mixed *create() {
                object maker = find_object("/maker");
                function f = maker->pointer();
                function ef = maker->efun_pointer();
                destruct(maker);
                return ({ function_info(f)["owner"] == 0,
                    function_object(f) == find_object("/target"), function_object(ef) == 0,
                    function_name(ef) == 0 });
            }
        "# },
        )
        .await;
        assert_eq!(results, vec![LpcRef::from(1); 4]);
    }
}
