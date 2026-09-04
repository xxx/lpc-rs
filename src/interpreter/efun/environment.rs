use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, lpc_ref::NULL};

pub async fn environment<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let arg_ref = context.arg(0);
    let result = efun::arg_or_this_object(arg_ref, context)
        .await?
        .and_then(|object| {
            context
                .txn()
                .with(|t| t.read(object.position.environment.id))
        })
        .unwrap_or(NULL);

    context.return_efun_result(result);

    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{permissive_master, test_config},
    };

    #[tokio::test]
    async fn environment_by_path() {
        let foo = indoc! { r#"
            void do_moves() {
                move_object("/bar");
            }
        "# };

        let master = indoc! { r#"
            object create() {
                "/foo"->do_moves();
                return environment("/foo");
            }
        "# };

        let vm = Vm::new(test_config());

        let foo_proc = vm.create_process_from_code("/foo.c", foo).await.unwrap();
        let _bar_proc = vm.create_process_from_code("/bar.c", "").await.unwrap();

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let LpcRef::Object(result) = master_proc.result().unwrap() else {
            panic!("Expected object result");
        };

        assert_eq!(
            vm.global_state.committed_environment(&foo_proc),
            Some(result.upgrade().unwrap())
        );
    }

    #[tokio::test]
    async fn environment_of_an_unloadable_path_errors() {
        let master = indoc! { r#"
            mixed create() {
                return environment("/nonexistent");
            }
        "# };

        let vm = Vm::new(test_config());
        permissive_master(&vm.global_state.object_space).await;
        let error = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .expect_err("the path cannot load");

        assert!(error.to_string().contains("nonexistent"), "{error}");
    }

    #[tokio::test]
    async fn test_environment() {
        let foo = indoc! { r#"
            void do_moves() {
                move_object("/bar");
            }
        "# };

        let master = indoc! { r#"
            object create() {
                "/foo"->do_moves();
                return environment(find_object("/foo"));
            }
        "# };

        let vm = Vm::new(test_config());

        let foo_proc = vm.create_process_from_code("/foo.c", foo).await.unwrap();
        let _bar_proc = vm.create_process_from_code("/bar.c", "").await.unwrap();

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let LpcRef::Object(result) = master_proc.result().unwrap() else {
            panic!("Expected object result");
        };

        let result = result.upgrade().unwrap();

        assert_eq!(
            vm.global_state.committed_environment(&foo_proc),
            Some(result)
        );
    }
}
