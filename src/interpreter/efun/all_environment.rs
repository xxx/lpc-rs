use lpc_rs_errors::Result;

use crate::interpreter::{efun, efun::efun_context::EfunContext, process::Process};

/// `all_environment`, an efun for returning all wrapping environments of an object.
pub async fn all_environment<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    efun::return_objects_of(context, |txn, object| {
        Process::all_environment(txn.clone(), object)
    })
    .await;
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{interpreter::vm::Vm, test_support::test_config};

    #[tokio::test]
    async fn test_all_environment() {
        let inner = indoc! { r#"
            void create() {
                move_object("/outer");
            }
        "# };
        let innermost = indoc! { r#"
            void create() {
                move_object("/inner");
            }
        "# };

        let foo = indoc! { r#"
            void do_moves() {
                move_object("/innermost");
            }
        "# };

        let outer = indoc! { r#"
            void create() {
                move_object("/foo"); // creates a loop, to ensure we handle them.
            }
        "# };

        let master = indoc! { r#"
            object *create() {
                "/foo"->do_moves();
                return all_environment(find_object("/foo"));
            }
        "# };

        let vm = Vm::new(test_config());

        let foo_proc = vm.create_process_from_code("/foo.c", foo).await.unwrap();
        let outer_proc = vm
            .initialize_process_from_code("/outer.c", outer)
            .await
            .unwrap();
        let inner_proc = vm
            .initialize_process_from_code("/inner.c", inner)
            .await
            .unwrap();
        let innermost_proc = vm
            .initialize_process_from_code("/innermost.c", innermost)
            .await
            .unwrap();

        let master_proc = vm
            .initialize_process_from_code("/master.c", master)
            .await
            .unwrap();

        let _ = master_proc
            .result()
            .unwrap()
            .with_array(master_proc.context.txn(), |result| {
                assert_eq!(
                    result.as_ref(),
                    &[
                        &*innermost_proc.context.process,
                        &*inner_proc.context.process,
                        &*outer_proc.context.process,
                        &*foo_proc,
                    ]
                );
            });
    }
}
