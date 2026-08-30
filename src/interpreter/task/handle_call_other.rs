use std::{path::PathBuf, sync::Arc};

use async_recursion::async_recursion;
use itertools::Itertools;
use lpc_rs_core::{lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::{Result, lpc_bug};
use tracing::{instrument, trace};

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_array::LpcArray,
        lpc_mapping::LpcMapping,
        lpc_ref::{LpcRef, NULL},
        process::Process,
        task::{Arg, Task, get_location},
        task_context::{ObjectLookup, TaskContext},
    },
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) async fn handle_call_other(
        &mut self,
        receiver: RegisterVariant,
        name_location: RegisterVariant,
    ) -> Result<()> {
        let (receiver_ref, function_name) = {
            let receiver_ref = get_location(&self.stack, &self.context.txn, receiver)?.into_owned();
            let name_ref = &*get_location(&self.stack, &self.context.txn, name_location)?;
            let Ok(function_name) = name_ref.with_string(|s| s.to_string()) else {
                let str = format!("Invalid name passed to `call_other`: {}", name_ref);
                return Err(self.runtime_error(str));
            };
            (receiver_ref, function_name)
        };
        trace!("Calling call_other: {}->{}", receiver_ref, function_name);

        if self.args.iter().any(|arg| matches!(arg, Arg::Ref(_))) {
            return Err(self.runtime_bug("a by-reference argument reached call_other"));
        }

        let result = match &receiver_ref {
            LpcRef::String(_) | LpcRef::Object(_) => {
                let resolved =
                    Self::resolve_call_other_receiver(&receiver_ref, &function_name, &self.context)
                        .await?;
                let Some(receiver) = resolved else {
                    self.stack.current_frame_mut()?.registers[0] = NULL;
                    return Ok(());
                };
                let function = receiver
                    .program
                    .lookup_function(&function_name)
                    .expect("resolve_call_other_receiver checked the function is present")
                    .clone();
                if !function.public() {
                    NULL
                } else {
                    debug_assert!(!function.prototype.is_efun(), "a `->` callee has a body");
                    // The callee returns through `pop_frame!`'s `copy_result`.
                    let frame = self.prepare_new_call_frame(receiver, function).await?;
                    self.stack.push(frame)?;
                    return Ok(());
                }
            }
            LpcRef::Array(_) => {
                let args = self.arg_values()?;
                let mut refs = receiver_ref
                    .with_array(&self.context.txn, |a| a.iter().cloned().collect_vec())?;
                for lpc_ref in &mut refs {
                    let result =
                        Self::resolve_result(lpc_ref, &function_name, &args, &self.context).await?;
                    *lpc_ref = result;
                }
                LpcRef::Array(self.context.txn.with(|t| t.mint_array(LpcArray::new(refs))))
            }
            LpcRef::Mapping(_) => {
                let args = self.arg_values()?;
                let mut map = receiver_ref.with_mapping(&self.context.txn, |m| {
                    m.iter().map(|(k, v)| (k.clone(), v.clone())).collect_vec()
                })?;
                for (_key_ref, value_ref) in map.iter_mut() {
                    let result =
                        Self::resolve_result(value_ref, &function_name, &args, &self.context)
                            .await?;
                    *value_ref = result;
                }
                LpcRef::Mapping(
                    self.context
                        .txn
                        .with(|t| t.mint_mapping(LpcMapping::new(map.into_iter().collect()))),
                )
            }
            _ => {
                return Err(self
                    .runtime_error(format!("What are you trying to call `{function_name}` on?")));
            }
        };

        self.stack.current_frame_mut()?.registers[0] = result;
        Ok(())
    }

    /// The staged arguments as values; refs were rejected before this.
    fn arg_values(&self) -> Result<Vec<LpcRef>> {
        self.args
            .iter()
            .map(|arg| match *arg {
                Arg::Value(loc) => {
                    get_location(&self.stack, &self.context.txn, loc).map(|r| r.into_owned())
                }
                Arg::Ref(_) => Err(self.runtime_bug("a by-reference argument reached call_other")),
            })
            .collect()
    }

    #[async_recursion]
    async fn resolve_result<T>(
        receiver_ref: &LpcRef,
        function_name: T,
        args: &[LpcRef],
        task_context: &TaskContext,
    ) -> Result<LpcRef>
    where
        T: AsRef<str> + Send + Sync,
    {
        let resolved = Task::<MAX_CALL_STACK_SIZE>::resolve_call_other_receiver(
            receiver_ref,
            function_name.as_ref(),
            task_context,
        )
        .await?;

        if let Some(receiver) = resolved {
            let new_context = task_context.nested(receiver.clone())?;
            let mut task: Task<MAX_CALL_STACK_SIZE> = Task::new(new_context);

            // unwrap() is ok because resolve_call_other_receiver() checks
            // for the function's presence.
            let function = receiver
                .program
                .lookup_function(function_name.as_ref())
                .unwrap()
                .clone();

            let result = if function.public() {
                let max_execution_time = task_context.config().max_execution_time;
                task.timed_eval(function, args, max_execution_time).await?;

                let Some(r) = task.context.into_result() else {
                    return Err(lpc_bug!(
                        "resolve_result finished the task, but it has no result? wtf."
                    ));
                };

                r
            } else {
                NULL
            };

            Ok(result)
        } else {
            Ok(NULL)
        }
    }

    #[instrument(level = "debug", skip_all)]
    async fn resolve_call_other_receiver<T>(
        receiver_ref: &LpcRef,
        name: T,
        context: &TaskContext,
    ) -> Result<Option<Arc<Process>>>
    where
        T: AsRef<str>,
    {
        let process = match receiver_ref {
            LpcRef::String(_) => {
                let path = receiver_ref.with_string(|s| LpcPath::InGame(PathBuf::from(s)))?;

                match context.find_object(&path) {
                    ObjectLookup::Found(proc) => proc,
                    // A destructed object: don't create (that would resurrect
                    // it); the call below short-circuits.
                    ObjectLookup::Removed => return Ok(None),
                    // NotCreated: create-on-miss, transactionally.
                    ObjectLookup::NotCreated => {
                        let process = context.compile_process(&path).await?;
                        context.insert_process_transactional(&process);
                        process
                    }
                }
            }
            LpcRef::Object(_) => match receiver_ref.live_object(context.txn()) {
                Some(proc) => proc,
                None => return Ok(None),
            },
            _ => return Ok(None),
        };

        // If uninitialized, it's time to set that up. Note that we do this regardless
        // of whether the function exists or not, because this is a primary way of
        // initializing objects. If you've ever seen a call_other to some knowingly
        // undefined function in old lib code, this is why.
        let result = if !process.is_initialized(context.txn()) {
            Self::initialize_process(context.nested(process)?)
                .await?
                .context
                .process
        } else {
            process
        };

        // Only switch the process if there's actually a function to
        // call by this name on the other side.
        Ok(result.program.contains_function(name).then_some(result))
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::test_config,
    };

    const OTHER: &str = indoc! { r#"
        string who() { return file_name(this_object()); }
        private int hidden() { return 7; }
        int two() { return 2; }
        void take(int ref x) { x = 1; }
    "# };

    async fn vm_with_other() -> Vm {
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/other.c", OTHER)
            .await
            .unwrap();
        vm
    }

    /// 1000 `->` calls deep fits the one stack; a `Task` per call aborted
    /// the process near 50.
    #[tokio::test]
    async fn recursion_through_call_other_runs_on_the_one_stack() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int depth;
            int f(int n) { if (n < 1000) return this_object()->f(n + 1); return n; }
            void create() { depth = f(0); }
        "# };
        let process = vm
            .initialize_process_from_code("/deep.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&process, 0u16),
            LpcRef::from(1000)
        );
    }

    #[tokio::test]
    async fn recursion_past_the_stack_is_an_lpc_error() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int f(int n) { if (n < 2000) return this_object()->f(n + 1); return n; }
            void create() { f(0); }
        "# };
        let err = vm
            .initialize_process_from_code("/deeper.c", code)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("stack overflow"), "{err}");
    }

    #[tokio::test]
    async fn catch_takes_an_error_thrown_across_call_other() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            string caught;
            int after;
            void boom() { throw("kaboom"); }
            void create() {
                caught = catch(this_object()->boom());
                after = 1;
            }
        "# };
        let process = vm
            .initialize_process_from_code("/catcher.c", code)
            .await
            .unwrap()
            .context
            .process;
        let caught = vm.global_state.committed_global(&process, 0u16);
        let text = caught.with_string(|s| s.to_string()).unwrap_or_default();
        assert!(text.contains("kaboom"), "{text:?}");
        assert_eq!(
            vm.global_state.committed_global(&process, 1u16),
            LpcRef::from(1),
            "the caller continues after the catch"
        );
    }

    #[tokio::test]
    async fn the_callee_sees_itself_as_this_object() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            string seen;
            void create() { seen = "/other"->who(); }
        "# };
        let process = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap()
            .context
            .process;
        let seen = vm.global_state.committed_global(&process, 0u16);
        assert_eq!(
            seen.with_string(|s| s.to_string()).unwrap_or_default(),
            "/other"
        );
    }

    #[tokio::test]
    async fn a_private_function_is_zero() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            int got;
            void create() { got = "/other"->hidden() + 1; }
        "# };
        let process = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&process, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn an_array_receiver_yields_each_result() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            int total;
            void create() {
                int *r = ({ "/other", "/other" })->two();
                total = r[0] + r[1];
            }
        "# };
        let process = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&process, 0u16),
            LpcRef::from(4)
        );
    }

    /// `catch`'s bulk `truncate` has to unwind many `->` frames at once, not
    /// just the one a plain call would have left.
    #[tokio::test]
    async fn catch_unwinds_a_deep_call_other_chain() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            string caught;
            int after;
            int f(int n) { if (n < 500) return this_object()->f(n + 1); throw("deep"); }
            void create() {
                caught = catch(f(0));
                after = 1;
            }
        "# };
        let process = vm
            .initialize_process_from_code("/unwind.c", code)
            .await
            .unwrap()
            .context
            .process;
        let caught = vm.global_state.committed_global(&process, 0u16);
        let text = caught.with_string(|s| s.to_string()).unwrap_or_default();
        assert!(text.contains("deep"), "{text:?}");
        assert_eq!(
            vm.global_state.committed_global(&process, 1u16),
            LpcRef::from(1)
        );
    }

    /// Two distinct objects volleying `->` calls share the one stack too,
    /// not just self-recursion through `this_object()`.
    #[tokio::test]
    async fn mutual_recursion_through_call_other() {
        let vm = Vm::new(test_config());
        vm.create_process_from_code(
            "/ping.c",
            indoc! { r#"
                int ping(int n) { if (n <= 0) return 0; return "/pong"->pong(n - 1) + 1; }
            "# },
        )
        .await
        .unwrap();
        vm.create_process_from_code(
            "/pong.c",
            indoc! { r#"
                int pong(int n) { if (n <= 0) return 0; return "/ping"->ping(n - 1) + 1; }
            "# },
        )
        .await
        .unwrap();
        let code = indoc! { r#"
            int depth;
            void create() { depth = "/ping"->ping(200); }
        "# };
        let process = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&process, 0u16),
            LpcRef::from(200)
        );
    }

    #[tokio::test]
    async fn a_ref_parameter_is_refused_across_call_other() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            void create() { int v; "/other"->take(v); }
        "# };
        let err = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("by reference"), "{err}");
    }
}
