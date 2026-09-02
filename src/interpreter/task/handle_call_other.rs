use std::sync::Arc;

use itertools::Itertools;
use lpc_rs_core::{RegisterSize, register::RegisterVariant};
use lpc_rs_errors::Result;
use tracing::{instrument, trace};

use crate::interpreter::{
    call_frame::{CallFrame, CollectionCall},
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::VarId,
    task::{Arg, Task, get_location},
    task_context::{Loader, ObjectLookup, TaskContext},
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
                let loader = self.loader()?;
                let resolved = Self::resolve_call_other_receiver(
                    &receiver_ref,
                    &function_name,
                    &self.context,
                    &loader,
                )
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
            LpcRef::Array(_) | LpcRef::Mapping(_) => {
                let args = self.arg_values()?;
                let (remaining, keys) = match &receiver_ref {
                    LpcRef::Array(_) => {
                        let mut receivers = receiver_ref
                            .with_array(&self.context.txn, |a| a.iter().cloned().collect_vec())?;
                        receivers.reverse();
                        (receivers, None)
                    }
                    _ => {
                        let pairs = receiver_ref.with_mapping(&self.context.txn, |m| {
                            m.iter().map(|(k, v)| (k.clone(), v.clone())).collect_vec()
                        })?;
                        let (keys, mut receivers): (Vec<_>, Vec<_>) = pairs.into_iter().unzip();
                        receivers.reverse();
                        (receivers, Some(keys))
                    }
                };
                let results = Vec::with_capacity(remaining.len());
                self.stack.current_frame_mut()?.pending = Some(Box::new(CollectionCall {
                    name: function_name,
                    args,
                    remaining,
                    keys,
                    results,
                    owed: false,
                }));
                return self.advance_collection_call().await;
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

    /// Drive the current frame's collection `->`: bank the owed `r0`, push
    /// the next receiver that has the function, or mint the results into
    /// `r0` and clear the call.
    pub(crate) async fn advance_collection_call(&mut self) -> Result<()> {
        loop {
            let step = {
                let frame = self.stack.current_frame_mut()?;
                let r0 = frame.registers[0].clone();
                let Some(mut call) = frame.pending.take() else {
                    return Err(self.runtime_bug(
                        "advance_collection_call runs on a frame with no pending call",
                    ));
                };
                if call.owed {
                    call.results.push(r0);
                    call.owed = false;
                }
                match call.remaining.pop() {
                    Some(receiver) => {
                        let name = call.name.clone();
                        frame.pending = Some(call);
                        Step::Call(receiver, name)
                    }
                    None => Step::Done(*call),
                }
            };
            match step {
                Step::Done(call) => {
                    let value = match call.keys {
                        None => LpcRef::Array(
                            self.context
                                .txn
                                .with(|t| t.mint_array(LpcArray::new(call.results))),
                        ),
                        Some(keys) => {
                            debug_assert_eq!(keys.len(), call.results.len());
                            LpcRef::Mapping(self.context.txn.with(|t| {
                                t.mint_mapping(LpcMapping::new(
                                    keys.into_iter().zip(call.results).collect(),
                                ))
                            }))
                        }
                    };
                    self.stack.current_frame_mut()?.registers[0] = value;
                    return Ok(());
                }
                Step::Call(receiver, name) => {
                    let Some(frame) = self.collection_element_frame(&receiver, &name).await? else {
                        self.pending_call_mut()?.results.push(NULL);
                        continue;
                    };
                    self.pending_call_mut()?.owed = true;
                    self.stack.push(frame)?;
                    return Ok(());
                }
            }
        }
    }

    /// The current frame's collection call.
    fn pending_call_mut(&mut self) -> Result<&mut CollectionCall> {
        let bug = self.runtime_bug("a collection call is in flight");
        self.stack
            .current_frame_mut()?
            .pending
            .as_deref_mut()
            .ok_or(bug)
    }

    /// The frame for one receiver of a collection call, built from the
    /// captured values; `None` when it has no live object or no public
    /// function (its slot is 0).
    async fn collection_element_frame(
        &mut self,
        receiver: &LpcRef,
        name: &str,
    ) -> Result<Option<CallFrame>> {
        let loader = self.loader()?;
        let Some(process) =
            Self::resolve_call_other_receiver(receiver, name, &self.context, &loader).await?
        else {
            return Ok(None);
        };
        let function = process
            .program
            .lookup_function(name)
            .expect("resolve_call_other_receiver checked the function is present")
            .clone();
        if !function.public() {
            return Ok(None);
        }
        debug_assert!(!function.prototype.is_efun(), "a `->` callee has a body");
        let args = self.pending_call_mut()?.args.clone();
        // `push_arg` only refuses a `ref` parameter it is given a value for.
        if let Some(i) = function.prototype.first_ref_param()
            && i >= args.len()
        {
            return Err(self.runtime_error(format!(
                "argument {} of `{}` must be passed by reference",
                i + 1,
                name
            )));
        }
        let mut frame = CallFrame::new(
            process,
            function,
            RegisterSize::try_from(args.len())?,
            None::<&[VarId]>,
        );
        for (i, arg) in args.into_iter().enumerate() {
            frame.push_arg(&self.context.txn, i, arg)?;
        }
        Ok(Some(frame))
    }

    /// The identity a `->` from the current frame loads under.
    fn loader(&self) -> Result<Loader> {
        let frame = self.stack.current_frame()?;
        Ok(Loader {
            func: "call_other".to_string(),
            caller: frame.process.clone(),
            program: self
                .stack
                .calling_program(self.context.config().lib_dir.as_str()),
        })
    }

    #[instrument(level = "debug", skip_all)]
    async fn resolve_call_other_receiver<T>(
        receiver_ref: &LpcRef,
        name: T,
        context: &TaskContext,
        loader: &Loader,
    ) -> Result<Option<Arc<Process>>>
    where
        T: AsRef<str>,
    {
        let process = match receiver_ref {
            LpcRef::String(_) => {
                let path = receiver_ref
                    .with_string(|s| context.object_path(s.to_str(), "/", "call_other"))??;

                match context.find_object(&path) {
                    ObjectLookup::Found(proc) => proc,
                    // A destructed object: don't create (that would resurrect
                    // it); the call below short-circuits.
                    ObjectLookup::Removed => return Ok(None),
                    // NotCreated: create-on-miss, transactionally.
                    ObjectLookup::NotCreated => {
                        let process = context.compile_process(&path, loader).await?;
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

/// What `advance_collection_call` does next.
enum Step {
    /// Call `receiver`'s function of this name.
    Call(LpcRef, String),
    /// Every receiver is called; mint the results.
    Done(CollectionCall),
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
        int boom() { throw("second"); }
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

    /// `catch`'s bulk `truncate` unwinds many `->` frames at once.
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

    /// Two objects volleying `->` calls share the one stack.
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

    /// A collection `->` used to build a `Task` per element; 500 deep on
    /// the one stack.
    #[tokio::test]
    async fn collection_recursion_runs_on_the_one_stack() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int depth;
            int f(int n) {
                if (n < 500) {
                    int *r = ({ this_object() })->f(n + 1);
                    return r[0];
                }
                return n;
            }
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
            LpcRef::from(500)
        );
    }

    #[tokio::test]
    async fn collection_recursion_past_the_stack_is_an_lpc_error() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int f(int n) {
                if (n < 2000) {
                    int *r = ({ this_object() })->f(n + 1);
                    return r[0];
                }
                return n;
            }
            void create() { f(0); }
        "# };
        let err = vm
            .initialize_process_from_code("/deeper.c", code)
            .await
            .unwrap_err();
        assert!(err.to_string().contains("stack overflow"), "{err}");
    }

    #[tokio::test]
    async fn a_mapping_receiver_keeps_its_keys() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            int total;
            int size;
            void create() {
                mapping m = ([ "a": "/other", "b": "/other" ])->two();
                size = sizeof(m);
                total = m["a"] * 10 + m["b"];
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
            LpcRef::from(22)
        );
        assert_eq!(
            vm.global_state.committed_global(&process, 1u16),
            LpcRef::from(2)
        );
    }

    #[tokio::test]
    async fn a_missing_function_is_zero_in_its_slot() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            int total;
            void create() {
                int *r = ({ "/other", this_object(), "/other" })->two();
                total = r[0] * 100 + r[1] * 10 + r[2];
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
            LpcRef::from(202)
        );
    }

    /// The catch frame survives with the collection call in flight; a later
    /// `->` in the same frame must not resume it.
    #[tokio::test]
    async fn a_caught_element_error_leaves_no_dead_call() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            string caught;
            int later;
            void create() {
                caught = catch(({ "/other", "/other" })->boom());
                later = "/other"->two();
            }
        "# };
        let process = vm
            .initialize_process_from_code("/main.c", code)
            .await
            .unwrap()
            .context
            .process;
        let caught = vm.global_state.committed_global(&process, 0u16);
        let text = caught.with_string(|s| s.to_string()).unwrap_or_default();
        assert!(text.contains("second"), "{text:?}");
        assert_eq!(
            vm.global_state.committed_global(&process, 1u16),
            LpcRef::from(2),
            "the plain -> after the catch returns its own value"
        );
    }

    #[tokio::test]
    async fn an_empty_array_yields_an_empty_array() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            int size;
            void create() {
                mixed *none = ({});
                size = sizeof(none->two());
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
            LpcRef::from(0)
        );
    }

    /// The element's callee makes its own collection call: two frames each
    /// with a call in flight.
    #[tokio::test]
    async fn an_element_callee_may_make_its_own_collection_call() {
        let vm = vm_with_other().await;
        vm.create_process_from_code(
            "/inner.c",
            r#"int *pair() { return ({ "/other", "/other" })->two(); }"#,
        )
        .await
        .unwrap();
        let code = indoc! { r#"
            int total;
            void create() {
                mixed *rr = ({ "/inner" })->pair();
                total = rr[0][0] + rr[0][1];
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

    /// A missing trailing `ref` argument — `push_arg` never sees it.
    #[tokio::test]
    async fn a_ref_parameter_is_refused_across_a_collection_call_other() {
        let vm = vm_with_other().await;

        let with_arg = indoc! { r#"
            void create() { int v; ({ "/other" })->take(v); }
        "# };
        let err = vm
            .initialize_process_from_code("/main.c", with_arg)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("by reference"), "{err}");

        let no_arg = indoc! { r#"
            void create() { ({ "/other" })->take(); }
        "# };
        let err = vm
            .initialize_process_from_code("/none.c", no_arg)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("by reference"), "{err}");
    }

    #[tokio::test]
    async fn this_object_inside_an_element_is_the_receiver() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            string seen;
            void create() {
                string *r = ({ "/other" })->who();
                seen = r[0];
            }
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
    async fn a_non_object_element_is_zero_in_its_slot() {
        let vm = vm_with_other().await;
        let code = indoc! { r#"
            int total;
            void create() {
                int *r = ({ 1, "/other" })->two();
                total = r[0] * 10 + r[1];
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
            LpcRef::from(2)
        );
    }
}
