use std::sync::Arc;

use itertools::Itertools;
use lpc_rs_asm::instruction::{Arg, ArgList};
use lpc_rs_core::{RegisterSize, lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::Result;
use lpc_rs_function_support::program_function::ProgramFunction;
use tracing::{instrument, trace};

use crate::interpreter::{
    call_frame::{CallFrame, CollectionCall},
    lpc_array::LpcArray,
    lpc_mapping::LpcMapping,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    stm::VarId,
    task::{Task, get_location},
    task_context::{Loader, ObjectLookup, TaskContext},
};

impl<const STACKSIZE: usize> Task<STACKSIZE> {
    /// Make a `->` whose receiver needs no loading, with no future built:
    /// `true` when the instruction is done, `false` to defer it to
    /// [`Self::handle_call_other`].
    pub(crate) fn call_other_resident(
        &mut self,
        receiver: RegisterVariant,
        name_location: RegisterVariant,
        list: ArgList,
    ) -> Result<bool> {
        if self
            .args_of(list)?
            .iter()
            .any(|arg| matches!(arg, Arg::Ref(_)))
        {
            return Err(self.runtime_bug("a by-reference argument reached call_other"));
        }

        // Both registers stay borrowed: an owned copy of each is an Arc pair per `->`.
        let called = {
            let receiver_ref = get_location(&self.stack, &self.context.txn, receiver)?;
            if !matches!(*receiver_ref, LpcRef::String(_) | LpcRef::Object(_)) {
                return Ok(false);
            }
            let name_ref = get_location(&self.stack, &self.context.txn, name_location)?;
            let Some(name) = name_ref.as_str() else {
                return Ok(false);
            };
            match Self::standing(&receiver_ref, &self.context)? {
                Standing::Ready(process) => process
                    .program
                    .lookup_function(name)
                    .filter(|function| function.public())
                    .cloned()
                    .map(|function| (process, function)),
                Standing::Dead => None,
                Standing::Uncreated(_) | Standing::Uninitialized(_) => return Ok(false),
            }
        };
        match called {
            Some((process, function)) => {
                debug_assert!(!function.prototype.is_efun(), "a `->` callee has a body");
                self.push_call_frame(process, function, list, true)?;
            }
            None => self.stack.current_frame_mut()?.registers[0] = NULL,
        }
        Ok(true)
    }

    /// The `->`s [`Self::call_other_resident`] deferred: a receiver to
    /// create or initialize, a collection, or an error to report.
    #[instrument(level = "debug", skip_all)]
    #[inline]
    pub(crate) async fn handle_call_other(
        &mut self,
        receiver: RegisterVariant,
        name_location: RegisterVariant,
        list: ArgList,
    ) -> Result<()> {
        let receiver_ref = get_location(&self.stack, &self.context.txn, receiver)?.into_owned();
        let name_ref = get_location(&self.stack, &self.context.txn, name_location)?.into_owned();
        let Some(function_name) = name_ref.as_str() else {
            return Err(
                self.runtime_error(format!("Invalid name passed to `call_other`: {name_ref}"))
            );
        };
        trace!("Calling call_other: {}->{}", receiver_ref, function_name);

        let result = match &receiver_ref {
            LpcRef::String(_) | LpcRef::Object(_) => {
                let resolved = Self::resolve_call_other_receiver(
                    &receiver_ref,
                    function_name,
                    &self.context,
                    || self.loader(),
                )
                .await?;
                let Some((receiver, function)) = resolved else {
                    self.stack.current_frame_mut()?.registers[0] = NULL;
                    return Ok(());
                };
                if !function.public() {
                    NULL
                } else {
                    debug_assert!(!function.prototype.is_efun(), "a `->` callee has a body");
                    // The callee returns through `pop_frame`'s result copy.
                    self.push_call_frame(receiver, function, list, true)?;
                    return Ok(());
                }
            }
            LpcRef::Array(_) | LpcRef::Mapping(_) => {
                let args = self.arg_values(list)?;
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
                    name: function_name.to_string(),
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

    /// The call's arguments as values; refs were rejected before this.
    fn arg_values(&self, list: ArgList) -> Result<Vec<LpcRef>> {
        self.args_of(list)?
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
        let Some((process, function)) =
            Self::resolve_call_other_receiver(receiver, name, &self.context, || self.loader())
                .await?
        else {
            return Ok(None);
        };
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
        frame.external = true;
        Ok(Some(frame))
    }

    /// The identity a `->` from the current frame loads under.
    fn loader(&self) -> Result<Loader> {
        Ok(Loader {
            func: "call_other".to_string(),
            chain: self.chain()?,
            program: self
                .stack
                .calling_program(self.context.config().lib_dir.as_str()),
        })
    }

    /// Where a `->` receiver stands, loading nothing.
    fn standing(receiver_ref: &LpcRef, context: &TaskContext) -> Result<Standing> {
        let process = match receiver_ref {
            LpcRef::String(_) => {
                let path = receiver_ref
                    .with_string(|s| context.object_path(s.to_str(), "/", "call_other"))??;
                match context.find_object(&path) {
                    ObjectLookup::Found(process) => process,
                    // Creating a destructed object here would resurrect it.
                    ObjectLookup::Removed => return Ok(Standing::Dead),
                    ObjectLookup::NotCreated => return Ok(Standing::Uncreated(path)),
                }
            }
            LpcRef::Object(_) => match receiver_ref.live_object(context.txn()) {
                Some(process) => process,
                None => return Ok(Standing::Dead),
            },
            _ => return Ok(Standing::Dead),
        };
        Ok(if process.is_initialized(context.txn()) {
            Standing::Ready(process)
        } else {
            Standing::Uninitialized(process)
        })
    }

    /// The receiver's process and its function `name`, `None` for a dead
    /// receiver or a missing function. `loader` runs only for a receiver to
    /// create or initialize — a `->` to a resident, initialized object
    /// must allocate nothing.
    #[instrument(level = "debug", skip_all)]
    async fn resolve_call_other_receiver<T, L>(
        receiver_ref: &LpcRef,
        name: T,
        context: &TaskContext,
        loader: L,
    ) -> Result<Option<(Arc<Process>, Arc<ProgramFunction>)>>
    where
        T: AsRef<str>,
        L: Fn() -> Result<Loader>,
    {
        // A receiver is created and initialized whether or not it has the
        // function: old lib code calls an undefined one on purpose to load
        // an object.
        let process = match Self::standing(receiver_ref, context)? {
            Standing::Ready(process) => process,
            Standing::Dead => return Ok(None),
            Standing::Uncreated(path) => {
                let loader = loader()?;
                let process = context.compile_process(&path, &loader).await?;
                // This call created it, so a throwing initializer undoes the insert.
                context
                    .insert_and_initialize(loader.callers(), &process)
                    .await?;
                process
            }
            Standing::Uninitialized(process) => {
                Self::initialize_process(context.nested(loader()?.callers(), process)?)
                    .await?
                    .context
                    .process
            }
        };

        let function = process.program.lookup_function(name).cloned();
        Ok(function.map(|function| (process, function)))
    }
}

/// Where a `->` receiver stands before anything is loaded.
enum Standing {
    /// Resident and initialized.
    Ready(Arc<Process>),
    /// Destructed, or not an object: the call is 0.
    Dead,
    /// No object at this path yet.
    Uncreated(LpcPath),
    /// Resident, its `create` not yet run.
    Uninitialized(Arc<Process>),
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
    use lpc_rs_asm::instruction::Instruction;
    use lpc_rs_core::register::RegisterVariant;
    use thin_vec::ThinVec;

    use super::super::eval_loop::{AsyncCall, Slice};
    use crate::{
        interpreter::{
            CommittedReader,
            call_frame::CallFrame,
            lpc_ref::{LpcRef, NULL},
            stm::{LiveSnapshot, Transaction, TxnHandle, VarId, start_txn},
            task::{Task, task_template::TaskTemplate},
            vm::Vm,
        },
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

    /// A task on `vm` at `/main.c`'s `create`, stepped to its first `->`,
    /// under a live transaction: the snapshot returned keeps it open.
    async fn task_at_first_call_other(
        vm: &Vm,
        code: &str,
    ) -> (
        Task<{ crate::compile_time_config::MAX_CALL_STACK_SIZE }>,
        LiveSnapshot,
    ) {
        let process = vm.create_process_from_code("/main.c", code).await.unwrap();
        let live = start_txn(&vm.global_state.committer_tx).await.unwrap();
        let mut context =
            TaskTemplate::from(vm.global_state.clone()).into_task_context(process.clone());
        context.txn = TxnHandle::new(Transaction::new(live.inner.clone()));
        let mut task = Task::new(context);
        let create = process.program.lookup_function("create").unwrap().clone();
        let frame = CallFrame::new(process, create, 0, None::<ThinVec<VarId>>);
        task.stack.push(frame).unwrap();
        for _ in 0..32 {
            let at = task.stack.current_frame().unwrap().instruction();
            if matches!(at, Some(Instruction::CallOther(..))) {
                return (task, live);
            }
            task.run_slice(&mut 1).unwrap();
        }
        panic!("no `->` in {code}");
    }

    #[tokio::test]
    async fn a_resident_receiver_is_called_with_no_await() {
        let vm = Vm::new(test_config());
        vm.initialize_process_from_code("/other.c", OTHER)
            .await
            .unwrap();
        let code = r#"int got; void create() { got = "/other"->two(); }"#;
        let (mut task, _live) = task_at_first_call_other(&vm, code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        assert_eq!(task.stack.current_frame().unwrap().function.name(), "two");
    }

    #[tokio::test]
    async fn a_receiver_still_to_initialize_awaits() {
        let vm = vm_with_other().await;
        let code = r#"int got; void create() { got = "/other"->two(); }"#;
        let (mut task, _live) = task_at_first_call_other(&vm, code).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Await(AsyncCall::Other(..))));
        assert_eq!(task.stack.len(), 1);
    }

    #[tokio::test]
    async fn a_dead_receiver_is_zero_with_no_await() {
        let vm = vm_with_other().await;
        let code = r#"int got; void create() { object ob; got = ob->two(); }"#;
        let (mut task, _live) = task_at_first_call_other(&vm, code).await;
        let frame = task.stack.current_frame_mut().unwrap();
        let Some(Instruction::CallOther(RegisterVariant::Local(receiver), _, _)) =
            frame.instruction()
        else {
            panic!("the receiver is a local");
        };
        frame.registers[receiver] = LpcRef::Object(std::sync::Weak::new());

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 1);
        assert_eq!(task.stack.current_frame().unwrap().registers[0], NULL);
    }
}
