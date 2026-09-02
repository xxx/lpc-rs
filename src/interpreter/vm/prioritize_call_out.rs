use std::sync::Arc;

use lpc_rs_errors::lpc_error;
use tokio::task::JoinHandle;

use crate::{
    compile_time_config::MAX_CALL_STACK_SIZE,
    interpreter::{
        lpc_ref::LpcRef,
        task::{Task, apply_function::report_runtime_error, task_template::TaskTemplate},
        vm::global_state::GlobalState,
    },
};

impl GlobalState {
    /// Handler for [`VmOp::PrioritizeCallOut`](crate::interpreter::vm::vm_op::VmOp::PrioritizeCallOut):
    /// run the call out with `id`. A firing that cannot start removes the
    /// entry, repeating or not; every error goes to `error_handler`.
    pub async fn prioritize_call_out(self: &Arc<Self>, id: u64) -> JoinHandle<()> {
        let global_state = self.clone();

        tokio::spawn(async move {
            let entry = global_state.with_call_outs(|co| {
                let call_out = co.get_by_id(id)?;
                let func = match call_out.func_ref {
                    LpcRef::Function(ref func) => Ok(func.clone()),
                    _ => Err(lpc_error!("invalid function sent to `call_out`")),
                };
                Some((func, call_out.is_repeating(), call_out.process().upgrade()))
            });
            let Some((func, repeating, owner)) = entry else {
                return;
            };

            let prepared = match func {
                Ok(ptr) => global_state.prepare_function_ptr(&ptr, &[], None).await,
                Err(e) => Err(e),
            };
            let prepared = match prepared {
                Ok(Some(prepared)) => prepared,
                not_started => {
                    global_state.with_call_outs_mut(|co| co.remove_by_id(id));
                    if let Err(e) = not_started {
                        let template = TaskTemplate::from(global_state.clone());
                        report_runtime_error(&e, owner, template).await;
                    }
                    return;
                }
            };

            global_state.with_call_outs_mut(|co| {
                if repeating {
                    co.get_mut_by_id(id).unwrap().refresh();
                } else {
                    co.remove_by_id(id);
                }
            });

            let max_execution_time = global_state.config.max_execution_time;
            let receiver = prepared.context.process.clone();
            let mut task = Task::<MAX_CALL_STACK_SIZE>::new(prepared.context);

            if let Err(e) = task
                .timed_eval(prepared.function, &prepared.args, max_execution_time)
                .await
            {
                let template = TaskTemplate::from(global_state.clone());
                report_runtime_error(&e, Some(receiver), template).await;
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use thin_vec::thin_vec;
    use ustr::ustr;

    use super::*;
    use crate::{
        interpreter::{
            CommittedReader,
            call_outs::CallOutBuilder,
            function_type::{function_address::FunctionAddress, function_ptr::FunctionPtrBuilder},
            process::Process,
            vm::Vm,
        },
        test_support::test_config,
    };

    #[tokio::test]
    async fn test_prioritize_call_out() {
        let code = indoc! { r#"
            int i = 123;

            void foo() {
                i += 42;
            }
        "# };

        let vm = Vm::new(test_config());

        let r = vm.initialize_process_from_code("/foo/bar.c", code).await;
        let proc = r.unwrap().context.process;
        let func = proc.program.lookup_function("foo").unwrap().clone();
        let ptr = FunctionPtrBuilder::default()
            .address(FunctionAddress::Local(Arc::downgrade(&proc), func.clone()))
            .build()
            .unwrap();

        let call_out = CallOutBuilder::default()
            .id(0)
            .process(Arc::downgrade(&proc))
            .func_ref(ptr.into())
            ._handle(tokio::spawn(async {}))
            .build()
            .unwrap();

        vm.global_state.with_call_outs_mut(|co| co.push(call_out));

        let handle = vm.global_state.prioritize_call_out(0).await;
        handle.await.unwrap();

        assert_eq!(
            vm.global_state.committed_global(&proc, 0u16),
            LpcRef::from(165)
        );
        vm.global_state
            .with_call_outs(|co| assert!(co.get_by_id(0).is_none()));
    }

    mod test_string_receivers {
        use super::*;
        async fn check(vm: &Vm, bar_proc: &Arc<Process>) {
            let ptr = FunctionPtrBuilder::default()
                .owner(Arc::downgrade(bar_proc))
                .address(FunctionAddress::Dynamic(ustr("foo")))
                .partial_args(thin_vec![Some("/bar".into())])
                .build()
                .unwrap();

            let call_out = CallOutBuilder::default()
                .id(0)
                .process(Arc::downgrade(bar_proc))
                .func_ref(ptr.into())
                ._handle(tokio::spawn(async {}))
                .build()
                .unwrap();

            vm.global_state.with_call_outs_mut(|co| co.push(call_out));

            let handle = vm.global_state.prioritize_call_out(0).await;
            handle.await.unwrap();

            assert_eq!(
                vm.global_state.committed_global(bar_proc, 0u16),
                LpcRef::from(165)
            );
            assert!(vm.global_state.is_initialized(bar_proc));
            vm.global_state.with_call_outs(|co| {
                assert!(co.get_by_id(0).is_none());
            });
        }

        #[tokio::test]
        async fn works_with_string_preinitialized_receivers() {
            let bar = indoc! { r#"
            int i = 123;
            void foo(string s) {
                i += 42;
            }
        "# };

            let vm = Vm::new(test_config());

            let r = vm.initialize_process_from_code("/bar.c", bar).await;
            let bar_proc = r.unwrap().context.process;

            check(&vm, &bar_proc).await;
        }

        #[tokio::test]
        async fn works_with_string_noninitialized_receivers() {
            let bar = indoc! { r#"
            int i = 123;
            void foo(string s) {
                i += 42;
            }
        "# };

            let vm = Vm::new(test_config());

            let bar_proc = vm.create_process_from_code("/bar.c", bar).await.unwrap();

            check(&vm, &bar_proc).await;
        }
    }

    mod upvalues {
        use super::*;
        use crate::test_support::run_prog_with_vm_rx;

        async fn fire_the_one_call_out(code: &str) -> LpcRef {
            let (task, _rx) = run_prog_with_vm_rx(code).await;
            let gs = task.context.global_state.clone();
            let proc = task.context.process.clone();
            let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
            gs.prioritize_call_out(id).await.await.unwrap();
            gs.committed_global(&proc, 0u16)
        }

        #[tokio::test]
        async fn a_closure_fires_with_its_captures() {
            let code = r##"
                int result;
                void create() { int j = 5; call_out((: result = j + 1 :), 100); }
            "##;
            assert_eq!(fire_the_one_call_out(code).await, LpcRef::from(6));
        }

        #[tokio::test]
        async fn a_bound_dynamic_receiver_fires() {
            let code = r##"
                int result;
                void create() { call_out(papplyv(&->tick(1), ({ this_object() })), 100); }
                void tick(int x) { result = x + 5; }
            "##;
            assert_eq!(fire_the_one_call_out(code).await, LpcRef::from(6));
        }

        #[tokio::test]
        async fn a_static_function_fires_without_the_creators_captures() {
            let code = r##"
                int result;
                function g;
                void foo() { int k = 7; function h = (: k :); result = g(); }
                void create() { int j = 5; g = (: j :); call_out(&foo(), 100); }
            "##;
            assert_eq!(fire_the_one_call_out(code).await, LpcRef::from(5));
        }
    }
    /// Where a firing's failure is reported, and what it does to the entry.
    mod reporting {
        use std::time::Duration;

        use lpc_rs_utils::{config::ConfigBuilder, debug_log::DebugLog};
        use tokio::io::AsyncReadExt;

        use super::*;
        use crate::test_support::{TempLib, committed_string, temp_lib_config};

        /// Refuses `/refused.c`; globals: 0 `error`, 1 `blamed`, 2 `reports`.
        const MASTER: &str = r#"
            string error; mixed blamed; int reports;
            int valid_load(string path, string func, object caller, mixed program) {
                return path != "/refused.c";
            }
            void error_handler(mapping e) { error = e["error"]; blamed = e["object"]; reports++; }
        "#;

        const REFUSED_CALL_OUT: &str =
            r#"void create() { call_out(papplyv(&->f(), ({ "/refused" })), 100); }"#;

        /// `/refused.c` and `/x.c` each define `f()`; `/boom.c`'s throws.
        fn lib(name: &str) -> TempLib {
            let root = TempLib::new(name);
            std::fs::write(root.join("refused.c"), "void f() {}\n").unwrap();
            std::fs::write(root.join("x.c"), "void f() {}\n").unwrap();
            std::fs::write(root.join("boom.c"), r#"void f() { throw("boom"); }"#).unwrap();
            root
        }

        async fn master(vm: &Vm, code: &str) -> Arc<Process> {
            vm.initialize_process_from_code("/secure/master.c", code)
                .await
                .unwrap()
                .context
                .process
        }

        /// Load `/w.c` from `code`, then fire the one call out its `create()` queued.
        async fn fire(vm: &Vm, code: &str) -> Arc<Process> {
            let w = vm
                .initialize_process_from_code("/w.c", code)
                .await
                .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
                .context
                .process;
            let gs = &vm.global_state;
            let id = gs.with_call_outs(|co| co.queue().iter().next().unwrap().1.id);
            gs.prioritize_call_out(id).await.await.unwrap();
            w
        }

        fn queue_is_empty(vm: &Vm) -> bool {
            vm.global_state.with_call_outs(|co| co.is_empty())
        }

        #[tokio::test]
        async fn a_refused_receiver_is_reported_once_and_the_entry_dropped() {
            let root = lib("call-out-refused");
            let vm = Vm::new(temp_lib_config(&root));
            let master = master(&vm, MASTER).await;
            let w = fire(&vm, REFUSED_CALL_OUT).await;
            let error = committed_string(&vm, &master, 0);
            assert!(error.contains("permission denied"), "{error}");
            assert_eq!(
                vm.global_state.committed_global(&master, 1u16),
                LpcRef::from(Arc::downgrade(&w)),
                "blamed on the pointer's owner"
            );
            assert_eq!(
                vm.global_state.committed_global(&master, 2u16),
                LpcRef::from(1)
            );
            assert!(queue_is_empty(&vm));
        }

        #[tokio::test]
        async fn a_callback_error_is_blamed_on_the_receiver() {
            let root = lib("call-out-throws");
            let vm = Vm::new(temp_lib_config(&root));
            let master = master(&vm, MASTER).await;
            fire(
                &vm,
                r#"void create() { call_out(papplyv(&->f(), ({ "/boom" })), 100); }"#,
            )
            .await;
            let error = committed_string(&vm, &master, 0);
            assert!(error.contains("boom"), "{error}");
            let boom = vm.global_state.object_space.lookup("/boom").unwrap();
            assert_eq!(
                vm.global_state.committed_global(&master, 1u16),
                LpcRef::from(Arc::downgrade(&boom))
            );
            assert!(queue_is_empty(&vm));
        }

        #[tokio::test]
        async fn without_error_handler_the_debug_log_hears_it() {
            let root = lib("call-out-debug-log");
            let (writer, mut reader) = tokio::io::duplex(4096);
            let config = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .debug_log(DebugLog::new(writer))
                .build()
                .unwrap();
            let vm = Vm::new(config);
            master(
                &vm,
                r#"int valid_load(string path, string func, object caller, mixed program) {
                    return path != "/refused.c";
                }"#,
            )
            .await;
            fire(&vm, REFUSED_CALL_OUT).await;
            let mut buf = vec![0u8; 4096];
            let n = tokio::time::timeout(Duration::from_secs(1), reader.read(&mut buf))
                .await
                .expect("the log line arrives")
                .unwrap();
            let logged = String::from_utf8_lossy(&buf[..n]);
            assert!(logged.contains("permission denied"), "{logged}");
            assert!(queue_is_empty(&vm));
        }

        #[tokio::test]
        async fn a_firing_that_cannot_start_removes_a_repeating_entry() {
            let root = lib("call-out-no-function");
            let vm = Vm::new(temp_lib_config(&root));
            master(&vm, MASTER).await;
            fire(
                &vm,
                r#"void create() { call_out(papplyv(&->nope(), ({ "/x" })), 100, 100); }"#,
            )
            .await;
            assert!(queue_is_empty(&vm));
        }

        #[tokio::test]
        async fn a_repeating_callback_that_threw_stays_queued() {
            let root = lib("call-out-repeating-throws");
            let vm = Vm::new(temp_lib_config(&root));
            let master = master(&vm, MASTER).await;
            fire(
                &vm,
                r#"void create() { call_out(papplyv(&->f(), ({ "/boom" })), 100, 100); }"#,
            )
            .await;
            assert_eq!(
                vm.global_state.committed_global(&master, 2u16),
                LpcRef::from(1)
            );
            assert!(!queue_is_empty(&vm));
        }
    }
}
