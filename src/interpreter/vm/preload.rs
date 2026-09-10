//! The master's boot applies: `epilog` names the files to preload and
//! `preload` loads each one, between the master's initialization and the
//! listener.

use std::sync::Arc;

use lpc_rs_errors::LpcError;
use tracing::info;

use crate::interpreter::{
    EPILOG, PRELOAD,
    lpc_ref::{LpcRef, NULL},
    process::Process,
    task::{
        apply_function::{applied_in_master, apply_function_by_name, report_runtime_error},
        task_template::TaskTemplate,
    },
    vm::{Vm, global_state::GlobalState},
};

impl GlobalState {
    /// Apply `epilog(0)` on the master, then `preload(file)` for each
    /// string in the array it answers, one task per call in list order; an
    /// error goes to `error_handler` and the next file still runs. No
    /// `epilog`, or an answer that is not an array, preloads nothing.
    pub async fn preload(self: &Arc<Self>) {
        let files = self.preload_list().await;
        if files.is_empty() {
            return;
        }
        let Some(master) = self.object_space.master_object() else {
            return;
        };
        let noun = if files.len() == 1 { "file" } else { "files" };
        if !master.program.unmangled_functions.contains_key(PRELOAD) {
            tracing::warn!(
                target: "lpc_rs::applies",
                apply = PRELOAD,
                object = %master.filename(),
                files = files.len(),
                "Missing apply; epilog's preload list cannot be loaded"
            );
            self.config
                .debug_log(format!(
                    "epilog listed {} {noun} but the master defines no `preload`",
                    files.len()
                ))
                .await;
            return;
        }
        info!("preloading {} {noun}", files.len());

        for file in files {
            let Some(master) = self.object_space.master_object() else {
                return;
            };
            let template = TaskTemplate::from(self.clone());
            let timeout = Some(self.config.max_execution_time);
            if let Some(Err(e)) =
                apply_function_by_name(PRELOAD, &[file], master.clone(), template, timeout).await
            {
                self.report_boot_error(&e, Some(master)).await;
            }
        }
    }

    /// The string members of the array `epilog(0)` answers; empty without
    /// the apply, on 0, on any other non-array answer (logged with its
    /// type), or when it throws (reported).
    async fn preload_list(self: &Arc<Self>) -> Vec<LpcRef> {
        let template = TaskTemplate::from(self.clone());
        let timeout = Some(self.config.max_execution_time);
        let applied = match applied_in_master(EPILOG, &[LpcRef::from(0)], template, timeout).await {
            Some(Ok(applied)) => applied,
            None => return Vec::new(),
            Some(Err(e)) => {
                self.report_boot_error(&e, self.object_space.master_object())
                    .await;
                return Vec::new();
            }
        };
        let Some(array) = applied.array() else {
            if *applied.value() != NULL {
                self.config
                    .debug_log(format!(
                        "epilog answered a {}; nothing preloaded",
                        applied.value().type_name()
                    ))
                    .await;
            }
            return Vec::new();
        };

        let mut files = Vec::new();
        for element in array.iter() {
            match element {
                LpcRef::String(_) => files.push(element.clone()),
                other => {
                    self.config
                        .debug_log(format!(
                            "epilog: a {} in the preload list; skipped",
                            other.type_name()
                        ))
                        .await
                }
            }
        }
        files
    }

    /// `error` to `error_handler` with `master` as the erring object, or to
    /// the debug log when the master has no handler or it throws.
    async fn report_boot_error(self: &Arc<Self>, error: &LpcError, master: Option<Arc<Process>>) {
        report_runtime_error(error, master, TaskTemplate::from(self.clone())).await;
    }
}

impl Vm {
    /// See [`GlobalState::preload`].
    pub async fn preload(&self) {
        self.global_state.preload().await;
    }
}

#[cfg(test)]
mod tests {
    use std::{sync::Arc, time::Duration};

    use lpc_rs_utils::{config::ConfigBuilder, debug_log::DebugLog};
    use tokio::io::AsyncReadExt;

    use crate::{
        interpreter::{
            process::Process,
            vm::{Vm, vm_op::VmOp},
        },
        test_support::{PERMISSIVE_MASTER, TempLib, lib_holding, string_global, temp_lib_config},
    };

    /// A file whose `create()` runs quietly.
    const QUIET: &str = "void create() {}\n";

    /// The master's bookkeeping: `loaded` is every file `preload` was
    /// given, `errors` every error `error_handler` heard, each tagged with
    /// whether its object was the master.
    const BOOKKEEPING: &str = r#"
string loaded = "";
string errors = "";
void error_handler(mapping m) {
    errors += m["error"] + (m["object"] == this_object() ? "@master;" : "@other;");
}
"#;

    /// A master listing two files, whose `preload` records and loads each.
    const LISTS_TWO: &str = r#"
string *epilog(int load_empty) { return ({ "/good", "/also" }); }
void preload(string file) { loaded += file + ";"; file->ping(); }
"#;

    /// A [`Vm`] over a lib holding `files`, its master `/secure/master.c`
    /// from the bookkeeping, the permissive gates and `master`, booted up
    /// to (not including) the boot applies; the master's process; the lib,
    /// which lives as long as the test.
    async fn booted(
        name: &str,
        master: &str,
        files: &[(&str, &str)],
    ) -> (Vm, Arc<Process>, TempLib) {
        let root = lib_holding(name, files);
        let source = format!("{BOOKKEEPING}{PERMISSIVE_MASTER}{master}");
        std::fs::write(root.join("secure/master.c"), source).unwrap();
        let mut vm = Vm::new(temp_lib_config(&root));
        let master = vm.bootstrap().await.expect("the master boots").process;
        (vm, master, root)
    }

    fn is_loaded(vm: &Vm, path: &str) -> bool {
        vm.global_state.object_space.lookup(path).is_some()
    }

    #[tokio::test]
    async fn preload_is_applied_to_each_listed_file_in_order() {
        let (vm, master, _lib) = booted(
            "preload-order",
            LISTS_TWO,
            &[("good.c", QUIET), ("also.c", QUIET)],
        )
        .await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "/good;/also;");
        assert!(is_loaded(&vm, "/good"));
        assert!(is_loaded(&vm, "/also"));
        assert_eq!(string_global(&vm, &master, "errors"), "");
    }

    /// The failing entry's task aborts, so its own `loaded +=` is gone; the
    /// entries before and after it commit on their own.
    #[tokio::test]
    async fn a_failing_preload_reaches_error_handler_and_the_rest_still_load() {
        let master_source = r#"
string *epilog(int load_empty) { return ({ "/good", "/bad", "/also" }); }
void preload(string file) { loaded += file + ";"; file->ping(); }
"#;
        let (vm, master, _lib) = booted(
            "preload-failure",
            master_source,
            &[
                ("good.c", QUIET),
                ("bad.c", "void create() { throw(\"boom\"); }\n"),
                ("also.c", QUIET),
            ],
        )
        .await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "/good;/also;");
        let errors = string_global(&vm, &master, "errors");
        assert!(errors.contains("boom"), "{errors}");
        assert!(errors.ends_with("@master;"), "{errors}");
        assert_eq!(errors.matches(';').count(), 1, "one error: {errors}");
        assert!(is_loaded(&vm, "/good"));
        assert!(is_loaded(&vm, "/also"));
    }

    #[tokio::test]
    async fn preload_is_driver_fired() {
        let master_source = r#"
string *epilog(int load_empty) {
    loaded += sprintf("%d%d;", objectp(previous_object()), objectp(this_player()));
    return ({ "/good" });
}
void preload(string file) {
    loaded += sprintf("%d%d;", objectp(previous_object()), objectp(this_player()));
}
"#;
        let (vm, master, _lib) = booted("preload-driver-fired", master_source, &[]).await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "00;00;");
    }

    #[tokio::test]
    async fn epilog_is_given_zero() {
        let master_source = r#"
string *epilog(int load_empty) { loaded = sprintf("flag=%d;", load_empty); return 0; }
"#;
        let (vm, master, _lib) = booted("preload-flag", master_source, &[]).await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "flag=0;");
    }

    #[tokio::test]
    async fn without_epilog_nothing_is_preloaded() {
        let master_source = "void preload(string file) { loaded += file + \";\"; }\n";
        let (vm, master, _lib) = booted("preload-no-epilog", master_source, &[]).await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "");
        assert_eq!(string_global(&vm, &master, "errors"), "");
    }

    #[tokio::test]
    async fn a_non_array_answer_is_no_preload_list() {
        let master_source = r#"
mixed epilog(int load_empty) { return "/good"; }
void preload(string file) { loaded += file + ";"; }
"#;
        let (vm, master, _lib) = booted("preload-non-array", master_source, &[]).await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "");
        assert_eq!(string_global(&vm, &master, "errors"), "");
    }

    #[tokio::test]
    async fn only_string_entries_are_preloaded() {
        let master_source = r#"
mixed *epilog(int load_empty) { return ({ 0, "/good", ({ "/also" }) }); }
void preload(string file) { loaded += file + ";"; }
"#;
        let (vm, master, _lib) = booted("preload-non-strings", master_source, &[]).await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "/good;");
        assert_eq!(string_global(&vm, &master, "errors"), "");
    }

    #[tokio::test]
    async fn an_erroring_epilog_is_reported_and_boot_goes_on() {
        let master_source = r#"
string *epilog(int load_empty) { throw("no list"); }
void preload(string file) { loaded += file + ";"; }
"#;
        let (vm, master, _lib) = booted("preload-epilog-throws", master_source, &[]).await;

        vm.preload().await;

        assert_eq!(string_global(&vm, &master, "loaded"), "");
        let errors = string_global(&vm, &master, "errors");
        assert!(errors.contains("no list"), "{errors}");
        assert!(errors.ends_with("@master;"), "{errors}");
    }

    #[tokio::test]
    async fn a_list_without_a_preload_apply_is_logged() {
        let root = lib_holding("preload-no-apply", &[("good.c", QUIET)]);
        let source = format!(
            "{PERMISSIVE_MASTER}string *epilog(int load_empty) {{ return ({{ \"/good\" }}); }}\n"
        );
        std::fs::write(root.join("secure/master.c"), source).unwrap();
        let (writer, mut reader) = tokio::io::duplex(4096);
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .debug_log(DebugLog::new(writer))
            .build()
            .unwrap();
        let mut vm = Vm::new(config);
        vm.bootstrap().await.expect("the master boots");

        vm.preload().await;

        let logged = tokio::time::timeout(Duration::from_secs(1), async {
            let mut logged = String::new();
            let mut chunk = [0u8; 4096];
            while !logged.contains("no `preload`") {
                let n = reader.read(&mut chunk).await.unwrap();
                assert!(n > 0, "the log closed before the line arrived: {logged}");
                logged.push_str(&String::from_utf8_lossy(&chunk[..n]));
            }
            logged
        })
        .await
        .expect("the log line arrives");
        assert!(logged.contains("no `preload`"), "{logged}");
        assert!(!is_loaded(&vm, "/good"));
    }

    /// Nothing after the destructed master is applied.
    #[tokio::test]
    async fn the_master_destructing_itself_ends_the_list() {
        let master_source = r#"
string *epilog(int load_empty) { return ({ "/good", "/also" }); }
void preload(string file) {
    if (file == "/good") destruct(this_object());
    else file->ping();
}
"#;
        let (vm, _master, _lib) = booted(
            "preload-master-gone",
            master_source,
            &[("good.c", QUIET), ("also.c", QUIET)],
        )
        .await;

        vm.preload().await;

        assert!(vm.global_state.object_space.master_object().is_none());
        assert!(!is_loaded(&vm, "/also"));
    }

    /// The main loop has not started; the op the timer sends waits on the
    /// channel `run` drains.
    #[tokio::test]
    async fn a_call_out_from_epilog_is_queued_for_the_main_loop() {
        let master_source = r#"
void final_boot() { loaded = "final"; }
string *epilog(int load_empty) { call_out(&final_boot(), 0); return 0; }
"#;
        let (mut vm, _master, _lib) = booted("preload-call-out", master_source, &[]).await;

        vm.preload().await;

        let op = tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if let Some(op) = vm.next_op() {
                    break op;
                }
                tokio::time::sleep(Duration::from_millis(5)).await;
            }
        })
        .await
        .expect("the call_out's op is queued within five seconds");
        assert!(matches!(op, VmOp::PrioritizeCallOut(_)), "{op:?}");
    }
}
