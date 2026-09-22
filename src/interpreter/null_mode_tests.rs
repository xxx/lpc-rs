use lpc_rs_core::null_mode::NullMode;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{lpc_ref::LpcRef, vm::Vm},
    test_support::TempLib,
};

async fn check(mode: NullMode, code: &str) {
    let root = TempLib::new("null-mode");
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap(),
    );
    let result = vm
        .initialize_process_from_code("/test.c", code)
        .await
        .unwrap();
    assert_eq!(result.result(), Some(LpcRef::from(1)), "{mode}: {code}");
}

#[tokio::test]
async fn distinct_nil_is_false_but_is_not_a_number() {
    check(
        NullMode::Distinct,
        r#"
        int create() {
            mixed n = nil;
            return !n && n == nil && n != 0 && typeof(n) == 10 && !intp(n)
                && sizeof(n) == 0 && stringp(catch(n + 1))
                && stringp(catch(({})[<n..])) && stringp(catch(""[0..<n]))
                && intp(0) && !0 && !0.0;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn zero_mode_preserves_legacy_null_behavior() {
    check(
        NullMode::Zero,
        r#"
        string s = 0;
        float f;
        int create() {
            mapping m = ([]);
            return nil == 0 && intp(nil) && typeof(nil) == 1 && nil + 1 == 1
                && s == 0 && intp(f) && m["missing"] == 0;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_defaults_cover_globals_locals_fields_and_omitted_arguments() {
    check(
        NullMode::Distinct,
        r#"
        int gi; float gf; string gs; mixed gm; object go; int *ga;
        class record { int i; float f; string s; mixed m; }
        varargs int defaults(int i, float f, string s, mixed m) {
            function captured = (: ({ i, f, s, m }) :);
            mixed *args = captured();
            return intp(args[0]) && args[0] == 0 && floatp(args[1]) && args[1] == 0.0
                && args[2] == nil && args[3] == nil;
        }
        string no_result() { }
        int create() {
            int i; float f; string s; mixed m;
            class record r = new(class record);
            function capture = (: ({ i, f, s, m }) :);
            mixed *values = capture();
            return gi == 0 && intp(gi) && gf == 0.0 && floatp(gf)
                && gs == nil && gm == nil && go == nil && ga == nil
                && values[0] == 0 && floatp(values[1]) && values[2] == nil && values[3] == nil
                && r.i == 0 && floatp(r.f) && r.s == nil && r.m == nil
                && defaults() && no_result() == nil;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_mapping_keeps_nil_and_zero_keys_and_values() {
    check(
        NullMode::Distinct,
        r#"
        int create() {
            mapping m = ([ nil: "nil", 0: "zero", "value": nil ]);
            m["another"] = nil;
            mapping copied = deep_copy(m);
            return sizeof(m) == 4 && m[nil] == "nil" && m[0] == "zero"
                && m["missing"] == nil && copied["value"] == nil
                && sizeof(copied) == 4 && member_array("value", keys(m)) >= 0;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_absence_efuns_and_dynamic_calls_return_nil() {
    check(NullMode::Distinct, r#"
        int create() {
            return find_object("/missing") == nil && environment() == nil
                && this_player() == nil && this_interactive() == nil
                && this_object()->missing() == nil && function_exists("missing", this_object()) == nil
                && query_call_out(98765) == nil && allocate(1)[0] == nil
                && sizeof(query_call_outs()) == 0 && sizeof(functions()) > 0
                && regmatch("abc", "z") == nil && strindex("abc", "z") == -1;
        }
    "#).await;
}

#[tokio::test]
async fn distinct_json_round_trips_null_without_changing_false_or_zero() {
    check(
        NullMode::Distinct,
        r#"
        int create() {
            mixed *values = json_decode("[null,0,false]");
            return values[0] == nil && values[1] == 0 && values[2] == 0
                && json_encode(values) == "[null,0,0]" && json_encode(nil) == "null";
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_explicit_defaults_only_apply_to_omitted_arguments() {
    check(
        NullMode::Distinct,
        r#"
        mixed identity(mixed value = "default") { return value; }
        int create() {
            return identity() == "default" && identity(nil) == nil && identity(0) == 0;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_self_initializers_and_repeated_declarations_use_typed_defaults() {
    check(
        NullMode::Distinct,
        r#"
        string global = global;
        int create() {
            int ok = global == nil;
            for (int i = 0; i < 2; i++) {
                string value = value;
                float f = f;
                ok = ok && value == nil && floatp(f) && f == 0.0;
                value = "used";
            }
            return ok;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_rejects_numeric_null_for_reference_types_and_nil_for_numbers() {
    for code in [
        "string value = 0;",
        "object value = 0;",
        "int *value = 0;",
        "int value = nil;",
        "float value = nil;",
        "int value = 1 ? nil : 0;",
        "void use(string s) {} void create() { use(0); }",
    ] {
        let root = TempLib::new("null-types");
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(NullMode::Distinct)
                .build()
                .unwrap(),
        );
        assert!(
            vm.initialize_process_from_code("/bad.c", code)
                .await
                .is_err(),
            "accepted {code}"
        );
    }
}

#[tokio::test]
async fn distinct_dynamic_arguments_and_casts_observe_null_types() {
    check(
        NullMode::Distinct,
        r#"
        int number(int n) { return n; }
        string text(string s) { return s; }
        int create() {
            mixed n = nil;
            mixed zero = 0;
            function number_ptr = number;
            function text_ptr = text;
            return stringp(catch(number_ptr(n)))
                && stringp(catch(text_ptr(zero)))
                && stringp(catch((int)n)) && stringp(catch((string)zero))
                && text_ptr(n) == nil && (string)n == nil;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn save_files_preserve_zero_and_restore_nil_by_mode_with_one_warning() {
    use lpc_rs_utils::debug_log::DebugLog;
    use tokio::io::AsyncReadExt;
    let root = TempLib::new("nil-save");
    let master = "int valid_read(string p, string e, object o, string f) { return 1; } int valid_write(string p, string e, object o, string f) { return 1; }";
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    vm.initialize_process_from_code("/secure/master.c", master)
        .await
        .unwrap();
    vm.initialize_process_from_code(
        "/save.c",
        r#"
        mixed n = nil;
        mixed z = 0;
        mixed nested = ({ nil, ([ "n": nil, "z": 0 ]) });
        void create() { save_object("/saved"); }
    "#,
    )
    .await
    .unwrap();
    let saved = std::fs::read_to_string(root.join("saved.o")).unwrap();
    assert!(saved.contains("n nil\n"), "{saved}");
    assert!(saved.contains("z 0\n"), "{saved}");
    for mode in [NullMode::Distinct, NullMode::Zero] {
        let (writer, mut reader) = tokio::io::duplex(4096);
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .debug_log(DebugLog::new(writer))
                .build()
                .unwrap(),
        );
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        let result = vm
            .initialize_process_from_code(
                "/restore.c",
                r#"
            mixed n = "old", z = "old", nested;
            int create() {
                restore_object("/saved");
                mapping m = restore_map("/saved");
                save_map(m, "/again");
                return n == nil && intp(z) && z == 0 && nested[0] == nil
                    && nested[1]["n"] == nil && nested[1]["z"] == 0
                    && m["n"] == nil && intp(m["z"]);
            }
        "#,
            )
            .await
            .unwrap();
        assert_eq!(result.result(), Some(LpcRef::from(1)), "{mode}");
        let rewritten = std::fs::read_to_string(root.join("again.o")).unwrap();
        assert_eq!(
            rewritten,
            if mode == NullMode::Distinct {
                saved.clone()
            } else {
                saved.replace("nil", "0")
            }
        );
        let mut buf = [0; 4096];
        let read =
            tokio::time::timeout(std::time::Duration::from_millis(100), reader.read(&mut buf))
                .await;
        if mode == NullMode::Zero {
            let n = read.unwrap().unwrap();
            let log = String::from_utf8_lossy(&buf[..n]);
            assert_eq!(
                log.matches("converted 3 nil value(s) to 0").count(),
                2,
                "{log}"
            );
            assert!(log.contains("restore_object: /saved.o"), "{log}");
            assert!(log.contains("restore_map: /saved.o"), "{log}");
        } else {
            assert!(read.is_err(), "distinct mode emitted a warning");
        }
    }
}

#[tokio::test]
async fn aborted_nil_restores_do_not_emit_warnings() {
    use lpc_rs_utils::debug_log::DebugLog;
    use tokio::io::AsyncReadExt;
    let root = TempLib::new("nil-save-abort");
    std::fs::write(root.join("saved.o"), "n nil\n").unwrap();
    let (writer, mut reader) = tokio::io::duplex(4096);
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .debug_log(DebugLog::new(writer))
            .build()
            .unwrap(),
    );
    vm.initialize_process_from_code(
        "/secure/master.c",
        "int valid_read(string p, string e, object o, string f) { return 1; }",
    )
    .await
    .unwrap();
    assert!(
        vm.initialize_process_from_code(
            "/restore.c",
            r#"
        mixed n;
        void create() { restore_object("/saved"); throw("abort"); }
    "#
        )
        .await
        .is_err()
    );
    let mut buf = [0; 4096];
    assert!(
        tokio::time::timeout(std::time::Duration::from_millis(100), reader.read(&mut buf))
            .await
            .is_err()
    );
}

#[tokio::test]
async fn simultaneous_vms_keep_their_null_modes_after_commits_and_gc() {
    use crate::interpreter::{
        CommittedReader,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    };
    let root = TempLib::new("null-mode-worlds");
    let make = |mode| {
        Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .build()
                .unwrap(),
        )
    };
    let zero = make(NullMode::Zero);
    let distinct = make(NullMode::Distinct);
    let code = "mixed state = nil; mixed value() { return nil; }";
    let (a, b) = tokio::join!(
        zero.initialize_process_from_code("/same.c", code),
        distinct.initialize_process_from_code("/same.c", code)
    );
    for (vm, process, expected) in [
        (&zero, a.unwrap().context.process, LpcRef::from(0)),
        (&distinct, b.unwrap().context.process, LpcRef::Nil),
    ] {
        vm.global_state.gc().await.unwrap().unwrap();
        assert_eq!(vm.global_state.committed_global(&process, 0), expected);
        let result = apply_function_by_name(
            "value",
            &[],
            process,
            TaskTemplate::from(vm.global_state.clone()),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, expected);
    }
}

#[tokio::test]
async fn destructed_objects_observe_distinct_nil_before_and_after_commit() {
    use crate::interpreter::task::{
        apply_function::apply_function_by_name, task_template::TaskTemplate,
    };
    let root = TempLib::new("nil-destruct");
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    vm.initialize_process_from_code(
        "/secure/master.c",
        "int valid_destruct(object o, object c, string p) { return 1; }",
    )
    .await
    .unwrap();
    vm.initialize_process_from_code("/victim.c", "void create() {} ")
        .await
        .unwrap();
    let result = vm
        .initialize_process_from_code(
            "/observer.c",
            r#"
        object victim = find_object("/victim");
        int check() {
            mapping m = ([]);
            m[victim] = "dead";
            return !victim && victim == nil && victim != 0 && typeof(victim) == 10
                && !intp(victim) && !objectp(victim) && m[nil] == "dead"
                && json_encode(victim) == "null";
        }
        int create() { destruct(victim); return check(); }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(result.result(), Some(LpcRef::from(1)));
    vm.global_state.gc().await.unwrap().unwrap();
    let result = apply_function_by_name(
        "check",
        &[],
        result.context.process,
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(result, LpcRef::from(1));
}

#[tokio::test]
async fn distinct_nil_survives_class_saves_and_numeric_zero_is_never_promoted() {
    let root = TempLib::new("nil-class-save");
    std::fs::write(root.join("old.o"), "text 0\n").unwrap();
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    vm.initialize_process_from_code("/secure/master.c", "int valid_read(string p, string e, object o, string f) { return 1; } int valid_write(string p, string e, object o, string f) { return 1; }").await.unwrap();
    let result = vm
        .initialize_process_from_code(
            "/record.c",
            r#"
        class record { int number; string text; mixed value; }
        class record data = new(class record);
        string text = "original";
        int create() {
            save_object("/record");
            data.text = "changed";
            restore_object("/record");
            restore_object("/old");
            return data.number == 0 && data.text == nil && data.value == nil
                && text == 0 && intp(text) && text != nil;
        }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(result.result(), Some(LpcRef::from(1)));
}

#[tokio::test]
async fn caught_partial_restores_warn_for_nil_values_already_applied() {
    use lpc_rs_utils::debug_log::DebugLog;
    use tokio::io::AsyncReadExt;
    let root = TempLib::new("nil-save-partial");
    std::fs::write(root.join("saved.o"), "n nil\nbad invalid\n").unwrap();
    let (writer, mut reader) = tokio::io::duplex(4096);
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .debug_log(DebugLog::new(writer))
            .build()
            .unwrap(),
    );
    vm.initialize_process_from_code(
        "/secure/master.c",
        "int valid_read(string p, string e, object o, string f) { return 1; }",
    )
    .await
    .unwrap();
    let result = vm
        .initialize_process_from_code(
            "/restore.c",
            r#"
        mixed n = "old", bad;
        int create() { return stringp(catch(restore_object("/saved"))) && n == 0; }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(result.result(), Some(LpcRef::from(1)));
    let mut buf = [0; 4096];
    let n = tokio::time::timeout(std::time::Duration::from_secs(1), reader.read(&mut buf))
        .await
        .unwrap()
        .unwrap();
    let log = String::from_utf8_lossy(&buf[..n]);
    assert_eq!(
        log.matches("converted 1 nil value(s) to 0").count(),
        1,
        "{log}"
    );
}

#[tokio::test]
async fn compiled_code_from_a_different_null_mode_is_rejected() {
    use crate::{
        compiler::CompilerBuilder,
        interpreter::{process::Process, task::Task, task_context::TaskContext},
    };
    use std::sync::Arc;
    let root = TempLib::new("nil-code-mode");
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .build()
        .unwrap();
    let compiler = CompilerBuilder::default()
        .config(Arc::new(config))
        .build()
        .unwrap();
    let code = compiler
        .compile_string("/mismatch.c", "void create() {} ")
        .await
        .unwrap();
    let vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    let process = Arc::new(Process::new(code.program));
    let context = TaskContext::new(vm.global_state.clone(), process, None);
    let error = Task::<1024>::initialize_process(context).await.unwrap_err();
    assert!(error.to_string().contains("different NULL_MODE"), "{error}");
}

#[tokio::test]
async fn distinct_function_pointer_holes_and_void_results_are_nil() {
    check(
        NullMode::Distinct,
        r#"
        mixed first(mixed a, mixed b) { return a; }
        void nothing() {}
        int create() {
            function partial = &first(, 2);
            function noop = nothing;
            return partial() == nil && partial(0) == 0 && noop() == nil
                && sprintf("%O", nil) == "nil" && to_string(nil) == "nil";
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn retrying_a_nil_restore_delivers_its_warning_once() {
    use crate::interpreter::vm::global_state::GlobalState;
    use lpc_rs_utils::debug_log::DebugLog;
    use std::sync::Arc;
    use tokio::io::AsyncReadExt;
    let root = TempLib::new("nil-save-retry");
    std::fs::write(root.join("saved.o"), "n nil\n").unwrap();
    let (writer, mut reader) = tokio::io::duplex(4096);
    let config = ConfigBuilder::default()
        .lib_dir(root.to_str().unwrap())
        .debug_log(DebugLog::new(writer))
        .build()
        .unwrap();
    let (tx, _rx) = tokio::sync::mpsc::channel(16);
    let state = Arc::new(GlobalState::new_rejecting(Arc::new(config), tx, 3));
    state
        .object_space
        .create_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object o, string f) { return 1; }",
        )
        .await
        .unwrap();
    let result = state
        .initialize_process_from_code(
            "/restore.c",
            r#"
        mixed n = "old";
        int create() { restore_object("/saved"); return n == 0; }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(result.result(), Some(LpcRef::from(1)));
    assert_eq!(state.attempt_telemetry().conflicts, 3);
    let mut buf = [0; 4096];
    let n = tokio::time::timeout(std::time::Duration::from_secs(1), reader.read(&mut buf))
        .await
        .unwrap()
        .unwrap();
    let log = String::from_utf8_lossy(&buf[..n]);
    assert_eq!(
        log.matches("converted 1 nil value(s) to 0").count(),
        1,
        "{log}"
    );
}

#[tokio::test]
async fn distinct_command_and_parser_callbacks_receive_nil_for_absence() {
    check(
        NullMode::Distinct,
        r#"
        int can_nil, handled;
        mixed process_input(string line) { return nil; }
        int check_command(string text) { return text == nil; }
        string *parse_command_id_list() { return ({ "rock" }); }
        int can_take_obj(object item, string words) { can_nil = item == nil; return 1; }
        int direct_take_obj(object item, string words) { return item == this_object(); }
        void do_take_obj(object item, string words) { handled = item == this_object(); }
        int create() {
            enable_commands();
            set_this_player(this_object());
            add_action(check_command, "check");
            parse_init();
            parse_add_rule("take", "OBJ");
            return command("check") && parse_sentence("take rock", 0, ({ this_object() })) == 1
                && can_nil && handled;
        }
    "#,
    )
    .await;
}

#[tokio::test]
async fn distinct_recompilation_seeds_new_globals_and_preserves_existing_state() {
    use crate::interpreter::{
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::vm_op::VmOp,
    };
    let root = TempLib::new("nil-recompile");
    std::fs::write(
        root.join("parent.c"),
        r#"
        mixed first_float = read_float(), first_string = read_string();
        float inherited_float;
        string inherited_string;
        float read_float() { return inherited_float; }
        string read_string() { return inherited_string; }
        int inherited_ok() {
            return floatp(first_float) && first_float == 0.0 && first_string == nil;
        }
    "#,
    )
    .unwrap();
    std::fs::write(
        root.join("target.c"),
        r#"
        inherit "/parent";
        string retained = "keep";
        int check() { return inherited_ok() && retained == "keep"; }
    "#,
    )
    .unwrap();
    let mut vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    vm.initialize_process_from_code(
        "/secure/master.c",
        r#"
        int valid_load(string p, string e, object o, string f) { return 1; }
        int valid_clone(string p, object o, string f) { return 1; }
        int valid_inherit(string p, string f) { return 1; }
        int valid_recompile(object p, object o, string f) { return 1; }
    "#,
    )
    .await
    .unwrap();
    let task = vm
        .initialize_process_from_code(
            "/admin.c",
            r#"
        object prototype = load_object("/target");
        object copy = clone_object("/target");
        int create() { return prototype->check() && copy->check(); }
        int request() { return request_object_recompile(prototype); }
        int check() { return prototype->check() && copy->check(); }
    "#,
        )
        .await
        .unwrap();
    assert_eq!(task.result(), Some(1.into()));
    let admin = task.context.process;
    std::fs::write(
        root.join("target.c"),
        r#"
        inherit "/parent";
        string retained = "replace";
        mixed first_float = read_added_float(), first_string = read_added_string();
        float added_float;
        string added_string;
        float read_added_float() { return added_float; }
        string read_added_string() { return added_string; }
        int check() {
            return inherited_ok() && retained == "keep" && floatp(added_float)
                && added_float == 0.0 && added_string == nil
                && floatp(first_float) && first_float == 0.0 && first_string == nil;
        }
    "#,
    )
    .unwrap();
    apply_function_by_name(
        "request",
        &[],
        admin.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap();
    let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
        panic!("recompilation was not queued");
    };
    vm.global_state.run_object_update(request.clone()).await;
    let status = vm.global_state.updates.get(request.id).unwrap();
    assert_eq!(status.state, "succeeded", "{}", status.error);
    assert_eq!(status.updated, 2);
    let result = apply_function_by_name(
        "check",
        &[],
        admin,
        TaskTemplate::from(vm.global_state.clone()),
        Some(5000),
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(result, LpcRef::from(1));
}
