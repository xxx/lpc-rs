//! Explicit efun permissions through transactional compilation.

use crate::{
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::{Vm, vm_op::VmOp},
    },
    test_support::TempLib,
};
use lpc_rs_core::null_mode::NullMode;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::ConfigBuilder;
use std::sync::Arc;

fn vm(root: &TempLib, mode: NullMode) -> Vm {
    Vm::new(
        ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap(),
    )
}

async fn seed(vm: &Vm, path: &str, source: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, source)
        .await
        .unwrap_or_else(|error| panic!("{}", error.diagnostic_string()))
        .context
        .process
}

async fn call(vm: &Vm, process: &Arc<Process>, name: &str) -> Result<LpcRef> {
    let template = TaskTemplate::from(vm.global_state.clone());
    template.set_this_player(Some(process.clone()));
    apply_function_by_name(name, &[], process.clone(), template, Some(5000))
        .await
        .unwrap()
}

const TARGET: &str = "int value() { return efun::sizeof(({ 1, 2, 3 })); }";
const LOADER: &str = "int run() { return \"/target\"->value(); }";

#[tokio::test]
async fn simul_wrappers_require_permission_for_explicit_bypass() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-simul");
        std::fs::write(root.join("simul.c"), "nomask int abs(int n) { return 99; }").unwrap();
        std::fs::write(root.join("ordinary.c"), "int value() { return abs(-7); }").unwrap();
        for file in ["trusted.c", "escape.c"] {
            std::fs::write(root.join(file), "int value() { return efun::abs(-7); }").unwrap();
        }
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .simul_efun_file("/simul")
                .build()
                .unwrap(),
        );
        vm.global_state
            .initialize_simul_efuns()
            .await
            .unwrap()
            .unwrap();
        seed(
            &vm,
            "/secure/master.c",
            r#"
            int valid_load() { return 1; }
            int valid_override(string source, string name, string main_file) {
                return source=="/trusted.c" && name=="abs" && main_file==source;
            }
        "#,
        )
        .await;
        let loader = seed(
            &vm,
            "/loader.c",
            r#"
            int run() {
                return "/ordinary"->value()==99 && "/trusted"->value()==7
                    && !!catch("/escape"->value());
            }
        "#,
        )
        .await;
        assert_eq!(call(&vm, &loader, "run").await.unwrap(), LpcRef::from(1));
        assert!(vm.global_state.object_space.lookup("/escape").is_none());
    }
}

#[tokio::test]
async fn retried_compilation_reauthorizes_and_commits_policy_effects_once() {
    use crate::interpreter::vm::global_state::GlobalState;

    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-retry");
        std::fs::write(root.join("target.c"), TARGET).unwrap();
        let (tx, _rx) = tokio::sync::mpsc::channel(16);
        let config = ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .null_mode(mode)
            .build()
            .unwrap();
        let state = Arc::new(GlobalState::new_rejecting(Arc::new(config), tx, 3));
        let master = state
            .object_space
            .create_process_from_code(
                "/secure/master.c",
                r#"
            int calls;
            int valid_load() { return 1; }
            int valid_write() { return 1; }
            int valid_override() { calls++; write_file("/policy.log", "once"); return 1; }
        "#,
            )
            .await
            .unwrap();
        let task = state
            .initialize_process_from_code(
                "/loader.c",
                "int create() { return \"/target\"->value(); }",
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(3)));
        assert_eq!(state.attempt_telemetry().conflicts, 3);
        assert_eq!(state.committed_global(&master, 0u16), LpcRef::from(1));
        assert_eq!(
            std::fs::read_to_string(root.join("policy.log")).unwrap(),
            "once"
        );
    }
}

#[tokio::test]
async fn a_recompiled_master_cannot_authorize_its_own_efun_bypass() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-master-upgrade");
        std::fs::create_dir_all(root.join("secure")).unwrap();
        let original = r#"
            int valid_recompile() { return 1; }
            int valid_override() { return 0; }
            int version() { return 7; }
            void error_handler(mapping error) {}
        "#;
        std::fs::write(root.join("secure/master.c"), original).unwrap();
        let mut vm = vm(&root, mode);
        let master = seed(&vm, "/secure/master.c", original).await;
        let admin = seed(
            &vm,
            "/admin.c",
            "int request() { return request_object_recompile(\"master\"); }",
        )
        .await;
        std::fs::write(
            root.join("secure/master.c"),
            r#"
            int valid_recompile() { return 1; }
            int valid_override() { return 1; }
            int version() { return efun::sizeof(({ 1 })); }
        "#,
        )
        .unwrap();
        call(&vm, &admin, "request").await.unwrap();
        let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
            panic!("update queued");
        };
        vm.global_state.run_object_update(request.clone()).await;
        let status = vm.global_state.updates.get(request.id).unwrap();
        assert_eq!(status.state, "failed");
        assert!(
            status.error.contains("efun::sizeof: permission denied"),
            "{}",
            status.error
        );
        assert_eq!(
            call(&vm, &master, "version").await.unwrap(),
            LpcRef::from(7)
        );
    }
}

#[tokio::test]
async fn an_absent_override_hook_preserves_explicit_efuns() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-absent");
        std::fs::write(root.join("target.c"), TARGET).unwrap();
        let vm = vm(&root, mode);
        seed(&vm, "/secure/master.c", "int valid_load() { return 1; }").await;
        let loader = seed(&vm, "/loader.c", LOADER).await;
        assert_eq!(call(&vm, &loader, "run").await.unwrap(), LpcRef::from(3));
    }
}

#[tokio::test]
async fn override_policy_receives_source_identity_and_loading_context() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-context");
        std::fs::write(root.join("target.c"), TARGET).unwrap();
        let vm = vm(&root, mode);
        let master = seed(
            &vm,
            "/secure/master.c",
            r#"
            int calls; int correct;
            int valid_load() { return 1; }
            int valid_override(string source, string name, string main_file) {
                calls++;
                correct=source=="/target.c" && name=="sizeof" && main_file==source
                    && file_name(this_object())=="/secure/master"
                    && file_name(previous_object())=="/loader"
                    && this_player()==previous_object() && origin()=="driver";
                return 1;
            }
            int check() { return calls==1 && correct; }
        "#,
        )
        .await;
        let loader = seed(&vm, "/loader.c", LOADER).await;
        assert_eq!(call(&vm, &loader, "run").await.unwrap(), LpcRef::from(3));
        assert_eq!(call(&vm, &master, "check").await.unwrap(), LpcRef::from(1));
    }
}

#[tokio::test]
async fn denying_or_throwing_policy_blocks_every_source_loading_door() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        for decision in [
            "return 0;",
            "return nil;",
            "throw(\"override policy failed\");",
        ] {
            let root = TempLib::new("override-denial");
            std::fs::write(root.join("target.c"), TARGET).unwrap();
            let vm = vm(&root, mode);
            seed(
                &vm,
                "/secure/master.c",
                &format!(
                    r#"
                int valid_load() {{ return 1; }}
                int valid_write() {{ return 1; }}
                mixed valid_override() {{ {decision} }}
            "#
                ),
            )
            .await;
            let loader = seed(&vm, "/loader.c", r#"
                mixed dynamic() { return catch("/target"->value()); }
                mixed clone() { return catch(clone_object("/target")); }
                mixed supplied() { return catch(compile_string("/supplied", "int f() { return efun::sizeof(({ 1 })); }")); }
                int load() { return !load_object("/target"); }
            "#).await;
            for name in ["dynamic", "clone", "supplied"] {
                let error = call(&vm, &loader, name).await.unwrap().to_string();
                let expected = if decision.starts_with("throw") {
                    "override policy failed"
                } else {
                    "efun::sizeof: permission denied"
                };
                assert!(error.contains(expected), "{name}: {error}");
            }
            assert_eq!(call(&vm, &loader, "load").await.unwrap(), LpcRef::from(1));
            assert!(vm.global_state.object_space.lookup("/target").is_none());
            assert!(vm.global_state.object_space.lookup("/supplied").is_none());
        }
    }
}

#[tokio::test]
async fn override_policy_writes_commit_only_with_the_loading_transaction() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-transaction");
        std::fs::write(root.join("target.c"), TARGET).unwrap();
        let vm = vm(&root, mode);
        let master = seed(
            &vm,
            "/secure/master.c",
            r#"
            int calls;
            int valid_load() { return 1; }
            int valid_override() { calls++; return 0; }
        "#,
        )
        .await;
        let loader = seed(
            &vm,
            "/loader.c",
            r#"
            mixed caught() { return catch("/target"->value()); }
            int uncaught() { return "/target"->value(); }
        "#,
        )
        .await;
        assert!(
            call(&vm, &loader, "caught")
                .await
                .unwrap()
                .to_string()
                .contains("permission denied")
        );
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(1)
        );
        assert!(call(&vm, &loader, "uncaught").await.is_err());
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(1)
        );
    }
}

#[tokio::test]
async fn authorization_does_not_revoke_resident_code_or_replace_runtime_permissions() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-resident");
        std::fs::write(
            root.join("target.c"),
            r#"
            int value() { return efun::sizeof(({ 1, 2, 3 })); }
            void remove() { efun::destruct(this_object()); }
        "#,
        )
        .unwrap();
        let vm = vm(&root, mode);
        let master = seed(
            &vm,
            "/secure/master.c",
            r#"
            int calls; int allowed=1;
            int valid_load() { return 1; }
            int valid_override() { calls++; return allowed; }
            void deny() { allowed=0; }
        "#,
        )
        .await;
        let loader = seed(
            &vm,
            "/loader.c",
            r#"
            int run() { return "/target"->value(); }
            mixed remove() { return catch("/target"->remove()); }
        "#,
        )
        .await;
        assert_eq!(call(&vm, &loader, "run").await.unwrap(), LpcRef::from(3));
        call(&vm, &master, "deny").await.unwrap();
        assert_eq!(call(&vm, &loader, "run").await.unwrap(), LpcRef::from(3));
        assert!(
            call(&vm, &loader, "remove")
                .await
                .unwrap()
                .to_string()
                .contains("destruct: permission denied")
        );
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(2)
        );
    }
}

#[tokio::test]
async fn master_and_simul_boot_compile_without_override_authorization() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-bootstrap");
        std::fs::create_dir_all(root.join("secure")).unwrap();
        std::fs::write(
            root.join("secure/simul.c"),
            "int wrapped() { return efun::sizeof(({ 1 })); }",
        )
        .unwrap();
        std::fs::write(
            root.join("secure/master.c"),
            r#"
            int value=efun::sizeof(({ 1 }));
            int valid_override() { return 0; }
            int check() { return value+wrapped(); }
        "#,
        )
        .unwrap();
        let mut vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .simul_efun_file("/secure/simul")
                .build()
                .unwrap(),
        );
        vm.bootstrap().await.unwrap();
        let master = vm
            .global_state
            .object_space
            .lookup("/secure/master")
            .unwrap();
        assert_eq!(call(&vm, &master, "check").await.unwrap(), LpcRef::from(2));
    }
}

#[tokio::test]
async fn recompilation_checks_override_policy_before_replacing_live_code() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        let root = TempLib::new("override-recompile");
        std::fs::write(root.join("target.c"), "int value() { return 7; }").unwrap();
        let mut vm = vm(&root, mode);
        seed(
            &vm,
            "/secure/master.c",
            r#"
            int valid_load() { return 1; }
            int valid_recompile() { return 1; }
            int valid_override() { return 0; }
            void error_handler(mapping error) {}
        "#,
        )
        .await;
        let admin = seed(
            &vm,
            "/admin.c",
            r#"
            object target=load_object("/target");
            int request() { return request_object_recompile(target); }
            int value() { return target->value(); }
        "#,
        )
        .await;
        std::fs::write(root.join("target.c"), TARGET).unwrap();
        call(&vm, &admin, "request").await.unwrap();
        let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
            panic!("update queued");
        };
        vm.global_state.run_object_update(request.clone()).await;
        let status = vm.global_state.updates.get(request.id).unwrap();
        assert_eq!(status.state, "failed");
        assert!(
            status.error.contains("efun::sizeof: permission denied"),
            "{}",
            status.error
        );
        assert_eq!(call(&vm, &admin, "value").await.unwrap(), LpcRef::from(7));
    }
}
