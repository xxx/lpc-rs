//! Rule registration and removal latency, with compilation and population outside timing.
//!
//! Run `cargo run --release --example command_rule_latency -- 10` from the repository root.

use std::time::Instant;

use lpc_rs::interpreter::{
    CommittedReader,
    lpc_ref::LpcRef,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    vm::Vm,
};
use lpc_rs_utils::config::ConfigBuilder;

async fn measure(family: &str, base: usize, added: usize, operation: &str, sample: usize) {
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .max_execution_time(3000_u64)
        .build()
        .unwrap();
    let vm = Vm::new(config);
    let (initialize, register, inspect) = if family != "parser" {
        (
            "enable_commands(); set_this_player(this_object());",
            if family == "native" {
                "add_rule(\"'poke' %w\", \"handle\");"
            } else {
                "add_action(\"handle\", \"poke\");"
            },
            "remove_rule(0);",
        )
    } else {
        (
            "parse_init();",
            "parse_add_rule(\"poke\", \"WRD\");",
            "parse_remove(\"absent\");",
        )
    };
    let remove = if family == "parser" {
        "parse_remove(\"poke\");"
    } else {
        "remove_action(\"poke\");"
    };
    let temporary = register.replace("poke", "temporary");
    let remove_temporary = remove.replace("poke", "temporary");
    let operation_code = match operation {
        "append" => format!("for (int i = 0; i < n; i++) {{ {register} }}"),
        "observe" => format!("for (int i = 0; i < n; i++) {{ {register} {inspect} }}"),
        "remove" => remove.to_owned(),
        "churn" => format!("for (int i = 0; i < n; i++) {{ {temporary} {remove_temporary} }}"),
        _ => unreachable!(),
    };
    let expected = match operation {
        "remove" => 0,
        "churn" => base,
        _ => base + added,
    };
    let code = format!(
        "void create() {{ {initialize} for (int i = 0; i < {base}; i++) {{ {register} }} }}
         int handle(string arg) {{ return 1; }}
         void edit(int n) {{
             set_this_player(this_object());
             {operation_code}
         }}"
    );
    let process = vm
        .initialize_process_from_code("/probe.c", code)
        .await
        .unwrap()
        .context
        .process;
    let before_rules = if family != "parser" {
        vm.global_state.committed_rules(&process)
    } else {
        vm.global_state.committed_verb_rules()
    };
    assert_eq!(before_rules.len(), base);
    let before = vm.global_state.committer_stats().await.unwrap();
    let attempts = vm.global_state.attempt_telemetry();
    let started = Instant::now();
    apply_function_by_name(
        "edit",
        &[LpcRef::from(added as i64)],
        process.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        Some(3000),
    )
    .await
    .unwrap()
    .unwrap();
    let wall_us = started.elapsed().as_secs_f64() * 1e6;
    let after = vm.global_state.committer_stats().await.unwrap();
    let after_attempts = vm.global_state.attempt_telemetry();
    let rules = if family != "parser" {
        vm.global_state.committed_rules(&process)
    } else {
        vm.global_state.committed_verb_rules()
    };
    assert_eq!(before_rules.len(), base);
    assert_eq!(rules.len(), expected);
    assert!(rules.iter().all(|rule| rule.owned_by(&process)));
    assert_eq!(after.conflicts - before.conflicts, 0);
    assert_eq!(after_attempts.owning_attempts - attempts.owning_attempts, 1);
    println!(
        "{family},{base},{added},{operation},{sample},{wall_us:.3},{:.3},{:.3},{}",
        (after.commit_service_ns - before.commit_service_ns) as f64 / 1000.0,
        (after.busy_ns - before.busy_ns) as f64 / 1000.0,
        rules.len()
    );
}

#[tokio::main(flavor = "multi_thread", worker_threads = 8)]
async fn main() {
    let samples: usize = std::env::args()
        .nth(1)
        .map_or(10, |value| value.parse().unwrap());
    println!(
        "family,base,added,operation,sample,wall_us,commit_service_us,committer_busy_us,final_rules"
    );
    for sample in 0..samples {
        let families = if sample % 2 == 0 {
            ["action", "native", "parser"]
        } else {
            ["parser", "native", "action"]
        };
        for family in families {
            for base in [0, 256] {
                for added in [1, 16, 64, 256] {
                    for operation in ["append", "observe", "churn"] {
                        measure(family, base, added, operation, sample).await;
                    }
                }
                measure(family, base, 0, "remove", sample).await;
            }
        }
    }
}
