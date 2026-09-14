//! Room-entry latency and committed-work checks, with setup outside timing.
//!
//! Run `cargo run --release --example room_entry_latency -- 20 [scenario]`.

use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering::Relaxed},
    },
    time::{Duration, Instant},
};

use lpc_rs::interpreter::{
    CommittedReader,
    lpc_ref::LpcRef,
    process::Process,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    vm::Vm,
};
use lpc_rs_utils::config::ConfigBuilder;
use tokio::{sync::Barrier, task::JoinSet};

#[derive(Clone, Copy)]
struct Scenario {
    name: &'static str,
    roots: usize,
    population: usize,
    separate: bool,
    living: bool,
    source: bool,
    rules: bool,
    shared_hook: bool,
    staggered: bool,
    background: bool,
}

fn integer(value: LpcRef) -> i64 {
    match value {
        LpcRef::Int(value) => value.0,
        other => panic!("expected an integer, got {other:?}"),
    }
}

fn hooks(vm: &Vm, rooms: &[Arc<Process>]) -> i64 {
    rooms
        .iter()
        .map(|room| {
            integer(vm.global_state.committed_global(room, 0u16))
                + vm.global_state
                    .committed_inventory(room)
                    .iter()
                    .map(|npc| integer(vm.global_state.committed_global(npc, 0u16)))
                    .sum::<i64>()
        })
        .sum()
}

async fn measure(scenario: Scenario, sample: usize) {
    let config = ConfigBuilder::default()
        .lib_dir("./tests/fixtures/code")
        .max_execution_time(300_u64)
        .build()
        .unwrap();
    let vm = Vm::new(config);
    vm.initialize_process_from_code(
        "/latency_counter.c",
        "int count; void bump() { count = count + 1; }",
    )
    .await
    .unwrap();
    let npc = format!(
        "int hooks; void create() {{ {} }}
         void enter(object room) {{ move_object(room); }}
         void init() {{ hooks++; {} {} }}
         int greet(string arg) {{ return 1; }}",
        if scenario.living {
            "enable_commands();"
        } else {
            ""
        },
        if scenario.rules {
            "add_action(\"greet\", \"greet\");"
        } else {
            ""
        },
        if scenario.shared_hook {
            "\"/latency_counter\"->bump();"
        } else {
            ""
        },
    );
    vm.initialize_process_from_code("/latency_npc.c", npc)
        .await
        .unwrap();
    let mut rooms = Vec::new();
    let destinations = if scenario.separate { scenario.roots } else { 1 };
    for index in 0..=destinations {
        rooms.push(
            vm.initialize_process_from_code(
                format!("/latency_room_{index}.c"),
                "int hooks; void init() { hooks++; }",
            )
            .await
            .unwrap()
            .context
            .process,
        );
    }
    let population = scenario.population;
    let source_move = if scenario.source {
        format!("npcs[i]->enter(rooms[{destinations}]);")
    } else {
        String::new()
    };
    let arrivals = if population == 0 {
        "npcs[k]->enter(room);".to_owned()
    } else {
        format!(
            "for (int i = 0; i < {population}; i++) {{ clone_object(\"/latency_npc\")->enter(room); }}"
        )
    };
    let driver = format!(
        "object *rooms; object *npcs;
         void create() {{
             rooms = allocate({}); npcs = allocate({});
             for (int i = 0; i < {}; i++) {{ rooms[i] = find_object(\"/latency_room_\" + i); }}
             {}
         }}
         void arrive(int k) {{ object room = rooms[{}]; {arrivals} }}",
        destinations + 1,
        scenario.roots,
        destinations + 1,
        if population == 0 {
            format!(
                "for (int i = 0; i < {}; i++) {{ npcs[i] = clone_object(\"/latency_npc\"); {source_move} }}",
                scenario.roots
            )
        } else {
            String::new()
        },
        if scenario.separate { "k" } else { "0" },
    );
    let driver = vm
        .initialize_process_from_code("/latency_driver.c", driver)
        .await
        .unwrap()
        .context
        .process;
    let hooks_before = hooks(&vm, &rooms);
    let stats_before = vm.global_state.committer_stats().await.unwrap();
    let background = if scenario.background {
        Some(
            vm.initialize_process_from_code(
                "/latency_background.c",
                "int count; void tick() { count++; }",
            )
            .await
            .unwrap()
            .context
            .process,
        )
    } else {
        None
    };
    let gate = Arc::new(Barrier::new(
        scenario.roots + 1 + usize::from(scenario.background),
    ));
    let done = Arc::new(AtomicBool::new(false));
    let background = background.map(|process| {
        let gate = gate.clone();
        let done = done.clone();
        let template = TaskTemplate::from(vm.global_state.clone());
        tokio::spawn(async move {
            gate.wait().await;
            let mut latencies = Vec::new();
            while !done.load(Relaxed) {
                let started = Instant::now();
                apply_function_by_name("tick", &[], process.clone(), template.clone(), Some(300))
                    .await
                    .unwrap()
                    .unwrap();
                latencies.push(started.elapsed().as_secs_f64() * 1e3);
                tokio::time::sleep(Duration::from_micros(500)).await;
            }
            latencies
        })
    });
    let mut tasks = JoinSet::new();
    for index in 0..scenario.roots {
        let gate = gate.clone();
        let driver = driver.clone();
        let template = TaskTemplate::from(vm.global_state.clone());
        tasks.spawn(async move {
            gate.wait().await;
            if scenario.staggered && index >= scenario.roots / 2 {
                tokio::time::sleep(std::time::Duration::from_micros(
                    (index - scenario.roots / 2 + 1) as u64 * 200,
                ))
                .await;
            }
            let started = Instant::now();
            let result = apply_function_by_name(
                "arrive",
                &[LpcRef::from(index as i64)],
                driver,
                template,
                Some(300),
            )
            .await
            .expect("the arrival entry exists");
            (started.elapsed().as_secs_f64() * 1e3, result)
        });
    }
    let started = Instant::now();
    gate.wait().await;
    let mut latencies = Vec::new();
    let mut failed = 0;
    let mut timeouts = 0;
    while let Some(result) = tasks.join_next().await {
        let (elapsed, outcome) = result.unwrap();
        if let Err(error) = outcome {
            failed += 1;
            if error.to_string().contains("evaluation limit") {
                timeouts += 1;
            } else {
                eprintln!("{}: {error}", scenario.name);
            }
        }
        latencies.push(elapsed);
    }
    let wall_ms = started.elapsed().as_secs_f64() * 1e3;
    done.store(true, Relaxed);
    let mut background_latencies = match background {
        Some(handle) => handle.await.unwrap(),
        None => Vec::new(),
    };
    background_latencies.sort_by(f64::total_cmp);
    let background_p95 = if background_latencies.is_empty() {
        0.0
    } else {
        background_latencies[(background_latencies.len() * 95).div_ceil(100) - 1]
    };
    let stats_after = vm.global_state.committer_stats().await.unwrap();
    let mut members = 0;
    for room in &rooms[..destinations] {
        let inventory = vm.global_state.committed_inventory(room);
        members += inventory.len();
        for npc in &inventory {
            assert_eq!(
                vm.global_state.committed_environment(npc).as_ref(),
                Some(room)
            );
            if scenario.rules {
                let rules = vm.global_state.committed_rules(npc);
                assert_eq!(rules.len(), inventory.len() - 1);
                for other in &inventory {
                    if !Arc::ptr_eq(npc, other) {
                        assert!(rules.iter().any(|rule| {
                            rule.owner().is_some_and(|owner| Arc::ptr_eq(&owner, other))
                        }));
                    }
                }
            }
        }
    }
    let committed = (scenario.roots - failed) * population.max(1);
    assert_eq!(members, committed);
    let expected_hooks = if !scenario.living {
        0
    } else if scenario.separate {
        (scenario.roots - failed) * population.max(1).pow(2)
    } else {
        committed * committed
    };
    let committed_hooks = hooks(&vm, &rooms) - hooks_before;
    assert_eq!(committed_hooks, expected_hooks as i64);
    latencies.sort_by(f64::total_cmp);
    let percentile = |percent: usize| latencies[(latencies.len() * percent).div_ceil(100) - 1];
    println!(
        "{},{sample},{},{},{committed},{failed},{timeouts},{wall_ms:.3},{:.3},{:.3},{:.3},{},{committed_hooks},{},{background_p95:.3}",
        scenario.name,
        scenario.roots,
        scenario.roots * population.max(1),
        percentile(50),
        percentile(95),
        percentile(99),
        stats_after.conflicts - stats_before.conflicts,
        background_latencies.len(),
    );
}

#[tokio::main(flavor = "multi_thread", worker_threads = 8)]
async fn main() {
    let args: Vec<String> = std::env::args().collect();
    let samples: usize = args.get(1).map_or(10, |value| value.parse().unwrap());
    let filter = args.get(2).map(String::as_str).unwrap_or("");
    let shared = Scenario {
        name: "shared_64",
        roots: 64,
        population: 0,
        separate: false,
        living: true,
        source: false,
        rules: false,
        shared_hook: false,
        staggered: false,
        background: false,
    };
    let scenarios = [
        Scenario {
            name: "shared_1",
            roots: 1,
            ..shared
        },
        Scenario {
            name: "shared_4",
            roots: 4,
            ..shared
        },
        Scenario {
            name: "shared_16",
            roots: 16,
            ..shared
        },
        shared,
        Scenario {
            name: "separate_1",
            roots: 1,
            separate: true,
            ..shared
        },
        Scenario {
            name: "separate_4",
            roots: 4,
            separate: true,
            ..shared
        },
        Scenario {
            name: "separate_16",
            roots: 16,
            separate: true,
            ..shared
        },
        Scenario {
            name: "separate_64",
            separate: true,
            ..shared
        },
        Scenario {
            name: "nonliving_64",
            living: false,
            ..shared
        },
        Scenario {
            name: "source_64",
            source: true,
            ..shared
        },
        Scenario {
            name: "rules_64",
            rules: true,
            ..shared
        },
        Scenario {
            name: "shared_hook_64",
            shared_hook: true,
            ..shared
        },
        Scenario {
            name: "staggered_64",
            staggered: true,
            ..shared
        },
        Scenario {
            name: "shared_background_64",
            background: true,
            ..shared
        },
        Scenario {
            name: "staggered_background_64",
            background: true,
            staggered: true,
            ..shared
        },
        Scenario {
            name: "population_128",
            roots: 8,
            population: 16,
            ..shared
        },
    ];
    println!(
        "scenario,sample,owners,requested,committed,failed,timeouts,wall_ms,p50_ms,p95_ms,p99_ms,conflicts,hooks,background_samples,background_p95_ms"
    );
    for sample in 0..samples {
        for scenario in scenarios
            .iter()
            .filter(|scenario| scenario.name.contains(filter))
        {
            measure(*scenario, sample).await;
        }
    }
}
