//! The shared bench/test workload catalogue: one statement of each
//! measurement workload's LPC source, entry point, and volume.
//! Ungated: the bench targets build the lib without `cfg(test)`.

use std::sync::Arc;

use tokio::task::JoinSet;

use crate::interpreter::{
    process::Process,
    task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
    vm::Vm,
};

/// How a [`Workload`]'s objects are placed before measuring.
pub enum Kind {
    /// One object, compiled from `source` at `path`.
    Single {
        /// In-game path the object is created at.
        path: &'static str,
        /// LPC source compiled there.
        source: &'static str,
    },
    /// The `call_other` pair: a target object plus the caller that applies against it.
    Pair,
    /// A set of objects compiled in order; the apply target is the last one.
    Set {
        /// `(path, source)` per object, dependency-first.
        objects: &'static [(&'static str, &'static str)],
    },
}

/// One measurement workload: its LPC source, entry point, and contention volume.
pub struct Workload {
    /// Criterion id in the contention and m0 groups.
    pub name: &'static str,
    /// Criterion id in `m1_task_cost`.
    pub task_label: &'static str,
    /// The function one apply calls.
    pub entry: &'static str,
    /// Fixed total applies for a contention block.
    pub total: usize,
    /// Whether each worker passes its index as the entry's argument.
    pub indexed: bool,
    /// Object placement.
    pub kind: Kind,
}

/// Compute-bound, touches essentially no shared state.
pub static FIB: Workload = Workload {
    name: "fib",
    task_label: "fib10",
    entry: "run",
    total: 256,
    indexed: false,
    kind: Kind::Single {
        path: "/bench_fib.c",
        source: r#"
    int fib(int n) {
        if (n < 2) {
            return n;
        }
        return fib(n - 1) + fib(n - 2);
    }

    void run() {
        fib(10);
    }
"#,
    },
};

/// Maximal shared-state pressure for the work done: one global read-modify-write.
/// Does not use `++`, which compiles to a merge write and stops conflicting.
pub static COUNTER: Workload = Workload {
    name: "counter",
    task_label: "increment",
    entry: "increment",
    total: 8192,
    indexed: false,
    kind: Kind::Single {
        path: "/bench_counter.c",
        source: r#"
    int count = 0;

    void increment() {
        count = count + 1;
    }
"#,
    },
};

/// The counter with `++`: compiles to a merge write, so concurrent bumps
/// commute and the contention block should show zero conflicts.
pub static COUNTER_ATOMIC: Workload = Workload {
    name: "counter_atomic",
    task_label: "increment_atomic",
    entry: "increment",
    total: 8192,
    indexed: false,
    kind: Kind::Single {
        path: "/bench_counter_atomic.c",
        source: r#"
    int count = 0;

    void increment() {
        count++;
    }

    int get() {
        return count;
    }
"#,
    },
};

/// Realistic: a cross-object `call_other` pair, which is what most LPC actually does. Each
/// `call_other` builds a nested `Task`, so this exercises task setup, not just the eval loop.
pub static CALL_OTHER: Workload = Workload {
    name: "call_other",
    task_label: "call_other",
    entry: "bump",
    total: 2048,
    indexed: false,
    kind: Kind::Pair,
};

const CALLER_SOURCE: &str = r#"
    int total = 0;

    void bump() {
        object other = find_object("/bench_target");
        total = total + other->value();
    }
"#;

const TARGET_SOURCE: &str = r#"
    int value() {
        return 1;
    }
"#;

/// Array payload, light: the array analogue of `increment`. A small global array, a few
/// index reads and one index write per apply. This is the workload that actually touches
/// the payload container the persistent-payload swap changes; `fib`/`increment`/`call_other`
/// do not construct or index any array, so on their own they are a null measurement of the
/// payload's inner-loop cost.
pub static ARR_TOUCH: Workload = Workload {
    name: "arr_touch",
    task_label: "arr_touch",
    entry: "touch",
    total: 4096,
    indexed: false,
    kind: Kind::Single {
        path: "/bench_arr_touch.c",
        source: r#"
    int *a = ({ 1, 2, 3, 4 });

    void touch() {
        int x = a[0] + a[1] + a[2] + a[3];
        a[3] = x;
    }
"#,
    },
};

/// Array payload, heavy: read the whole array, then write a prefix of it, per apply.
/// Sizes 64 for the array and 8 for the write prefix keep it in the small-array regime
/// where a persistent structure's constant factors hurt most.
pub static ARR_CHURN: Workload = Workload {
    name: "arr_churn",
    task_label: "arr_churn",
    entry: "churn",
    total: 1024,
    indexed: false,
    kind: Kind::Single {
        path: "/bench_arr_churn.c",
        source: r#"
    int *a = ({ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
               16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
               32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,
               48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63 });

    void churn() {
        int i;
        int s = 0;
        for (i = 0; i < 64; i += 1) {
            s = s + a[i];
        }
        for (i = 0; i < 8; i += 1) {
            a[i] = s + i;
        }
    }
"#,
    },
};

/// Distinct tokens shuttling through two shared rooms: every move merges
/// both room inventories, so nothing conflicts across workers.
pub static MOVE_CHURN: Workload = Workload {
    name: "move_churn",
    task_label: "move_churn",
    entry: "shuttle",
    total: 1024,
    indexed: true,
    kind: Kind::Set {
        objects: &[
            ("/bench_token.c", TOKEN_SOURCE),
            ("/bench_room_a.c", ""),
            ("/bench_room_b.c", ""),
            ("/bench_move.c", MOVE_SOURCE),
        ],
    },
};

const TOKEN_SOURCE: &str = r#"
    void shuttle(object dest) {
        move_object(dest);
    }
"#;

const MOVE_SOURCE: &str = r#"
    object *tokens;
    object room_a;
    object room_b;

    void create() {
        int i;
        room_a = find_object("/bench_room_a");
        room_b = find_object("/bench_room_b");
        tokens = ({ clone_object("/bench_token"), clone_object("/bench_token"),
                    clone_object("/bench_token"), clone_object("/bench_token"),
                    clone_object("/bench_token"), clone_object("/bench_token"),
                    clone_object("/bench_token"), clone_object("/bench_token") });
        for (i = 0; i < 8; i += 1) {
            tokens[i]->shuttle(room_a);
        }
    }

    void shuttle(int k) {
        object t = tokens[k];
        if (environment(t) == room_a) {
            t->shuttle(room_b);
        } else {
            t->shuttle(room_a);
        }
    }
"#;

/// Workers resolving `this_object()` through `parse_command`: the applies
/// only read, so the block commits without a conflict.
pub static RESOLVE_SELF: Workload = Workload {
    name: "resolve_self",
    task_label: "resolve_self",
    entry: "resolve",
    total: 512,
    indexed: false,
    kind: Kind::Single {
        path: "/resolve_self.c",
        source: r#"
            string *parse_command_id_list() { return ({ "thing" }); }
            int resolve() { mixed *items; parse_command("thing", ({ this_object() }), "%i", items); return items[0]; }
        "#,
    },
};

/// Workers evaluating an expression through `parse_string`: the actions
/// only compute, so the block commits without a conflict.
pub static PARSE_STRING: Workload = Workload {
    name: "parse_string",
    task_label: "parse_string",
    entry: "evaluate",
    total: 512,
    indexed: false,
    kind: Kind::Single {
        path: "/parse_string.c",
        source: r#"
            string grammar = "
                whitespace = /[ \t]+/
                number = /[0-9]+/
                Expr: Term
                Expr: Expr '+' Term ? add
                Term: Factor
                Term: Term '*' Factor ? multiply
                Factor: number ? value
                Factor: '(' Expr ')' ? group
            ";
            mixed *value(mixed *tree) { int n; sscanf(tree[0], "%d", n); return ({ n }); }
            mixed *add(mixed *tree) { return ({ tree[0] + tree[2] }); }
            mixed *multiply(mixed *tree) { return ({ tree[0] * tree[2] }); }
            mixed *group(mixed *tree) { return ({ tree[1] }); }
            int evaluate() { return parse_string(grammar, "2 + 3 * (4 + 1)")[0]; }
        "#,
    },
};

/// Workers typing a line at one verb object whose applies only read: `can_`,
/// `direct_` and `do_` all just return, so the block commits without a
/// conflict.
pub static PARSER_VERB: Workload = Workload {
    name: "parser_verb",
    task_label: "parser_verb",
    entry: "poke",
    total: 512,
    indexed: false,
    kind: Kind::Set {
        objects: &[
            ("/bench_parser_verb.c", PARSER_VERB_SOURCE),
            ("/bench_parser_target.c", PARSER_TARGET_SOURCE),
            ("/bench_parser_actor.c", PARSER_ACTOR_SOURCE),
        ],
    },
};

const PARSER_VERB_SOURCE: &str = r#"
    void create() {
        parse_init();
        parse_add_rule("poke", "OBJ");
    }
    mixed can_poke_obj(object o, string w) { return 1; }
    void do_poke_obj(object o, string w) {}
"#;

const PARSER_TARGET_SOURCE: &str = r#"
    string *parse_command_id_list() { return ({ "thing" }); }
    mixed direct_poke_obj(object o, string w) { return 1; }
    void go(object d) { move_object(d); }
"#;

const PARSER_ACTOR_SOURCE: &str = r#"
    void create() {
        enable_commands();
        "/bench_parser_target"->go(this_object());
    }
    int poke() {
        set_this_player(this_object());
        return parse_sentence("poke thing");
    }
"#;

/// Every workload, in report order.
pub static WORKLOADS: [&Workload; 10] = [
    &FIB,
    &COUNTER,
    &COUNTER_ATOMIC,
    &CALL_OTHER,
    &ARR_TOUCH,
    &ARR_CHURN,
    &MOVE_CHURN,
    &RESOLVE_SELF,
    &PARSE_STRING,
    &PARSER_VERB,
];

/// Initialize `workload`'s object(s) on `vm` and hand back the apply target.
pub async fn setup_on(vm: &Vm, workload: &Workload) -> (Arc<Process>, TaskTemplate, u64) {
    let task = match &workload.kind {
        Kind::Single { path, source } => vm
            .initialize_process_from_code(path, source)
            .await
            .expect("the workload's object failed to initialize"),
        Kind::Pair => {
            vm.initialize_process_from_code("/bench_target.c", TARGET_SOURCE)
                .await
                .expect("the target object failed to initialize");
            vm.initialize_process_from_code("/bench_caller.c", CALLER_SOURCE)
                .await
                .expect("the caller object failed to initialize")
        }
        Kind::Set { objects } => {
            let mut last = None;
            for (path, source) in *objects {
                last = Some(
                    vm.initialize_process_from_code(path, source)
                        .await
                        .unwrap_or_else(|e| panic!("{path} failed to initialize: {e}")),
                );
            }
            last.expect("a Set workload places at least one object")
        }
    };
    let proc = task.context.process.clone();
    let template = TaskTemplate::from(vm.global_state.clone());
    let timeout = vm.global_state.config.max_execution_time;
    (proc, template, timeout)
}

/// Drive `workers` concurrent tasks, each applying `entry` `per_worker`
/// times; `indexed` passes each worker's index as the entry's argument. A
/// failed apply or a panicked worker fails the caller.
pub async fn fan_out_applies(
    template: &TaskTemplate,
    proc: &Arc<Process>,
    entry: &str,
    workers: usize,
    per_worker: usize,
    timeout: u64,
    indexed: bool,
) {
    let mut set = JoinSet::new();
    for worker in 0..workers {
        let proc = proc.clone();
        let template = template.clone();
        let entry = entry.to_owned();
        let args = if indexed {
            vec![crate::interpreter::lpc_ref::LpcRef::from(worker as i64)]
        } else {
            vec![]
        };
        set.spawn(async move {
            for _ in 0..per_worker {
                apply_function_by_name(
                    &entry,
                    &args,
                    proc.clone(),
                    template.clone(),
                    Some(timeout),
                )
                .await
                .expect("the apply failed")
                .expect("the apply returned an error");
            }
        });
    }
    while let Some(joined) = set.join_next().await {
        joined.expect("a worker panicked");
    }
}

#[cfg(test)]
mod tests {
    use lpc_rs_utils::config::ConfigBuilder;

    use super::*;

    /// Distinct tokens through shared rooms: inventory merges commute, so
    /// the whole block commits without a single conflict.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn move_churn_commits_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 128;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let (proc, template, timeout) = setup_on(&vm, &MOVE_CHURN).await;

        let before = vm.global_state.attempt_telemetry.snapshot();
        fan_out_applies(
            &template, &proc, "shuttle", WORKERS, PER_WORKER, timeout, true,
        )
        .await;
        let after = vm.global_state.attempt_telemetry.snapshot();

        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "inventory merges commute; distinct tokens must not conflict"
        );
    }

    /// Concurrent `count++` commutes: the whole block commits without a
    /// single conflict, and no update is lost.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn counter_atomic_commits_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 256;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let (proc, template, timeout) = setup_on(&vm, &COUNTER_ATOMIC).await;

        let before = vm.global_state.attempt_telemetry.snapshot();
        fan_out_applies(
            &template,
            &proc,
            "increment",
            WORKERS,
            PER_WORKER,
            timeout,
            false,
        )
        .await;
        let after = vm.global_state.attempt_telemetry.snapshot();

        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "merge writes commute; concurrent ++ must not conflict"
        );

        let total = (WORKERS * PER_WORKER) as i64;
        let result = apply_function_by_name("get", &[], proc.clone(), template, Some(timeout))
            .await
            .expect("the get apply failed")
            .expect("the get apply returned an error");
        assert_eq!(
            result,
            crate::interpreter::lpc_ref::LpcRef::from(total),
            "no lost updates"
        );
    }

    /// Workers scanning into their own locals through `sscanf`: no shared
    /// cell is written, so the block commits without a conflict.
    static SCAN_LOCALS: Workload = Workload {
        name: "scan_locals",
        task_label: "scan_locals",
        entry: "scan",
        total: 512,
        indexed: false,
        kind: Kind::Single {
            path: "/scan_locals.c",
            source: r#"
                int scan() { int n; string s; sscanf("hp 12 left", "hp %d %s", n, s); return n; }
            "#,
        },
    };

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn sscanf_into_locals_commits_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 128;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let (proc, template, timeout) = setup_on(&vm, &SCAN_LOCALS).await;
        let before = vm.global_state.attempt_telemetry.snapshot();
        fan_out_applies(
            &template, &proc, "scan", WORKERS, PER_WORKER, timeout, false,
        )
        .await;
        let after = vm.global_state.attempt_telemetry.snapshot();
        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "locals are private cells; no conflict"
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn parse_command_over_a_shared_scope_commits_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 128;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let (proc, template, timeout) = setup_on(&vm, &RESOLVE_SELF).await;
        let before = vm.global_state.attempt_telemetry.snapshot();
        fan_out_applies(
            &template, &proc, "resolve", WORKERS, PER_WORKER, timeout, false,
        )
        .await;
        let after = vm.global_state.attempt_telemetry.snapshot();
        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "the applies only read"
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn parse_string_actions_commit_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 128;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let (proc, template, timeout) = setup_on(&vm, &PARSE_STRING).await;
        let before = vm.global_state.attempt_telemetry.snapshot();
        fan_out_applies(
            &template, &proc, "evaluate", WORKERS, PER_WORKER, timeout, false,
        )
        .await;
        let after = vm.global_state.attempt_telemetry.snapshot();
        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "the actions only compute"
        );
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn parse_sentence_over_read_only_handlers_commits_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 128;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let (proc, template, timeout) = setup_on(&vm, &PARSER_VERB).await;
        let before = vm.global_state.attempt_telemetry.snapshot();
        fan_out_applies(
            &template, &proc, "poke", WORKERS, PER_WORKER, timeout, false,
        )
        .await;
        let after = vm.global_state.attempt_telemetry.snapshot();
        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "can_/direct_/do_ all just read and return"
        );
    }

    const VERB_OWNER_SOURCE: &str = r#"
        void register() {
            parse_init();
            parse_add_rule("give", "OBJ");
        }
        string dump() { return parse_dump(); }
    "#;

    /// `parse_add_rule` is a pure append onto the shared `verb_rules` cell
    /// (B1/B2's conflict point): distinct verb objects registering
    /// concurrently must commute, the way `COUNTER_ATOMIC`'s `++` does.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn verb_registration_from_distinct_owners_commits_without_conflicts() {
        const WORKERS: usize = 4;
        const PER_WORKER: usize = 64;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        let template = TaskTemplate::from(vm.global_state.clone());
        let timeout = vm.global_state.config.max_execution_time;

        let mut owners = Vec::with_capacity(WORKERS);
        for i in 0..WORKERS {
            let path = format!("/bench_verb_owner_{i}.c");
            let task = vm
                .initialize_process_from_code(&path, VERB_OWNER_SOURCE)
                .await
                .expect("the verb owner failed to initialize");
            owners.push(task.context.process.clone());
        }

        let before = vm.global_state.attempt_telemetry.snapshot();
        let mut set = JoinSet::new();
        for owner in &owners {
            let owner = owner.clone();
            let template = template.clone();
            set.spawn(async move {
                for _ in 0..PER_WORKER {
                    apply_function_by_name(
                        "register",
                        &[],
                        owner.clone(),
                        template.clone(),
                        Some(timeout),
                    )
                    .await
                    .expect("the register apply failed")
                    .expect("the register apply returned an error");
                }
            });
        }
        while let Some(joined) = set.join_next().await {
            joined.expect("a worker panicked");
        }
        let after = vm.global_state.attempt_telemetry.snapshot();

        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "parse_add_rule is a pure append; appends from different owners commute"
        );

        let dump = apply_function_by_name("dump", &[], owners[0].clone(), template, Some(timeout))
            .await
            .expect("the dump apply failed")
            .expect("the dump apply returned an error");
        let lines = match dump {
            crate::interpreter::lpc_ref::LpcRef::String(s) => s.to_string().lines().count(),
            other => panic!("parse_dump() did not return a string: {other:?}"),
        };
        assert_eq!(lines, WORKERS * PER_WORKER, "no appended rule is lost");
    }

    const READER_SOURCE: &str = r#"
        void create() { enable_commands(); }
        int poke() {
            set_this_player(this_object());
            return parse_sentence("xyzzy nothing");
        }
    "#;

    const DESTROYER_SOURCE: &str = r#"
        void kill() {
            object v = clone_object("/bench_victim");
            destruct(v);
        }
    "#;

    /// B1's gate: a process that never called `parse_init()` cannot own a
    /// parser rule, so destructing one must not touch `verb_rules` — a
    /// concurrent `parse_sentence` for a verb nothing registered keeps
    /// reading that cell without a conflict.
    #[tokio::test(flavor = "multi_thread", worker_threads = 4)]
    async fn destructing_a_non_verb_object_does_not_conflict_with_unhandled_commands() {
        const READERS: usize = 3;
        const PER_READER: usize = 128;
        const DESTROYS: usize = 128;
        let config = ConfigBuilder::default()
            .lib_dir("./tests/fixtures/code")
            .max_execution_time(30_000_u64)
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.initialize_process_from_code("/bench_victim.c", "void create() {}")
            .await
            .expect("the victim prototype failed to initialize");
        let reader = vm
            .initialize_process_from_code("/bench_command_reader.c", READER_SOURCE)
            .await
            .expect("the reader object failed to initialize")
            .context
            .process;
        let destroyer = vm
            .initialize_process_from_code("/bench_destroyer.c", DESTROYER_SOURCE)
            .await
            .expect("the destroyer object failed to initialize")
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let timeout = vm.global_state.config.max_execution_time;

        let before = vm.global_state.attempt_telemetry.snapshot();
        let mut set = JoinSet::new();
        for _ in 0..READERS {
            let reader = reader.clone();
            let template = template.clone();
            set.spawn(async move {
                for _ in 0..PER_READER {
                    apply_function_by_name(
                        "poke",
                        &[],
                        reader.clone(),
                        template.clone(),
                        Some(timeout),
                    )
                    .await
                    .expect("the poke apply failed")
                    .expect("the poke apply returned an error");
                }
            });
        }
        {
            let destroyer = destroyer.clone();
            let template = template.clone();
            set.spawn(async move {
                for _ in 0..DESTROYS {
                    apply_function_by_name(
                        "kill",
                        &[],
                        destroyer.clone(),
                        template.clone(),
                        Some(timeout),
                    )
                    .await
                    .expect("the kill apply failed")
                    .expect("the kill apply returned an error");
                }
            });
        }
        while let Some(joined) = set.join_next().await {
            joined.expect("a worker panicked");
        }
        let after = vm.global_state.attempt_telemetry.snapshot();

        assert_eq!(
            after.conflicts - before.conflicts,
            0,
            "a non-verb object's destruct must not touch verb_rules (B1's gate)"
        );
    }

    #[tokio::test]
    async fn every_workload_sets_up_and_applies_once() {
        for workload in WORKLOADS {
            let config = ConfigBuilder::default()
                .lib_dir("./tests/fixtures/code")
                .max_execution_time(30_000_u64)
                .build()
                .unwrap();
            let vm = Vm::new(config);
            let (proc, template, timeout) = setup_on(&vm, workload).await;
            fan_out_applies(
                &template,
                &proc,
                workload.entry,
                1,
                1,
                timeout,
                workload.indexed,
            )
            .await;
        }
    }
}
