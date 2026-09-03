//! Presence events: `init()` on movement, and the rules a move or a
//! destruct takes away.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::scope::{self, Scope},
    interpreter::{
        INIT,
        apply::apply_hook,
        process::Process,
        stm::{MergeOp, TxnHandle},
        task_context::{Caller, Callers, TaskContext},
    },
};

/// Before a move: the livings left behind forget the mover's rules, and a
/// living mover keeps only rules whose owners stay in its post-move scope.
/// Without the first half, a living that leaves and returns would register
/// its rules on its neighbours once per visit.
pub(crate) fn before_move(txn: &TxnHandle, mover: &Arc<Process>, new_env: &Arc<Process>) {
    forget_departure(txn, mover);
    if mover.commands_enabled(txn) {
        let keep = scope::after_move(txn, mover, new_env);
        txn.with(|t| t.merge(mover.rules.id, MergeOp::RulesRetainOwners(keep)));
    }
}

/// The livings `object` leaves behind forget the rules it registered; its own
/// contents keep theirs.
pub(crate) fn forget_departure(txn: &TxnHandle, object: &Arc<Process>) {
    forget(txn, object, &witnesses_left_behind(txn, object));
}

/// Every living holding `object`'s rules forgets them: those it leaves
/// behind, plus the living contents that go down with it.
pub(crate) fn forget_destruct(txn: &TxnHandle, object: &Arc<Process>) {
    // `commands_enabled` takes the handle; inside the lock it would deadlock.
    let mut livings = witnesses_left_behind(txn, object);
    livings.extend(
        Process::inventory_of(txn, object)
            .into_iter()
            .filter(|holder| holder.commands_enabled(txn)),
    );
    forget(txn, object, &livings);
}

/// The livings around `object` that its rules can have reached; do not read
/// the environment's whole inventory here, a concurrent non-living mover
/// would conflict.
fn witnesses_left_behind(txn: &TxnHandle, object: &Arc<Process>) -> Vec<Arc<Process>> {
    let Some(environment) = Process::environment_of(txn, object) else {
        return Vec::new();
    };
    let mut livings: Vec<Arc<Process>> = Process::livings_of(txn, &environment)
        .into_iter()
        .filter(|living| !Arc::ptr_eq(living, object))
        .collect();
    if environment.commands_enabled(txn) {
        livings.push(environment);
    }
    livings
}

/// Each of `livings` drops every rule `object` owns.
fn forget(txn: &TxnHandle, object: &Arc<Process>, livings: &[Arc<Process>]) {
    if livings.is_empty() {
        return;
    }
    let gone = Scope::new([object.clone()]);
    txn.with(|t| {
        for living in livings {
            t.merge(living.rules.id, MergeOp::RulesRemoveOwners(gone.clone()));
        }
    });
}

/// After a move: `init()` in MudOS order, with `this_player` the living
/// each object is meeting; a non-living mover reads only `new_env`'s
/// `livings` cell, a living one the whole inventory.
pub(crate) async fn after_move(
    ctx: &TaskContext,
    callers: Callers,
    mover: &Arc<Process>,
    new_env: &Arc<Process>,
) -> Result<()> {
    let txn = ctx.txn();
    let env_is_living = new_env.commands_enabled(txn);

    if mover.commands_enabled(txn) {
        // The inventory can still name an object this attempt destructed.
        let others: Vec<Arc<Process>> = Process::inventory_of(txn, new_env)
            .into_iter()
            .filter(|ob| !Arc::ptr_eq(ob, mover) && ob.is_live(txn))
            .collect();
        let livings: Vec<&Arc<Process>> = others
            .iter()
            .filter(|ob| ob.commands_enabled(txn))
            .collect();
        fire_init(ctx, &callers, new_env, mover).await?;
        for ob in &others {
            fire_init(ctx, &callers, ob, mover).await?;
        }
        for living in &livings {
            fire_init(ctx, &callers, mover, living).await?;
        }
    } else {
        for living in Process::livings_of(txn, new_env) {
            fire_init(ctx, &callers, mover, &living).await?;
        }
    }
    if env_is_living {
        fire_init(ctx, &callers, mover, new_env).await?;
    }
    Ok(())
}

/// `target->init()` with `this_player` set, entered for that living in
/// front of `callers`, if the target defines it.
pub(crate) async fn fire_init(
    ctx: &TaskContext,
    callers: &Callers,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
) -> Result<()> {
    let chain = Some(Caller::link(this_player.clone(), callers.clone()));
    apply_hook(ctx, chain, target, this_player, INIT, &[])
        .await
        .map(|_| ())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_utils::lpc_string::LpcString;
    use tokio::{sync::Barrier, task::JoinSet};

    use super::*;
    use crate::{
        command::command_task::run_command_line,
        interpreter::{
            CommittedReader,
            lpc_ref::LpcRef,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::Vm,
        },
        test_support::test_config,
    };

    fn s(text: &str) -> LpcRef {
        LpcString::from(text).into()
    }

    /// The objects named by `process`'s committed `livings` cell.
    fn committed_livings(vm: &Vm, process: &Arc<Process>) -> Vec<Arc<Process>> {
        let Some(livings) = vm.global_state.committed_array(process.position.livings.id) else {
            return Vec::new();
        };
        livings
            .iter()
            .filter_map(|item| match item {
                LpcRef::Object(weak) => weak.upgrade(),
                _ => None,
            })
            .collect()
    }

    /// The filenames of `processes`.
    fn names(processes: &[Arc<Process>]) -> Vec<String> {
        processes.iter().map(|p| p.filename().to_string()).collect()
    }

    const LOG: &str = indoc! { r#"
        string log = "";
        void note(string entry) { log += entry + ";"; }
    "# };

    #[tokio::test]
    async fn a_rooms_verb_is_usable_in_the_task_that_entered() {
        let room = indoc! { r#"
            string seen;
            void init() { add_action("do_look", "look"); }
            int do_look(string arg) { seen = arg; return 1; }
        "# };
        let player = indoc! { r#"
            int r;
            void create() { enable_commands(); move_object("/room"); r = command("look here", this_object()); }
        "# };
        let vm = Vm::new(test_config());
        let room_proc = vm.create_process_from_code("/room.c", room).await.unwrap();
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            LpcRef::from(1)
        );
        assert_eq!(
            vm.global_state.committed_global(&room_proc, 0u16),
            s("here")
        );
    }

    #[tokio::test]
    async fn init_fires_in_mudos_order_for_a_living_entering_a_room() {
        let room = indoc! { r#"
            void init() { "/log"->note("room:" + file_name(this_player())); }
        "# };
        let npc = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
            void init() { "/log"->note("npc:" + file_name(this_player())); }
        "# };
        let player = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
            void init() { "/log"->note("player:" + file_name(this_player())); }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/npc.c", npc)
            .await
            .unwrap();
        vm.initialize_process_from_code("/player.c", player)
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&log, 0u16),
            s("room:/npc;room:/player;npc:/player;player:/npc;")
        );
    }

    #[tokio::test]
    async fn a_thing_entering_a_room_inits_once_per_living() {
        let room = "void create() {}";
        let npc = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let player = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let item = indoc! { r#"
            void create() { move_object("/room"); }
            void init() { "/log"->note("item:" + file_name(this_player())); }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/npc.c", npc)
            .await
            .unwrap();
        vm.initialize_process_from_code("/player.c", player)
            .await
            .unwrap();
        vm.initialize_process_from_code("/item.c", item)
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&log, 0u16),
            s("item:/npc;item:/player;")
        );
    }

    #[tokio::test]
    async fn a_living_entering_a_livings_inventory_inits_with_it() {
        let holder = indoc! { r#"
            void create() { enable_commands(); }
            void init() { "/log"->note("holder:" + file_name(this_player())); }
        "# };
        let pet = indoc! { r#"
            void create() { enable_commands(); move_object("/holder"); }
            void init() { "/log"->note("pet:" + file_name(this_player())); }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code("/holder.c", holder)
            .await
            .unwrap();
        vm.initialize_process_from_code("/pet.c", pet)
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&log, 0u16),
            s("holder:/pet;pet:/holder;")
        );
    }

    #[tokio::test]
    async fn a_living_that_enables_commands_after_arriving_is_seen_by_the_next_arrival() {
        let room = "void create() {}";
        let occupant = indoc! { r#"
            void create() { move_object("/room"); enable_commands(); }
        "# };
        let item = indoc! { r#"
            void create() { move_object("/room"); }
            void init() { "/log"->note("item:" + file_name(this_player())); }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/occupant.c", occupant)
            .await
            .unwrap();
        vm.initialize_process_from_code("/item.c", item)
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&log, 0u16),
            s("item:/occupant;")
        );
    }

    #[tokio::test]
    async fn enabling_commands_twice_registers_one_living() {
        let room = "void create() {}";
        let occupant = indoc! { r#"
            void create() { move_object("/room"); enable_commands(); enable_commands(); }
        "# };
        let item = indoc! { r#"
            void create() { move_object("/room"); }
            void init() { "/log"->note("item:" + file_name(this_player())); }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/occupant.c", occupant)
            .await
            .unwrap();
        vm.initialize_process_from_code("/item.c", item)
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&log, 0u16),
            s("item:/occupant;")
        );
    }

    #[tokio::test]
    async fn leaving_a_room_takes_its_verbs_away() {
        let room_a = indoc! { r#"
            void init() { add_action("do_a", "a"); }
            int do_a(string arg) { return 1; }
        "# };
        let room_b = indoc! { r#"
            void init() { add_action("do_b", "b"); }
            int do_b(string arg) { return 1; }
        "# };
        let player = indoc! { r#"
            int a; int b;
            void create() {
                enable_commands();
                move_object("/room_a");
                move_object("/room_b");
                a = command("a", this_object());
                b = command("b", this_object());
            }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room_a.c", room_a)
            .await
            .unwrap();
        vm.create_process_from_code("/room_b.c", room_b)
            .await
            .unwrap();
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 0u16),
            LpcRef::from(0)
        );
        assert_eq!(
            vm.global_state.committed_global(&player_proc, 1u16),
            LpcRef::from(1)
        );
        let verbs: Vec<String> = vm
            .global_state
            .committed_rules(&player_proc)
            .iter()
            .map(|r| r.verb.to_string())
            .collect();
        assert_eq!(verbs, vec!["b"]);
    }

    #[tokio::test]
    async fn destructing_an_owner_removes_its_rules_from_every_living_around_it() {
        let room = "void create() {}";
        let sign = indoc! { r#"
            void create() { move_object("/room"); }
            void init() { add_action("do_read", "read"); }
            int do_read(string arg) { return 1; }
        "# };
        let player = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let master = indoc! { r#"
            void create() { destruct(find_object("/sign")); }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room.c", room).await.unwrap();
        let player_proc = vm
            .initialize_process_from_code("/player.c", player)
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code("/sign.c", sign)
            .await
            .unwrap();
        assert_eq!(vm.global_state.committed_rules(&player_proc).len(), 1);
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        assert!(vm.global_state.committed_rules(&player_proc).is_empty());
    }

    #[tokio::test]
    async fn disabling_commands_drops_the_rules() {
        let code = indoc! { r#"
            void create() {
                set_this_player(this_object());
                enable_commands();
                add_action("do_look", "look");
                disable_commands();
            }
            int do_look(string arg) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        let proc = vm
            .initialize_process_from_code("/player.c", code)
            .await
            .unwrap()
            .context
            .process;
        assert!(vm.global_state.committed_rules(&proc).is_empty());
    }

    #[tokio::test]
    async fn a_living_that_leaves_and_returns_registers_once() {
        let room = "void create() {}";
        let void = "void create() {}";
        let stationary = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let traveller = indoc! { r#"
            void create() {
                enable_commands();
                move_object("/room");
                move_object("/void");
                move_object("/room");
            }
            void init() { add_action("do_wave", "wave"); }
            int do_wave(string arg) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.create_process_from_code("/void.c", void).await.unwrap();
        let stationary_proc = vm
            .initialize_process_from_code("/stationary.c", stationary)
            .await
            .unwrap()
            .context
            .process;
        vm.initialize_process_from_code("/traveller.c", traveller)
            .await
            .unwrap();
        let verbs: Vec<String> = vm
            .global_state
            .committed_rules(&stationary_proc)
            .iter()
            .map(|r| r.verb.to_string())
            .collect();
        assert_eq!(verbs, vec!["wave"]);
    }

    #[tokio::test]
    async fn a_handler_that_returns_zero_runs_once_after_a_round_trip() {
        let room = "void create() {}";
        let void = "void create() {}";
        let stationary = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let traveller = indoc! { r#"
            int tries;
            void create() {
                enable_commands();
                move_object("/room");
                move_object("/void");
                move_object("/room");
            }
            void init() { add_action("do_wave", "wave"); }
            int do_wave(string arg) { tries += 1; return 0; }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.create_process_from_code("/void.c", void).await.unwrap();
        let stationary_proc = vm
            .initialize_process_from_code("/stationary.c", stationary)
            .await
            .unwrap()
            .context
            .process;
        let traveller_proc = vm
            .initialize_process_from_code("/traveller.c", traveller)
            .await
            .unwrap()
            .context
            .process;
        let template = TaskTemplate::from(vm.global_state.clone());
        run_command_line(&template, stationary_proc, "wave".into())
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&traveller_proc, 0u16),
            LpcRef::from(1)
        );
    }

    #[tokio::test]
    async fn a_living_inside_a_moving_container_keeps_its_actions() {
        let room_a = "void create() {}";
        let room_b = "void create() {}";
        let box_code = indoc! { r#"
            void create() { move_object("/room_a"); }
            void relocate() { move_object("/room_b"); }
            void init() { add_action("do_open", "open"); }
            int do_open(string arg) { return 1; }
        "# };
        let pet = indoc! { r#"
            void create() { enable_commands(); move_object("/box"); }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room_a.c", room_a)
            .await
            .unwrap();
        vm.create_process_from_code("/room_b.c", room_b)
            .await
            .unwrap();
        let box_proc = vm
            .initialize_process_from_code("/box.c", box_code)
            .await
            .unwrap()
            .context
            .process;
        let pet_proc = vm
            .initialize_process_from_code("/pet.c", pet)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_rules(&pet_proc).len(), 1);
        apply_function_by_name(
            "relocate",
            &[],
            box_proc,
            TaskTemplate::from(vm.global_state.clone()),
            None,
        )
        .await
        .unwrap()
        .unwrap();
        let verbs: Vec<String> = vm
            .global_state
            .committed_rules(&pet_proc)
            .iter()
            .map(|r| r.verb.to_string())
            .collect();
        assert_eq!(verbs, vec!["open"]);
    }

    #[tokio::test]
    async fn an_object_destructed_in_this_attempt_is_not_initialized() {
        let room = "void create() {}";
        let ghost = indoc! { r#"
            void create() { move_object("/room"); }
            void init() { "/log"->note("ghost:" + file_name(this_player())); }
        "# };
        let player = indoc! { r#"
            void create() {
                enable_commands();
                destruct(find_object("/ghost"));
                move_object("/room");
            }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/ghost.c", ghost)
            .await
            .unwrap();
        vm.initialize_process_from_code("/player.c", player)
            .await
            .unwrap();
        assert_eq!(vm.global_state.committed_global(&log, 0u16), s(""));
    }

    #[tokio::test]
    async fn a_destructed_object_keeps_no_rules() {
        let sign = indoc! { r#"
            void create() { set_this_player(this_object()); enable_commands(); add_action("do_read", "read"); }
            int do_read(string arg) { return 1; }
        "# };
        let master = indoc! { r#"
            void create() { destruct(find_object("/sign")); }
        "# };
        let vm = Vm::new(test_config());
        let sign_proc = vm
            .initialize_process_from_code("/sign.c", sign)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_rules(&sign_proc).len(), 1);
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        assert!(vm.global_state.committed_rules(&sign_proc).is_empty());
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn two_livings_entering_one_room_at_once_meet_each_other() {
        let room = "void create() {}";
        let body = indoc! { r#"
            void create() { enable_commands(); }
            void enter() { move_object("/room"); }
            void init() { add_action("do_greet", "greet"); }
            int do_greet(string arg) { return 1; }
        "# };
        let vm = Vm::new(test_config());
        vm.create_process_from_code("/room.c", room).await.unwrap();
        let first = vm
            .initialize_process_from_code("/first.c", body)
            .await
            .unwrap()
            .context
            .process;
        let second = vm
            .initialize_process_from_code("/second.c", body)
            .await
            .unwrap()
            .context
            .process;

        let template = TaskTemplate::from(vm.global_state.clone());
        let gate = Arc::new(Barrier::new(2));
        let mut set = JoinSet::new();
        for process in [first.clone(), second.clone()] {
            let template = template.clone();
            let gate = gate.clone();
            set.spawn(async move {
                gate.wait().await;
                apply_function_by_name("enter", &[], process, template, None)
                    .await
                    .expect("enter() is defined")
                    .expect("the move succeeded")
            });
        }
        while let Some(joined) = set.join_next().await {
            joined.expect("a mover panicked");
        }

        for (mover, other) in [(&first, &second), (&second, &first)] {
            let rules = vm.global_state.committed_rules(mover);
            assert_eq!(
                rules.len(),
                1,
                "{} has {} rules",
                mover.filename(),
                rules.len()
            );
            assert!(
                rules[0].owner().is_some_and(|o| Arc::ptr_eq(&o, other)),
                "{} holds a rule owned by {}",
                mover.filename(),
                other.filename()
            );
        }
    }

    #[tokio::test]
    async fn a_rooms_livings_are_living_members_of_its_inventory() {
        let room = "void create() {}";
        let enabler = indoc! { r#"
            void create() { move_object("/room"); enable_commands(); }
        "# };
        let mover = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let quitter = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); disable_commands(); }
        "# };
        let doomed = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let master = indoc! { r#"
            void create() { destruct(find_object("/doomed")); }
        "# };
        let vm = Vm::new(test_config());
        let room_proc = vm.create_process_from_code("/room.c", room).await.unwrap();
        for (path, code) in [
            ("/enabler.c", enabler),
            ("/mover.c", mover),
            ("/quitter.c", quitter),
            ("/doomed.c", doomed),
        ] {
            vm.initialize_process_from_code(path, code).await.unwrap();
        }
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();

        let livings = committed_livings(&vm, &room_proc);
        assert_eq!(names(&livings), vec!["/enabler", "/mover"]);
        let inventory = vm.global_state.committed_inventory(&room_proc);
        assert!(
            livings.iter().all(|living| inventory
                .iter()
                .any(|member| Arc::ptr_eq(member, living)
                    && vm.global_state.commands_enabled(member))),
            "livings {:?} are living members of inventory {:?}",
            names(&livings),
            names(&inventory)
        );
    }

    #[tokio::test]
    async fn disabling_commands_leaves_the_rooms_livings() {
        let room = "void create() {}";
        let occupant = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); disable_commands(); }
        "# };
        let vm = Vm::new(test_config());
        let room_proc = vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/occupant.c", occupant)
            .await
            .unwrap();
        assert!(committed_livings(&vm, &room_proc).is_empty());
    }

    #[tokio::test]
    async fn a_destructed_living_leaves_the_rooms_livings() {
        let room = "void create() {}";
        let occupant = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); }
        "# };
        let master = indoc! { r#"
            void create() { destruct(find_object("/occupant")); }
        "# };
        let vm = Vm::new(test_config());
        let room_proc = vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/occupant.c", occupant)
            .await
            .unwrap();
        assert_eq!(committed_livings(&vm, &room_proc).len(), 1);
        vm.initialize_process_from_code("/secure/master.c", master)
            .await
            .unwrap();
        assert!(committed_livings(&vm, &room_proc).is_empty());
    }

    #[tokio::test]
    async fn moving_to_the_current_room_fires_nothing() {
        let room = indoc! { r#"
            void init() { "/log"->note("room:" + file_name(this_player())); }
        "# };
        let player = indoc! { r#"
            void create() { enable_commands(); move_object("/room"); move_object("/room"); }
        "# };
        let vm = Vm::new(test_config());
        let log = vm
            .initialize_process_from_code("/log.c", LOG)
            .await
            .unwrap()
            .context
            .process;
        vm.create_process_from_code("/room.c", room).await.unwrap();
        vm.initialize_process_from_code("/player.c", player)
            .await
            .unwrap();
        assert_eq!(
            vm.global_state.committed_global(&log, 0u16),
            s("room:/player;")
        );
    }
}
