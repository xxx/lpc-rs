//! Presence events: `init()` on movement, and the rules a move or a
//! destruct takes away.

use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    command::{dispatch::apply_on, registry::Scope},
    interpreter::{
        INIT,
        process::Process,
        stm::{MergeOp, TxnHandle},
        task_context::TaskContext,
    },
};

/// The scope `mover` will have once it stands in `new_env`.
pub(crate) fn scope_after_move(
    txn: &TxnHandle,
    mover: &Arc<Process>,
    new_env: &Arc<Process>,
) -> Scope {
    let mut members = vec![mover.clone(), new_env.clone()];
    members.extend(Process::inventory_of(txn, new_env));
    members.extend(Process::inventory_of(txn, mover));
    Scope::new(members)
}

/// Before a move: a living forgets rules from owners leaving its scope; a
/// thing is forgotten by the livings it leaves.
pub(crate) fn before_move(txn: &TxnHandle, mover: &Arc<Process>, new_env: &Arc<Process>) {
    if mover.commands_enabled(txn) {
        let keep = scope_after_move(txn, mover, new_env);
        txn.with(|t| t.merge(mover.rules.id, MergeOp::RulesRetainOwners(keep)));
    } else {
        forget_owner(txn, mover);
    }
}

/// Every living around `object` forgets the rules it registered; the
/// environment's members come from its `livings` cell, not its whole
/// inventory, so this does not conflict with a concurrent non-living mover.
pub(crate) fn forget_owner(txn: &TxnHandle, object: &Arc<Process>) {
    // Living-ness is read before the lock: `commands_enabled` takes the handle.
    let mut livings: Vec<Arc<Process>> = Process::inventory_of(txn, object)
        .into_iter()
        .filter(|holder| holder.commands_enabled(txn))
        .collect();
    if let Some(environment) = Process::environment_of(txn, object) {
        livings.extend(Process::livings_of(txn, &environment));
        if environment.commands_enabled(txn) {
            livings.push(environment);
        }
    }
    let gone = Scope::new([object.clone()]);
    txn.with(|t| {
        for living in &livings {
            t.merge(living.rules.id, MergeOp::RulesRemoveOwners(gone.clone()));
        }
    });
}

/// After a move: `init()` in MudOS order, with `this_player` the living
/// each object is meeting; a non-living mover reads only `new_env`'s
/// `livings` cell, a living one reads the whole inventory since it must
/// `init()` every object there.
pub(crate) async fn after_move(
    ctx: &TaskContext,
    mover: &Arc<Process>,
    new_env: &Arc<Process>,
) -> Result<()> {
    let txn = ctx.txn();
    let env_is_living = new_env.commands_enabled(txn);

    if mover.commands_enabled(txn) {
        let others: Vec<Arc<Process>> = Process::inventory_of(txn, new_env)
            .into_iter()
            .filter(|ob| !Arc::ptr_eq(ob, mover))
            .collect();
        let livings: Vec<&Arc<Process>> = others
            .iter()
            .filter(|ob| ob.commands_enabled(txn))
            .collect();
        fire_init(ctx, new_env, mover).await?;
        for ob in &others {
            fire_init(ctx, ob, mover).await?;
        }
        for living in &livings {
            fire_init(ctx, mover, living).await?;
        }
    } else {
        for living in Process::livings_of(txn, new_env) {
            fire_init(ctx, mover, &living).await?;
        }
    }
    if env_is_living {
        fire_init(ctx, mover, new_env).await?;
    }
    Ok(())
}

/// `target->init()` with `this_player` set, if the target defines it.
pub(crate) async fn fire_init(
    ctx: &TaskContext,
    target: &Arc<Process>,
    this_player: &Arc<Process>,
) -> Result<()> {
    let Some(init) = target.program.unmangled_functions.get(INIT).cloned() else {
        return Ok(());
    };
    apply_on(ctx, target, this_player, init, &[])
        .await
        .map(|_| ())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, lpc_string::LpcString, vm::Vm},
        test_support::test_config,
    };

    fn s(text: &str) -> LpcRef {
        LpcString::from(text).into()
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
