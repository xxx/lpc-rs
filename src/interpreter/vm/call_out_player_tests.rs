use std::sync::Arc;

use lpc_rs_core::null_mode::NullMode;
use lpc_rs_errors::Result;
use lpc_rs_utils::config::ConfigBuilder;

use crate::{
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    },
    test_support::{connect, permissive_master},
};

async fn vm(preserve: bool, mode: NullMode) -> Vm {
    let vm = Vm::new(
        crate::test_config_builder!()
            .call_out_preserve_player(preserve)
            .null_mode(mode)
            .build()
            .unwrap(),
    );
    permissive_master(&vm.global_state.object_space).await;
    vm
}

async fn object(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, code)
        .await
        .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
        .context
        .process
}

async fn call(
    vm: &Vm,
    owner: &Arc<Process>,
    player: Option<&Arc<Process>>,
    name: &str,
) -> Result<LpcRef> {
    let template = TaskTemplate::from(vm.global_state.clone());
    template.set_this_player(player.cloned());
    apply_function_by_name(name, &[], owner.clone(), template, Some(4000))
        .await
        .unwrap()
}

fn timers(vm: &Vm) -> Vec<u64> {
    vm.global_state
        .with_call_outs(|calls| calls.queue().iter().map(|(_, call)| call.id).collect())
}

async fn fire(vm: &Vm, id: u64) {
    vm.global_state.prioritize_call_out(id).await.await.unwrap();
}

fn reference(player: &Arc<Process>) -> LpcRef {
    Arc::downgrade(player).into()
}

const RECORDER: &str = r#"
    object seen;
    object interactive_seen;
    int runs;
    string dispatch;
    object caller;
    void record() {
        seen = this_player(); interactive_seen = this_interactive();
        runs++; dispatch = origin(); caller = previous_object();
        set_this_player(nil);
    }
    void start() { call_out(record, 100); set_this_player(nil); }
    void repeat() { call_out(record, 100, 100); set_this_player(nil); }
    void fail_start() { call_out(record, 100); throw("no commit"); }
    void cancel_start() { int id = call_out(record, 100); remove_call_out(id); }
"#;

#[tokio::test]
async fn call_out_preservation_is_opt_in_and_independent_of_null_mode() {
    for mode in [NullMode::Zero, NullMode::Distinct] {
        for preserve in [false, true] {
            let vm = vm(preserve, mode).await;
            let player = object(&vm, "/player.c", "").await;
            let owner = object(&vm, "/owner.c", RECORDER).await;
            call(&vm, &owner, Some(&player), "start").await.unwrap();
            fire(&vm, timers(&vm)[0]).await;
            assert_eq!(
                vm.global_state.committed_global(&owner, 0u16),
                if preserve {
                    reference(&player)
                } else {
                    LpcRef::nil(mode)
                }
            );
            assert_eq!(
                vm.global_state.committed_global(&owner, 1u16),
                LpcRef::nil(mode)
            );
            assert_eq!(
                vm.global_state.committed_global(&owner, 3u16),
                LpcRef::from("call_out")
            );
            assert_eq!(
                vm.global_state.committed_global(&owner, 4u16),
                reference(&owner)
            );
        }
    }
}

#[tokio::test]
async fn connected_scheduling_player_does_not_become_this_interactive() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(&vm, "/player.c", "").await;
    let _connection = connect(&vm, &player).await;
    let owner = object(&vm, "/owner.c", RECORDER).await;
    call(&vm, &owner, Some(&player), "start").await.unwrap();
    fire(&vm, timers(&vm)[0]).await;
    assert_eq!(
        vm.global_state.committed_global(&owner, 0u16),
        reference(&player)
    );
    assert_eq!(vm.global_state.committed_global(&owner, 1u16), LpcRef::Nil);
}

#[tokio::test]
async fn lazy_receiver_setup_is_separate_from_the_preserved_callback_context() {
    let lib = crate::test_support::lib_holding(
        "call-out-player",
        &[
            ("secure/master.c", crate::test_support::PERMISSIVE_MASTER),
            (
                "receiver.c",
                r#"
            object initialized_player; object initialized_interactive;
            object seen; object interactive_seen;
            void create() {
                initialized_player = this_player();
                initialized_interactive = this_interactive();
                set_this_player(this_object());
            }
            void tick() { seen = this_player(); interactive_seen = this_interactive(); }
        "#,
            ),
        ],
    );
    let mut vm = Vm::new(
        ConfigBuilder::default()
            .lib_dir(lib.to_str().unwrap())
            .call_out_preserve_player(true)
            .null_mode(NullMode::Distinct)
            .build()
            .unwrap(),
    );
    vm.bootstrap().await.unwrap();
    let player = object(&vm, "/player.c", "").await;
    let _connection = connect(&vm, &player).await;
    let owner = object(
        &vm,
        "/owner.c",
        r#"
        void start() { call_out(&call_other("/receiver", "tick"), 100); }
    "#,
    )
    .await;
    call(&vm, &owner, Some(&player), "start").await.unwrap();
    assert!(vm.global_state.object_space.lookup("/receiver").is_none());
    fire(&vm, timers(&vm)[0]).await;
    let receiver = vm.global_state.object_space.lookup("/receiver").unwrap();
    for register in [0u16, 1, 3] {
        assert_eq!(
            vm.global_state.committed_global(&receiver, register),
            LpcRef::Nil
        );
    }
    assert_eq!(
        vm.global_state.committed_global(&receiver, 2u16),
        reference(&player)
    );
}

#[tokio::test]
async fn repeats_restore_the_original_player_after_callback_mutation() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(&vm, "/player.c", "").await;
    let owner = object(&vm, "/owner.c", RECORDER).await;
    call(&vm, &owner, Some(&player), "repeat").await.unwrap();
    let id = timers(&vm)[0];
    for runs in 1..=2 {
        fire(&vm, id).await;
        assert_eq!(
            vm.global_state.committed_global(&owner, 0u16),
            reference(&player)
        );
        assert_eq!(
            vm.global_state.committed_global(&owner, 2u16),
            LpcRef::from(runs)
        );
        assert_eq!(timers(&vm), [id]);
    }
}

#[tokio::test]
async fn callbacks_keep_separate_players_and_do_not_fall_back_to_the_owner() {
    let vm = vm(true, NullMode::Distinct).await;
    let first = object(&vm, "/first.c", "").await;
    let second = object(&vm, "/second.c", "").await;
    let owner = object(&vm, "/owner.c", RECORDER).await;
    for player in [Some(&first), Some(&second), None] {
        call(&vm, &owner, player, "start").await.unwrap();
    }
    for (id, expected) in
        timers(&vm)
            .into_iter()
            .zip([reference(&first), reference(&second), LpcRef::Nil])
    {
        fire(&vm, id).await;
        assert_eq!(vm.global_state.committed_global(&owner, 0u16), expected);
    }
}

#[tokio::test]
async fn destructed_player_is_nil_even_when_rust_still_holds_it() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(
        &vm,
        "/player.c",
        "void retire() { destruct(this_object()); }",
    )
    .await;
    let owner = object(&vm, "/owner.c", RECORDER).await;
    call(&vm, &owner, Some(&player), "repeat").await.unwrap();
    let id = timers(&vm)[0];
    fire(&vm, id).await;
    call(&vm, &player, None, "retire").await.unwrap();
    let _replacement = object(&vm, "/player.c", "").await;
    fire(&vm, id).await;
    assert_eq!(vm.global_state.committed_global(&owner, 0u16), LpcRef::Nil);
    assert_eq!(
        vm.global_state.committed_global(&owner, 2u16),
        LpcRef::from(2)
    );
}

#[tokio::test]
async fn queued_timer_does_not_retain_the_player_allocation() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(
        &vm,
        "/player.c",
        "void retire() { destruct(this_object()); }",
    )
    .await;
    let owner = object(&vm, "/owner.c", RECORDER).await;
    call(&vm, &owner, Some(&player), "start").await.unwrap();
    call(&vm, &player, None, "retire").await.unwrap();
    let weak = Arc::downgrade(&player);
    drop(player);
    assert!(weak.upgrade().is_none());
    fire(&vm, timers(&vm)[0]).await;
    assert_eq!(vm.global_state.committed_global(&owner, 0u16), LpcRef::Nil);
}

#[tokio::test]
async fn aborted_and_cancelled_schedules_leave_no_timer() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(&vm, "/player.c", "").await;
    let owner = object(&vm, "/owner.c", RECORDER).await;
    assert!(
        call(&vm, &owner, Some(&player), "fail_start")
            .await
            .is_err()
    );
    assert!(timers(&vm).is_empty());
    call(&vm, &owner, Some(&player), "cancel_start")
        .await
        .unwrap();
    assert!(timers(&vm).is_empty());
}

#[tokio::test]
async fn closures_and_nested_timers_capture_the_player_at_each_schedule() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(&vm, "/player.c", "").await;
    let owner = object(
        &vm,
        "/owner.c",
        r#"
        object first; object second; int capture;
        void finish() { second = this_player(); }
        void start() {
            int value = 42;
            call_out((:
                first = this_player(); capture = value;
                set_this_player(this_object()); call_out(finish, 100);
                set_this_player(nil);
            :), 100);
        }
    "#,
    )
    .await;
    call(&vm, &owner, Some(&player), "start").await.unwrap();
    fire(&vm, timers(&vm)[0]).await;
    assert_eq!(
        vm.global_state.committed_global(&owner, 0u16),
        reference(&player)
    );
    assert_eq!(
        vm.global_state.committed_global(&owner, 2u16),
        LpcRef::from(42)
    );
    fire(&vm, timers(&vm)[0]).await;
    assert_eq!(
        vm.global_state.committed_global(&owner, 1u16),
        reference(&owner)
    );
}

#[tokio::test]
async fn an_uncaught_error_does_not_replace_the_repeating_timers_player() {
    let vm = vm(true, NullMode::Distinct).await;
    let player = object(&vm, "/player.c", "").await;
    let owner = object(
        &vm,
        "/owner.c",
        r#"
        object seen; int fail = 1;
        void tick() { seen = this_player(); set_this_player(nil); if (fail) throw("round failed"); }
        void start() { call_out(tick, 100, 100); }
        void recover() { fail = 0; }
    "#,
    )
    .await;
    call(&vm, &owner, Some(&player), "start").await.unwrap();
    let id = timers(&vm)[0];
    fire(&vm, id).await;
    assert_eq!(vm.global_state.committed_global(&owner, 0u16), LpcRef::Nil);
    call(&vm, &owner, None, "recover").await.unwrap();
    fire(&vm, id).await;
    assert_eq!(
        vm.global_state.committed_global(&owner, 0u16),
        reference(&player)
    );
}
