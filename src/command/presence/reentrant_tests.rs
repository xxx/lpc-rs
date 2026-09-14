use std::sync::Arc;

use lpc_rs_errors::Result;

use crate::{
    interpreter::{
        CommittedReader,
        lpc_ref::LpcRef,
        process::Process,
        task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
        vm::Vm,
    },
    test_support::{allow_destruct, test_config},
};

struct Meeting {
    vm: Vm,
    actor: Arc<Process>,
    room: Arc<Process>,
    log: Arc<Process>,
}

async fn object(vm: &Vm, path: &str, code: &str) -> Arc<Process> {
    vm.initialize_process_from_code(path, code)
        .await
        .unwrap_or_else(|error| panic!("{}", error.diagnostic_string()))
        .context
        .process
}

impl Meeting {
    async fn new(room_hook: &str, living_peer: bool) -> Self {
        let vm = Vm::new(test_config());
        allow_destruct(&vm).await;
        let log = object(
            &vm,
            "/log.c",
            r#"
            string entries = "";
            void note(string text) { entries += text + ";"; }
        "#,
        )
        .await;
        object(
            &vm,
            "/elsewhere.c",
            r#"
            void init() { "/log"->note("elsewhere"); }
        "#,
        )
        .await;
        let room = object(
            &vm,
            "/room.c",
            &format!(
                r#"
            int changed;
            void init() {{
                if (file_name(this_player()) != "/actor") return;
                changed++;
                "/log"->note("room");
                {room_hook}
            }}
            int do_look(string arg) {{ return 1; }}
        "#
            ),
        )
        .await;
        object(
            &vm,
            "/peer.c",
            &format!(
                r#"
            void create() {{ {} move_object("/room"); }}
            void init() {{ "/log"->note("peer:" + file_name(this_player())); }}
            void leave() {{ move_object("/elsewhere"); }}
            void start() {{ enable_commands(); }}
            void stop() {{ disable_commands(); }}
        "#,
                if living_peer {
                    "enable_commands();"
                } else {
                    ""
                }
            ),
        )
        .await;
        let actor = object(
            &vm,
            "/actor.c",
            r#"
            mixed failure;
            void create() { enable_commands(); }
            void enter() { move_object("/room"); }
            void enter_caught() { failure = catch(move_object("/room")); }
            void redirect() { move_object("/elsewhere"); }
            void stop() { disable_commands(); }
            void init() { "/log"->note("actor:" + file_name(this_player())); }
        "#,
        )
        .await;
        Self {
            vm,
            actor,
            room,
            log,
        }
    }

    async fn enter(&self, name: &str) -> Result<()> {
        apply_function_by_name(
            name,
            &[],
            self.actor.clone(),
            TaskTemplate::from(self.vm.global_state.clone()),
            None,
        )
        .await
        .expect("the entry exists")
        .map(|_| ())
    }

    fn log(&self) -> String {
        self.vm
            .global_state
            .committed_global(&self.log, 0u16)
            .to_string()
    }
}

#[tokio::test]
async fn a_redirected_arrival_stops_the_old_rooms_hooks() {
    let meeting = Meeting::new("this_player()->redirect();", true).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;elsewhere;");
    assert_eq!(
        meeting
            .vm
            .global_state
            .committed_environment(&meeting.actor)
            .unwrap()
            .filename(),
        "/elsewhere"
    );
}

#[tokio::test]
async fn a_peer_destructed_by_an_earlier_hook_gets_no_init() {
    let meeting = Meeting::new("destruct(find_object(\"/peer\"));", true).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;");
    assert!(
        meeting
            .vm
            .global_state
            .object_space
            .lookup("/peer")
            .is_none()
    );
}

#[tokio::test]
async fn destructing_the_mover_stops_its_arrival_hooks() {
    let meeting = Meeting::new("destruct(this_player());", false).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;");
    assert!(
        meeting
            .vm
            .global_state
            .object_space
            .lookup("/actor")
            .is_none()
    );
}

#[tokio::test]
async fn destructing_the_destination_stops_the_arrival_hooks() {
    let meeting = Meeting::new("destruct(this_object());", true).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;");
    assert!(
        meeting
            .vm
            .global_state
            .object_space
            .lookup("/room")
            .is_none()
    );
}

#[tokio::test]
async fn a_nonliving_mover_that_redirects_skips_the_living_destination() {
    let vm = Vm::new(test_config());
    let log = object(
        &vm,
        "/log.c",
        r#"
        string entries = "";
        void note(string text) { entries += text + ";"; }
    "#,
    )
    .await;
    object(&vm, "/elsewhere.c", "").await;
    object(&vm, "/room.c", "void create() { enable_commands(); }").await;
    object(
        &vm,
        "/peer.c",
        r#"
        void create() { enable_commands(); move_object("/room"); }
    "#,
    )
    .await;
    let item = object(
        &vm,
        "/item.c",
        r#"
        void enter() { move_object("/room"); }
        void init() {
            "/log"->note(file_name(this_player()));
            move_object("/elsewhere");
        }
    "#,
    )
    .await;
    apply_function_by_name(
        "enter",
        &[],
        item.clone(),
        TaskTemplate::from(vm.global_state.clone()),
        None,
    )
    .await
    .unwrap()
    .unwrap();
    assert_eq!(
        vm.global_state.committed_global(&log, 0u16).to_string(),
        "/peer;"
    );
    assert_eq!(
        vm.global_state
            .committed_environment(&item)
            .unwrap()
            .filename(),
        "/elsewhere"
    );
}

#[tokio::test]
async fn a_peer_that_leaves_during_an_earlier_hook_is_skipped() {
    let meeting = Meeting::new("\"/peer\"->leave();", false).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;");
}

#[tokio::test]
async fn moving_an_unrelated_object_does_not_cancel_the_arrival() {
    let meeting = Meeting::new("\"/unrelated\"->leave();", true).await;
    object(
        &meeting.vm,
        "/unrelated.c",
        r#"void leave() { move_object("/elsewhere"); }"#,
    )
    .await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;peer:/actor;actor:/peer;");
}

#[tokio::test]
async fn a_disabled_peer_still_meets_the_mover_but_is_no_longer_a_command_giver() {
    let meeting = Meeting::new("\"/peer\"->stop();", true).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;peer:/actor;");
}

#[tokio::test]
async fn a_disabled_mover_can_still_offer_rules_to_other_livings() {
    let meeting = Meeting::new("this_player()->stop();", true).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;actor:/peer;");
    assert!(!meeting.vm.global_state.commands_enabled(&meeting.actor));
}

#[tokio::test]
async fn enabling_a_peer_does_not_add_it_to_an_arrivals_command_givers() {
    let meeting = Meeting::new("\"/peer\"->start();", false).await;
    meeting.enter("enter").await.unwrap();
    assert_eq!(meeting.log(), "room;peer:/actor;");
}

#[tokio::test]
async fn only_an_uncaught_init_error_rolls_back_the_move_and_its_rules() {
    for caught in [false, true] {
        let meeting = Meeting::new(
            r#"add_action("do_look", "look"); throw("init failed");"#,
            false,
        )
        .await;
        let result = meeting
            .enter(if caught { "enter_caught" } else { "enter" })
            .await;
        if caught {
            result.unwrap();
            assert_eq!(
                meeting
                    .vm
                    .global_state
                    .committed_global(&meeting.actor, 0u16)
                    .to_string(),
                "init failed"
            );
            assert_eq!(
                meeting
                    .vm
                    .global_state
                    .committed_environment(&meeting.actor),
                Some(meeting.room.clone())
            );
            assert_eq!(meeting.log(), "room;");
        } else {
            let error = result.expect_err("init throws");
            assert_eq!(error.to_string(), "init failed");
            assert!(error.diagnostic_string().contains("/room.c"));
            assert_eq!(
                meeting
                    .vm
                    .global_state
                    .committed_environment(&meeting.actor),
                None
            );
            assert_eq!(meeting.log(), "");
        }
        assert_eq!(
            meeting
                .vm
                .global_state
                .committed_rules(&meeting.actor)
                .len(),
            usize::from(caught)
        );
        assert_eq!(
            meeting
                .vm
                .global_state
                .committed_global(&meeting.room, 0u16),
            LpcRef::from(i64::from(caught))
        );
        assert_eq!(
            meeting
                .vm
                .global_state
                .committed_inventory(&meeting.room)
                .iter()
                .any(|member| Arc::ptr_eq(member, &meeting.actor)),
            caught
        );
    }
}
