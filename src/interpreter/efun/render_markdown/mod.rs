//! Markdown documents rendered for a recipient's terminal.

mod render;

use lpc_rs_errors::Result;
use lpc_rs_telnet::ColourDepth;

use super::efun_context::EfunContext;
use crate::interpreter::lpc_ref::LpcRef;

const MAX_WIDTH: usize = 4096;

/// Render Markdown as terminal text without sending it.
pub fn render_markdown<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(source) = context.arg(0) else {
        return Err(context.runtime_error("render_markdown: source must be a string"));
    };
    let LpcRef::Int(width) = context.arg(1) else {
        return Err(context.runtime_error("render_markdown: width must be an int"));
    };
    if !(0..=MAX_WIDTH as i64).contains(&width.0) {
        return Err(context.runtime_error("render_markdown: width must be 0 through 4096"));
    }
    let terminal = (context.arg_count() >= 3).then(|| context.arg(2));
    let mut depth = None;
    let target = match terminal {
        None => context.this_player().load_full(),
        Some(LpcRef::Int(i)) => {
            if i.0 != -1 {
                depth = Some(ColourDepth::from_bits(i.0).ok_or_else(|| {
                    context.runtime_error("render_markdown: depth must be -1, 0, 3, 4, 8, or 24")
                })?);
            }
            context.this_player().load_full()
        }
        Some(arg @ LpcRef::Object(_)) => arg.live_object(context.txn()),
        Some(_) => {
            return Err(
                context.runtime_error("render_markdown: terminal must be a depth or object")
            );
        }
    };
    let snapshot = target
        .and_then(|process| {
            context
                .txn()
                .with(|t| t.read_connection(process.connection.id))
        })
        .map(|connection| connection.snapshot());
    let depth = depth
        .or_else(|| snapshot.as_ref().and_then(|s| s.terminal.colour_depth))
        .unwrap_or(ColourDepth::Plain);
    let width = if width.0 == 0 {
        snapshot
            .as_ref()
            .map(|s| usize::from(s.cols))
            .filter(|&n| n != 0)
            .unwrap_or(80)
            .min(MAX_WIDTH)
    } else {
        width.0 as usize
    };
    let output = render::render(source.to_str(), width, depth)?;
    context.return_efun_result(LpcRef::from(output));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::{task::Task, task::task_template::TaskTemplate, vm::Vm},
        telnet::ops::ConnectionOp,
        test_support::{connect, run_prog, test_config, try_run_prog},
    };

    #[tokio::test]
    async fn direct_and_function_pointer_calls_apply_defaults() {
        for code in [
            r##"string create() { return render_markdown("# Help\n\n**Hello**."); }"##,
            r##"string create() { function f = &render_markdown(); return f("# Help\n\n**Hello**."); }"##,
        ] {
            assert_eq!(
                run_prog(code).await.result(),
                Some(LpcRef::from("# Help\n\nHello.\n"))
            );
        }
        assert_eq!(
            run_prog(r#"string create() { return render_markdown("one two three", 5, 0); }"#)
                .await
                .result(),
            Some(LpcRef::from("one\ntwo\nthree\n"))
        );
    }

    #[tokio::test]
    async fn missing_and_disconnected_players_use_eighty_plain_columns() {
        let source = "word ".repeat(20);
        let expected = render::render(&source, 80, ColourDepth::Plain).unwrap();
        for terminal in ["-1", "this_object()"] {
            let code = format!(
                r#"string create() {{ return render_markdown("{source}", 0, {terminal}); }}"#
            );
            assert_eq!(
                run_prog(&code).await.result(),
                Some(LpcRef::from(expected.as_str()))
            );
        }
    }

    #[tokio::test]
    async fn recipient_capabilities_determine_width_and_styles() {
        let vm = Vm::new(test_config());
        for (name, mask, expected) in [
            ("plain", 0, "one\ntwo\nthree\n"),
            (
                "ansi",
                1,
                "\x1b[1mone\x1b[0m\n\x1b[1mtwo\x1b[0m\n\x1b[1mthree\x1b[0m\n",
            ),
        ] {
            let player = vm
                .create_process_from_code(
                    format!("/{name}.c"),
                    r#"
                string create() {
                    set_this_player(this_object());
                    return render_markdown("**one two three**");
                }
            "#,
                )
                .await
                .unwrap();
            let connected = connect(&vm, &player).await;
            let mut session = lpc_rs_telnet::Session::new();
            session.feed(b"\xff\xfb\x1f\xff\xfa\x1f\0\x05\0\x18\xff\xf0");
            session.feed(b"\xff\xfb\x18");
            let mut report = b"\xff\xfa\x18\0".to_vec();
            report.extend_from_slice(format!("MTTS {mask}").as_bytes());
            report.extend_from_slice(b"\xff\xf0");
            session.feed(&report);
            connected.connection.refresh(&session);
            let result = Task::<16>::initialize_process(
                TaskTemplate::from(vm.global_state.clone()).into_task_context(player),
            )
            .await
            .unwrap()
            .result();
            assert_eq!(result, Some(LpcRef::from(expected)));
            let result = vm
                .initialize_process_from_code(
                    format!("/sender_{name}.c"),
                    format!(
                        r#"
                string create() {{
                    return render_markdown("**one two three**", 0, find_object("/{name}"));
                }}
            "#
                    ),
                )
                .await
                .unwrap()
                .result();
            assert_eq!(result, Some(LpcRef::from(expected)));
        }
    }

    #[tokio::test]
    async fn explicit_depth_and_width_override_detected_capabilities() {
        let vm = Vm::new(test_config());
        let player = vm
            .create_process_from_code(
                "/player.c",
                r#"
            string create() {
                set_this_player(this_object());
                return render_markdown("**one two**", 80, 0)
                    + render_markdown("**one two**", 0, 24);
            }
        "#,
            )
            .await
            .unwrap();
        let connected = connect(&vm, &player).await;
        let mut session = lpc_rs_telnet::Session::new();
        session.feed(b"\xff\xfb\x1f\xff\xfa\x1f\0\x05\0\x18\xff\xf0");
        connected.connection.refresh(&session);
        let result = Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(player),
        )
        .await
        .unwrap()
        .result();
        assert_eq!(
            result,
            Some(LpcRef::from(
                "one two\n\x1b[1mone\x1b[0m\n\x1b[1mtwo\x1b[0m\n"
            ))
        );
    }

    #[tokio::test]
    async fn invalid_runtime_arguments_raise_named_errors() {
        for (args, message) in [
            ("42", "source must be a string"),
            (r#""x", "wide""#, "width must be an int"),
            (r#""x", -1"#, "width must be"),
            (r#""x", 4097"#, "width must be"),
            (r#""x", 0, 16"#, "depth must be"),
            (r#""x", 0, "ansi""#, "terminal must be"),
        ] {
            let code =
                format!("mixed create() {{ mixed f = &render_markdown(); return f({args}); }}");
            let error = try_run_prog(&code).await.expect_err(args).to_string();
            assert!(
                error.contains(&format!("render_markdown: {message}")),
                "{args}: {error}"
            );
        }
    }

    #[tokio::test]
    async fn oversized_reported_widths_are_capped() {
        let vm = Vm::new(test_config());
        let player = vm
            .create_process_from_code(
                "/player.c",
                format!(
                    r#"
            string create() {{
                set_this_player(this_object());
                return render_markdown("{}");
            }}
        "#,
                    "x".repeat(5000)
                ),
            )
            .await
            .unwrap();
        let connected = connect(&vm, &player).await;
        let mut session = lpc_rs_telnet::Session::new();
        session.feed(b"\xff\xfb\x1f\xff\xfa\x1f\x13\x88\0\x18\xff\xf0");
        connected.connection.refresh(&session);
        let result = Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(player),
        )
        .await
        .unwrap()
        .result()
        .unwrap();
        let lines: Vec<_> = result.as_str().unwrap().lines().map(str::len).collect();
        assert_eq!(lines, [4096, 904]);
    }

    #[tokio::test]
    async fn output_is_delivered_only_when_the_sending_transaction_commits() {
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", "").await.unwrap();
        let mut connected = connect(&vm, &player).await;
        let error = vm
            .initialize_process_from_code(
                "/failed.c",
                r##"
            void create() {
                tell_object(find_object("/player"), render_markdown("# Uncommitted", 80, 0));
                throw("failed");
            }
        "##,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("failed"));
        assert!(connected.rx.try_recv().is_err());
        vm.initialize_process_from_code(
            "/committed.c",
            r##"
            void create() {
                tell_object(find_object("/player"), render_markdown("# Committed", 80, 0));
            }
        "##,
        )
        .await
        .unwrap();
        assert_eq!(
            connected.rx.try_recv(),
            Ok(ConnectionOp::SendMessage("# Committed\n".into()))
        );
    }
}
