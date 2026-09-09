//! Pinkfish expansion, terminal color conversion, and display-column wrapping.

mod colour;
mod format;

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_telnet::ColourDepth;
use lpc_rs_utils::lpc_string::LpcString;
use smallvec::smallvec;

use self::format::{LIMIT, Part, Text, next_part};
use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::efun_context::EfunContext,
    function_type::function_ptr::FunctionPtr,
    lpc_mapping::LpcMapping,
    lpc_ref::LpcRef,
    stm::TxnHandle,
};

/// Format concrete color tokens and caller-supplied replacements for a terminal.
pub fn terminal_colour<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let LpcRef::String(text) = context.arg(0) else {
        return Err(context.runtime_error("terminal_colour: text must be a string"));
    };
    if text.to_str().len() > LIMIT {
        return Err(context.runtime_error("terminal_colour: input exceeds 1 MiB"));
    }
    let mut state = Expansion {
        text: text.clone(),
        cursor: 0,
        mapping: None,
        resolver: None,
        default: None,
        strip: false,
        formatted: Text::default(),
        depth: depth_arg(context)?,
        wrap: 0,
        indent: 0,
    };
    if context.arg_count() >= 2 {
        match context.arg(1) {
            LpcRef::Int(i) if i.0 == 0 => state.strip = true,
            LpcRef::Int(i) if i.0 == 1 => {}
            LpcRef::Mapping(_) => {
                let mapping = context.arg(1).with_mapping(context.txn(), Clone::clone)?;
                state.default = mapping
                    .get(&LpcRef::from(0))
                    .cloned()
                    .filter(|v| !v.is_null());
                if state
                    .default
                    .as_ref()
                    .is_some_and(|v| !matches!(v, LpcRef::String(_) | LpcRef::Function(_)))
                {
                    return Err(context.runtime_error(
                        "terminal_colour: mapping default must be a string or function",
                    ));
                }
                state.mapping = Some(mapping);
            }
            LpcRef::Function(ptr) => state.resolver = Some(ptr.clone()),
            _ => {
                return Err(context.runtime_error(
                    "terminal_colour: colours must be 0, 1, a mapping, or a function",
                ));
            }
        }
    }
    for (i, name) in [(2, "wrap"), (3, "indent")] {
        let LpcRef::Int(value) = context.arg(i) else {
            return Err(context.runtime_error(format!("terminal_colour: {name} must be an int")));
        };
        if value.0.unsigned_abs() > LIMIT as u64 || (i == 3 && value.0 < 0) {
            return Err(context.runtime_error(format!("terminal_colour: {name} is out of range")));
        }
        if i == 2 {
            state.wrap = value.0;
        } else {
            state.indent = value.0 as usize;
        }
    }
    if state.wrap != 0 && state.indent >= state.wrap.unsigned_abs() as usize {
        return Err(
            context.runtime_error("terminal_colour: indent must be smaller than wrap width")
        );
    }
    if state.strip {
        state.depth = ColourDepth::Plain;
    }
    if state.resolver.is_some() || matches!(state.default, Some(LpcRef::Function(_))) {
        context.continue_with(Box::new(state));
    } else if let Next::Done(result) = state.advance(None, context.txn())? {
        context.return_efun_result(result);
    }
    Ok(())
}

fn depth_arg<const N: usize>(context: &EfunContext<'_, N>) -> Result<ColourDepth> {
    let terminal = (context.arg_count() >= 5).then(|| context.arg(4));
    let target = match terminal {
        None => context.this_player().load_full(),
        Some(LpcRef::Int(i)) if i.0 == -1 => context.this_player().load_full(),
        Some(LpcRef::Int(i)) => {
            return ColourDepth::from_bits(i.0).ok_or_else(|| {
                context.runtime_error("terminal_colour: depth must be -1, 0, 3, 4, 8, or 24")
            });
        }
        Some(arg @ LpcRef::Object(_)) => arg.live_object(context.txn()),
        Some(_) => {
            return Err(
                context.runtime_error("terminal_colour: terminal must be a depth or object")
            );
        }
    };
    Ok(target
        .and_then(|process| {
            context
                .txn()
                .with(|t| t.read_connection(process.connection.id))
        })
        .and_then(|connection| connection.snapshot().terminal.colour_depth)
        .unwrap_or(ColourDepth::Plain))
}

#[derive(Debug, Clone)]
struct Expansion {
    text: Arc<LpcString>,
    cursor: usize,
    mapping: Option<LpcMapping>,
    resolver: Option<Arc<FunctionPtr>>,
    default: Option<LpcRef>,
    strip: bool,
    formatted: Text,
    depth: ColourDepth,
    wrap: i64,
    indent: usize,
}

impl Continuation for Expansion {
    fn advance(&mut self, result: Option<LpcRef>, _txn: &TxnHandle) -> Result<Next> {
        if let Some(result) = result {
            let Some(text) = result.as_str() else {
                return Err(lpc_error!("terminal_colour: resolver must return a string"));
            };
            self.formatted.builtins(text)?;
        }
        while let Some(part) = next_part(self.text.to_str(), &mut self.cursor) {
            let key = match part {
                Part::Text(text) => {
                    self.formatted.literal(text)?;
                    continue;
                }
                Part::Key(key) if self.strip || key.is_empty() => continue,
                Part::Key(key) => key,
            };
            let mut replacement = self
                .mapping
                .as_ref()
                .and_then(|m| m.get(&LpcRef::from(key)))
                .filter(|v| matches!(v, LpcRef::String(_)))
                .cloned();
            if let Some(ptr) = &self.resolver {
                replacement = Some(LpcRef::Function(ptr.clone()));
            } else if replacement.is_none() {
                if self.formatted.token(key)? {
                    continue;
                }
                replacement = self.default.clone();
            }
            match replacement {
                Some(LpcRef::String(s)) => self.formatted.builtins(s.to_str())?,
                Some(LpcRef::Function(ptr)) => {
                    return Ok(Next::Call(Callee::Pointer {
                        ptr,
                        args: smallvec![LpcRef::from(key)],
                    }));
                }
                _ => {}
            }
        }
        self.formatted
            .render(self.depth, self.wrap, self.indent)
            .map(|s| Next::Done(LpcRef::from(s)))
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::{run_prog, try_run_prog};

    async fn formatted(args: &str) -> String {
        run_prog(&format!(
            "string create() {{ return terminal_colour({args}); }}"
        ))
        .await
        .result()
        .unwrap()
        .as_str()
        .unwrap()
        .to_owned()
    }

    #[tokio::test]
    async fn cd_tokens_rgb_and_backgrounds_are_available_without_themes() {
        assert_eq!(
            formatted(r#""%^ORANGE%^orange %^B_#123%^background%^RESET%^", 1, 0, 0, 24"#).await,
            "\x1b[38;5;208morange \x1b[0m\x1b[38;5;208;48;2;17;34;51mbackground\x1b[0m"
        );
        assert_eq!(
            formatted(r#""%^FG_255%^x%^BG_232%^y", 1, 0, 0, 8"#).await,
            "\x1b[38;5;255mx\x1b[0m\x1b[38;5;255;48;5;232my\x1b[0m"
        );
        assert_eq!(
            formatted(r#""%^T_HIGH%^x%^T_NORM%^", 1, 0, 0, 24"#).await,
            "x"
        );
    }

    #[tokio::test]
    async fn plain_mode_strips_tokens_and_raw_ansi_even_with_a_depth_override() {
        assert_eq!(
            formatted(r#""%^RED%^red" + sprintf("%c[0m!", 27), 0, 0, 0, 24"#).await,
            "red!"
        );
        assert_eq!(formatted(r#""%^RED%^red%^RESET%^""#).await, "red");
    }

    #[tokio::test]
    async fn custom_mappings_override_builtins_and_share_rgb_fallback() {
        let result = formatted(r#""%^RED%^x%^RESET%^", (["RED":"%^#f00%^"]), 0, 0, 8"#).await;
        assert_eq!(result, "\x1b[38;5;196mx\x1b[0m");
        assert_eq!(
            formatted(r#""%^UNKNOWN%^x", ([0: "fallback "]), 0, 0, 24"#).await,
            "fallback x"
        );
        assert_eq!(
            formatted(r#""%^RED%^x", (["RED": 42]), 0, 0, 3"#).await,
            "\x1b[31mx\x1b[0m"
        );
    }

    #[tokio::test]
    async fn replacements_are_measured_and_custom_resolution_is_not_recursive() {
        assert_eq!(
            formatted(r#""%^WORD%^ x", (["WORD":"hello"]), 5"#).await,
            "hello\nx"
        );
        assert_eq!(
            formatted(r#""%^A%^x", (["A":"%^B%^", "B":"loop"]), 0, 0, 24"#).await,
            "x"
        );
    }

    #[tokio::test]
    async fn resolvers_receive_each_marked_token_and_can_supply_concrete_markup() {
        assert_eq!(
            formatted(r#""%^one%^/%^two%^", (: "[" + $1 + "]" :)"#).await,
            "[one]/[two]"
        );
        assert_eq!(
            formatted(r#""%^RED%^x%^CUSTOM%^y", ([0: (: "%^#0f0%^" :)]), 0, 0, 24"#).await,
            "\x1b[38;5;1mx\x1b[0m\x1b[38;2;0;255;0my\x1b[0m"
        );
    }

    #[tokio::test]
    async fn formatting_does_not_modify_the_mapping() {
        let result = run_prog(
            r#"
            int create() {
                mapping m = (["CUSTOM": "word"]);
                terminal_colour("%^CUSTOM%^", m, 2, 0, 24);
                return sizeof(m) == 1 && m["CUSTOM"] == "word";
            }
        "#,
        )
        .await
        .result();
        assert_eq!(result, Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn recipients_render_the_same_message_for_their_own_capabilities() {
        use crate::{
            interpreter::{CommittedReader, vm::Vm},
            telnet::ops::ConnectionOp,
            test_support::{connect, test_config},
        };
        let vm = Vm::new(test_config());
        for (name, mask, depth, expected) in [
            ("rgb", 269, 24, "\x1b[38;2;255;0;0mred\x1b[0m"),
            ("indexed", 13, 8, "\x1b[38;5;196mred\x1b[0m"),
            ("ansi", 1, 3, "\x1b[31mred\x1b[0m"),
            ("plain", 0, 0, "red"),
        ] {
            let player = vm
                .create_process_from_code(
                    format!("/{name}.c"),
                    r#"
                int depth;
                void catch_tell(string text) {
                    depth = query_connection(this_object())["colour_depth"];
                    write_socket(terminal_colour(text, 1, 0, 0, this_object()));
                }
            "#,
                )
                .await
                .unwrap();
            let mut connected = connect(&vm, &player).await;
            let mut session = lpc_rs_telnet::Session::new();
            session.feed(b"\xff\xfb\x18\xff\xfa\x18\0CLIENT\xff\xf0");
            session.feed(b"\xff\xfa\x18\0XTERM\xff\xf0");
            let mut report = b"\xff\xfa\x18\0".to_vec();
            report.extend_from_slice(format!("MTTS {mask}").as_bytes());
            report.extend_from_slice(b"\xff\xf0");
            session.feed(&report);
            connected.connection.refresh(&session);
            vm.initialize_process_from_code(
                format!("/send_{name}.c"),
                format!(
                    r#"
                void create() {{ tell_object(find_object("/{name}"), "%^#f00%^red%^RESET%^"); }}
            "#
                ),
            )
            .await
            .unwrap();
            assert_eq!(
                connected.rx.try_recv(),
                Ok(ConnectionOp::SendMessage(expected.into()))
            );
            assert_eq!(
                vm.global_state.committed_global(&player, 0u16),
                LpcRef::from(depth)
            );
        }
    }

    #[tokio::test]
    async fn automatic_depth_uses_this_player_and_an_explicit_depth_overrides_it() {
        use crate::{
            interpreter::{task::Task, task::task_template::TaskTemplate, vm::Vm},
            test_support::{connect, test_config},
        };
        let vm = Vm::new(test_config());
        let player = vm.create_process_from_code("/player.c", r#"
            string create() {
                set_this_player(this_object());
                return terminal_colour("%^#f00%^x") + "|" + terminal_colour("%^#f00%^x", 1, 0, 0, 24);
            }
        "#).await.unwrap();
        let connected = connect(&vm, &player).await;
        let mut session = lpc_rs_telnet::Session::new();
        session.feed(b"\xff\xfb\x18\xff\xfa\x18\0MTTS 1\xff\xf0");
        connected.connection.refresh(&session);
        let result = Task::<16>::initialize_process(
            TaskTemplate::from(vm.global_state.clone()).into_task_context(player),
        )
        .await
        .unwrap()
        .result();
        assert_eq!(
            result,
            Some(LpcRef::from("\x1b[31mx\x1b[0m|\x1b[38;2;255;0;0mx\x1b[0m"))
        );
    }

    #[tokio::test]
    async fn resolver_writes_roll_back_with_a_failed_transaction() {
        use crate::{
            interpreter::{CommittedReader, vm::Vm},
            test_support::test_config,
        };
        let vm = Vm::new(test_config());
        let target = vm
            .initialize_process_from_code(
                "/target.c",
                r#"
            int count;
            string resolve(string key) { count++; return key; }
            void fail() { terminal_colour("%^X%^", &resolve()); throw("failed"); }
        "#,
            )
            .await
            .unwrap()
            .context
            .process;
        let error = vm
            .initialize_process_from_code(
                "/main.c",
                r#"
            void create() { call_other("/target", "fail"); }
        "#,
            )
            .await
            .unwrap_err();
        assert!(error.to_string().contains("failed"));
        assert_eq!(
            vm.global_state.committed_global(&target, 0u16),
            LpcRef::from(0)
        );
    }

    #[tokio::test]
    async fn invalid_arguments_and_resolver_results_are_runtime_errors() {
        for (args, message) in [
            (r#"42"#, "text must be a string"),
            (r#""x", 2"#, "colours must be"),
            (r#""x", ([0:42])"#, "mapping default"),
            (r#""x", 1, "bad""#, "wrap must be an int"),
            (r#""x", 1, 8, -1"#, "indent is out of range"),
            (r#""x", 1, 8, 8"#, "indent must be smaller"),
            (r#""x", 1, 0, 0, 16"#, "depth must be"),
            (r#""%^X%^", (: 0 :)"#, "resolver must return a string"),
        ] {
            let code =
                format!("mixed create() {{ mixed f = &terminal_colour(); return f({args}); }}");
            let err = try_run_prog(&code).await.expect_err(args).to_string();
            assert!(err.contains(message), "{args}: {err}");
        }
    }
}
