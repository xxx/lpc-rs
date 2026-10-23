use lpc_rs_errors::Result;
use lpc_rs_telnet::InputMode;

use crate::interpreter::efun::{efun_context::EfunContext, input_to};

/// Register a callback for the command giver's next Unicode scalar or control key.
pub fn get_char<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    input_to::register(context, InputMode::Character, "get_char")
}

#[cfg(test)]
mod tests {
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_config_builder,
    };

    #[tokio::test]
    async fn no_command_giver_or_noninteractive_giver_registers_nothing() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
            let task = vm
                .initialize_process_from_code(
                    "/character.c",
                    r#"
                int without_player;
                int without_connection;
                void heard(string c) {}
                void create() {
                    without_player = get_char(heard);
                    set_this_player(this_object());
                    function register = get_char;
                    without_connection = register("heard", 2);
                }
            "#,
                )
                .await
                .unwrap();
            for global in [0u16, 1u16] {
                assert_eq!(
                    vm.global_state
                        .committed_global(&task.context.process, global),
                    LpcRef::from(0)
                );
            }
        }
    }

    #[tokio::test]
    async fn get_char_validates_callbacks_and_echo_even_without_a_connection() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            for (call, expected) in [
                ("get_char(\"missing\")", "get_char: no function `missing`"),
                ("get_char(&->heard())", "get_char: the receiver"),
                (
                    "get_char(heard, wrong)",
                    "non-integer sent as second argument to `get_char`",
                ),
            ] {
                let vm = Vm::new(test_config_builder!().null_mode(mode).build().unwrap());
                let code = format!(
                    "void heard(string c) {{}} void create() {{ mixed wrong = \"yes\"; {call}; }}"
                );
                let error = vm
                    .initialize_process_from_code("/character.c", &code)
                    .await
                    .unwrap_err()
                    .to_string();
                assert!(error.contains(expected), "{error}");
            }
        }
    }
}
