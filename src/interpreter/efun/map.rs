//! `map(collection, f, extra...)`: `f(element, extra...)` per element, the
//! results in a fresh array, mapping over the same keys, or string of scalars.

use std::sync::Arc;

use lpc_rs_errors::{Result, lpc_error};
use lpc_rs_utils::{lpc_string::LpcString, string::MAX_STRING_LENGTH};

use crate::interpreter::{
    continuation::{Callee, Continuation, Next},
    efun::{
        callback::{
            Items, callback_args, extra_args, function_arg, items_arg, mint_array, mint_mapping,
            return_empty,
        },
        efun_context::EfunContext,
    },
    function_type::function_ptr::FunctionPtr,
    lpc_ref::LpcRef,
    stm::TxnHandle,
};

/// `map(collection, f, extra...)`: `f(element, extra...)` per element, the
/// results in a fresh array, mapping or string.
pub fn map<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    match context.arg(0) {
        LpcRef::String(text) => {
            let ptr = function_arg(context, "map", 1)?;
            if text.is_empty() {
                context.return_efun_result(context.arg(0).clone());
                return Ok(());
            }
            let state = TextMap {
                text: text.clone(),
                offset: 0,
                output: String::with_capacity(text.len().min(MAX_STRING_LENGTH)),
                ptr,
                extra: extra_args(context, 2),
            };
            context.continue_with(Box::new(state));
            return Ok(());
        }
        LpcRef::Array(_) | LpcRef::Mapping(_) => {}
        other => {
            return Err(context.runtime_error(format!(
                "map: {} is not an array, mapping or string",
                other.type_name()
            )));
        }
    }
    let items = items_arg(context, "map")?;
    let ptr = function_arg(context, "map", 1)?;
    let extra = extra_args(context, 2);
    if items.is_empty() {
        return_empty(context, &items);
        return Ok(());
    }
    context.continue_with(Box::new(Map {
        ptr,
        items,
        extra,
        keys: Vec::new(),
        results: Vec::new(),
    }));
    Ok(())
}

#[derive(Debug, Clone)]
struct TextMap {
    text: Arc<LpcString>,
    offset: usize,
    output: String,
    ptr: Arc<FunctionPtr>,
    extra: Vec<LpcRef>,
}

impl Continuation for TextMap {
    fn advance(&mut self, result: Option<LpcRef>, _txn: &TxnHandle) -> Result<Next> {
        if let Some(value) = result {
            let LpcRef::Int(value) = value else {
                return Err(lpc_error!(
                    "map: string callback must return an int, got {}",
                    value.type_name()
                ));
            };
            let character = u32::try_from(value.0)
                .ok()
                .and_then(char::from_u32)
                .ok_or_else(|| {
                    lpc_error!("map: string callback returned an invalid Unicode scalar value")
                })?;
            if self.output.len() + character.len_utf8() > MAX_STRING_LENGTH {
                return Err(lpc_error!("map: result exceeds maximum string length"));
            }
            self.output.push(character);
        }
        let Some(character) = self.text.to_str()[self.offset..].chars().next() else {
            return Ok(Next::Done(std::mem::take(&mut self.output).into()));
        };
        self.offset += character.len_utf8();
        Ok(Next::Call(Callee::Pointer {
            ptr: self.ptr.clone(),
            args: callback_args(
                &[LpcRef::from(i64::from(u32::from(character)))],
                &self.extra,
            ),
        }))
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

/// The walk: one callback per element, its answer kept in order.
#[derive(Debug, Clone)]
struct Map {
    ptr: Arc<FunctionPtr>,
    items: Items,
    extra: Vec<LpcRef>,
    /// A mapping's keys, in `results` order.
    keys: Vec<LpcRef>,
    results: Vec<LpcRef>,
}

impl Continuation for Map {
    fn advance(&mut self, result: Option<LpcRef>, txn: &TxnHandle) -> Result<Next> {
        if let Some(value) = result {
            self.results.push(value);
        }
        let args = match &mut self.items {
            Items::Array(items) => match items.next() {
                Some(item) => callback_args(&[item], &self.extra),
                None => {
                    return Ok(Next::Done(mint_array(
                        txn,
                        std::mem::take(&mut self.results),
                    )));
                }
            },
            Items::Mapping(entries) => match entries.next() {
                Some((key, value)) => {
                    self.keys.push(key.clone());
                    callback_args(&[key, value], &self.extra)
                }
                None => {
                    return Ok(Next::Done(mint_mapping(
                        txn,
                        std::mem::take(&mut self.keys),
                        std::mem::take(&mut self.results),
                    )));
                }
            },
        };
        Ok(Next::Call(Callee::Pointer {
            ptr: self.ptr.clone(),
            args,
        }))
    }

    fn clone_box(&self) -> Box<dyn Continuation> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use indoc::indoc;
    use lpc_rs_asm::instruction::Instruction;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::{config::ConfigBuilder, string::MAX_STRING_LENGTH};

    use crate::{
        compiler::CompilerBuilder,
        interpreter::{
            efun::Efun,
            lpc_ref::LpcRef,
            task::eval_loop::{AsyncCall, Slice},
            vm::{Vm, global_state::GlobalState},
        },
        test_support::{
            TempLib, committed_global, run_prog, run_prog_with_config, strings_of, task_at,
            test_config, try_run_prog,
        },
    };

    #[tokio::test]
    async fn map_replaces_each_item_with_the_functions_result() {
        let code = "int *create() { return map(({ 1, 2 }), (: $1 * 10 :)); }";
        assert_eq!(strings_of(code).await, ["10", "20"]);
    }

    #[tokio::test]
    async fn map_passes_the_extra_arguments_after_the_item() {
        let code = "int *create() { return map(({ 1, 2 }), (: $1 + $2 :), 5); }";
        assert_eq!(strings_of(code).await, ["6", "7"]);
    }

    #[tokio::test]
    async fn map_of_a_mapping_keeps_the_keys() {
        let code = r#"mixed *create() { return keys(map(([ "a": 1, "b": 2 ]), (: $2 + 1 :))); }"#;
        assert_eq!(strings_of(code).await, ["a", "b"]);
    }

    #[tokio::test]
    async fn map_of_a_mapping_maps_each_value_from_key_and_value() {
        let code = r#"mixed *create() { return values(map(([ "a": 1, "b": 2 ]), (: $2 + 1 :))); }"#;
        assert_eq!(strings_of(code).await, ["2", "3"]);
    }

    #[tokio::test]
    async fn map_returns_a_new_mapping() {
        let code = "int create() { mapping m = ([ ]); return map(m, (: $2 :)) == m; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn map_of_a_non_collection_is_an_error() {
        let code = "mixed create() { mixed x = 7; return map(x, (: 1 :)); }";
        let err = try_run_prog(code).await.unwrap_err().to_string();
        assert!(
            err.contains("map: int is not an array, mapping or string"),
            "{err}"
        );
    }

    #[tokio::test]
    async fn map_runs_its_first_callback_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int one(int x) { return x; }
            int *got;
            void create() { got = map(({ 1, 2 }), &one()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        let callee = task.stack.current_frame().unwrap();
        assert_eq!(callee.function.name(), "one");
        assert!(callee.external);
        assert_eq!(callee.registers[1], LpcRef::from(1));
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }

    #[tokio::test]
    async fn a_returning_callback_starts_the_next_with_no_await() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int one(int x) { return x; }
            int *got;
            void create() { got = map(({ 1, 2 }), &one()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;
        task.run_slice(&mut 1).unwrap();

        for _ in 0..16 {
            assert!(matches!(task.run_slice(&mut 1).unwrap(), Slice::Budget));
            if task.stack.get(0).unwrap().pending.is_none() {
                break;
            }
            assert!(task.stack.len() >= 2, "the caller never surfaces mid-walk");
        }

        assert!(task.stack.get(0).unwrap().pending.is_none());
    }

    #[tokio::test]
    async fn map_with_a_plain_efun_pointer_runs_it_in_an_entry_frame() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int *got;
            void create() { got = map(({ 1, "a" }), &intp()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Budget));
        assert_eq!(task.stack.len(), 2);
        assert!(task.stack.current_frame().unwrap().is_entry());
    }

    #[tokio::test]
    async fn map_with_a_suspending_efun_pointer_awaits_the_pending_call() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            mixed *got;
            void create() { got = map(({ "/nowhere" }), &load_object()); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Await(AsyncCall::Pending)));
        assert_eq!(task.stack.len(), 1);
    }

    #[tokio::test]
    async fn map_with_a_suspending_efun_pointer_answers() {
        let code =
            r#"mixed *create() { return map(({ "/nowhere", "/nowhere" }), &load_object()); }"#;
        assert_eq!(strings_of(code).await, ["0", "0"]);
    }

    #[tokio::test]
    async fn a_callback_efun_whose_own_callback_suspends_awaits_the_pending_call() {
        let vm = Vm::new(test_config());
        let code = indoc! { r#"
            int *got;
            void create() { got = map(({ ({ "/nowhere" }) }), &map(, &load_object())); }
        "# };
        let (mut task, _live) =
            task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;

        let slice = task.run_slice(&mut 1).unwrap();

        assert!(matches!(slice, Slice::Await(AsyncCall::Pending)));
        assert_eq!(task.stack.len(), 2);
        let top = task.stack.current_frame().unwrap();
        assert!(top.is_entry());
        assert!(top.pending.is_some());
        assert!(task.stack.get(0).unwrap().pending.is_some());
    }

    #[tokio::test]
    async fn a_callback_efun_whose_own_callback_suspends_answers() {
        let code = indoc! { r#"
            int create() {
                mixed *r = map(({ ({ "/nowhere", "/nowhere" }) }), &map(, &load_object()));
                return sizeof(r[0]);
            }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(2)));
    }

    #[tokio::test]
    async fn map_of_an_empty_array_needs_no_callback() {
        let code = "int create() { return sizeof(map(({ }), (: 1 / 0 :))); }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(0)));
    }

    #[tokio::test]
    async fn previous_object_inside_a_callback_is_the_caller() {
        let code = "int create() { int *r = map(({ 1 }), (: previous_object() == this_object() :)); return r[0]; }";
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_callback_calling_map_twenty_deep_succeeds() {
        let code = indoc! { r#"
            int deep(int n) { int *r; if (n == 0) { return 0; } r = map(({ n - 1 }), &deep()); return 1 + r[0]; }
            int create() { return deep(20); }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(20)));
    }

    #[tokio::test]
    async fn a_caught_callback_error_leaves_the_frame_walking() {
        let code = indoc! { r#"
            string err;
            int create() { err = catch(map(({ 1 }), (: throw("x") :))); return sizeof(map(({ 1, 2 }), (: $1 :))); }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(2)));
    }

    #[tokio::test]
    async fn map_with_a_dynamic_pointer_calls_each_element() {
        let code = indoc! { r#"
            int one(int x) { return x; }
            int create() { int *r = map(({ this_object() }), &->one(5)); return r[0]; }
        "# };
        assert_eq!(run_prog(code).await.result(), Some(LpcRef::from(5)));
    }

    async fn string_result(code: &str, mode: NullMode) -> LpcRef {
        let config = crate::test_config_builder!()
            .null_mode(mode)
            .build()
            .unwrap();
        run_prog_with_config(code, config.into())
            .await
            .result()
            .unwrap()
    }

    #[tokio::test]
    async fn string_map_transforms_unicode_scalars_and_changes_encoding_width() {
        let code = "int create() { string s = \"é猫🐉e\u{301}\"; return map(s, (: $1 :)) == s\n\
            && map(\"aa\", (: 0x1f409 :)) == \"🐉🐉\" && map(\"🐉🐉\", (: 'a' :)) == \"aa\"\n\
            && map(\"Ab\", (: $1+1 :)) == \"Bc\"; }";
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_visits_and_produces_nuls_without_truncation() {
        let code = r#"
            int create() {
                string text = sprintf("a%cb", 0);
                string result = map(text, (: $1 :));
                string zeros = map("abc", (: 0 :));
                return result == text && sizeof(result) == 3 && result[2] == 'b'
                    && sizeof(zeros) == 3 && zeros[0] == 0 && zeros[2] == 0;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn empty_string_map_checks_the_function_but_does_not_call_it() {
        let code = r#"
            int create() {
                mixed invalid = 7;
                mixed err = catch(map("", invalid));
                return map("", (: throw("unreachable") :)) == "" && stringp(err)
                    && strindex(err, "map: int is not a function") >= 0;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_rejects_non_integer_and_invalid_scalar_results() {
        let code = r#"
            int create() {
                foreach (mixed bad in ({ "a", 65.0, ({}), ([]), sizeof })) {
                    mixed err = catch(map("x", (: $(bad) :)));
                    if (!stringp(err) || strindex(err, "string callback must return an int") < 0) return 0;
                }
                foreach (int bad in ({ -1, 0xd800, 0xdfff, 0x110000, 9223372036854775807 })) {
                    mixed err = catch(map("x", (: $(bad) :)));
                    if (!stringp(err) || strindex(err, "invalid Unicode scalar value") < 0) return 0;
                }
                return map("x", (: 0x10ffff :))[0] == 0x10ffff;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_validates_each_result_before_the_next_callback() {
        let code = r#"
            string seen = "";
            mixed bad;
            mixed transform(int c) { seen += sprintf("%c", c); return c == 'b' ? bad : c; }
            int create() {
                foreach (mixed invalid in ({ -1, "wrong" })) {
                    string output = "old";
                    bad = invalid; seen = "";
                    if (!stringp(catch(output = map("abc", transform))) || seen != "ab" || output != "old")
                        return 0;
                }
                return map("xy", (: $1+1 :)) == "yz";
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_stops_on_callback_errors_and_recovers() {
        let code = r#"
            string seen = "";
            int transform(int c) { seen += sprintf("%c", c); if (c == 'b') throw("stop"); return c; }
            int create() {
                mixed err = catch(map("abc", transform));
                return stringp(err) && strindex(err, "stop") >= 0 && seen == "ab"
                    && map("xy", (: $1+1 :)) == "yz";
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_supports_mixed_inputs_efun_pointers_and_bound_arguments() {
        let code = r#"
            int transform(int bound, int c, int extra) { return c + bound + extra; }
            int create() {
                mixed text = "AB";
                function mapper = map;
                return map(text, (: $1+$2 :), 32) == "ab"
                    && mapper(text, &transform(30), 2) == "ab"
                    && map("ab", intp) == to_text(({1,1}));
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_preserves_callback_order_context_and_input() {
        let code = r#"
            string source = "AB";
            string seen = "";
            int transform(int c) {
                if (origin() != "efun" || previous_object() != this_object()
                    || this_player() != this_object()) throw("bad context");
                source = "replacement";
                seen += sprintf("%c",c);
                return c + 32;
            }
            int create() {
                string original = source;
                set_this_player(this_object());
                string result = map(source, transform);
                return result == "ab" && original == "AB" && seen == "AB" && source == "replacement";
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_nested_callbacks_keep_their_own_output() {
        let code = r#"
            int transform(int c) { return map(sprintf("%c",c), (: $1+1 :))[0]; }
            int create() { return map("abcd", transform) == "bcde"; }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_resumes_after_a_suspending_callback() {
        let code = r#"
            int transform(int c) { load_object("/nowhere"); return c+1; }
            string create() { return map("é猫", transform); }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let config = crate::test_config_builder!()
                .null_mode(mode)
                .build()
                .unwrap();
            let vm = Vm::new(config);
            let (mut task, _live) =
                task_at(&vm, code, |at| matches!(at, Instruction::CallEfun(..))).await;
            let mut suspended = false;
            for _ in 0..64 {
                match task.run_slice(&mut 1).unwrap() {
                    Slice::Budget => {}
                    Slice::Await(AsyncCall::Efun(Efun::load_object, _)) => {
                        suspended = true;
                        break;
                    }
                    _ => panic!("callback did not suspend through an efun"),
                }
            }
            assert!(suspended, "callback should suspend");
            assert_eq!(
                string_result(code, mode).await,
                LpcRef::from("ê猬"),
                "{mode}"
            );
        }
    }

    #[tokio::test]
    async fn string_map_checks_output_size_in_utf8_bytes_before_continuing() {
        let limit = MAX_STRING_LENGTH / 4;
        let code = format!(
            r#"
            int calls;
            int expand(int c) {{ calls++; return 0x1f409; }}
            int create() {{
                string full = map("a" * {limit}, expand);
                if (sizeof(full) != {limit} || calls != {limit}) return 0;
                calls = 0;
                mixed err = catch(map("a" * ({limit}+2), expand));
                return stringp(err) && strindex(err, "result exceeds maximum string length") >= 0
                    && calls == {limit}+1 && map("a", (: $1 :)) == "a";
            }}
        "#
        );
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(&code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_interprets_void_results_using_the_null_mode() {
        let code = r#"
            int calls;
            void touch(int c) { calls++; }
            int create() {
                string output;
                mixed err = catch(output = map("ab", touch));
                if (nil == 0) return !stringp(err) && sizeof(output) == 2 && output[0] == 0 && calls == 2;
                return stringp(err) && strindex(err, "must return an int, got nil") >= 0 && calls == 1;
            }
        "#;
        for mode in [NullMode::Zero, NullMode::Distinct] {
            assert_eq!(string_result(code, mode).await, LpcRef::from(1), "{mode}");
        }
    }

    #[tokio::test]
    async fn string_map_retries_commit_callback_writes_once() {
        for mode in [NullMode::Zero, NullMode::Distinct] {
            let root = TempLib::new("string-map-retry");
            let config = ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(mode)
                .build()
                .unwrap();
            let (tx, _rx) = tokio::sync::mpsc::channel(16);
            let state = Arc::new(GlobalState::new_rejecting(Arc::new(config), tx, 2));
            let task = state
                .initialize_process_from_code(
                    "/test.c",
                    r#"
                int calls;
                string seen = "";
                int transform(int c, mixed *extra) {
                    calls++; extra[0]++; seen += sprintf("%c", c); return c+1;
                }
                int create() {
                    mixed *extra = ({0});
                    return map("é猫", transform, extra) == "ê猬" && extra[0] == 2;
                }
            "#,
                )
                .await
                .unwrap();
            assert_eq!(task.result(), Some(LpcRef::from(1)), "{mode}");
            assert_eq!(state.attempt_telemetry().conflicts, 2);
            assert_eq!(committed_global(&task, "calls"), LpcRef::from(2));
            assert_eq!(committed_global(&task, "seen"), LpcRef::from("é猫"));
        }
    }

    #[tokio::test]
    async fn string_overload_does_not_admit_bytes_or_named_callbacks() {
        let compiler = CompilerBuilder::default()
            .config(test_config())
            .build()
            .unwrap();
        for expression in [
            r#"map(to_bytes("a", "UTF-8"), (: $1 :))"#,
            r#"map("a", "callback")"#,
        ] {
            let error = compiler
                .compile_string(
                    "/map.c",
                    &format!("mixed create() {{ return {expression}; }}"),
                )
                .await
                .unwrap_err()
                .to_string();
            assert!(
                error.contains("unexpected argument type"),
                "{expression}: {error}"
            );
        }
    }
}
