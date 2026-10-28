//! Capture and materialize bounded diagnostics without retaining failed world cells.

use std::fmt::{self, Write};

use indexmap::IndexMap;
use lpc_rs_core::register::{Register, RegisterVariant};
use lpc_rs_errors::error_frames::{
    ErrorFrame, ErrorFrames, ErrorVariable, MAX_ERROR_FRAME_BYTES, MAX_ERROR_FRAMES,
    MAX_ERROR_TEXT_BYTES, MAX_ERROR_VARIABLES,
};

use super::{
    call_frame::CallFrame, call_stack::CallStack, lpc_array::LpcArray, lpc_mapping::LpcMapping,
    lpc_ref::LpcRef, stm::TxnHandle,
};

struct Text {
    text: String,
    truncated: bool,
}

impl Write for Text {
    fn write_str(&mut self, text: &str) -> fmt::Result {
        let end = text.floor_char_boundary(MAX_ERROR_TEXT_BYTES - self.text.len());
        self.text.push_str(&text[..end]);
        if end < text.len() {
            self.truncated = true;
            return Err(fmt::Error);
        }
        Ok(())
    }
}

fn text(args: fmt::Arguments<'_>) -> Text {
    let mut result = Text {
        text: String::new(),
        truncated: false,
    };
    let _ = fmt::write(&mut result, args);
    if result.truncated {
        let end = result.text.floor_char_boundary(MAX_ERROR_TEXT_BYTES - 3);
        result.text.truncate(end);
        result.text.push_str("...");
    }
    result
}

fn rendered(value: &LpcRef, txn: &TxnHandle) -> Text {
    match value {
        LpcRef::String(string) => text(format_args!("{:?}", string.to_str())),
        LpcRef::Bytes(bytes) => text(format_args!("<bytes: {} bytes>", bytes.len())),
        LpcRef::Array(cell) => txn.with(|t| match t.read_array(cell.id) {
            Some(array) => text(format_args!("<array: {} elements>", array.len())),
            None => text(format_args!("<unavailable array>")),
        }),
        LpcRef::Mapping(cell) => txn.with(|t| match t.read_mapping(cell.id) {
            Some(mapping) => text(format_args!("<mapping: {} entries>", mapping.len())),
            None => text(format_args!("<unavailable mapping>")),
        }),
        LpcRef::Class(cell) => txn.with(|t| match t.read_class(cell.id) {
            Some(class) => text(format_args!("<{}>", class.id())),
            None => text(format_args!("<unavailable class>")),
        }),
        LpcRef::Object(object) => match object.upgrade() {
            Some(object) if object.is_live(txn) => {
                text(format_args!("<object: {}>", object.filename()))
            }
            _ => text(format_args!("<destructed object>")),
        },
        LpcRef::Function(_) => text(format_args!("<function>")),
        other => text(format_args!("{other}")),
    }
}

fn variable(
    frame: &CallFrame,
    name: &str,
    location: Option<RegisterVariant>,
    txn: &TxnHandle,
) -> ErrorVariable {
    let name = text(format_args!("{name}"));
    let value = location.and_then(|location| frame.get_location(txn, location).ok());
    let (type_, value) = match value {
        Some(value) => (value.type_name().to_owned(), rendered(&value, txn)),
        None => ("unknown".to_owned(), text(format_args!("<unavailable>"))),
    };
    ErrorVariable {
        name: name.text,
        type_,
        value: value.text,
        truncated: name.truncated || value.truncated,
    }
}

fn capture_frame(frame: &CallFrame, txn: &TxnHandle, budget: &mut usize) -> Option<ErrorFrame> {
    if *budget < MAX_ERROR_TEXT_BYTES * 4 {
        return None;
    }
    let function = text(format_args!("{}", frame.function.name()));
    let program = text(format_args!("{}", frame.function.prototype.filename));
    let object = text(format_args!("{}", frame.process.filename()));
    let position = frame
        .current_debug_span()
        .and_then(|span| span.file_position());
    let (file, line, column) = position.unwrap_or_else(|| (String::new(), 0, 0));
    let file = text(format_args!("{file}"));
    let mut result = ErrorFrame {
        function: function.text,
        program: program.text,
        object: object.text,
        file: file.text,
        line,
        column,
        truncated: function.truncated || program.truncated || object.truncated || file.truncated,
        ..Default::default()
    };
    *budget -= result.text_bytes();
    let arguments = usize::from(frame.function.arity().num_args);
    let locals = &frame.function.local_variables;
    let named_parameters = frame.function.arg_locations.len().min(locals.len());
    let local_count = locals.len() - named_parameters;
    let total = arguments + local_count;
    for index in 0..total.min(MAX_ERROR_VARIABLES) {
        if *budget < MAX_ERROR_TEXT_BYTES * 2 + 16 {
            break;
        }
        let value = if index < arguments {
            let location = frame
                .function
                .arg_locations
                .get(index)
                .copied()
                .unwrap_or(RegisterVariant::Local(Register((index + 1) as _)));
            match locals
                .get(index)
                .filter(|symbol| index < named_parameters && symbol.location == Some(location))
            {
                Some(symbol) => variable(frame, &symbol.name, Some(location), txn),
                None => variable(frame, &format!("${}", index + 1), Some(location), txn),
            }
        } else {
            let symbol = &locals[named_parameters + index - arguments];
            variable(frame, &symbol.name, symbol.location, txn)
        };
        *budget -= value.name.len() + value.type_.len() + value.value.len();
        result.truncated |= value.truncated;
        if index < arguments {
            result.arguments.push(value);
        } else {
            result.locals.push(value);
        }
    }
    result.variables_omitted = total - result.arguments.len() - result.locals.len();
    result.truncated |= result.variables_omitted > 0;
    Some(result)
}

#[cold]
pub(crate) fn capture<const N: usize>(stack: &CallStack<N>, txn: &TxnHandle) -> ErrorFrames {
    let mut budget = MAX_ERROR_FRAME_BYTES;
    let mut frames = Vec::new();
    for frame in stack.iter().rev().take(MAX_ERROR_FRAMES) {
        let Some(frame) = capture_frame(frame, txn, &mut budget) else {
            break;
        };
        frames.push(frame);
    }
    frames.reverse();
    ErrorFrames {
        omitted: stack.len() - frames.len(),
        frames,
    }
}

/// Nested cells must be minted inside each reporting attempt, including retries.
#[derive(Debug, Clone)]
pub struct ErrorMapping {
    pub mapping: LpcMapping,
    pub frames: ErrorFrames,
}

fn mapping(txn: &TxnHandle, fields: impl IntoIterator<Item = (&'static str, LpcRef)>) -> LpcRef {
    let contents: IndexMap<_, _> = fields
        .into_iter()
        .map(|(name, value)| (LpcRef::from(name), value))
        .collect();
    LpcRef::Mapping(txn.with(|t| t.mint_mapping(LpcMapping::new(contents))))
}

fn array(txn: &TxnHandle, values: impl IntoIterator<Item = LpcRef>) -> LpcRef {
    let array = LpcArray::from_iter(values);
    LpcRef::Array(txn.with(|t| t.mint_array(array)))
}

fn variables(txn: &TxnHandle, values: &[ErrorVariable]) -> LpcRef {
    array(
        txn,
        values.iter().map(|v| {
            mapping(
                txn,
                [
                    ("name", v.name.as_str().into()),
                    ("type", v.type_.as_str().into()),
                    ("value", v.value.as_str().into()),
                    ("truncated", i64::from(v.truncated).into()),
                ],
            )
        }),
    )
}

impl ErrorMapping {
    pub(crate) fn materialize(&self, txn: &TxnHandle) -> LpcRef {
        let mut record = self.mapping.clone();
        let frames = array(
            txn,
            self.frames.frames.iter().map(|f| {
                mapping(
                    txn,
                    [
                        ("function", f.function.as_str().into()),
                        ("program", f.program.as_str().into()),
                        ("object", f.object.as_str().into()),
                        ("file", f.file.as_str().into()),
                        ("line", (f.line as i64).into()),
                        ("column", (f.column as i64).into()),
                        ("arguments", variables(txn, &f.arguments)),
                        ("locals", variables(txn, &f.locals)),
                        ("variables_omitted", (f.variables_omitted as i64).into()),
                        ("truncated", i64::from(f.truncated).into()),
                    ],
                )
            }),
        );
        record.insert("frames".into(), frames);
        record.insert("frames_omitted".into(), (self.frames.omitted as i64).into());
        LpcRef::Mapping(txn.with(|t| t.mint_mapping(record)))
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_errors::LpcError;
    use lpc_rs_utils::config::ConfigBuilder;
    use serde_json::Value;

    use super::*;
    use crate::interpreter::{
        CommittedReader,
        process::Process,
        task::{
            SeedArg, SeedEntry, TaskSeed,
            apply_function::{apply_function_by_name, apply_runtime_error},
            task_template::TaskTemplate,
        },
        vm::Vm,
    };

    fn vm() -> Vm {
        Vm::new(
            ConfigBuilder::default()
                .lib_dir("tests/fixtures/code")
                .null_mode(NullMode::Distinct)
                .build()
                .unwrap(),
        )
    }

    async fn process(vm: &Vm, name: &str, source: &str) -> Arc<Process> {
        vm.initialize_process_from_code(name, source)
            .await
            .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()))
            .context
            .process
    }

    async fn failed(
        vm: &Vm,
        process: &Arc<Process>,
        name: &str,
        args: &[LpcRef],
        timeout: u64,
    ) -> LpcError {
        apply_function_by_name(
            name,
            args,
            process.clone(),
            TaskTemplate::from(vm.global_state.clone()),
            Some(timeout),
        )
        .await
        .unwrap()
        .unwrap_err()
    }

    fn slot<'a>(frame: &'a ErrorFrame, name: &str) -> &'a ErrorVariable {
        frame
            .arguments
            .iter()
            .chain(&frame.locals)
            .find(|v| v.name == name)
            .unwrap()
    }

    #[test]
    fn formatted_text_truncates_at_unicode_boundaries_across_parts() {
        for character in ["é", "界", "🦀"] {
            let prefix = "x".repeat(MAX_ERROR_TEXT_BYTES - character.len() - 1);
            let suffix = character.repeat(2);
            let result = text(format_args!("{prefix}{suffix}"));

            assert!(result.truncated, "{character}");
            assert_eq!(result.text, format!("{prefix}..."), "{character}");
            assert!(result.text.len() <= MAX_ERROR_TEXT_BYTES);
        }
    }

    #[test]
    fn formatted_text_preserves_an_exact_budget_without_truncation() {
        let prefix = "x".repeat(MAX_ERROR_TEXT_BYTES - 4);
        let result = text(format_args!("{prefix}🦀"));

        assert!(!result.truncated);
        assert_eq!(result.text, format!("{prefix}🦀"));
        assert_eq!(result.text.len(), MAX_ERROR_TEXT_BYTES);
    }

    async fn frame_with_names(
        program_name: &str,
        function_name: &str,
        receiver_name: &str,
        txn: &TxnHandle,
    ) -> CallFrame {
        let config = crate::test_support::test_config();
        let path = lpc_rs_core::lpc_path::LpcPath::new_in_game(program_name, "/", config.lib_dir);
        let compiled = crate::compiler::Compiler::new(config)
            .compile_string(path, format!("int {function_name}() {{ return 1; }}"))
            .await
            .unwrap();
        let function = compiled
            .program
            .lookup_function(function_name)
            .unwrap()
            .clone();
        let process = Process::new_virtual(compiled.program.into(), receiver_name.to_owned());
        let mut frame = CallFrame::new(
            process,
            function,
            0,
            None::<&[crate::interpreter::stm::VarId]>,
            txn,
        );
        let at = frame
            .function
            .debug_spans
            .iter()
            .position(Option::is_some)
            .unwrap();
        frame.set_pc(at + 1);
        frame
    }

    #[tokio::test]
    async fn a_full_header_fits_the_remaining_frame_budget() {
        let name = "n".repeat(MAX_ERROR_TEXT_BYTES + 1);
        let txn = TxnHandle::default();
        let directory = "p".repeat(MAX_ERROR_TEXT_BYTES / 2);
        let basename = "n".repeat(MAX_ERROR_TEXT_BYTES / 2);
        let program = format!("/{directory}/{basename}.c");
        let receiver = format!("/{directory}/{basename}");
        let frame = frame_with_names(&program, &name, &receiver, &txn).await;
        let mut budget = MAX_ERROR_TEXT_BYTES * 4;
        let captured = capture_frame(&frame, &txn, &mut budget).unwrap();

        for field in [
            &captured.function,
            &captured.program,
            &captured.object,
            &captured.file,
        ] {
            assert_eq!(field.len(), MAX_ERROR_TEXT_BYTES);
            assert!(field.ends_with("..."));
        }
        assert!(captured.truncated);
        assert_eq!(captured.text_bytes(), MAX_ERROR_TEXT_BYTES * 4);
        assert_eq!(budget, 0);
        assert!(captured.arguments.is_empty() && captured.locals.is_empty());
    }

    #[tokio::test]
    async fn a_truncated_header_field_marks_the_frame_as_truncated() {
        let txn = TxnHandle::default();
        let long = "r".repeat(MAX_ERROR_TEXT_BYTES + 1);
        for (function, receiver) in [
            ("failure", format!("/instance/{long}")),
            (long.as_str(), "/receiver".to_owned()),
        ] {
            let frame = frame_with_names("/base.c", function, &receiver, &txn).await;
            let mut budget = MAX_ERROR_FRAME_BYTES;
            let captured = capture_frame(&frame, &txn, &mut budget).unwrap();

            assert_eq!(captured.program, "/base.c");
            assert_eq!(captured.file, "/base.c");
            let clipped = if function == "failure" {
                &captured.object
            } else {
                &captured.function
            };
            assert_eq!(clipped.len(), MAX_ERROR_TEXT_BYTES);
            assert!(clipped.ends_with("..."));
            assert!(captured.truncated);
        }
    }

    #[tokio::test]
    async fn error_frames_observe_destruction_before_the_failed_attempt_rolls_back() {
        let vm = vm();
        crate::test_support::permissive_master(&vm.global_state.object_space).await;
        let victim = process(&vm, "/victim.c", "").await;
        let caller = process(
            &vm,
            "/caller.c",
            "void failure(object doomed) { destruct(doomed); throw(\"dead-object-snapshot\"); }",
        )
        .await;
        let value = LpcRef::from(Arc::downgrade(&victim));
        let error = failed(&vm, &caller, "failure", &[value], 4000).await;
        assert!(error.to_string().contains("dead-object-snapshot"));
        let frame = error.error_frames().unwrap().frames.last().unwrap();

        assert_eq!(slot(frame, "doomed").value, "<destructed object>");
        assert!(vm.global_state.object_space.lookup("/victim").is_some());
    }

    fn frame_with_arguments(
        mut function: lpc_rs_function_support::program_function::ProgramFunction,
        receiver: &str,
        txn: &TxnHandle,
    ) -> CallFrame {
        function.instructions = vec![lpc_rs_asm::instruction::Instruction::Ret];
        function.debug_spans = vec![None];
        let count = function.arity().num_args;
        let program = crate::interpreter::program::Program {
            filename: function.prototype.filename.clone(),
            ..Default::default()
        };
        CallFrame::new(
            Process::new_virtual(Arc::new(program), receiver.to_owned()),
            Arc::new(function),
            count,
            None::<&[crate::interpreter::stm::VarId]>,
            txn,
        )
    }

    #[test]
    fn unnamed_arguments_keep_their_positions_with_local_debug_metadata() {
        use lpc_rs_core::{function_arity::FunctionArity, lpc_type::LpcType};
        use lpc_rs_function_support::{
            function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
            symbol::Symbol,
        };

        for (count, omitted, truncated) in [(2, 0, false), (MAX_ERROR_VARIABLES + 1, 2, true)] {
            let count = u16::try_from(count).unwrap();
            let prototype = FunctionPrototypeBuilder::default()
                .name("arguments")
                .filename(Arc::new("/arguments.c".into()))
                .return_type(LpcType::Void)
                .arity(FunctionArity::new(count))
                .build()
                .unwrap();
            let mut function = ProgramFunction::new(prototype, 0);
            let mut alias = Symbol::new("alias", LpcType::Int(false));
            alias.location = Some(Register(1).as_local());
            function.local_variables.push(alias);
            let txn = TxnHandle::default();
            let mut frame = frame_with_arguments(function, "/receiver", &txn);
            for (register, value) in [(0, 99), (1, 40), (2, 41)] {
                frame
                    .set_location(&txn, Register(register).as_local(), value.into())
                    .unwrap();
            }
            let mut budget = MAX_ERROR_FRAME_BYTES;
            let captured = capture_frame(&frame, &txn, &mut budget).unwrap();

            assert_eq!(captured.arguments[0].name, "$1");
            assert_eq!(captured.arguments[0].value, "40");
            assert_eq!(captured.arguments[1].name, "$2");
            assert_eq!(captured.arguments[1].value, "41");
            assert_eq!(captured.variables_omitted, omitted);
            assert_eq!(captured.truncated, truncated);
            if omitted == 0 {
                assert_eq!(captured.locals[0].name, "alias");
                assert_eq!(captured.locals[0].value, "40");
            }
        }
    }

    #[test]
    fn variable_capture_respects_its_remaining_byte_budget() {
        use lpc_rs_core::{function_arity::FunctionArity, lpc_type::LpcType};
        use lpc_rs_function_support::{
            function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
            symbol::Symbol,
        };

        let name = "f".repeat(MAX_ERROR_TEXT_BYTES + 1);
        let prototype = FunctionPrototypeBuilder::default()
            .name(name)
            .filename(Arc::new("/base.c".into()))
            .return_type(LpcType::Void)
            .arity(FunctionArity::new(1))
            .build()
            .unwrap();
        let mut function = ProgramFunction::new(prototype, 0);
        let mut argument = Symbol::new(&"a".repeat(MAX_ERROR_TEXT_BYTES), LpcType::String(false));
        argument.location = Some(Register(1).as_local());
        function.local_variables.push(argument);
        function.arg_locations.push(Register(1).as_local());
        let txn = TxnHandle::default();
        let mut frame = frame_with_arguments(
            function,
            &format!("/instance/{}", "r".repeat(MAX_ERROR_TEXT_BYTES + 1)),
            &txn,
        );
        frame
            .set_location(
                &txn,
                Register(1).as_local(),
                "x".repeat(MAX_ERROR_TEXT_BYTES - 2).into(),
            )
            .unwrap();
        let mut full_budget = MAX_ERROR_FRAME_BYTES;
        let full = capture_frame(&frame, &txn, &mut full_budget).unwrap();
        let header = full.function.len() + full.program.len() + full.object.len() + full.file.len();
        let variable = &full.arguments[0];
        let bytes = variable.name.len() + variable.type_.len() + variable.value.len();
        let allowance = MAX_ERROR_TEXT_BYTES * 2 + 16;

        for (remaining, expected) in [(bytes - 1, 0), (allowance - 1, 0), (allowance, 1)] {
            let initial = header + remaining;
            let mut budget = initial;
            let captured = capture_frame(&frame, &txn, &mut budget).unwrap();

            assert_eq!(captured.arguments.len(), expected);
            assert_eq!(captured.variables_omitted, 1 - expected);
            assert!(captured.truncated);
            assert!(captured.text_bytes() <= initial);
            assert_eq!(budget, initial - captured.text_bytes());
            if expected == 1 {
                assert_eq!(captured.arguments, full.arguments);
            }
        }
    }

    #[tokio::test]
    async fn error_frames_capture_named_cells_and_container_summaries_before_rollback() {
        let vm = vm();
        let target = process(
            &vm,
            "/snapshot.c",
            r#"
            class sample { int number; }
            int changes;
            mixed *shared;
            void create() { shared = ({ 0 }); }
            void boom(int ref number, string label) {
                int calculated = number + 2;
                function increment = (: calculated++ :);
                increment();
                number += 3;
                mixed *cycle = ({ nil, 2 });
                cycle[0] = cycle;
                mapping table = (["self": nil]);
                table["self"] = table;
                class sample record = new(class sample, number: 9);
                bytes octets = to_bytes("abc", "UTF-8");
                object receiver = this_object();
                changes = 99;
                shared[0] = 99;
                throw("snapshot-marker");
            }
            void entry(int number, string label) { boom(ref number, label); }
        "#,
        )
        .await;
        let error = failed(
            &vm,
            &target,
            "entry",
            &[40.into(), "line\n\u{1b}".into()],
            4000,
        )
        .await;
        let trace = error.error_frames().unwrap();
        assert_eq!(trace.omitted, 0);
        assert_eq!(
            trace
                .frames
                .iter()
                .map(|f| f.function.as_str())
                .collect::<Vec<_>>(),
            ["entry", "boom"]
        );
        for frame in &trace.frames {
            assert_eq!(slot(frame, "number").value, "43");
            assert_eq!(frame.program, "/snapshot.c");
            assert_eq!(frame.object, "/snapshot");
            assert_eq!(frame.file, "/snapshot.c");
            assert!(frame.line > 0 && frame.column > 0);
        }
        let frame = trace.frames.last().unwrap();
        assert_eq!(slot(frame, "calculated").value, "43");
        assert_eq!(slot(frame, "label").value, "\"line\\n\\u{1b}\"");
        assert_eq!(slot(frame, "cycle").value, "<array: 2 elements>");
        assert_eq!(slot(frame, "table").value, "<mapping: 1 entries>");
        assert_eq!(slot(frame, "record").value, "<class sample (/snapshot.c)>");
        assert_eq!(slot(frame, "octets").value, "<bytes: 3 bytes>");
        assert_eq!(slot(frame, "receiver").value, "<object: /snapshot>");
        assert_eq!(slot(frame, "increment").value, "<function>");
        assert_eq!(
            vm.global_state.committed_global(&target, 0u16),
            LpcRef::from(0)
        );
        let LpcRef::Array(shared) = vm.global_state.committed_global(&target, 1u16) else {
            panic!("array global")
        };
        assert_eq!(
            vm.global_state.committed_array(shared.id).unwrap()[0],
            LpcRef::from(0)
        );
        vm.global_state.gc().await.unwrap().unwrap();
        assert_eq!(
            slot(
                error.error_frames().unwrap().frames.last().unwrap(),
                "calculated"
            )
            .value,
            "43"
        );
    }

    #[tokio::test]
    async fn error_frames_reach_the_fresh_handler_and_survive_gc() {
        let vm = vm();
        let master = process(
            &vm,
            "/secure/master.c",
            r#"
            mapping saved;
            void error_handler(mapping error) { saved = error; }
            string inspect() {
                return json_encode((["frames": saved["frames"],
                    "frames_omitted": saved["frames_omitted"],
                    "diagnostic": saved["diagnostic"]]));
            }
        "#,
        )
        .await;
        let target = process(
            &vm,
            "/snapshot.c",
            r#"
            void failure(int argument) { int local = argument + 1; throw("handler-snapshot"); }
        "#,
        )
        .await;
        let error = failed(&vm, &target, "failure", &[41.into()], 4000).await;
        let result = apply_runtime_error(
            &error,
            Some(target),
            TaskTemplate::from(vm.global_state.clone()),
        )
        .await;
        assert!(matches!(result, Some(Ok(_))), "{result:?}");
        vm.global_state.gc().await.unwrap().unwrap();
        let result = apply_function_by_name(
            "inspect",
            &[],
            master,
            TaskTemplate::from(vm.global_state.clone()),
            Some(4000),
        )
        .await
        .unwrap()
        .unwrap();
        let record: Value = serde_json::from_str(&result.to_string()).unwrap();
        assert_eq!(record["frames_omitted"], 0);
        assert_eq!(record["frames"][0]["function"], "failure");
        assert_eq!(record["frames"][0]["arguments"][0]["name"], "argument");
        assert_eq!(record["frames"][0]["arguments"][0]["type"], "int");
        assert_eq!(record["frames"][0]["arguments"][0]["value"], "41");
        assert_eq!(record["frames"][0]["locals"][0]["name"], "local");
        assert_eq!(record["frames"][0]["locals"][0]["value"], "42");
        assert!(
            record["diagnostic"]
                .as_str()
                .unwrap()
                .contains("handler-snapshot")
        );
    }

    #[tokio::test]
    async fn error_frames_merge_nested_tasks_in_root_to_leaf_order() {
        let vm = vm();
        process(&vm, "/inner.c", "void create(int value) { if (value) { int local = value + 1; throw(\"nested-snapshot\"); } }").await;
        let target = process(
            &vm,
            "/outer.c",
            r#"
            void outer(int value) { clone_object("/inner", value + 1); }
        "#,
        )
        .await;
        let error = failed(&vm, &target, "outer", &[40.into()], 4000).await;
        let trace = error.error_frames().unwrap();
        assert_eq!(
            trace
                .frames
                .iter()
                .map(|f| f.function.as_str())
                .collect::<Vec<_>>(),
            ["outer", "init-program", "create"]
        );
        assert_eq!(slot(&trace.frames[0], "value").value, "40");
        assert_eq!(slot(&trace.frames[2], "local").value, "42");
        assert_eq!(trace.frames[2].object, "/inner#0");
    }

    #[tokio::test]
    async fn error_frames_preserve_interrupted_nested_tasks() {
        let vm = vm();
        process(
            &vm,
            "/inner.c",
            "void create(int value) { if (value) { int local = value + 1; while (1) {} } }",
        )
        .await;
        let target = process(
            &vm,
            "/outer.c",
            "void outer(int value) { clone_object(\"/inner\", value + 1); }",
        )
        .await;
        let error = failed(&vm, &target, "outer", &[40.into()], 30).await;
        assert!(error.to_string().contains("evaluation limit"));
        let trace = error.error_frames().unwrap();
        assert_eq!(
            trace
                .frames
                .iter()
                .map(|f| f.function.as_str())
                .collect::<Vec<_>>(),
            ["outer", "init-program", "create"]
        );
        assert_eq!(slot(&trace.frames[2], "local").value, "42");
    }

    #[tokio::test]
    async fn error_frames_do_not_change_catch_results_or_report_caught_failures() {
        let vm = vm();
        let master = process(
            &vm,
            "/secure/master.c",
            "int calls; void error_handler(mapping error) { calls++; }",
        )
        .await;
        let target = process(
            &vm,
            "/caught.c",
            r#"
            int written;
            void failure() { written = 7; throw("caught-marker"); }
            mixed caught() { return catch(failure()); }
        "#,
        )
        .await;
        let result = apply_function_by_name(
            "caught",
            &[],
            target.clone(),
            TaskTemplate::from(vm.global_state.clone()),
            Some(4000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(result, LpcRef::from("caught-marker"));
        assert_eq!(
            vm.global_state.committed_global(&target, 0u16),
            LpcRef::from(7)
        );
        assert_eq!(
            vm.global_state.committed_global(&master, 0u16),
            LpcRef::from(0)
        );
        let sample = process(
            &vm,
            "/sample.c",
            "void failure(int argument) { int local = argument + 1; throw(\"fresh-mapping\"); }",
        )
        .await;
        let error = failed(&vm, &sample, "failure", &[41.into()], 4000).await;
        let mapping = ErrorMapping {
            mapping: LpcMapping::default(),
            frames: error.error_frames().unwrap().clone(),
        };
        let seed = TaskSeed {
            process: master,
            entry: SeedEntry::Master("error_handler".into()),
            args: vec![SeedArg::FreshErrorMapping(Box::new(mapping))],
            initializes: false,
        };
        for _ in 0..2 {
            let txn = TxnHandle::default();
            let value = seed.arg_values(&txn).pop().unwrap();
            let frames = value
                .with_mapping(&txn, |m| m.get(&"frames".into()).unwrap().clone())
                .unwrap();
            let frame = frames.with_array(&txn, |a| a[0].clone()).unwrap();
            let locals = frame
                .with_mapping(&txn, |m| m.get(&"locals".into()).unwrap().clone())
                .unwrap();
            let local = locals.with_array(&txn, |a| a[0].clone()).unwrap();
            assert_eq!(
                local
                    .with_mapping(&txn, |m| m.get(&"value".into()).unwrap().clone())
                    .unwrap(),
                LpcRef::from("42")
            );
        }
    }

    #[tokio::test]
    async fn error_frames_limit_frames_variables_and_unicode_text() {
        let vm = vm();
        let declarations = (0..40)
            .map(|i| format!("string local{i} = payload; "))
            .collect::<String>();
        let source = format!(
            "void recurse(int depth, string payload) {{ {declarations} if (depth) recurse(depth - 1, payload); else throw(\"bounded-snapshot\"); }}"
        );
        let target = process(&vm, "/bounded.c", &source).await;
        let error = failed(
            &vm,
            &target,
            "recurse",
            &[40.into(), "界".repeat(300).into()],
            4000,
        )
        .await;
        let trace = error.error_frames().unwrap();
        assert!(trace.frames.len() <= MAX_ERROR_FRAMES);
        assert_eq!(trace.frames.len() + trace.omitted, 41);
        assert_eq!(slot(trace.frames.last().unwrap(), "depth").value, "0");
        assert!(
            trace
                .frames
                .iter()
                .map(ErrorFrame::text_bytes)
                .sum::<usize>()
                <= MAX_ERROR_FRAME_BYTES
        );
        for frame in &trace.frames {
            assert!(frame.arguments.len() + frame.locals.len() <= MAX_ERROR_VARIABLES);
            assert!(frame.variables_omitted >= 10);
            assert!(frame.truncated);
            let payload = slot(frame, "payload");
            assert_eq!(payload.type_, "string");
            assert!(payload.truncated && payload.value.ends_with("..."));
            assert!(payload.value.len() <= MAX_ERROR_TEXT_BYTES);
        }
    }

    #[tokio::test]
    async fn error_frames_keep_implicit_arguments_and_named_variadic_collectors() {
        let vm = vm();
        let target = process(
            &vm,
            "/arguments.c",
            r#"
            void variadic(int first, mixed rest...) { throw("variadic-snapshot"); }
            void implicit() { evaluate((: int local = $1 + 1; throw("closure-snapshot"); :), 41); }
        "#,
        )
        .await;
        let error = failed(
            &vm,
            &target,
            "variadic",
            &[40.into(), 41.into(), 42.into()],
            4000,
        )
        .await;
        let frame = error.error_frames().unwrap().frames.last().unwrap();
        assert_eq!(frame.arguments.len(), 1);
        assert_eq!(slot(frame, "first").value, "40");
        assert_eq!(slot(frame, "rest").value, "<array: 2 elements>");
        let error = failed(&vm, &target, "implicit", &[], 4000).await;
        let frame = error.error_frames().unwrap().frames.last().unwrap();
        assert_eq!(slot(frame, "$1").value, "41");
        assert_eq!(slot(frame, "local").value, "42");
    }

    #[tokio::test]
    async fn error_frames_distinguish_source_includes_and_inherited_receivers() {
        let root = tempfile::TempDir::new().unwrap();
        std::fs::write(root.path().join("failure.h"), "void failure(int value) { int shadowed = 1; { int shadowed = 2; } throw(\"include-snapshot\"); }\n").unwrap();
        std::fs::write(root.path().join("base.c"), "#include \"/failure.h\"\n").unwrap();
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.path().to_str().unwrap())
                .null_mode(NullMode::Distinct)
                .build()
                .unwrap(),
        );
        let target = process(
            &vm,
            "/child.c",
            "inherit \"/base\"; void entry() { failure(41); }",
        )
        .await;
        let error = failed(&vm, &target, "entry", &[], 4000).await;
        let frame = error.error_frames().unwrap().frames.last().unwrap();
        assert_eq!(frame.program, "/base.c");
        assert_eq!(frame.object, "/child");
        assert_eq!(frame.file, "/failure.h");
        assert_eq!(frame.line, 1);
        assert_eq!(
            frame
                .locals
                .iter()
                .filter(|v| v.name == "shadowed")
                .map(|v| v.value.as_str())
                .collect::<Vec<_>>(),
            ["1", "2"]
        );
    }
}
