use lpc_rs_errors::Result;
use lpc_rs_utils::string::MAX_STRING_LENGTH;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef};

/// Return diagnostic instruction lines from the target's current compiled program.
pub fn disassemble<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(target) = context.arg(0).live_object(context.txn()) else {
        return Err(context.runtime_error("disassemble: expected a live object"));
    };
    let lines = target.program(context.txn()).listing();
    if lines.iter().any(|line| line.len() > MAX_STRING_LENGTH) {
        return Err(
            context.runtime_error("disassemble: listing line exceeds maximum string length")
        );
    }
    context.return_array(lines.into_iter().map(LpcRef::from));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use lpc_rs_core::null_mode::NullMode;
    use lpc_rs_utils::config::ConfigBuilder;

    use crate::{
        interpreter::{
            lpc_ref::LpcRef,
            task::{apply_function::apply_function_by_name, task_template::TaskTemplate},
            vm::{Vm, vm_op::VmOp},
        },
        test_support::{
            PERMISSIVE_MASTER, lib_holding, permissive_master, run_prog, strings_of,
            temp_lib_config,
        },
    };

    #[tokio::test]
    async fn includes_initializers_nonpublic_code_closures_and_instruction_operands() {
        let lines = strings_of(indoc! { r#"
            class record { int number; }
            int initialized = 7;
            private string hidden() { throw("must not execute"); return "private literal"; }
            protected int guarded() { return initialized; }
            mixed construct(int number) { return new(class record, number: number); }
            function closure() { int local = 3; return (: $(local) + initialized + 5 :); }
            int branch(int number) { if (number) return guarded(); return 0; }
            string *create() { return disassemble(this_object()); }
        "# })
        .await;
        for name in [
            "init-program",
            "init-globals",
            "hidden",
            "guarded",
            "construct",
            "closure",
            "closure-0",
            "branch",
            "create",
        ] {
            assert!(
                lines
                    .iter()
                    .any(|line| line.starts_with(&format!("fn {name}"))),
                "missing {name}: {lines:?}"
            );
        }
        assert!(lines.iter().any(|line| line.contains("private literal")));
        assert!(lines.iter().any(|line| line.starts_with("    t0 = ")));
        assert!(lines.iter().any(|line| line.starts_with("    a0 = ")));
        assert!(lines.iter().any(|line| line.contains("num_upvalues=1")));
        assert!(lines.iter().any(|line| line.starts_with("    0000  ")));
        assert!(lines.iter().any(|line| line.starts_with("  ")
            && line.ends_with(':')
            && !line.starts_with("    ")));
    }

    #[tokio::test]
    async fn listings_are_fresh_and_exclude_mutable_global_values() {
        let root = lib_holding("disassemble-inspection", &[]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/target.c",
            indoc! { r#"
            string secret;
            int creates;
            void create() { creates++; }
            int created() { return creates; }
            private void hidden() {}
            void set_secret(string value) { secret = value; }
        "# },
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                indoc! { r#"
            int create() {
                object ob = find_object("/target");
                ob->set_secret("runtime-only secret");
                string *first = disassemble(ob);
                int count = sizeof(first);
                first[0] = "changed";
                string *second = disassemble(ob);
                return ob->created() == 1 && sizeof(second) == count && second[0] != "changed"
                    && sizeof(filter(second, (: strindex($1, "fn hidden (") == 0 :))) == 1
                    && !sizeof(filter(second, (: strindex($1, "runtime-only secret") >= 0 :)));
            }
        "# },
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn an_uninitialized_target_is_inspected_without_executing_its_initializer() {
        let root = lib_holding("disassemble-uninitialized", &[]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/target.c",
            "void create() { throw(\"must not initialize\"); }",
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/inspector.c",
                "int create() { return sizeof(disassemble(find_object(\"/target\"))) > 0; }",
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn rejects_nonobjects_and_dead_objects_without_loading_paths() {
        let root = lib_holding(
            "disassemble-arguments",
            &[
                ("target.c", "void create() { throw(\"must not load\"); }"),
                ("empty.c", ""),
            ],
        );
        let vm = Vm::new(
            ConfigBuilder::default()
                .lib_dir(root.to_str().unwrap())
                .null_mode(NullMode::Distinct)
                .build()
                .unwrap(),
        );
        permissive_master(&vm.global_state.object_space).await;
        let task = vm.initialize_process_from_code("/inspector.c", indoc! { r#"
            int create() {
                int failures;
                foreach (mixed value in ({ 0, nil, 1, "/target", ({}), ([]), abs })) {
                    mixed failure = catch(disassemble(value));
                    if (stringp(failure) && strindex(failure, "disassemble: expected a live object") >= 0) failures++;
                }
                object dead = clone_object("/empty");
                destruct(dead);
                mixed failure = catch(disassemble(dead));
                return failures == 7 && stringp(failure) && !find_object("/target");
            }
        "# }).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn a_program_without_user_functions_still_lists_generated_initializers() {
        let lines =
            strings_of("string *create() { return disassemble(clone_object(\"/empty\")); }").await;
        let headers: Vec<_> = lines
            .iter()
            .filter(|line| line.starts_with("fn "))
            .collect();
        assert!(!headers.is_empty());
        assert!(headers.iter().all(|line| line.starts_with("fn init-")));
    }

    #[tokio::test]
    async fn inherited_instructions_use_the_receivers_relocated_global_slots() {
        let root = lib_holding(
            "disassemble-inheritance",
            &[
                ("left.c", "int left; int get_left() { return left; }"),
                ("right.c", "int right; int get_right() { return right; }"),
            ],
        );
        let vm = Vm::new(temp_lib_config(&root));
        let task = vm
            .initialize_process_from_code(
                "/child.c",
                indoc! { r#"
            inherit "/left";
            inherit "/right";
            string *create() { return disassemble(this_object()); }
        "# },
            )
            .await
            .unwrap();
        let lines = task
            .result()
            .unwrap()
            .with_array(task.context.txn(), |array| {
                array.iter().map(ToString::to_string).collect::<Vec<_>>()
            })
            .unwrap();
        for (name, slot) in [("get_left", "g0"), ("get_right", "g1")] {
            let start = lines
                .iter()
                .position(|line| line.starts_with(&format!("fn {name} (")))
                .unwrap();
            let mut function = lines[start + 1..]
                .iter()
                .take_while(|line| !line.starts_with("fn "));
            assert!(
                function.any(|line| line.contains(slot)),
                "{name}: {lines:?}"
            );
        }
    }

    #[tokio::test]
    async fn a_listing_can_exceed_one_strings_limit() {
        let mut code = String::from("string *create() { return disassemble(this_object()); }\n");
        for index in 0..200 {
            code.push_str(&format!("int method_{index}() {{ return {index}; }}\n"));
        }
        let lines = strings_of(&code).await;
        assert!(
            lines.iter().map(String::len).sum::<usize>() > lpc_rs_utils::string::MAX_STRING_LENGTH
        );
        assert_eq!(
            lines
                .iter()
                .filter(|line| line.starts_with("fn method_"))
                .count(),
            200
        );
    }

    #[tokio::test]
    async fn oversized_diagnostic_lines_are_rejected_without_truncation() {
        let code = format!(
            r#"
            string literal() {{ return "{}"; }}
            int create() {{
                mixed failure = catch(disassemble(this_object()));
                return stringp(failure) && strindex(failure, "listing line exceeds maximum string length") >= 0;
            }}
        "#,
            "x".repeat(lpc_rs_utils::string::MAX_STRING_LENGTH)
        );
        assert_eq!(run_prog(&code).await.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn prototypes_and_existing_clones_follow_recompilation() {
        let root = lib_holding(
            "disassemble-recompile",
            &[("target.c", "int original() { return 7; }")],
        );
        let mut vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", format!("{PERMISSIVE_MASTER}\nint valid_clone() {{ return 1; }}\nint valid_recompile() {{ return 1; }}")).await.unwrap();
        let inspector = vm.initialize_process_from_code("/inspector.c", indoc! { r#"
            object prototype = load_object("/target");
            object copy = clone_object("/target");
            int request() { return request_object_recompile(prototype); }
            int contains(string name) {
                foreach (object ob in ({prototype, copy})) {
                    if (sizeof(filter(disassemble(ob), (: strindex($1, $("fn " + name + " (")) == 0 :))) != 1) return 0;
                }
                return 1;
            }
        "# }).await.unwrap().context.process;
        let template = TaskTemplate::from(vm.global_state.clone());
        let before = apply_function_by_name(
            "contains",
            &["original".into()],
            inspector.clone(),
            template.clone(),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        assert_eq!(before, LpcRef::from(1));
        std::fs::write(root.join("target.c"), "int replacement() { return 42; }").unwrap();
        apply_function_by_name(
            "request",
            &[],
            inspector.clone(),
            template.clone(),
            Some(5000),
        )
        .await
        .unwrap()
        .unwrap();
        let Some(VmOp::ObjectUpdate(request)) = vm.next_op() else {
            panic!("recompilation queued");
        };
        vm.global_state.run_object_update(request.clone()).await;
        let status = vm.global_state.updates.get(request.id).unwrap();
        assert_eq!(status.state, "succeeded", "{}", status.error);
        assert_eq!(status.updated, 2);
        for (name, expected) in [("replacement", 1), ("original", 0)] {
            let after = apply_function_by_name(
                "contains",
                &[name.into()],
                inspector.clone(),
                template.clone(),
                Some(5000),
            )
            .await
            .unwrap()
            .unwrap();
            assert_eq!(after, LpcRef::from(expected));
        }
    }
}
