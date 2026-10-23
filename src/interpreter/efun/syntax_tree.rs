use lpc_rs_errors::Result;

use crate::interpreter::{efun::efun_context::EfunContext, lpc_ref::LpcRef, task_context::Loader};

/// Inspect checked source without publishing or executing a program.
pub async fn syntax_tree<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let Some(name) = context.arg(0).as_str() else {
        return Err(context.runtime_error("syntax_tree: name must be a string"));
    };
    let Some(code) = context.arg(1).as_str() else {
        return Err(context.runtime_error("syntax_tree: source must be a string"));
    };
    let path = context
        .task_context()
        .object_path(name, context.in_game_cwd(), "syntax_tree")?;
    if path.is_clone() {
        return Err(context.runtime_error("syntax_tree: name must not identify a clone"));
    }
    let loader = Loader {
        func: "syntax_tree".into(),
        chain: context.chain(),
        program: context.calling_program(),
    };
    let lines = context
        .task_context()
        .syntax_tree(&path, &loader, code)
        .await
        .map_err(|error| error.with_label("inspected from here", context.call_site_span()))?;
    context.return_array(lines.into_iter().map(LpcRef::from));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        interpreter::vm::Vm,
        test_support::{PERMISSIVE_MASTER, TempLib, temp_lib_config},
    };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.create_process_from_code(
            "/secure/master.c",
            format!("{PERMISSIVE_MASTER}\nint valid_write() {{ return 1; }}"),
        )
        .await
        .unwrap();
        vm
    }

    #[tokio::test]
    async fn inspects_without_running_or_publishing_any_root_or_parent_program() {
        let root = TempLib::new("syntax-tree-no-execution");
        std::fs::write(root.join("parent.c"), "int parent_global = write_file(\"/parent-effect\", \"bad\"); void create() { throw(\"parent create\"); } int inherited() { return 7; }").unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", r##"
            int create() {
                string *lines = syntax_tree("/scratch", "inherit \"/parent\"; int initialized = write_file(\"/root-effect\", \"bad\"); void create() { throw(\"root create\"); } mixed run() { throw(\"must not run\"); return inherited(); }");
                string text = implode(lines, "\n");
                return sizeof(lines)>0 && strindex(text,"Inherit:")>=0 && strindex(text,"must not run")>=0
                    && !find_object("/scratch") && !find_object("/parent");
            }
        "##).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert!(!root.join("scratch.c").exists());
        assert!(!root.join("parent-effect").exists());
        assert!(!root.join("root-effect").exists());
        assert!(vm.global_state.object_space.lookup("/scratch").is_none());
        assert!(vm.global_state.object_space.lookup("/parent").is_none());
    }

    #[tokio::test]
    async fn reads_pending_relative_includes_and_inherits_and_reports_only_the_root_tree() {
        let root = TempLib::new("syntax-tree-pending-source");
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", r##"
            int create() {
                write_file("/value.h", "#define VALUE 41\n");
                write_file("/parent.c", "int inherited() { return 99; }");
                string text = implode(syntax_tree("/scratch", "#include \"value.h\"\ninherit \"parent\"; mixed f(){ return VALUE+1+inherited(); }"),"\n");
                return strindex(text,"Int: 42")>=0 && strindex(text,"Int: 99")==-1 && strindex(text,"Inherit:")>=0;
            }
        "##).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn existing_file_and_object_under_the_diagnostic_name_are_untouched() {
        let root = TempLib::new("syntax-tree-existing-name");
        let source = "int value(){ return 7; }";
        std::fs::write(root.join("scratch.c"), source).unwrap();
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", r##"
            int create() {
                object original = load_object("/scratch");
                string *first = syntax_tree("/scratch.c", "int other(){ return 9; }");
                first[0] = "changed";
                string *second = evaluate(syntax_tree, "/scratch", "int other(){ return 9; }");
                return original==find_object("/scratch") && original->value()==7 && second[0]=="Program";
            }
        "##).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
        assert_eq!(
            std::fs::read_to_string(root.join("scratch.c")).unwrap(),
            source
        );
    }

    #[tokio::test]
    async fn syntax_type_and_argument_errors_leave_the_name_available_for_retry() {
        let root = TempLib::new("syntax-tree-errors");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r##"
            int create() {
                mixed bad=123;
                string syntax=catch(syntax_tree("/scratch", "mixed f( {"));
                string types=catch(syntax_tree("/scratch", "int f(){ return \"wrong\"; }"));
                string unknown=catch(syntax_tree("/scratch", "mixed f(){ return absent; }"));
                string clone=catch(syntax_tree("/scratch#1", ""));
                string escape=catch(syntax_tree("../../escape", ""));
                string name=catch(syntax_tree(bad, ""));
                string source=catch(syntax_tree("/scratch", bad));
                string *retry=syntax_tree("/scratch", "mixed f(){ return nil; }");
                return !!syntax && !!types && !!unknown && !!clone && !!escape && !!name && !!source
                    && sizeof(retry)>0 && !find_object("/scratch");
            }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn source_identity_uses_existing_write_and_load_authorization() {
        for (write, load) in [(0, 1), (1, 0)] {
            let root = TempLib::new("syntax-tree-refused-source");
            let vm = Vm::new(temp_lib_config(&root));
            vm.create_process_from_code(
                "/secure/master.c",
                format!(
                    "int valid_write() {{ return {write}; }} int valid_load() {{ return {load}; }}"
                ),
            )
            .await
            .unwrap();
            let error = vm
                .initialize_process_from_code(
                    "/caller.c",
                    "void create(){ syntax_tree(\"/scratch\", \"\"); }",
                )
                .await
                .unwrap_err();
            assert!(
                error.to_string().contains("syntax_tree: permission denied"),
                "{error}"
            );
        }
    }

    #[tokio::test]
    async fn source_hooks_receive_the_canonical_identity_and_actual_caller() {
        let root = TempLib::new("syntax-tree-hook-context");
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code("/secure/master.c", r#"
            int wrote;
            int valid_write(string path,string func,object caller,string program){
                wrote = path=="/wizard/scratch.c" && func=="syntax_tree" && file_name(caller)=="/wizard/caller" && program=="/wizard/caller.c";
                return wrote;
            }
            int valid_load(string path,string func,object caller,string program){
                return wrote && path=="/wizard/scratch.c" && func=="syntax_tree" && file_name(caller)=="/wizard/caller" && program=="/wizard/caller.c";
            }
        "#).await.unwrap();
        let task = vm
            .initialize_process_from_code(
                "/wizard/caller.c",
                "int create(){ return sizeof(syntax_tree(\"./scratch.c\", \"\"))==1; }",
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn includes_inherits_and_explicit_efuns_keep_the_existing_compiler_gates() {
        let root = TempLib::new("syntax-tree-dependency-gates");
        std::fs::write(root.join("secret.h"), "").unwrap();
        std::fs::write(root.join("parent.c"), "").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/secure/master.c",
            "int valid_write(){ return 1; } int valid_load(){ return 1; } int valid_override(){ return 0; }",
        )
        .await
        .unwrap();
        let task = vm.initialize_process_from_code("/caller.c", r##"
            int create(){
                string include=catch(syntax_tree("/scratch", "#include \"secret.h\"\n"));
                string inherit_error=catch(syntax_tree("/scratch", "inherit \"parent\";"));
                string override_error=catch(syntax_tree("/scratch", "int f(){ return efun::sizeof(({1})); }"));
                return !!include && !!inherit_error && !!override_error;
            }
        "##).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn listings_can_exceed_one_lpc_string_and_escape_control_literals() {
        let root = TempLib::new("syntax-tree-large-output");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r##"
            int create(){
                string source="mixed f(){";
                for(int i=0;i<100;i++) source += sprintf("int local_%d=%d;",i,i);
                source += "return \"line1\\nline2\";}";
                string *lines=syntax_tree("/scratch",source);
                int byte_count;
                foreach(string line in lines){
                    if(strindex(line,"\n")>=0 || sizeof(line)>8192) return 0;
                    byte_count += sizeof(line);
                }
                return byte_count>8192 && sizeof(lines)>100;
            }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn inherited_warnings_use_the_normal_warning_handler() {
        let root = TempLib::new("syntax-tree-warnings");
        std::fs::write(root.join("parent.c"), "void unused(){ int dead; }").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.create_process_from_code(
            "/secure/master.c",
            format!(
                r#"{PERMISSIVE_MASTER}
            int warnings;
            int valid_write() {{ return 1; }}
            void warning_handler(mapping warning) {{ warnings++; }}
            int count() {{ return warnings; }}
        "#
            ),
        )
        .await
        .unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r##"
            int create(){
                syntax_tree("/scratch", "inherit \"parent\"; void f(){ int unused; }");
                return "/secure/master"->count();
            }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(2)));
    }

    #[tokio::test]
    async fn analysis_uses_the_attempts_simul_efun_signatures() {
        let root = TempLib::new("syntax-tree-simuls");
        std::fs::write(
            root.join("simul.c"),
            "int answer(int value) { return value; }",
        )
        .unwrap();
        let config = lpc_rs_utils::config::ConfigBuilder::default()
            .lib_dir(root.to_str().unwrap())
            .simul_efun_file("/simul.c")
            .build()
            .unwrap();
        let vm = Vm::new(config);
        vm.create_process_from_code(
            "/secure/master.c",
            format!("{PERMISSIVE_MASTER}\nint valid_write() {{ return 1; }}"),
        )
        .await
        .unwrap();
        vm.initialize_simul_efuns().await.unwrap().unwrap();
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r##"
            int create(){
                string text=implode(syntax_tree("/scratch", "int f(){ return answer(1); }"),"\n");
                string error=catch(syntax_tree("/scratch", "int f(){ return answer(\"bad\"); }"));
                return strindex(text,"name: answer")>=0 && !!error && !find_object("/scratch");
            }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn an_oversized_escaped_line_returns_no_partial_array() {
        let root = TempLib::new("syntax-tree-line-limit");
        let vm = allowing_vm(&root).await;
        let task = vm
            .initialize_process_from_code(
                "/caller.c",
                r##"
            int create(){
                string source="string f(){ return \"" + sprintf("%c",27)*1500 + "\"; }";
                string *lines=({"retained"});
                string error=catch(lines=syntax_tree("/scratch",source));
                return !!error && strindex(error,"output line exceeds")>=0
                    && sizeof(lines)==1 && lines[0]=="retained" && !find_object("/scratch");
            }
        "##,
            )
            .await
            .unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }

    #[tokio::test]
    async fn source_identity_controls_in_type_metadata_cannot_split_output_lines() {
        let root = TempLib::new("syntax-tree-source-controls");
        let vm = allowing_vm(&root).await;
        let task = vm.initialize_process_from_code("/caller.c", r##"
            int create(){
                string *lines=syntax_tree("/line\nname", "class Record { int field; } class Record f(){ return new(class Record, field: 1); }");
                foreach(string line in lines) if(strindex(line,"\n")>=0) return 0;
                return strindex(implode(lines,"\n"),"/line\\nname.c")>=0;
            }
        "##).await.unwrap();
        assert_eq!(task.result(), Some(LpcRef::from(1)));
    }
}
