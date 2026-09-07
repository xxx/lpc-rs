use std::{path::Path, sync::Arc};

use lpc_rs_core::{lpc_path::LpcPath, register::RegisterVariant};
use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize_save},
    lpc_ref::LpcRef,
    process::Process,
    save_format::{read_value, split_line},
    task_context::ObjectLookup,
};

/// The save file at `server` as text: UTF-8, or Latin-1 when it is not
/// (each byte its own character). `None` on any I/O failure: the efuns
/// answer that as "no file".
pub(crate) async fn read_save_file(server: &Path) -> Option<String> {
    let bytes = tokio::fs::read(server).await.ok()?;
    Some(match String::from_utf8(bytes) {
        Ok(text) => text,
        Err(e) => e.into_bytes().iter().map(|&b| b as char).collect(),
    })
}

/// The object resolver for `$created@name$` references: the live object
/// the context finds under `name`, never loaded.
pub(crate) fn resolve_object<'a, const N: usize>(
    context: &'a EfunContext<'_, N>,
) -> impl Fn(&str) -> Option<Arc<Process>> + 'a {
    move |name| {
        let path = LpcPath::new_in_game(name, "/", &*context.config().lib_dir);
        match context.find_object(&path) {
            ObjectLookup::Found(process) => Some(process),
            ObjectLookup::Removed | ObjectLookup::NotCreated => None,
        }
    }
}

/// `restore_object(file)`: the calling object's globals from `<file>.o`,
/// root-relative, once the master's `valid_read` allows the unsuffixed
/// path. Unknown and `static` names are ignored, variables absent from the
/// file keep their values. 1 on success, 0 when the file cannot be read,
/// an error on a corrupt line (earlier lines stay applied).
pub async fn restore_object<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize_save(context, "restore_object", VALID_READ, 0).await?;
    let Some(text) = read_save_file(&access.server).await else {
        context.return_efun_result(LpcRef::from(0));
        return Ok(());
    };
    let process = context.process().clone();
    {
        let resolve = resolve_object(context);
        for (index, line) in text.lines().enumerate() {
            let line_error = |e: lpc_rs_errors::LpcError| {
                context.runtime_error(format!(
                    "restore_object: {} line {}: {}",
                    access.in_game,
                    index + 1,
                    e.to_string().trim_start_matches("runtime error: ")
                ))
            };
            let (name, value_text) = split_line(line).map_err(line_error)?;
            let Some(symbol) = process.program.global_variables.get(name) else {
                continue;
            };
            if symbol.flags.is_static() {
                continue;
            }
            let Some(RegisterVariant::Global(reg)) = symbol.location else {
                continue;
            };
            let value = read_value(value_text, context.txn(), &resolve).map_err(line_error)?;
            context
                .txn()
                .with(|t| t.write(process.var_id(reg.index()), value));
        }
    }
    context.return_efun_result(LpcRef::from(1));
    Ok(())
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use crate::{
        interpreter::{CommittedReader, lpc_ref::LpcRef, vm::Vm},
        test_support::{TempLib, committed_string, lib_holding, string_global, temp_lib_config},
    };

    const ALLOWING: &str = indoc! { r#"
        int valid_read(string p, string e, object c, string g) { return 1; }
        int valid_write(string p, string e, object c, string g) { return 1; }
    "# };

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        vm
    }

    /// `/r.c`: `a`, `s`, `m`, `hidden` and `left` are the restore targets;
    /// `got` is what `restore_object("/r")` returned.
    const R: &str = indoc! { r#"
        int a;
        string s;
        mapping m;
        static int hidden = 5;
        int left = 77;
        int got;
        void create() { got = restore_object("/r"); }
    "# };

    #[tokio::test]
    async fn restores_the_named_globals_and_leaves_the_rest() {
        let root = TempLib::new("restore-basic");
        std::fs::write(
            root.join("r.o"),
            "a 42\ns \"hi\\nthere\"\nm ([\"k\":({1,2,}),])\nhidden 9\nunknown 1\n",
        )
        .unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(42));
        assert_eq!(string_global(&vm, &r, "s"), "hi\nthere");
        let m = vm.global_state.committed_global(&r, 2u16);
        assert!(matches!(m, LpcRef::Mapping(_)), "{m:?}");
        assert_eq!(
            vm.global_state.committed_global(&r, 3u16),
            LpcRef::from(5),
            "static untouched"
        );
        assert_eq!(
            vm.global_state.committed_global(&r, 4u16),
            LpcRef::from(77),
            "absent untouched"
        );
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(1));
    }

    #[tokio::test]
    async fn a_missing_file_is_zero_without_an_error() {
        let root = TempLib::new("restore-missing");
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(0));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(77));
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn an_unreadable_file_is_zero_without_an_error() {
        use std::os::unix::fs::PermissionsExt;
        let root = TempLib::new("restore-unreadable");
        let file = root.join("r.o");
        std::fs::write(&file, "a 1\n").unwrap();
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o000)).unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap()
            .context
            .process;
        std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();
        assert_eq!(vm.global_state.committed_global(&r, 5u16), LpcRef::from(0));
    }

    #[tokio::test]
    async fn a_corrupt_line_is_an_error_naming_the_file_and_line() {
        let root = TempLib::new("restore-corrupt");
        std::fs::write(root.join("r.o"), "a 5\ns ({1,2})\nleft 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/r.c", R)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("restore_object: /r.o line 2"), "{err}");
        assert!(err.contains("expected `,`"), "{err}");
    }

    #[tokio::test]
    async fn under_catch_the_earlier_lines_stay_applied() {
        let root = TempLib::new("restore-partial");
        std::fs::write(root.join("r.o"), "a 5\ns ({1,2})\nleft 1\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int a;
                    string s;
                    mapping m;
                    static int hidden = 5;
                    int left = 77;
                    string err;
                    void create() { err = catch(restore_object("/r")); }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(5));
        assert_eq!(vm.global_state.committed_global(&r, 4u16), LpcRef::from(77));
        assert!(string_global(&vm, &r, "err").contains("restore_object: /r.o line 2"));
    }

    #[tokio::test]
    async fn the_master_sees_the_path_before_the_suffix_and_a_refusal_is_an_error() {
        let root = TempLib::new("restore-gate");
        std::fs::write(root.join("r.o"), "a 1\n").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                indoc! { r#"
                    string seen;
                    int valid_read(string p, string e, object c, string g) {
                        seen = p + " " + e;
                        return 0;
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"int a; string err; void create() { err = catch(restore_object("r")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(committed_string(&vm, &master, 0), "/r restore_object");
        assert!(string_global(&vm, &r, "err").contains("restore_object: permission denied"));
        assert_eq!(vm.global_state.committed_global(&r, 0u16), LpcRef::from(0));
    }

    #[tokio::test]
    async fn a_saved_object_restores_into_a_fresh_object() {
        let root = lib_holding("restore-round-trip", &[("secure/master.c", ALLOWING)]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        vm.initialize_process_from_code(
            "/w.c",
            indoc! { r#"
                int i = 42;
                float f = -2.5e-7;
                string s = "q\"b\\n\nt\tuéé";
                mixed *a = ({ 1, "two", ({ 3, ({}) }), ([ "k": 4 ]) });
                mapping m = ([ "b": 1, "c": 3, "a": 2 ]);
                void create() { save_object("/data"); }
            "# },
        )
        .await
        .unwrap();
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    int i;
                    float f;
                    string s;
                    mixed *a;
                    mapping m;
                    string check;
                    void create() {
                        restore_object("/data");
                        check = i + " " + to_string(f) + " " + s + " " + sizeof(a) + " " + a[2][0] + " " + a[3]["k"] + " " + m["c"] + " " + implode(keys(m), ",");
                    }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            string_global(&vm, &r, "check"),
            "42 -0.00000025 q\"b\\n\nt\tuéé 4 3 4 3 b,c,a"
        );
    }

    #[tokio::test]
    async fn an_object_reference_is_re_found_while_the_object_lives() {
        let root = lib_holding("restore-object-ref", &[("secure/master.c", ALLOWING)]);
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code("/secure/master.c", ALLOWING)
            .await
            .unwrap();
        let w = vm
            .initialize_process_from_code(
                "/w.c",
                r#"object me; void create() { me = this_object(); save_object("/data"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                indoc! { r#"
                    object me;
                    string got;
                    void create() { restore_object("/data"); got = me ? file_name(me) : "none"; }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "got"), "/w");
        drop(w);
        std::fs::write(root.join("data.o"), "me $1@/w$\n").unwrap();
        let r2 = vm
            .initialize_process_from_code(
                "/r2.c",
                indoc! { r#"
                    object me;
                    string got;
                    void create() { restore_object("/data"); got = me ? file_name(me) : "none"; }
                "# },
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            string_global(&vm, &r2, "got"),
            "none",
            "creation time mismatch"
        );
    }

    #[tokio::test]
    async fn a_latin1_file_reads_as_text() {
        let root = TempLib::new("restore-latin1");
        std::fs::write(root.join("r.o"), b"s \"caf\xe9\"\n").unwrap();
        let vm = allowing_vm(&root).await;
        let r = vm
            .initialize_process_from_code(
                "/r.c",
                r#"string s; void create() { restore_object("/r"); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(string_global(&vm, &r, "s"), "café");
    }
}
