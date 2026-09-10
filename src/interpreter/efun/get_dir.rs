use regex::Regex;

use lpc_rs_errors::Result;

use crate::interpreter::{
    VALID_READ,
    efun::{efun_context::EfunContext, file_access::authorize},
    lpc_ref::LpcRef,
};

/// List sorted plain names, optionally matching `*`, `?`, and backslash
/// escapes in the final path component, after the master's `valid_read` allows it.
pub async fn get_dir<const N: usize>(context: &mut EfunContext<'_, N>) -> Result<()> {
    let access = authorize(context, "get_dir", VALID_READ, 0).await?;
    let path = access.path().server();
    let pattern = path
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| name.contains(['*', '?', '\\']))
        .map(wildcard_regex)
        .transpose()
        .map_err(|e| access.error(context, e))?;
    let directory = if pattern.is_some() {
        path.parent()
            .ok_or_else(|| access.error(context, "pattern has no parent directory"))?
    } else {
        path
    };
    let io_error = |e: std::io::Error| access.error(context, e);
    let mut entries = tokio::fs::read_dir(directory).await.map_err(io_error)?;
    let mut names = Vec::new();
    while let Some(entry) = entries.next_entry().await.map_err(io_error)? {
        let name = entry.file_name().to_string_lossy().into_owned();
        if pattern.as_ref().is_none_or(|p| p.is_match(&name)) {
            names.push(name);
        }
    }
    names.sort();
    context.return_array(names.into_iter().map(LpcRef::from));
    Ok(())
}

fn wildcard_regex(pattern: &str) -> std::result::Result<Regex, String> {
    let mut expression = String::from(r"(?s)\A");
    let mut chars = pattern.chars();
    while let Some(ch) = chars.next() {
        match ch {
            '*' => expression.push_str(".*"),
            '?' => expression.push('.'),
            '\\' => {
                let escaped = chars
                    .next()
                    .ok_or_else(|| "trailing backslash in wildcard pattern".to_owned())?;
                expression.push_str(&regex::escape(&escaped.to_string()));
            }
            literal => expression.push_str(&regex::escape(&literal.to_string())),
        }
    }
    expression.push_str(r"\z");
    Regex::new(&expression).map_err(|e| format!("invalid wildcard pattern: {e}"))
}

#[cfg(test)]
mod tests {
    use super::wildcard_regex;
    use crate::{
        interpreter::vm::Vm,
        test_support::{TempLib, string_global, temp_lib_config},
    };

    #[test]
    fn wildcard_syntax_matches_whole_unicode_names() {
        for (pattern, name, expected) in [
            ("*", "", true),
            ("**", "", true),
            ("*.c", ".c", true),
            ("*.c", "room.c", true),
            ("*.c", "room.c.bak", false),
            ("*.c", "room.C", false),
            ("room*", "myroom", false),
            ("*a*b", "aaab", true),
            ("*a*b", "aaac", false),
            ("?", "", false),
            ("?.c", "é.c", true),
            ("?.c", "龍.c", true),
            ("?.c", "ab.c", false),
            ("?.c", "\n.c", true),
            ("*.c", "a\nb.c", true),
            ("*", ".hidden", true),
            (r"\*.c", "*.c", true),
            (r"\*.c", "a.c", false),
            (r"file\?.c", "file?.c", true),
            (r"file\\*.c", r"file\name.c", true),
            (r"\a*", "abc", true),
            ("[ab]*", "[ab].c", true),
            ("[ab]*", "a.c", false),
            ("{a,b}*", "{a,b}.c", true),
            ("(a|b)+.$*", "(a|b)+.$file", true),
            ("(a|b)+.$*", "aXfile", false),
        ] {
            assert_eq!(
                wildcard_regex(pattern).unwrap().is_match(name),
                expected,
                "pattern {pattern:?}, name {name:?}"
            );
        }
    }

    #[test]
    fn a_trailing_escape_is_an_invalid_pattern() {
        assert_eq!(
            wildcard_regex("file\\").unwrap_err(),
            "trailing backslash in wildcard pattern"
        );
    }

    async fn allowing_vm(root: &TempLib) -> Vm {
        let vm = Vm::new(temp_lib_config(root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 1; }",
        )
        .await
        .unwrap();
        vm
    }

    /// `create()`'s array result, as strings.
    async fn names_of(vm: &Vm, code: &str) -> Vec<String> {
        let task = vm
            .initialize_process_from_code("/lister.c", code)
            .await
            .unwrap();
        let mut names = Vec::new();
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                names = arr.iter().map(|x| x.to_string()).collect();
            })
            .expect("an array");
        names
    }

    #[tokio::test]
    async fn entries_come_back_sorted_as_plain_names() {
        let root = TempLib::new("dir-sorted");
        std::fs::create_dir_all(root.join("d/sub")).unwrap();
        std::fs::write(root.join("d/b.c"), "").unwrap();
        std::fs::write(root.join("d/a.c"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d"); }"#).await;
        assert_eq!(names, ["a.c", "b.c", "sub"]);
    }

    #[tokio::test]
    async fn an_empty_directory_is_an_empty_array() {
        let root = TempLib::new("dir-empty");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d"); }"#).await;
        assert!(names.is_empty());
    }

    #[tokio::test]
    async fn patterns_filter_files_and_directories_without_recursing() {
        let root = TempLib::new("dir-patterns");
        std::fs::create_dir_all(root.join("d/sub.c")).unwrap();
        for name in ["b.c", "a.c", ".hidden.c", "notes.txt", "sub.c/nested.c"] {
            std::fs::write(root.join("d").join(name), "").unwrap();
        }
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d/*.c"); }"#).await;
        assert_eq!(names, [".hidden.c", "a.c", "b.c", "sub.c"]);
    }

    #[tokio::test]
    async fn a_question_mark_matches_one_unicode_character() {
        let root = TempLib::new("dir-unicode");
        std::fs::create_dir_all(root.join("d")).unwrap();
        for name in ["ab.c", "é.c", "龍.c", ".c"] {
            std::fs::write(root.join("d").join(name), "").unwrap();
        }
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d/?.c"); }"#).await;
        assert_eq!(names, ["é.c", "龍.c"]);
    }

    #[tokio::test]
    async fn no_matches_is_an_empty_array() {
        let root = TempLib::new("dir-no-matches");
        std::fs::create_dir_all(root.join("d")).unwrap();
        std::fs::write(root.join("d/a.c"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/d/*.o"); }"#).await;
        assert!(names.is_empty());
    }

    #[tokio::test]
    async fn a_pattern_can_list_the_mudlib_root() {
        let root = TempLib::new("dir-root-pattern");
        std::fs::write(root.join("a.c"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/*.c"); }"#).await;
        assert_eq!(names, ["a.c"]);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn escapes_match_literal_wildcards_without_shadowing_patterns() {
        let root = TempLib::new("dir-escaped-pattern");
        std::fs::create_dir_all(root.join("d")).unwrap();
        for name in ["*.c", "?.c", "a.c", "other.c"] {
            std::fs::write(root.join("d").join(name), "").unwrap();
        }
        let vm = allowing_vm(&root).await;
        let names = names_of(
            &vm,
            r#"string *create() {
                return get_dir("/d/*.c") + get_dir("/d/\\*.c") + get_dir("/d/\\?.c");
            }"#,
        )
        .await;
        assert_eq!(names, ["*.c", "?.c", "a.c", "other.c", "*.c", "?.c"]);
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn parent_directory_components_are_literal() {
        let root = TempLib::new("dir-literal-parent");
        for directory in ["room*", "rooms"] {
            std::fs::create_dir_all(root.join(directory)).unwrap();
        }
        std::fs::write(root.join("room*/a.c"), "").unwrap();
        std::fs::write(root.join("rooms/b.c"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let names = names_of(&vm, r#"string *create() { return get_dir("/room*/*.c"); }"#).await;
        assert_eq!(names, ["a.c"]);
    }

    #[tokio::test]
    async fn the_master_authorizes_the_normalized_relative_pattern_once() {
        let root = TempLib::new("dir-pattern-access");
        std::fs::create_dir_all(root.join("d")).unwrap();
        std::fs::write(root.join("d/a.c"), "").unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        let master = vm
            .initialize_process_from_code(
                "/secure/master.c",
                r#"string calls = "";
                int valid_read(string p, string e, object c, string g) {
                    calls += e + ":" + p + ":" + file_name(c) + ":" + g;
                    return p == "/d/*.c";
                }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        let task = vm
            .initialize_process_from_code(
                "/d/lister.c",
                r#"string *create() { return get_dir("../d/./*.c"); }"#,
            )
            .await
            .unwrap();
        task.result()
            .unwrap()
            .with_array(task.context.txn(), |arr| {
                assert_eq!(
                    arr.iter().map(|x| x.to_string()).collect::<Vec<_>>(),
                    ["a.c"]
                );
            })
            .expect("an array");
        assert_eq!(
            string_global(&vm, &master, "calls"),
            "get_dir:/d/*.c:/d/lister:/d/lister.c"
        );
    }

    #[tokio::test]
    async fn pattern_io_errors_name_only_the_in_game_path() {
        let root = TempLib::new("dir-pattern-errors");
        std::fs::write(root.join("file"), "").unwrap();
        let vm = allowing_vm(&root).await;
        for path in ["/missing/*.c", "/file/*.c", "/d*/a.c"] {
            let code = format!("void create() {{ get_dir(\"{path}\"); }}");
            let err = vm
                .initialize_process_from_code("/lister.c", code)
                .await
                .unwrap_err()
                .to_string();
            assert!(err.contains(&format!("get_dir: {path}:")), "{err}");
            assert!(!err.contains(root.to_str().unwrap()), "{err}");
        }
    }

    #[tokio::test]
    async fn a_denied_pattern_is_rejected_before_reading_the_directory() {
        let root = TempLib::new("dir-denied-pattern");
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            "int valid_read(string p, string e, object c, string g) { return 0; }",
        )
        .await
        .unwrap();
        let caller = vm
            .initialize_process_from_code(
                "/lister.c",
                r#"string err; void create() { err = catch(get_dir("/missing/*.c")); }"#,
            )
            .await
            .unwrap()
            .context
            .process;
        assert_eq!(
            string_global(&vm, &caller, "err"),
            "runtime error: get_dir: permission denied"
        );
    }

    #[tokio::test]
    async fn a_file_is_not_a_directory() {
        let root = TempLib::new("dir-file");
        std::fs::write(root.join("f.txt"), "").unwrap();
        let vm = allowing_vm(&root).await;
        let err = vm
            .initialize_process_from_code("/lister.c", r#"void create() { get_dir("/f.txt"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("get_dir: /f.txt:"), "{err}");
    }

    #[tokio::test]
    async fn the_master_hears_get_dir() {
        let root = TempLib::new("dir-denied");
        std::fs::create_dir_all(root.join("d")).unwrap();
        let vm = Vm::new(temp_lib_config(&root));
        vm.initialize_process_from_code(
            "/secure/master.c",
            r#"int valid_read(string p, string e, object c, string g) { return e != "get_dir"; }"#,
        )
        .await
        .unwrap();
        let err = vm
            .initialize_process_from_code("/lister.c", r#"void create() { get_dir("/d"); }"#)
            .await
            .unwrap_err()
            .to_string();
        assert!(err.contains("get_dir: permission denied"), "{err}");
    }
}
