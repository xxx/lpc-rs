//! Keep the distributed learning material aligned with the compiler and apply API.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
    sync::Arc,
};

use lpc_rs::{compiler::CompilerBuilder, interpreter::vm::Vm};
use lpc_rs_core::{CREATE_FUNCTION, lpc_path::LpcPath, lpc_type::LpcType};
use lpc_rs_function_support::function_prototype::FunctionPrototype;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use regex::Regex;
use sha2::{Digest, Sha256};

fn config() -> Config {
    ConfigBuilder::default()
        .lib_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/ulib"))
        .system_include_dirs(vec!["/include"])
        .build()
        .unwrap()
}

fn files_below(root: &Path) -> Vec<std::path::PathBuf> {
    let mut files = Vec::new();
    for entry in fs::read_dir(root).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            files.extend(files_below(&path));
        } else {
            files.push(path);
        }
    }
    files.sort();
    files
}

fn assert_apply_signature(
    actual: &FunctionPrototype,
    expected: &FunctionPrototype,
    location: &str,
) {
    assert_eq!(
        actual.arity, expected.arity,
        "{location}: argument count changed"
    );
    assert_eq!(
        actual.arg_types, expected.arg_types,
        "{location}: argument types changed"
    );
    assert_eq!(
        actual.flags.varargs(),
        expected.flags.varargs(),
        "{location}: varargs changed"
    );
    assert_eq!(
        actual.flags.ellipsis(),
        expected.flags.ellipsis(),
        "{location}: ellipsis changed"
    );
    for index in 0..expected.arg_types.len() {
        assert_eq!(
            actual.is_ref_param(index),
            expected.is_ref_param(index),
            "{location}: argument {index} ref modifier changed"
        );
    }
    // A mixed-result apply may use a narrower return type; create's result is ignored.
    let narrower = expected.return_type == LpcType::Mixed(false)
        && (actual.return_type != LpcType::Void || expected.name == CREATE_FUNCTION);
    assert!(
        actual.return_type == expected.return_type || narrower,
        "{location}: returns {}, but the contract requires {}",
        actual.return_type,
        expected.return_type
    );
}

#[test]
fn apply_references_have_been_reviewed_for_ulib() {
    let repo = Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut paths = files_below(&repo.join("doc/apply"));
    paths.push(repo.join("doc/efun/parse_add_rule.md"));
    let current: BTreeMap<_, _> = paths
        .into_iter()
        .filter(|path| path.extension().is_some_and(|ext| ext == "md"))
        .map(|path| {
            let text = fs::read_to_string(&path).unwrap().replace("\r\n", "\n");
            let relative = path
                .strip_prefix(repo)
                .unwrap()
                .to_str()
                .unwrap()
                .replace('\\', "/");
            (relative, hex::encode(Sha256::digest(text)))
        })
        .collect();
    let reviewed: BTreeMap<String, String> = serde_json::from_str(
        &fs::read_to_string(repo.join("tests/fixtures/ulib_apply_review.json")).unwrap(),
    )
    .unwrap();
    let changed: BTreeSet<_> = current
        .keys()
        .chain(reviewed.keys())
        .filter(|path| current.get(*path) != reviewed.get(*path))
        .collect();
    assert!(
        changed.is_empty(),
        "Apply references changed: {changed:#?}\n\
         Review the corresponding ulib implementations, source comments and doc/applies.md,\n\
         then record only the reviewed paths with tests/update_ulib_apply_review.py.\n\
         See doc/maintaining-ulib.md; updating the record does not update the examples."
    );
}

#[tokio::test]
async fn live_objects_and_enabled_examples_cover_every_documented_apply() {
    let config = Arc::new(config());
    let root = Path::new(config.lib_dir.as_str());
    let compiler = CompilerBuilder::default()
        .config(config.clone())
        .build()
        .unwrap();
    let mut functions = BTreeMap::new();
    for path in files_below(root) {
        if path.extension().is_none_or(|ext| ext != "c") {
            continue;
        }
        let relative = path.strip_prefix(root).unwrap().to_str().unwrap();
        let code = fs::read_to_string(&path)
            .unwrap()
            .replace("#if 0\n", "#if 1\n");
        let in_game = LpcPath::new_in_game(relative, "/", config.lib_dir.as_str());
        let compiled = compiler
            .compile_string(in_game, code)
            .await
            .unwrap_or_else(|e| panic!("{relative}: {}", e.diagnostic_string()));
        for group in compiled.warnings {
            assert!(
                group.warnings.is_empty(),
                "{relative}: {:?}",
                group.warnings
            );
        }
        functions.insert(relative.to_owned(), compiled.program.unmangled_functions);
    }

    let reference = fs::read_to_string(root.join("doc/applies.md")).unwrap();
    let constants =
        fs::read_to_string(Path::new(env!("CARGO_MANIFEST_DIR")).join("src/interpreter/mod.rs"))
            .unwrap();
    let applies = Regex::new(r#"pub const \w+: &str = "([^"]+)";"#).unwrap();
    let mut driver_names: BTreeSet<_> = applies
        .captures_iter(&constants)
        .map(|apply| apply[1].to_owned())
        .collect();
    driver_names.insert(CREATE_FUNCTION.to_owned());
    let mut documented_names = BTreeSet::new();
    let signature_line = Regex::new(r"(?m)^`([^`\n]+\([^`\n]*\))`\s*$").unwrap();
    let index_row = Regex::new(r"(?m)^\| `(\w+)(\([^`]*\))?` \|").unwrap();
    for (category, heading, sources) in [
        (
            "master",
            "Master",
            vec!["secure/master.c", "examples/master.c"],
        ),
        (
            "object",
            "Object",
            vec![
                "std/room.c",
                "room/lounge.c",
                "obj/player.c",
                "examples/object.c",
                "examples/verb.c",
            ],
        ),
        ("living", "Living", vec!["obj/player.c"]),
        (
            "special",
            "Special",
            vec!["obj/player.c", "examples/player.c"],
        ),
    ] {
        let section = reference
            .split(&format!("## {heading} applies"))
            .nth(1)
            .unwrap()
            .split("\n## ")
            .next()
            .unwrap();
        let directory = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("doc/apply")
            .join(category);
        let mut category_names = BTreeSet::new();
        for path in files_below(&directory) {
            let name = path.file_stem().unwrap().to_str().unwrap();
            if name == "README" {
                continue;
            }
            category_names.insert(name.to_owned());
            assert!(
                section.contains(&format!("`{name}")),
                "missing {category}/{name} reference"
            );
            if name == "parser_handlers" {
                for family in ["can", "direct", "indirect", "do"] {
                    for suffix in ["give_obj_to_liv", "verb_rule"] {
                        let handler = format!("{family}_{suffix}");
                        assert!(
                            functions["examples/verb.c"].contains_key(&handler),
                            "missing {handler}"
                        );
                    }
                }
            } else {
                documented_names.insert(name.to_owned());
                let contract = fs::read_to_string(&path).unwrap();
                let signature = &signature_line.captures(&contract).unwrap_or_else(|| {
                    panic!("{} needs a canonical LPC signature", path.display())
                })[1];
                let expected = compiler
                    .compile_string(
                        LpcPath::new_in_game("/contract.c", "/", config.lib_dir.as_str()),
                        format!("#pragma strict_types\n{signature} {{}}"),
                    )
                    .await
                    .unwrap_or_else(|e| panic!("{}: {}", path.display(), e.diagnostic_string()));
                let expected = &expected
                    .program
                    .unmangled_functions
                    .get(name)
                    .unwrap_or_else(|| {
                        panic!("{} signature names a different apply", path.display())
                    })
                    .prototype;
                assert!(
                    sources
                        .iter()
                        .any(|source| functions[*source].contains_key(name)),
                    "missing {category}/{name} implementation or example"
                );
                for source in &sources {
                    if let Some(function) = functions[*source].get(name) {
                        assert_apply_signature(
                            &function.prototype,
                            expected,
                            &format!("{source}: {name}"),
                        );
                    }
                }
                let row = index_row
                    .captures_iter(section)
                    .find(|row| &row[1] == name)
                    .unwrap_or_else(|| panic!("missing {category}/{name} index row"));
                let index_signature = compiler
                    .compile_string(
                        LpcPath::new_in_game("/index.c", "/", config.lib_dir.as_str()),
                        format!(
                            "#pragma strict_types\n{} {}{} {{}}",
                            expected.return_type, name, &row[2]
                        ),
                    )
                    .await
                    .unwrap_or_else(|e| {
                        panic!(
                            "ulib/doc/applies.md {category}/{name}: {}",
                            e.diagnostic_string()
                        )
                    });
                assert_apply_signature(
                    &index_signature.program.unmangled_functions[name].prototype,
                    expected,
                    &format!("ulib/doc/applies.md: {category}/{name}"),
                );
            }
        }
        let indexed_names: BTreeSet<_> = index_row
            .captures_iter(section)
            .filter(|row| row.get(2).is_some() || &row[1] == "parser_handlers")
            .map(|row| row[1].to_owned())
            .collect();
        assert_eq!(
            indexed_names, category_names,
            "{category}: apply index is stale"
        );
    }
    assert_eq!(
        driver_names, documented_names,
        "driver apply names and doc/apply disagree"
    );
}

#[test]
fn documentation_links_stay_inside_the_copyable_directory() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("ulib")
        .canonicalize()
        .unwrap();
    let links = Regex::new(r"\]\(([^)#]+)(?:#[^)]*)?\)").unwrap();
    for path in files_below(&root) {
        if path.extension().is_none_or(|ext| ext != "md") {
            continue;
        }
        let content = fs::read_to_string(&path).unwrap();
        for link in links.captures_iter(&content) {
            let target = path
                .parent()
                .unwrap()
                .join(&link[1])
                .canonicalize()
                .unwrap_or_else(|e| panic!("{} links to {}: {e}", path.display(), &link[1]));
            assert!(
                target.starts_with(&root),
                "{} escapes ulib",
                target.display()
            );
        }
    }
}

#[tokio::test]
async fn policy_allows_game_objects_and_refuses_privileged_operations() {
    let mut vm = Vm::new(config());
    vm.bootstrap().await.unwrap();
    vm.initialize_string(
        r#"
        #pragma strict_types
        #include <ulib.h>

        private void require(int condition, string message) {
            if (!condition) throw(message);
        }

        private void require_denied(string error, string operation) {
            if (!error || !regmatch(error, "permission denied")) {
                throw(operation + " should fail with permission denied");
            }
        }

        void create() {
            object room = load_object(START_ROOM);
            object player = clone_object(PLAYER_OBJECT);
            object master = find_object("/secure/master");
            require(room != 0 && player != 0, "game objects must load");
            require_denied(catch(read_file("/include/ulib.h")), "ordinary reads");
            require_denied(catch(write_file("/unexpected", "data")), "writes");
            require_denied(catch(destruct(room)), "destructing another object");
            require_denied(catch(compile_string("/obj/injected", "void create() {}")), "source injection");
            require(!master->valid_load("/secure/other.c", "load_object", this_object(), "/obj/probe.c"), "secure loads must fail");
            require(!master->valid_inherit("/obj/player.c", "/obj/probe.c"), "player inheritance must fail");
            require(!master->valid_exec("obj/probe.c", this_object(), player), "exec must fail");
            require(!master->valid_variable_info(this_object(), player, "/obj/probe.c"), "inspection must fail");
            require(!master->valid_reload("master", this_object(), "/obj/probe.c"), "reload must fail");
            require(!master->valid_shutdown(this_object(), "/obj/probe.c"), "shutdown must fail");
            require(!master->query_allow_shadow(player), "shadowing must fail");
            string player_path = file_name(player);
            player->net_dead();
            require(!find_object(player_path), "a nameless disconnected body must be removed");
            destruct(this_object());
        }
        "#,
        "/obj/probe.c",
    )
    .await
    .unwrap_or_else(|e| panic!("{}", e.diagnostic_string()));
    assert!(vm.global_state.object_space.lookup("/obj/probe").is_none());
}
