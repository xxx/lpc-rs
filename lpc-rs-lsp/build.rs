use std::{env, fs, path::PathBuf};

fn main() {
    let directory = PathBuf::from("../doc/efun");
    println!("cargo:rerun-if-changed={}", directory.display());
    let mut files: Vec<_> = fs::read_dir(directory)
        .expect("efun documentation directory")
        .map(|entry| entry.expect("efun documentation file").path())
        .filter(|path| path.extension().is_some_and(|ext| ext == "md"))
        .collect();
    files.sort();
    let mut code =
        String::from("fn documentation(name: &str) -> Option<&'static str> { match name {\n");
    for path in files {
        let name = path.file_stem().unwrap().to_string_lossy();
        let text = fs::read_to_string(&path).expect("UTF-8 efun documentation");
        code.push_str(&format!("{name:?} => Some({text:?}),\n"));
    }
    code.push_str("_ => None, } }\n");
    fs::write(
        PathBuf::from(env::var_os("OUT_DIR").unwrap()).join("efun_docs.rs"),
        code,
    )
    .expect("generated efun documentation");
}
