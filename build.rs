fn main() {
    // Tracking parser inputs keeps Cargo from scanning unrelated workspace directories.
    lalrpop::Configuration::new()
        .set_in_dir("./src")
        .emit_rerun_directives(true)
        .process()
        .unwrap();
}
