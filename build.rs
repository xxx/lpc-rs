fn main() {
    // Scanning the workspace root races with rustc's temporary directories in target.
    lalrpop::process_src().unwrap();
}
