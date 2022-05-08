use std::rc::Rc;
use claim::assert_err;
use lpc_rs::compiler::Compiler;
use lpc_rs::util::config::Config;

#[test]
fn errors_on_max_inherit_depth() {
    let code = r#"inherit "/std/inherit_loop1";"#;

    let config: Rc<Config> = Config::new(None::<&str>)
        .unwrap()
        .with_lib_dir("tests/fixtures/code")
        .into();
    let compiler = Compiler::new(config.clone());

    let result = compiler.compile_string("foo.c", code);

    assert_err!(result, "maximum inheritance depth of 10 reached reached");
}