mod support;

use claim::assert_err;
use lpc_rs::{compiler::Compiler, util::config::Config};
use std::rc::Rc;
use indoc::indoc;
use crate::support::run_prog;

fn default_compiler() -> Compiler {
    let config: Rc<Config> = Config::new(None::<&str>)
        .unwrap()
        .with_lib_dir("tests/fixtures/code")
        .into();
    Compiler::new(config.clone())
}

#[test]
fn errors_on_max_inherit_depth() {
    let code = r#"inherit "/std/inherit_loop1";"#;

    let compiler = default_compiler();
    let result = compiler.compile_string("foo.c", code);

    assert_err!(result, "maximum inheritance depth of 10 reached reached");
}

#[test]
fn test_inheritance() {
    let code = indoc! { r##"
        inherit "/parent";

        void create() {
            dump("child create");
            dump(get_vars());
            grandparent_method();
            parent_method();
            overridden_method();
        }
    "## };

    let (task, ctx) = run_prog(code);
    let proc = ctx.process();
    let prog = &proc.borrow().program;
    println!("task: {}, {}, {:?}", prog.num_globals, prog.num_init_registers, prog.global_variables);
}