mod support;

use crate::support::run_prog;
use claim::assert_err;
use indoc::indoc;
use lpc_rs::{compiler::Compiler, util::config::Config};
use std::rc::Rc;

fn default_compiler() -> Compiler {
    let config: Rc<Config> = Config::new(None::<&str>)
        .unwrap()
        .with_lib_dir("tests/fixtures/code")
        .into();
    Compiler::new(config.clone())
}

// fn lookup_global_variable(prog: &Program, name: &str) -> LpcRef {
//     prog.globals.iter().find(|(n, _)| n == name).unwrap().1.clone()
// }

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

    let (_task, ctx) = run_prog(code);
    let proc = ctx.process();
    let prog = &proc.borrow().program;
    println!(
        "task: {}, {}, {:?}",
        prog.num_globals, prog.num_init_registers, prog.global_variables
    );

    assert_eq!(prog.num_globals, 5);
    assert_eq!(prog.num_init_registers, 7);
}

#[test]
fn test_dynamic_receiver() {
    let code = indoc! { r##"
        void create() {
            function f = &->tacos();
            dump(f(this_object(), "assmar"));
        }

        string tacos(string s) {
            dump(s);
            return "tacos";
        }
    "## };

    let (_task, ctx) = run_prog(code);
    let proc = ctx.process();
    let prog = &proc.borrow().program;
    println!(
        "task: {}, {}, {:?}",
        prog.num_globals, prog.num_init_registers, prog.global_variables
    );
}
