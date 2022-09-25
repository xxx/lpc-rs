mod support;

use std::rc::Rc;

use claim::assert_err;
use if_chain::if_chain;
use indoc::indoc;
use lpc_rs::{
    compiler::Compiler,
    interpreter::{lpc_ref::LpcRef, lpc_value::LpcValue},
};
use lpc_rs_utils::config::Config;

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

    let (_task, ctx) = run_prog(code);
    let proc = ctx.process();
    let prog = &proc.borrow().program;

    assert_eq!(prog.num_globals, 5);
    assert_eq!(prog.num_init_registers, 6);
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

    assert_eq!(prog.num_globals, 1);
    assert_eq!(prog.num_init_registers, 1);
}

#[test]
fn test_duffs_device() {
    let code = indoc! { r##"
        int *copy(int *array, int count) {
            int n = (count + 7) / 8, idx = 0;
            int *result = ({ 0, 0, 0, 0, 0, 0, 0, 0 });

            switch (count % 8) {
            case 0: do { result[0] = array[0];
            case 7:      result[7] = array[7];
            case 6:      result[6] = array[6];
            case 5:      result[5] = array[5];
            case 4:      result[4] = array[4];
            case 3:      result[3] = array[3];
            case 2:      result[2] = array[2];
            case 1:      result[1] = array[1];
                    } while (--n > 0);
            }

            return result;
        }

        mixed a = ({ 1, 2, 3, 4, 5, 6, 7, 8 });
        mixed b = copy(a, 6);
    "## };

    let (_task, ctx) = run_prog(code);
    let proc = ctx.process();
    let borrowed = proc.borrow();
    let b = &borrowed.globals[1];

    if_chain! {
        if let LpcRef::Array(pool_ref) = &*b;
        let b = pool_ref.borrow();
        if let LpcValue::Array(arr) = &*b;
        then {
            assert_eq!(
                arr,
                &[
                    LpcRef::Int(0),
                    LpcRef::Int(2),
                    LpcRef::Int(3),
                    LpcRef::Int(4),
                    LpcRef::Int(5),
                    LpcRef::Int(6),
                    LpcRef::Int(7),
                    LpcRef::Int(0),
                ]
            );
        } else {
            panic!("expected array");
        }
    }
}

// TODO: re-enable this once it's supported
// #[test]
// fn test_closures() {
//     let code = indoc! { r##"
//         function f = (:
//             function f = &->tacos();
//             function g = (: f($1, $2) :);
//             dump(g(this_object(), "assmar"));
//             return 666;
//         :);
//
//         void create() {
//             int i = f();
//         }
//     "## };
//
//     let (_task, ctx) = run_prog(code);
//     let proc = ctx.process();
//     let borrowed = proc.borrow();
//     let b = borrowed.globals[1].borrow();
//
//     if_chain! {
//         if let LpcRef::Array(pool_ref) = &*b;
//         let b = pool_ref.borrow();
//         if let LpcValue::Array(arr) = &*b;
//         then {
//             assert_eq!(
//                 arr,
//                 &[
//                     LpcRef::Int(0),
//                     LpcRef::Int(2),
//                     LpcRef::Int(3),
//                     LpcRef::Int(4),
//                     LpcRef::Int(5),
//                     LpcRef::Int(6),
//                     LpcRef::Int(7),
//                     LpcRef::Int(0),
//                 ]
//             );
//         } else {
//             panic!("expected array");
//         }
//     }
// }
