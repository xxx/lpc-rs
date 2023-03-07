mod support;

use std::rc::Rc;

use claim::assert_err;
use if_chain::if_chain;
use indoc::indoc;
use lpc_rs::{
    compiler::{Compiler, CompilerBuilder},
    interpreter::{lpc_ref::LpcRef, lpc_value::LpcValue},
};
use lpc_rs_utils::config::{Config, ConfigBuilder};
use qcell::QCellOwner;
use lpc_rs::util::keyable::Keyable;

use crate::support::run_prog;

// #[ctor::ctor]
// fn init() {
//     tracing::subscriber::set_global_default(
//         tracing_subscriber::fmt()
//             .with_max_level(tracing::Level::TRACE)
//             .with_writer(std::io::stdout)
//             .finish(),
//     )
//         .expect("setting tracing default failed");
// }

fn default_compiler() -> Compiler {
    let config: Rc<Config> = ConfigBuilder::default()
        .lib_dir("tests/fixtures/code")
        .build()
        .unwrap()
        .into();
    CompilerBuilder::default().config(config).build().unwrap()
}

#[test]
fn errors_on_max_inherit_depth() {
    let mut cell_key = QCellOwner::new();
    let code = r#"inherit "/std/inherit_loop1";"#;

    let compiler = default_compiler();
    let result = compiler.compile_string("foo.c", code, &mut cell_key);

    assert_err!(result, "maximum inheritance depth of 10 reached reached");
}

#[test]
fn test_inheritance() {
    let mut cell_key = QCellOwner::new();
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

    let (_task, ctx) = run_prog(code, &mut cell_key);
    let proc = ctx.process();
    let prog = &proc.ro(&cell_key).program;

    assert_eq!(prog.num_globals, 5);
    assert_eq!(prog.num_init_registers, 6);
}

#[test]
fn test_dynamic_receiver() {
    let mut cell_key = QCellOwner::new();
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

    let (_task, ctx) = run_prog(code, &mut cell_key);
    let proc = ctx.process();
    let prog = &proc.ro(&cell_key).program;

    assert_eq!(prog.num_globals, 0);
    assert_eq!(prog.num_init_registers, 1);
}

#[test]
fn test_duffs_device() {
    let mut cell_key = QCellOwner::new();
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

    let (_task, ctx) = run_prog(code, &mut cell_key);
    let proc = ctx.process();
    let borrowed = proc.ro(&cell_key);
    let b = &borrowed.globals[1];

    if_chain! {
        if let LpcRef::Array(pool_ref) = b;
        let b = pool_ref.borrow();
        if let LpcValue::Array(arr) = &*b;
        then {
            assert_eq!(
                &arr.iter().map(|x| { x.with_key(&cell_key) }).collect::<Vec<_>>(),
                &[
                    LpcRef::Int(0),
                    LpcRef::Int(2),
                    LpcRef::Int(3),
                    LpcRef::Int(4),
                    LpcRef::Int(5),
                    LpcRef::Int(6),
                    LpcRef::Int(7),
                    LpcRef::Int(0),
                ].iter().map(|x| { x.with_key(&cell_key) }).collect::<Vec<_>>()
            );
        } else {
            panic!("expected array");
        }
    }
}

#[test]
fn test_closures() {
    let mut cell_key = QCellOwner::new();

    let code = indoc! { r##"
        function f = (:
            function f = &->tacos(,);
            function g = (: f($1, $2, $3) :);
            return g(this_object(), 4, "crema");
        :);
        string str;

        void create() {
            str = f();
        }

        string tacos(int i, string t) {
            return "I'll take " + i + " tacos with " + t + " on the side, por favor.";
        }
    "## };

    let (_task, ctx) = run_prog(code, &mut cell_key);
    let proc = ctx.process();
    let borrowed = proc.ro(&cell_key);

    assert_eq!(borrowed.globals.len(), 2);
    assert_eq!(
        borrowed.globals.last().unwrap().with_key(&cell_key).to_string(),
        r##""I'll take 4 tacos with crema on the side, por favor.""##.to_string()
    );
}
