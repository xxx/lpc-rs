mod support;


use std::sync::Arc;

use claim::assert_err;
use if_chain::if_chain;
use indoc::indoc;
use lpc_rs::{
    compiler::{Compiler, CompilerBuilder},
    extract_value,
    interpreter::{lpc_ref::LpcRef, lpc_value::LpcValue, vm::Vm},
    util::keyable::Keyable,
};
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_utils::config::{Config, ConfigBuilder};
use qcell::QCellOwner;

use crate::support::{run_prog, test_config, test_config_builder};

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
    let config: Arc<Config> = ConfigBuilder::default()
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

    let task = run_prog(code, &mut cell_key);
    let ctx = task.context;
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

    let task = run_prog(code, &mut cell_key);
    let ctx = task.context;
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

    let task = run_prog(code, &mut cell_key);
    let ctx = task.context;
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

    let task = run_prog(code, &mut cell_key);
    let ctx = task.context;
    let proc = ctx.process();
    let borrowed = proc.ro(&cell_key);

    assert_eq!(borrowed.globals.len(), 2);
    assert_eq!(
        borrowed
            .globals
            .last()
            .unwrap()
            .with_key(&cell_key)
            .to_string(),
        r##""I'll take 4 tacos with crema on the side, por favor.""##.to_string()
    );
}

#[test]
fn test_multi_dimensional_arrays() {
    let mut cell_key = QCellOwner::new();
    let code = indoc! { r##"
        int *a = ({ 1, 2, 3, 4, 5, 6, 7, 8 });
        mixed *b = ({ 9, 10, 11, 12, 13, ({ "14a", "14b", "14c" }), 15, 16 });
        int *c = ({ 17, 18, 19, 20, 21, 22, 23, 24 });
        int *d = ({ 25, 26, 27, 28, 29, 30, 31, 32 });

        mixed *arr = ({ a, b, c, d });

        string *x = arr[1][5][1..];
    "## };

    let task = run_prog(code, &mut cell_key);
    let ctx = task.context;
    let pr = ctx.process();
    let proc = pr.ro(&cell_key);
    let x_ref = proc.globals.last().unwrap();
    let LpcRef::Array(arr) = x_ref else {
        panic!("this shouldn't be reachable.");
    };
    let ab = arr.borrow();
    let lpc_array = extract_value!(*ab, LpcValue::Array);

    let vals = lpc_array
        .array
        .iter()
        .map(|a| {
            let LpcRef::String(s) = a else {
            panic!("this shouldn't be reachable.");
        };
            s.borrow().clone()
        })
        .collect::<Vec<_>>();

    assert_eq!(
        vals,
        vec![
            LpcValue::from("14b".to_string()),
            LpcValue::from("14c".to_string())
        ]
    );
}

#[test]
fn test_positional_vars_into_argv() {
    let mut cell_key = QCellOwner::new();
    let code = indoc! { r##"
        void create() {
            function f = (: [...] $2 :);
            f(666, 777);
        }
    "## };

    let task = run_prog(code, &mut cell_key);
    let ctx = task.context;
    assert_eq!(&LpcRef::Int(777), ctx.result().unwrap());
}

#[test]
fn test_inherited_create_called_when_not_overridden() {
    let cell_key = QCellOwner::new();
    let mut vm = Vm::new_with_key(test_config(), cell_key);
    let grandparent = indoc! { r#"
        void create() {
            dump("grandparent create");
        }
    "# };

    let parent = indoc! { r#"
        inherit "test_grandparent";

        void create() {
            dump("parent create");
        }
    "# };
    let _parent2 = indoc! { r#"
        inherit "test_grandparent";

        void create() {
            dump("parent2 create"); // this should be called because child inherits it last.
        }
    "# };

    let child = indoc! { r#"
        inherit "test_parent";
        inherit "test_parent2";
    "# };

    let _grandparent_ctx = vm
        .initialize_string(grandparent, "test_grandparent.c")
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
    let _parent_ctx = vm
        .initialize_string(parent, "test_parent.c")
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
    let _parent2_ctx = vm
        .initialize_string(parent, "test_parent2.c")
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
    let child_ctx = vm
        .initialize_string(child, "test_child.c")
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();

    let init = child_ctx
        .process()
        .ro(&vm.cell_key)
        .program
        .initializer
        .clone()
        .unwrap();

    let expected = vec![Instruction::Call(4), Instruction::Ret];

    let inst = &init.instructions;
    assert_eq!(&inst[(inst.len() - 2)..], &expected);
}

#[test]
fn test_calls_simul_efuns() {
    let cell_key = QCellOwner::new();

    let config = test_config_builder()
        .simul_efun_file("/secure/simul_efuns.c")
        .build()
        .unwrap();

    let mut vm = Vm::new_with_key(config, cell_key);
    vm.initialize_simul_efuns()
        .expect("no simul efuns?")
        .expect("init error");

    let code = indoc! { r##"
        void create() {
            simul_efun("cool!");
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("\"this is a simul_efun: cool!\"", val.to_string());

    let code = indoc! { r##"
        string simul_efun(string s) {
            return "local simul_efun: " + s;
        }

        void create() {
            simul_efun("cool!");
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("\"local simul_efun: cool!\"", val.to_string());

    let code = indoc! { r##"
        void create() {
            function f = &simul_efun("pointed!");
            f();
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("\"this is a simul_efun: pointed!\"", val.to_string());

    let code = indoc! { r##"
        string simul_efun(string s) {
            return "local simul_efun: " + s;
        }

        void create() {
            function f = &simul_efun("pointed!");
            f();
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("\"local simul_efun: pointed!\"", val.to_string());
}
