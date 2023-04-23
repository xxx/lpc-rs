mod support;

use std::sync::Arc;

use claims::assert_err;
use if_chain::if_chain;
use indoc::indoc;
use lpc_rs::{
    compiler::{Compiler, CompilerBuilder},
    interpreter::{lpc_int::LpcInt, lpc_ref::LpcRef, lpc_string::LpcString, vm::Vm},
};
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_utils::config::{Config, ConfigBuilder};

use crate::support::{run_prog, test_config, test_config_builder};

fn default_compiler() -> Compiler {
    let config: Arc<Config> = ConfigBuilder::default()
        .lib_dir("tests/fixtures/code")
        .build()
        .unwrap()
        .into();
    CompilerBuilder::default().config(config).build().unwrap()
}

#[tokio::test]
async fn errors_on_max_inherit_depth() {
    let code = r#"inherit "/std/inherit_loop1";"#;

    let compiler = default_compiler();
    let result = compiler.compile_string("foo.c", code).await;

    assert_err!(result, "maximum inheritance depth of 10 reached reached");
}

#[tokio::test]
async fn test_inheritance() {
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

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let prog = &proc.program;

    assert_eq!(prog.num_globals, 5);
    assert_eq!(prog.num_init_registers(), 6);
}

#[tokio::test]
async fn test_dynamic_receiver() {
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

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let prog = &proc.program;

    assert_eq!(prog.num_globals, 0);
    assert_eq!(prog.num_init_registers(), 1);
}

#[tokio::test]
async fn test_duffs_device() {
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

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let b = &proc.globals.read()[1];

    if_chain! {
        if let LpcRef::Array(pool_ref) = b;
        let arr = pool_ref.read();
        then {
            assert_eq!(
                &*arr,
                &[
                    LpcRef::Int(LpcInt(0)),
                    LpcRef::Int(LpcInt(2)),
                    LpcRef::Int(LpcInt(3)),
                    LpcRef::Int(LpcInt(4)),
                    LpcRef::Int(LpcInt(5)),
                    LpcRef::Int(LpcInt(6)),
                    LpcRef::Int(LpcInt(7)),
                    LpcRef::Int(LpcInt(0)),
                ].to_vec()
            );
        } else {
            panic!("expected array");
        }
    }
}

#[tokio::test]
async fn test_closures() {
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

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let globals = proc.globals.read();

    assert_eq!(globals.len(), 2);
    assert_eq!(
        globals.last().unwrap().to_string(),
        "I'll take 4 tacos with crema on the side, por favor.".to_string()
    );
}

#[tokio::test]
async fn test_multi_dimensional_arrays() {
    let code = indoc! { r##"
        int *a = ({ 1, 2, 3, 4, 5, 6, 7, 8 });
        mixed *b = ({ 9, 10, 11, 12, 13, ({ "14a", "14b", "14c" }), 15, 16 });
        int *c = ({ 17, 18, 19, 20, 21, 22, 23, 24 });
        int *d = ({ 25, 26, 27, 28, 29, 30, 31, 32 });

        mixed *arr = ({ a, b, c, d });

        string *x = arr[1][5][1..];
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    let proc = ctx.process();
    let globals = proc.globals.read();
    let x_ref = globals.last().unwrap();
    let LpcRef::Array(arr) = x_ref else {
        panic!("this shouldn't be reachable.");
    };
    let lpc_array = arr.read();

    let vals = lpc_array
        .array
        .iter()
        .map(|a| {
            let LpcRef::String(s) = a else {
            panic!("this shouldn't be reachable.");
        };
            s.read().clone()
        })
        .collect::<Vec<_>>();

    assert_eq!(vals, vec![LpcString::from("14b"), LpcString::from("14c")]);
}

#[tokio::test]
async fn test_positional_vars_into_argv() {
    let code = indoc! { r##"
        void create() {
            function f = (: [...] $2 :);
            f(666, 777);
        }
    "## };

    let task = run_prog(code).await;
    let ctx = task.context;
    assert_eq!(&LpcRef::Int(LpcInt(777)), ctx.result().unwrap());
}

#[tokio::test]
async fn test_inherited_create_called_when_not_overridden() {
    let mut vm = Vm::new(test_config());
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
        .await
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
    let _parent_ctx = vm
        .initialize_string(parent, "test_parent.c")
        .await
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
    let _parent2_ctx = vm
        .initialize_string(parent, "test_parent2.c")
        .await
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();
    let child_ctx = vm
        .initialize_string(child, "test_child.c")
        .await
        .map_err(|e| {
            e.emit_diagnostics();
            e
        })
        .unwrap();

    let init = child_ctx.process().program.initializer.clone().unwrap();

    let expected = vec![Instruction::Call(4), Instruction::Ret];

    let inst = &init.instructions;
    assert_eq!(&inst[(inst.len() - 2)..], &expected);
}

#[tokio::test]
async fn test_calls_simul_efuns() {
    let config = test_config_builder()
        .simul_efun_file("/secure/simul_efuns.c")
        .build()
        .unwrap();

    let mut vm = Vm::new(config);
    vm.initialize_simul_efuns()
        .await
        .expect("no simul efuns?")
        .expect("init error");

    let code = indoc! { r##"
        void create() {
            simul_efun("cool!");
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("this is a simul_efun: cool!", val.to_string());

    let code = indoc! { r##"
        string simul_efun(string s) {
            return "local simul_efun: " + s;
        }

        void create() {
            simul_efun("cool!");
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("local simul_efun: cool!", val.to_string());

    let code = indoc! { r##"
        void create() {
            function f = &simul_efun("pointed!");
            f();
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("this is a simul_efun: pointed!", val.to_string());

    let code = indoc! { r##"
        string simul_efun(string s) {
            return "local simul_efun: " + s;
        }

        void create() {
            function f = &simul_efun("pointed!");
            f();
        }
    "## };
    let ctx = vm.initialize_string(code, "foo.c").await.unwrap();
    let val = ctx.result().unwrap();
    assert_eq!("local simul_efun: pointed!", val.to_string());
}
