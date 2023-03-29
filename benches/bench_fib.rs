use std::borrow::Cow;
use std::rc::Rc;
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use qcell::QCellOwner;
use refpool::Pool;
use lpc_rs::compiler::Compiler;
use lpc_rs::interpreter::gc::gc_bank::{GcBank, GcRefBank};
use lpc_rs::interpreter::lpc_ref::LpcRef;
use lpc_rs::interpreter::memory::Memory;
use lpc_rs::interpreter::object_space::ObjectSpace;
use lpc_rs::interpreter::task::Task;
use lpc_rs_utils::config::Config;

pub fn criterion_benchmark(c: &mut Criterion) {
    let mut cell_key = QCellOwner::new();

    let code = r#"
        int fib(int n) {
            if (n < 2) {
                return n;
            }
            return fib(n - 1) + fib(n - 2);
        }

        void create() {
            int i = fib(20);
            dump(i);
        }
    "#;

    let program = Compiler::default()
        .compile_string("~/my_file.c", code, &mut cell_key)
        .expect("Failed to compile.");

    let program = Rc::new(program);
    let memory = Memory::default();
    let upvalues = GcRefBank::default();
    let task = Task::<64>::new(memory, cell_key.cell(upvalues));

    c.bench_function("fib 20", |b| {
        b.iter(|| {
            let mut t = task.clone();
            let _ = t.initialize_program(program.clone(), black_box(Config::default()), cell_key.cell(ObjectSpace::default()), &mut cell_key);
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);