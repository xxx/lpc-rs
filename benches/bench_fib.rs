use std::rc::Rc;

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use lpc_rs::{
    compiler::Compiler,
    interpreter::{
        call_outs::CallOuts, gc::gc_bank::GcRefBank, memory::Memory, object_space::ObjectSpace,
        task::Task,
    },
};
use lpc_rs_utils::config::Config;
use qcell::QCellOwner;

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
    let upvalues = Rc::new(cell_key.cell(GcRefBank::default()));
    let (tx, _) = tokio::sync::mpsc::channel();
    let call_outs = Rc::new(cell_key.cell(CallOuts::new(tx.clone())));
    let memory = Rc::new(Memory::default());

    c.bench_function("fib 20", |b| {
        b.iter(|| {
            let _ = Task::<64>::initialize_program(
                program.clone(),
                black_box(Config::default()),
                cell_key.cell(ObjectSpace::default()),
                memory.clone(),
                upvalues.clone(),
                call_outs.clone(),
                tx.clone(),
                &mut cell_key,
            );
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
