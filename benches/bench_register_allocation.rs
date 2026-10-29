use std::hint::black_box;

use criterion::{BatchSize, Criterion, Throughput, criterion_group, criterion_main};
use lpc_rs::compiler::Compiler;
use lpc_rs::compiler::codegen::register_packing::pack;
use lpc_rs_asm::{
    address::Address,
    instruction::{Arg, ArgList, Instruction},
};
use lpc_rs_core::{
    lpc_type::LpcType,
    register::{Register, RegisterVariant},
    register_counter::RegisterCounter,
};
use lpc_rs_function_support::{
    function_prototype::FunctionPrototypeBuilder, program_function::ProgramFunction,
};

#[path = "support/profiler.rs"]
mod profiler;

fn allocation(c: &mut Criterion) {
    let mut group = c.benchmark_group("register_allocation");
    group.throughput(Throughput::Elements(1024));
    group.bench_function("sequential", |b| {
        b.iter(|| {
            let mut counter = RegisterCounter::new(1);
            for _ in 0..1024 {
                black_box(black_box(&mut counter).next().unwrap());
            }
        });
    });
    group.bench_function("scoped", |b| {
        b.iter(|| {
            let mut counter = RegisterCounter::new(1);
            for _ in 0..32 {
                counter.push();
                for _ in 0..32 {
                    black_box(black_box(&mut counter).next().unwrap());
                }
                black_box(counter.pop());
            }
        });
    });
    group.finish();
}

fn packing_program(loop_carried: bool) -> ProgramFunction {
    let prototype = FunctionPrototypeBuilder::default()
        .name("packing")
        .filename(std::sync::Arc::new("/packing.c".into()))
        .return_type(LpcType::Void)
        .build()
        .unwrap();
    let mut function = ProgramFunction::new(prototype, if loop_carried { 257 } else { 256 });
    let local = |index| RegisterVariant::Local(Register(index));
    let constant = RegisterVariant::Constant(Register(0));
    if loop_carried {
        function
            .instructions
            .push(Instruction::Copy(constant, local(1)));
    }
    for index in 0..256_u16 {
        let temporary = local(index + if loop_carried { 2 } else { 1 });
        function
            .instructions
            .push(Instruction::Copy(constant, temporary));
        let args = if loop_carried {
            vec![Arg::Value(local(1)), Arg::Value(temporary)]
        } else {
            vec![Arg::Value(temporary)]
        };
        function.arg_lists.push(args);
        function
            .instructions
            .push(Instruction::CallEfun(0, ArgList(index)));
    }
    if loop_carried {
        function.instructions.push(Instruction::Inc(local(1)));
        function
            .instructions
            .push(Instruction::Jnz(local(0), Address(1)));
    }
    function.instructions.push(Instruction::Ret);
    function.debug_spans = vec![None; function.instructions.len()];
    function
}

fn packing(c: &mut Criterion) {
    let mut group = c.benchmark_group("register_packing");
    for (name, loop_carried, expected) in [("straight_line", false, 1), ("loop_carried", true, 2)] {
        let function = packing_program(loop_carried);
        let mut checked = function.clone();
        pack(&mut checked);
        assert_eq!(checked.num_locals, expected);
        group.bench_with_input(name, &function, |b, function| {
            b.iter_batched(
                || function.clone(),
                |mut function| {
                    pack(&mut function);
                    black_box(function)
                },
                BatchSize::SmallInput,
            );
        });
    }
    group.finish();
}

fn compilation(c: &mut Criterion) {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .unwrap();
    let small =
        "int run(int value) { if (value < 2) return value; return value + run(value - 1); }";
    let temporaries = format!(
        "int run(int value) {{ {} return value; }}",
        "value += 1;\n".repeat(1024)
    );
    let globals = format!(
        "{} int run() {{ return value_0 + value_255; }}",
        (0..256)
            .map(|index| format!("int value_{index} = {index};\n"))
            .collect::<String>()
    );
    let closures = (0..64)
        .map(|index| format!("function make_{index}() {{ return (: $1 + $16 :); }}\n"))
        .collect::<String>();
    let literals = (0..128)
        .map(|index| {
            format!(
                "int folded_{index}() {{ return (({index} + 7) * 3 - 2) / 4 % 97 | (11 << 5) ^ (704 >> 3) & 127; }}\n"
            )
        })
        .collect::<String>();
    let initializers = format!(
        "int run(int value) {{ {} return value; }}",
        (0..128)
            .map(|index| format!("int local_{index} = value + {index}; value = local_{index};\n"))
            .collect::<String>()
    );
    let call_prefix = "int pack(int left, int right) { return left * 10 + right; }";
    let calls = format!(
        "{call_prefix} int run(int value) {{ {} return value; }}",
        "value = pack(value, 1);\n".repeat(128)
    );
    let mutating_calls = format!(
        "{call_prefix} int run(int value) {{ {} return value; }}",
        "value = pack(value, value = 2);\n".repeat(128)
    );
    let mut group = c.benchmark_group("register_allocation_compile");
    for (name, source) in [
        ("small_function", small),
        ("temporaries", temporaries.as_str()),
        ("globals", globals.as_str()),
        ("closure_positionals", closures.as_str()),
        ("literal_expressions", literals.as_str()),
        ("local_initializers", initializers.as_str()),
        ("value_arguments", calls.as_str()),
        ("mutating_arguments", mutating_calls.as_str()),
    ] {
        group.bench_function(name, |b| {
            b.to_async(&runtime).iter(|| async {
                black_box(
                    Compiler::default()
                        .compile_string("/allocation.c", black_box(source))
                        .await
                        .unwrap(),
                )
            });
        });
    }
    group.finish();
}

criterion_group! {
    name = benches;
    config = profiler::profiled();
    targets = allocation, packing, compilation
}
criterion_main!(benches);
