//! Register packing: the frame layout `finalize_function` settles after the
//! peephole, from the liveness of the emitted instructions.

use bit_set::BitSet;
use lpc_rs_asm::instruction::Instruction;
use lpc_rs_core::{
    RegisterSize,
    function_arity::FunctionArity,
    register::{Register, RegisterVariant},
};
use lpc_rs_function_support::program_function::ProgramFunction;

/// Renumber `func`'s locals: r0 and the parameters keep their slots, each
/// named local keeps a slot of its own below the temps, and the temps share
/// slots wherever their live ranges never meet. `num_locals` becomes the
/// packed count.
pub fn pack(func: &mut ProgramFunction) {
    let arity = func.arity();
    let num_args = usize::from(arity.num_args);
    let len = func.instructions.len();
    let flows: Vec<Flow> = func
        .instructions
        .iter()
        .enumerate()
        .map(|(at, instruction)| Flow::of(func, instruction, at, num_args, &arity, len))
        .collect();

    let mut named: Vec<usize> = func
        .local_variables
        .iter()
        .filter_map(|symbol| match symbol.location {
            Some(RegisterVariant::Local(register)) => Some(usize::from(register.index())),
            _ => None,
        })
        .filter(|&register| register > num_args)
        .collect();
    named.sort_unstable();
    named.dedup();

    let universe = flows
        .iter()
        .flat_map(|flow| flow.def.into_iter().chain(flow.uses.iter().copied()))
        .chain(named.iter().copied())
        .max()
        .map_or(0, |max| max + 1);

    let live_in = liveness(&flows, universe);

    let mut occupancy: Vec<Option<BitSet>> = vec![None; universe];
    for (at, flow) in flows.iter().enumerate() {
        for register in live_in[at].iter().chain(flow.def) {
            if register > num_args && named.binary_search(&register).is_err() {
                occupancy[register]
                    .get_or_insert_with(|| BitSet::with_capacity(len))
                    .insert(at);
            }
        }
    }
    let mut temps: Vec<(usize, usize)> = occupancy
        .iter()
        .enumerate()
        .filter_map(|(register, points)| {
            let first = points.as_ref()?.iter().next().unwrap_or(0);
            Some((first, register))
        })
        .collect();
    temps.sort_unstable();

    // Packing never adds a slot, so a packed index fits wherever its
    // original did.
    let mut map: Vec<RegisterSize> = (0..universe)
        .map(|register| register as RegisterSize)
        .collect();
    for (rank, &register) in named.iter().enumerate() {
        map[register] = (num_args + 1 + rank) as RegisterSize;
    }
    let base = num_args + 1 + named.len();
    let mut slots: Vec<BitSet> = vec![];
    for (_, register) in temps {
        let Some(points) = occupancy[register].take() else {
            continue;
        };
        let slot = slots
            .iter()
            .position(|taken| taken.is_disjoint(&points))
            .unwrap_or_else(|| {
                slots.push(BitSet::with_capacity(len));
                slots.len() - 1
            });
        slots[slot].union_with(&points);
        map[register] = (base + slot) as RegisterSize;
    }

    let remap = |register: RegisterVariant| match register {
        RegisterVariant::Local(old) if usize::from(old.index()) > num_args => {
            RegisterVariant::Local(Register(map[usize::from(old.index())]))
        }
        other => other,
    };
    func.rename_registers(remap);
    for symbol in &mut func.local_variables {
        if let Some(RegisterVariant::Local(old)) = symbol.location {
            symbol.location = Some(remap(RegisterVariant::Local(old)));
        }
    }
    func.num_locals = (named.len() + slots.len()) as RegisterSize;
}

/// The registers live on entry to each instruction: read there, or read
/// later along some path with no write in between.
fn liveness(flows: &[Flow], universe: usize) -> Vec<BitSet> {
    let mut live_in = vec![BitSet::with_capacity(universe); flows.len()];
    loop {
        let mut changed = false;
        for (at, flow) in flows.iter().enumerate().rev() {
            let mut live = BitSet::with_capacity(universe);
            for &successor in &flow.succ {
                live.union_with(&live_in[successor]);
            }
            if let Some(def) = flow.def {
                live.remove(def);
            }
            for &register in &flow.uses {
                live.insert(register);
            }
            if live != live_in[at] {
                live_in[at] = live;
                changed = true;
            }
        }
        if !changed {
            return live_in;
        }
    }
}

/// One instruction's reads and write of the tracked locals, and where
/// control goes next.
struct Flow {
    def: Option<usize>,
    uses: Vec<usize>,
    succ: Vec<usize>,
}

impl Flow {
    fn of(
        func: &ProgramFunction,
        instruction: &Instruction,
        at: usize,
        num_args: usize,
        arity: &FunctionArity,
        len: usize,
    ) -> Self {
        let mut operands = local_operands(func, instruction);
        let def = match *instruction {
            // Bumped in place, so the operand stays a read as well.
            Instruction::Inc(register) | Instruction::Dec(register) => local_index(register),
            Instruction::PopulateArgv(register, _, _) | Instruction::CatchStart(register, _) => {
                take_dest(&mut operands, register)
            }
            _ => instruction
                .dest_register()
                .and_then(|dest| take_dest(&mut operands, dest)),
        };
        let tracked = |register: usize| register > num_args;
        Self {
            def: def.filter(|&register| tracked(register)),
            uses: operands
                .into_iter()
                .filter(|&register| tracked(register))
                .collect(),
            succ: successors(instruction, at, arity, len),
        }
    }
}

/// The addresses control can reach from the instruction at `at`, all below
/// `len`.
fn successors(
    instruction: &Instruction,
    at: usize,
    arity: &FunctionArity,
    len: usize,
) -> Vec<usize> {
    let next = match *instruction {
        Instruction::Jmp(address) => vec![address.0],
        Instruction::Ret => vec![],
        Instruction::PopulateDefaults => (0..=usize::from(arity.num_default_args))
            .map(|entry| at + 1 + entry)
            .collect(),
        other => match other.address() {
            Some(address) => vec![address.0, at + 1],
            None => vec![at + 1],
        },
    };
    next.into_iter().filter(|&address| address < len).collect()
}

/// Every Local operand of `instruction`, dests and its argument list
/// included, in operand order.
fn local_operands(func: &ProgramFunction, instruction: &Instruction) -> Vec<usize> {
    func.operand_registers(instruction)
        .into_iter()
        .filter_map(local_index)
        .collect()
}

/// Drop the dest's own occurrence from `operands`, leaving any read of the
/// same register, and name it when it is a Local.
fn take_dest(operands: &mut Vec<usize>, dest: RegisterVariant) -> Option<usize> {
    let index = local_index(dest)?;
    if let Some(position) = operands.iter().position(|&register| register == index) {
        operands.remove(position);
    }
    Some(index)
}

fn local_index(register: RegisterVariant) -> Option<usize> {
    match register {
        RegisterVariant::Local(register) => Some(usize::from(register.index())),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::Arc};

    use lpc_rs_asm::{
        address::Address,
        instruction::{Arg, ArgList, Comparison, Instruction::*},
    };
    use lpc_rs_core::{
        lpc_path::LpcPath,
        lpc_type::LpcType,
        register::{Register, RegisterVariant},
    };
    use lpc_rs_function_support::{function_prototype::FunctionPrototypeBuilder, symbol::Symbol};

    use super::*;

    fn func_with(
        arity: FunctionArity,
        named: &[u16],
        instructions: Vec<Instruction>,
    ) -> ProgramFunction {
        let prototype = FunctionPrototypeBuilder::default()
            .name("t")
            .filename(Arc::new(LpcPath::new_in_game("/t.c", "/", "/")))
            .return_type(LpcType::Void)
            .arity(arity)
            .build()
            .unwrap();
        let mut func = ProgramFunction::new(prototype, 0);
        func.debug_spans = vec![None; instructions.len()];
        func.labels = Some(HashMap::new());
        func.instructions = instructions;
        for &register in named {
            let mut symbol = Symbol::new("v", LpcType::Int(false));
            symbol.location = Some(local(register));
            func.local_variables.push(symbol);
        }
        func
    }

    fn temps_only(instructions: Vec<Instruction>) -> ProgramFunction {
        func_with(FunctionArity::default(), &[], instructions)
    }

    fn with_lists(mut func: ProgramFunction, lists: Vec<Vec<Arg>>) -> ProgramFunction {
        func.arg_lists = lists;
        func
    }

    fn local(i: u16) -> RegisterVariant {
        RegisterVariant::Local(Register(i))
    }

    fn constant(i: u16) -> RegisterVariant {
        RegisterVariant::Constant(Register(i))
    }

    #[test]
    fn temps_read_through_one_list_are_live_together_at_the_call() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(1)),
                Copy(constant(1), local(2)),
                CallEfun(0, ArgList(0)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(1)), Arg::Value(local(2))]],
        );
        pack(&mut func);
        assert_eq!(
            func.arg_lists,
            vec![vec![Arg::Value(local(1)), Arg::Value(local(2))]]
        );
    }

    #[test]
    fn a_list_reads_the_packed_slot() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(5)),
                CallEfun(0, ArgList(0)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(5))]],
        );
        pack(&mut func);
        assert_eq!(
            (func.instructions, func.arg_lists),
            (
                vec![Copy(constant(0), local(1)), CallEfun(0, ArgList(0)), Ret],
                vec![vec![Arg::Value(local(1))]]
            )
        );
    }

    #[test]
    fn a_temp_dead_before_another_is_defined_shares_its_slot() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(1)),
                CallEfun(0, ArgList(0)),
                Copy(constant(1), local(2)),
                CallEfun(0, ArgList(1)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(1))], vec![Arg::Value(local(2))]],
        );
        pack(&mut func);
        assert_eq!(
            func.instructions,
            vec![
                Copy(constant(0), local(1)),
                CallEfun(0, ArgList(0)),
                Copy(constant(1), local(1)),
                CallEfun(0, ArgList(1)),
                Ret
            ]
        );
        assert_eq!(
            func.arg_lists,
            vec![vec![Arg::Value(local(1))], vec![Arg::Value(local(1))]]
        );
        assert_eq!(func.num_locals, 1);
    }

    #[test]
    fn a_temp_defined_while_others_are_read_keeps_its_own_slot() {
        let mut func = temps_only(vec![
            Copy(constant(0), local(1)),
            Copy(constant(1), local(2)),
            Add(local(1), local(2), local(3)),
            Copy(local(3), local(0)),
            Ret,
        ]);
        let before = func.instructions.clone();
        pack(&mut func);
        assert_eq!(func.instructions, before);
        assert_eq!(func.num_locals, 3);
    }

    #[test]
    fn a_named_local_keeps_its_slot_for_the_whole_function() {
        let mut func = with_lists(
            func_with(
                FunctionArity::default(),
                &[1],
                vec![
                    Copy(constant(0), local(1)),
                    CallEfun(0, ArgList(0)),
                    Copy(constant(1), local(2)),
                    CallEfun(0, ArgList(1)),
                    Ret,
                ],
            ),
            vec![vec![Arg::Value(local(1))], vec![Arg::Value(local(2))]],
        );
        let before = func.instructions.clone();
        pack(&mut func);
        assert_eq!(func.instructions, before);
        assert_eq!(func.num_locals, 2);
    }

    #[test]
    fn named_locals_come_first_dense_in_declaration_order() {
        let mut func = with_lists(
            func_with(
                FunctionArity::default(),
                &[3],
                vec![
                    Copy(constant(0), local(1)),
                    CallEfun(0, ArgList(0)),
                    Copy(constant(1), local(2)),
                    CallEfun(0, ArgList(1)),
                    Copy(constant(2), local(3)),
                    CallEfun(0, ArgList(2)),
                    Ret,
                ],
            ),
            vec![
                vec![Arg::Value(local(1))],
                vec![Arg::Value(local(2))],
                vec![Arg::Value(local(3))],
            ],
        );
        pack(&mut func);
        assert_eq!(
            func.instructions,
            vec![
                Copy(constant(0), local(2)),
                CallEfun(0, ArgList(0)),
                Copy(constant(1), local(2)),
                CallEfun(0, ArgList(1)),
                Copy(constant(2), local(1)),
                CallEfun(0, ArgList(2)),
                Ret
            ]
        );
        assert_eq!(
            func.arg_lists,
            vec![
                vec![Arg::Value(local(2))],
                vec![Arg::Value(local(2))],
                vec![Arg::Value(local(1))]
            ]
        );
        assert_eq!(func.local_variables[0].location, Some(local(1)));
        assert_eq!(func.num_locals, 2);
    }

    #[test]
    fn a_back_edge_keeps_a_temp_live_until_its_next_read() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(1)),
                CallEfun(0, ArgList(0)),
                Copy(constant(1), local(2)),
                CallEfun(0, ArgList(1)),
                Jnz(local(0), Address(1)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(1))], vec![Arg::Value(local(2))]],
        );
        let before = func.instructions.clone();
        pack(&mut func);
        assert_eq!(func.instructions, before);
        assert_eq!(func.num_locals, 2);
    }

    #[test]
    fn a_loop_bodys_temps_share_a_slot_when_they_never_meet() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(1)),
                Copy(constant(1), local(2)),
                CallEfun(0, ArgList(0)),
                Inc(local(1)),
                Cmp(Comparison::Lt, local(1), constant(2), local(3)),
                Jnz(local(3), Address(1)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(2))]],
        );
        pack(&mut func);
        assert_eq!(
            func.instructions,
            vec![
                Copy(constant(0), local(1)),
                Copy(constant(1), local(2)),
                CallEfun(0, ArgList(0)),
                Inc(local(1)),
                Cmp(Comparison::Lt, local(1), constant(2), local(2)),
                Jnz(local(2), Address(1)),
                Ret
            ]
        );
        assert_eq!(func.arg_lists, vec![vec![Arg::Value(local(2))]]);
        assert_eq!(func.num_locals, 2);
    }

    #[test]
    fn gaps_left_by_the_peephole_close() {
        let mut func = temps_only(vec![
            Copy(constant(0), local(2)),
            Copy(local(2), local(0)),
            Ret,
        ]);
        pack(&mut func);
        assert_eq!(
            func.instructions,
            vec![Copy(constant(0), local(1)), Copy(local(1), local(0)), Ret]
        );
        assert_eq!(func.num_locals, 1);
    }

    #[test]
    fn r0_and_the_parameters_keep_their_slots() {
        let arity = FunctionArity {
            num_args: 2,
            ..Default::default()
        };
        let mut func = func_with(
            arity,
            &[1, 2],
            vec![
                Add(local(1), local(2), local(4)),
                Copy(local(4), local(0)),
                Ret,
            ],
        );
        pack(&mut func);
        assert_eq!(
            func.instructions,
            vec![
                Add(local(1), local(2), local(3)),
                Copy(local(3), local(0)),
                Ret
            ]
        );
        assert_eq!(func.num_locals, 1);
    }

    #[test]
    fn a_catch_result_is_live_through_its_body() {
        let mut func = with_lists(
            temps_only(vec![
                CatchStart(local(1), Address(5)),
                Copy(constant(0), local(2)),
                CallEfun(0, ArgList(0)),
                CatchEnd,
                Copy(local(1), local(0)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(2))]],
        );
        let before = func.instructions.clone();
        pack(&mut func);
        assert_eq!(func.instructions, before);
        assert_eq!(func.num_locals, 2);
    }

    #[test]
    fn an_inc_reads_its_register_before_writing_it() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(1)),
                CallEfun(0, ArgList(0)),
                Inc(local(2)),
                Copy(local(2), local(0)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(1))]],
        );
        let before = func.instructions.clone();
        pack(&mut func);
        assert_eq!(func.instructions, before);
        assert_eq!(func.num_locals, 2);
    }

    #[test]
    fn populate_argv_writes_its_register() {
        let mut func = with_lists(
            temps_only(vec![
                Copy(constant(0), local(1)),
                CallEfun(0, ArgList(0)),
                PopulateArgv(local(2), 0, 0),
                Copy(local(2), local(0)),
                Ret,
            ]),
            vec![vec![Arg::Value(local(1))]],
        );
        pack(&mut func);
        assert_eq!(
            func.instructions,
            vec![
                Copy(constant(0), local(1)),
                CallEfun(0, ArgList(0)),
                PopulateArgv(local(1), 0, 0),
                Copy(local(1), local(0)),
                Ret
            ]
        );
        assert_eq!(func.arg_lists, vec![vec![Arg::Value(local(1))]]);
        assert_eq!(func.num_locals, 1);
    }

    #[test]
    fn a_defaults_table_flows_into_every_slot() {
        let arity = FunctionArity {
            num_args: 2,
            num_default_args: 2,
        };
        assert_eq!(successors(&PopulateDefaults, 0, &arity, 8), vec![1, 2, 3]);
        assert_eq!(
            successors(&Jnz(local(1), Address(6)), 4, &arity, 8),
            vec![6, 5]
        );
        assert_eq!(
            successors(
                &Jcmp(Comparison::Lt, local(1), local(2), Address(6)),
                4,
                &arity,
                8
            ),
            vec![6, 5]
        );
        assert_eq!(successors(&Ret, 7, &arity, 8), Vec::<usize>::new());
        assert_eq!(
            successors(&Jmp(Address(9)), 7, &arity, 8),
            Vec::<usize>::new()
        );
    }
}
