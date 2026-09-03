//! The peephole: the per-function rewrite behind
//! [`finalize_function`](super::codegen_walker::CodegenWalker), run after
//! label backpatch so every jump operand is a concrete [`Address`].

use lpc_rs_asm::{address::Address, instruction::Instruction};
use lpc_rs_core::register::{Register, RegisterVariant};
use lpc_rs_function_support::program_function::ProgramFunction;

/// Rewrite `func` to a fixpoint of the copy rules and the control rules.
///
/// Copy rules: fold a define-into-fresh-temp followed by its `Copy` into
/// a retargeted dest, read a call's result straight from r0 where no
/// clobber intervenes, drop identity copies, and drop copies into temps
/// nothing reads. Control rules: drop the unreachable tail after a `Ret`
/// or `Jmp`, and drop a `Jmp` to the next address.
pub fn peephole(func: &mut ProgramFunction) {
    loop {
        let copies = coalesce_copies(func);
        let control = prune_control(func);
        if !copies && !control {
            break;
        }
    }
}

/// The addresses named by jump operands and catch ends; a
/// `PopulateDefaults` table slot is not among them, being entered by
/// offset.
fn jump_targets(func: &ProgramFunction) -> Vec<Address> {
    func.instructions
        .iter()
        .filter_map(Instruction::address)
        .collect()
}

/// One sweep of the control rules; true when it deleted anything.
fn prune_control(func: &mut ProgramFunction) -> bool {
    let len = func.instructions.len();
    let mut landing = vec![false; len];
    for a in jump_targets(func) {
        if a.0 < len {
            landing[a.0] = true;
        }
    }
    // A table slot is landed on by offset and must keep its position.
    let mut slot = vec![false; len];
    let num_default_args = usize::from(func.arity().num_default_args);
    for (i, instruction) in func.instructions.iter().enumerate() {
        if matches!(instruction, Instruction::PopulateDefaults) {
            for s in slot.iter_mut().skip(i + 1).take(num_default_args) {
                *s = true;
            }
        }
    }

    let mut delete = vec![false; len];
    let mut changed = false;
    let mut dead = false;
    for i in 0..len {
        if landing[i] || slot[i] {
            dead = false;
        }
        if dead {
            delete[i] = true;
            changed = true;
            continue;
        }
        match func.instructions[i] {
            Instruction::Jmp(a) if a.0 == i + 1 && !slot[i] => {
                delete[i] = true;
                changed = true;
            }
            Instruction::Jmp(_) | Instruction::Ret => dead = true,
            _ => {}
        }
    }

    if changed {
        remove(func, &delete);
    }
    changed
}

/// One sweep of the copy rules; true when it deleted anything.
fn coalesce_copies(func: &mut ProgramFunction) -> bool {
    let r0 = RegisterVariant::Local(Register(0));
    let named: Vec<RegisterVariant> = func
        .local_variables
        .iter()
        .filter_map(|sym| sym.location)
        .chain(func.arg_locations.iter().copied())
        .collect();
    let jump_targets = jump_targets(func);

    let mut delete = vec![false; func.instructions.len()];
    let mut changed = false;

    for i in 0..func.instructions.len() {
        let Instruction::Copy(src, dst) = func.instructions[i] else {
            continue;
        };

        if src == dst {
            delete[i] = true;
            changed = true;
            continue;
        }

        // A jump landing on the Copy would run it on a path where the
        // fold never did; such a Copy stays.
        if i > 0
            && !delete[i - 1]
            && func.instructions[i - 1].dest_register() == Some(src)
            && matches!(src, RegisterVariant::Local(_))
            && src != r0
            && !named.contains(&src)
            && mentions(func, src) == 2
            && !jump_targets.contains(&Address(i))
        {
            func.instructions[i - 1] =
                func.instructions[i - 1].map_registers(|r| if r == src { dst } else { r });
            delete[i] = true;
            changed = true;
            continue;
        }

        // A call's result reads straight from r0.
        if src == r0
            && matches!(dst, RegisterVariant::Local(_))
            && !named.contains(&dst)
            && mentions(func, dst) == 2
            && let Some(use_at) = propagation_use(func, i, dst, &jump_targets)
            && !delete[use_at]
        {
            func.rename_registers_at(use_at, |r| if r == dst { r0 } else { r });
            delete[i] = true;
            changed = true;
            continue;
        }

        // A Global/Upvalue source stays — its tracked read is part of
        // the attempt's conflict set.
        if matches!(
            src,
            RegisterVariant::Local(_) | RegisterVariant::Constant(_)
        ) && matches!(dst, RegisterVariant::Local(_))
            && dst != r0
            && !named.contains(&dst)
            && mentions(func, dst) == 1
        {
            delete[i] = true;
            changed = true;
        }
    }

    if changed {
        remove(func, &delete);
    }
    changed
}

/// How many operand slots across the function name `reg`, dests included.
/// Whole-function, never a suffix — a loop back edge re-reads earlier
/// addresses.
fn mentions(func: &ProgramFunction, reg: RegisterVariant) -> usize {
    func.instructions
        .iter()
        .map(|i| mentions_in(func, i, reg))
        .sum()
}

/// How many of this instruction's operand slots, its argument list's
/// included, name `reg`.
fn mentions_in(func: &ProgramFunction, instruction: &Instruction, reg: RegisterVariant) -> usize {
    func.operand_registers(instruction)
        .into_iter()
        .filter(|&r| r == reg)
        .count()
}

/// The single use of `temp` reachable straight-line from the copy at
/// `from`, or None when an r0 write, a control edge, or a join gets
/// there first. Conditional branches fall through with r0 intact, so
/// the scan continues past them.
fn propagation_use(
    func: &ProgramFunction,
    from: usize,
    temp: RegisterVariant,
    jump_targets: &[Address],
) -> Option<usize> {
    let r0 = RegisterVariant::Local(Register(0));
    for j in (from + 1)..func.instructions.len() {
        // A join may arrive with a different r0, so it ends the scan
        // even when it lands on the use itself.
        if jump_targets.contains(&Address(j)) {
            return None;
        }
        let instruction = &func.instructions[j];
        if mentions_in(func, instruction, temp) > 0 {
            return Some(j);
        }
        let clobbers_r0 = instruction.dest_register() == Some(r0)
            || matches!(
                instruction,
                Instruction::Call(..)
                    | Instruction::CallEfun(..)
                    | Instruction::CallSimulEfun(..)
                    | Instruction::CallFp(..)
                    | Instruction::CallOther(..)
            );
        if clobbers_r0
            || matches!(
                instruction,
                Instruction::Jmp(_)
                    | Instruction::Ret
                    | Instruction::CatchStart(_, _)
                    | Instruction::CatchEnd
            )
        {
            return None;
        }
    }
    None
}

/// Drop the marked instructions, keeping `debug_spans` in lockstep and
/// shifting every absolute address (jump operands and label values) down
/// past the deletions.
fn remove(func: &mut ProgramFunction, delete: &[bool]) {
    // new_index[a] = kept instructions before `a`; a deleted address maps
    // to the next surviving instruction. The extra entry serves jumps to
    // one past the end.
    let mut new_index = Vec::with_capacity(delete.len() + 1);
    let mut kept = 0;
    for &d in delete {
        new_index.push(kept);
        if !d {
            kept += 1;
        }
    }
    new_index.push(kept);

    for instruction in func.instructions.iter_mut() {
        *instruction = instruction.map_address(|a| Address(new_index[a.0]));
    }
    if let Some(labels) = func.labels.as_mut() {
        for a in labels.values_mut() {
            *a = Address(new_index[a.0]);
        }
    }

    let mut keep = delete.iter();
    func.instructions.retain(|_| !*keep.next().unwrap());
    let mut keep = delete.iter();
    func.debug_spans.retain(|_| !*keep.next().unwrap());
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::Arc};

    use lpc_rs_asm::instruction::{Arg, ArgList, Comparison, Instruction::*};
    use lpc_rs_core::{function_arity::FunctionArity, lpc_path::LpcPath, lpc_type::LpcType};
    use lpc_rs_function_support::function_prototype::FunctionPrototypeBuilder;

    use super::*;

    fn func_with(instructions: Vec<Instruction>) -> ProgramFunction {
        let prototype = FunctionPrototypeBuilder::default()
            .name("t")
            .filename(Arc::new(LpcPath::new_in_game("/t.c", "/", "/")))
            .return_type(LpcType::Void)
            .build()
            .unwrap();
        let mut func = ProgramFunction::new(prototype, 0);
        func.debug_spans = vec![None; instructions.len()];
        func.labels = Some(HashMap::new());
        func.instructions = instructions;
        func
    }

    /// A function whose calls read `lists`.
    fn func_with_lists(instructions: Vec<Instruction>, lists: Vec<Vec<Arg>>) -> ProgramFunction {
        let mut func = func_with(instructions);
        func.arg_lists = lists;
        func
    }

    /// A call reading argument list `list`, the reader these tests use.
    fn reads(list: u16) -> Instruction {
        CallEfun(0, ArgList(list))
    }

    fn local(i: u16) -> RegisterVariant {
        RegisterVariant::Local(Register(i))
    }

    fn global(i: u16) -> RegisterVariant {
        RegisterVariant::Global(Register(i))
    }

    #[test]
    fn a_dead_copy_from_a_constant_is_deleted() {
        let mut func = func_with(vec![Copy(Register(0).as_constant(), local(2)), Ret]);
        peephole(&mut func);
        assert_eq!(func.instructions, vec![Ret]);
    }

    #[test]
    fn a_fold_retargets_the_definer_and_remaps_addresses() {
        let mut func = func_with(vec![
            Add(local(8), local(9), local(1)),
            Copy(local(1), global(0)),
            Jz(local(8), Address(3)),
            Ret,
        ]);
        func.labels
            .as_mut()
            .unwrap()
            .insert("end".into(), Address(3));

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![
                Add(local(8), local(9), global(0)),
                Jz(local(8), Address(2)),
                Ret
            ]
        );
        assert_eq!(func.debug_spans.len(), 3);
        assert_eq!(func.labels.as_ref().unwrap()["end"], Address(2));
    }

    #[test]
    fn a_jump_targeted_copy_survives() {
        let instructions = vec![
            Add(local(8), local(9), local(1)),
            Copy(local(1), global(0)),
            Jz(local(8), Address(1)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_reread_temp_keeps_its_copy() {
        let instructions = vec![
            Add(local(8), local(9), local(1)),
            Copy(local(1), global(0)),
            reads(0),
            Ret,
        ];
        let mut func = func_with_lists(instructions.clone(), vec![vec![Arg::Value(local(1))]]);

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn an_argument_location_is_not_folded() {
        let instructions = vec![
            Add(local(8), local(9), local(1)),
            Copy(local(1), global(0)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());
        func.arg_locations = vec![local(1)];

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_global_sourced_dead_copy_survives() {
        let instructions = vec![Copy(global(0), local(1)), Ret];
        let mut func = func_with(instructions.clone());

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_catch_address_shifts_past_a_deletion() {
        let mut func = func_with(vec![
            Copy(local(1), local(1)),
            CatchStart(local(2), Address(2)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![CatchStart(local(2), Address(1)), Ret]
        );
    }

    #[test]
    fn a_clobbered_result_keeps_its_copy() {
        let instructions = vec![
            Call(ustr::ustr("f"), ArgList(0)),
            Copy(local(0), local(1)),
            CallEfun(0, ArgList(1)),
            reads(2),
            Ret,
        ];
        let mut func = func_with_lists(
            instructions.clone(),
            vec![vec![], vec![], vec![Arg::Value(local(1))]],
        );

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_join_inside_the_window_keeps_the_copy() {
        let instructions = vec![
            Copy(local(0), local(1)),
            Add(local(8), local(9), local(3)),
            reads(0),
            Jz(local(3), Address(1)),
            Ret,
        ];
        let mut func = func_with_lists(instructions.clone(), vec![vec![Arg::Value(local(1))]]);

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_use_beyond_a_jmp_keeps_the_copy() {
        let instructions = vec![
            Copy(local(0), local(1)),
            Jmp(Address(3)),
            reads(0),
            Jz(local(2), Address(2)),
            Ret,
        ];
        let mut func = func_with_lists(instructions.clone(), vec![vec![Arg::Value(local(1))]]);

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_branch_condition_reads_r0() {
        let mut func = func_with(vec![
            Copy(local(0), local(1)),
            Jnz(local(1), Address(3)),
            Add(local(8), local(9), local(2)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![
                Jnz(local(0), Address(2)),
                Add(local(8), local(9), local(2)),
                Ret
            ]
        );
    }

    #[test]
    fn a_fall_through_use_past_a_branch_reads_r0() {
        let mut func = func_with_lists(
            vec![
                Copy(local(0), local(1)),
                Jz(local(2), Address(4)),
                reads(0),
                Ret,
                Ret,
            ],
            vec![vec![Arg::Value(local(1))]],
        );

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![Jz(local(2), Address(3)), reads(0), Ret, Ret]
        );
        assert_eq!(func.arg_lists, vec![vec![Arg::Value(local(0))]]);
    }

    #[test]
    fn chained_copies_collapse_to_the_final_dest() {
        let mut func = func_with(vec![
            Add(local(8), local(9), local(1)),
            Copy(local(1), local(2)),
            Copy(local(2), global(0)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![Add(local(8), local(9), global(0)), Ret]
        );
    }

    #[test]
    fn a_ref_argument_is_never_rewritten() {
        let upvalue = RegisterVariant::Upvalue(Register(0));
        let mut func = func_with_lists(
            vec![
                Add(local(8), local(9), local(1)),
                Copy(local(1), upvalue),
                Call(ustr::ustr("inc"), ArgList(0)),
                Ret,
            ],
            vec![vec![Arg::Ref(upvalue)]],
        );

        peephole(&mut func);

        assert_eq!(func.arg_lists, vec![vec![Arg::Ref(upvalue)]]);
    }

    fn func_with_defaults(
        num_default_args: u16,
        instructions: Vec<Instruction>,
    ) -> ProgramFunction {
        let prototype = FunctionPrototypeBuilder::default()
            .name("t")
            .filename(Arc::new(LpcPath::new_in_game("/t.c", "/", "/")))
            .return_type(LpcType::Void)
            .arity(FunctionArity {
                num_args: num_default_args,
                num_default_args,
            })
            .build()
            .unwrap();
        let mut func = ProgramFunction::new(prototype, 0);
        func.debug_spans = vec![None; instructions.len()];
        func.labels = Some(HashMap::new());
        func.instructions = instructions;
        func
    }

    #[test]
    fn the_tail_after_a_ret_is_deleted() {
        let mut func = func_with(vec![Ret, Add(local(1), local(2), local(3))]);

        peephole(&mut func);

        assert_eq!(func.instructions, vec![Ret]);
    }

    #[test]
    fn a_jmp_over_a_dead_tail_collapses() {
        let mut func = func_with(vec![
            Jmp(Address(2)),
            Add(local(1), local(2), local(3)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(func.instructions, vec![Ret]);
    }

    #[test]
    fn a_jump_target_ends_the_tail() {
        let mut func = func_with(vec![
            Jz(local(1), Address(3)),
            Jmp(Address(4)),
            Add(local(1), local(2), local(3)),
            Sub(local(1), local(2), local(3)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![
                Jz(local(1), Address(2)),
                Jmp(Address(3)),
                Sub(local(1), local(2), local(3)),
                Ret,
            ]
        );
    }

    #[test]
    fn a_catch_end_is_a_target() {
        let instructions = vec![
            CatchStart(local(1), Address(2)),
            Ret,
            Add(local(1), local(2), local(3)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_defaults_table_is_untouched() {
        let instructions = vec![
            PopulateDefaults,
            Jmp(Address(5)),
            Jmp(Address(6)),
            Mul(local(1), local(2), local(0)),
            Ret,
            Copy(RegisterVariant::Constant(Register(0)), local(2)),
            Copy(RegisterVariant::Constant(Register(1)), local(1)),
            Jmp(Address(3)),
        ];
        let mut func = func_with_defaults(2, instructions.clone());

        peephole(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_fused_branch_target_is_a_landing_site() {
        let mut func = func_with(vec![
            Jcmp(Comparison::Lt, local(1), local(2), Address(3)),
            Ret,
            Ret,
            Copy(local(1), local(0)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![
                Jcmp(Comparison::Lt, local(1), local(2), Address(2)),
                Ret,
                Copy(local(1), local(0)),
                Ret,
            ]
        );
    }

    #[test]
    fn a_jmp_to_the_next_address_is_deleted() {
        let mut func = func_with(vec![Jmp(Address(1)), Ret]);

        peephole(&mut func);

        assert_eq!(func.instructions, vec![Ret]);
    }

    #[test]
    fn labels_and_spans_shift_past_a_pruned_tail() {
        let mut func = func_with(vec![
            Jz(local(1), Address(3)),
            Ret,
            Add(local(1), local(2), local(3)),
            Ret,
        ]);
        func.labels
            .as_mut()
            .unwrap()
            .insert("end".into(), Address(3));

        peephole(&mut func);

        assert_eq!(func.instructions, vec![Jz(local(1), Address(2)), Ret, Ret]);
        assert_eq!(func.labels.as_ref().unwrap()["end"], Address(2));
        assert_eq!(func.debug_spans.len(), 3);
    }

    #[test]
    fn a_pruned_jmp_unlocks_a_fold() {
        let mut func = func_with(vec![
            Jz(local(1), Address(3)),
            Ret,
            Jmp(Address(4)),
            Add(local(1), local(2), local(3)),
            Copy(local(3), local(4)),
            Sub(local(4), local(1), local(0)),
            Ret,
        ]);

        peephole(&mut func);

        assert_eq!(
            func.instructions,
            vec![
                Jz(local(1), Address(2)),
                Ret,
                Add(local(1), local(2), local(4)),
                Sub(local(4), local(1), local(0)),
                Ret,
            ]
        );
    }
}
