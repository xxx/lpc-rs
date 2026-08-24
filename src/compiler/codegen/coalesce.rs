//! Copy coalescing: the per-function rewrite behind
//! [`finalize_function`](super::codegen_walker::CodegenWalker), run after
//! label backpatch so every jump operand is a concrete [`Address`].

use lpc_rs_asm::{address::Address, instruction::Instruction};
use lpc_rs_core::register::{Register, RegisterVariant};
use lpc_rs_function_support::program_function::ProgramFunction;

/// Rewrite `func` so values land where they are used: fold a
/// define-into-fresh-temp followed by its `Copy` into a retargeted dest,
/// read a call's result straight from r0 where no clobber intervenes,
/// drop identity copies, and drop copies into temps nothing reads.
pub fn coalesce(func: &mut ProgramFunction) {
    while pass(func) {}
}

/// One sweep; true when it deleted anything.
fn pass(func: &mut ProgramFunction) -> bool {
    let r0 = RegisterVariant::Local(Register(0));
    let named: Vec<RegisterVariant> = func
        .local_variables
        .iter()
        .filter_map(|sym| sym.location)
        .chain(func.arg_locations.iter().copied())
        .collect();
    let jump_targets: Vec<Address> = func
        .instructions
        .iter()
        .filter_map(|i| match *i {
            Instruction::Jmp(a)
            | Instruction::Jnz(_, a)
            | Instruction::Jz(_, a)
            | Instruction::CatchStart(_, a) => Some(a),
            _ => None,
        })
        .collect();

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
            func.instructions[use_at] =
                func.instructions[use_at].map_registers(|r| if r == dst { r0 } else { r });
            delete[i] = true;
            changed = true;
            continue;
        }

        // A Global/Upvalue source stays — its tracked read is part of
        // the attempt's conflict set.
        if matches!(src, RegisterVariant::Local(_))
            && matches!(dst, RegisterVariant::Local(_))
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
    func.instructions.iter().map(|i| mentions_in(i, reg)).sum()
}

/// How many of this instruction's operand slots name `reg`.
fn mentions_in(instruction: &Instruction, reg: RegisterVariant) -> usize {
    let count = std::cell::Cell::new(0);
    instruction.map_registers(|r| {
        if r == reg {
            count.set(count.get() + 1);
        }
        r
    });
    count.get()
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
        if mentions_in(instruction, temp) > 0 {
            return Some(j);
        }
        let clobbers_r0 = instruction.dest_register() == Some(r0)
            || matches!(
                instruction,
                Instruction::Call(_)
                    | Instruction::CallEfun(_)
                    | Instruction::CallSimulEfun(_)
                    | Instruction::CallFp(_)
                    | Instruction::CallOther(_, _)
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
        *instruction = match *instruction {
            Instruction::Jmp(a) => Instruction::Jmp(Address(new_index[a.0])),
            Instruction::Jnz(r, a) => Instruction::Jnz(r, Address(new_index[a.0])),
            Instruction::Jz(r, a) => Instruction::Jz(r, Address(new_index[a.0])),
            Instruction::CatchStart(r, a) => Instruction::CatchStart(r, Address(new_index[a.0])),
            other => other,
        };
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

    use lpc_rs_asm::instruction::Instruction::*;
    use lpc_rs_core::{lpc_path::LpcPath, lpc_type::LpcType};
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

    fn local(i: u16) -> RegisterVariant {
        RegisterVariant::Local(Register(i))
    }

    fn global(i: u16) -> RegisterVariant {
        RegisterVariant::Global(Register(i))
    }

    #[test]
    fn a_fold_retargets_the_definer_and_remaps_addresses() {
        let mut func = func_with(vec![
            IConst(local(1), 5),
            Copy(local(1), global(0)),
            Jmp(Address(3)),
            Ret,
        ]);
        func.labels
            .as_mut()
            .unwrap()
            .insert("end".into(), Address(3));

        coalesce(&mut func);

        assert_eq!(
            func.instructions,
            vec![IConst(global(0), 5), Jmp(Address(2)), Ret]
        );
        assert_eq!(func.debug_spans.len(), 3);
        assert_eq!(func.labels.as_ref().unwrap()["end"], Address(2));
    }

    #[test]
    fn a_jump_targeted_copy_survives() {
        let instructions = vec![
            IConst(local(1), 5),
            Copy(local(1), global(0)),
            Jmp(Address(1)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_reread_temp_keeps_its_copy() {
        let instructions = vec![
            IConst(local(1), 5),
            Copy(local(1), global(0)),
            PushArg(local(1)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn an_argument_location_is_not_folded() {
        let instructions = vec![IConst(local(1), 5), Copy(local(1), global(0)), Ret];
        let mut func = func_with(instructions.clone());
        func.arg_locations = vec![local(1)];

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_global_sourced_dead_copy_survives() {
        let instructions = vec![Copy(global(0), local(1)), Ret];
        let mut func = func_with(instructions.clone());

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_catch_address_shifts_past_a_deletion() {
        let mut func = func_with(vec![
            Copy(local(1), local(1)),
            CatchStart(local(2), Address(2)),
            Ret,
        ]);

        coalesce(&mut func);

        assert_eq!(
            func.instructions,
            vec![CatchStart(local(2), Address(1)), Ret]
        );
    }

    #[test]
    fn a_clobbered_result_keeps_its_copy() {
        let instructions = vec![
            Call(ustr::ustr("f")),
            Copy(local(0), local(1)),
            CallEfun(0),
            PushArg(local(1)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_join_inside_the_window_keeps_the_copy() {
        let instructions = vec![
            Copy(local(0), local(1)),
            IConst0(local(3)),
            PushArg(local(1)),
            Jmp(Address(1)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_use_beyond_a_jmp_keeps_the_copy() {
        let instructions = vec![
            Copy(local(0), local(1)),
            Jmp(Address(3)),
            PushArg(local(1)),
            Ret,
        ];
        let mut func = func_with(instructions.clone());

        coalesce(&mut func);

        assert_eq!(func.instructions, instructions);
    }

    #[test]
    fn a_branch_condition_reads_r0() {
        let mut func = func_with(vec![
            Copy(local(0), local(1)),
            Jnz(local(1), Address(3)),
            IConst0(local(2)),
            Ret,
        ]);

        coalesce(&mut func);

        assert_eq!(
            func.instructions,
            vec![Jnz(local(0), Address(2)), IConst0(local(2)), Ret]
        );
    }

    #[test]
    fn a_fall_through_use_past_a_branch_reads_r0() {
        let mut func = func_with(vec![
            Copy(local(0), local(1)),
            Jz(local(2), Address(4)),
            PushArg(local(1)),
            Ret,
            Ret,
        ]);

        coalesce(&mut func);

        assert_eq!(
            func.instructions,
            vec![Jz(local(2), Address(3)), PushArg(local(0)), Ret, Ret]
        );
    }

    #[test]
    fn chained_copies_collapse_to_the_final_dest() {
        let mut func = func_with(vec![
            IConst(local(1), 5),
            Copy(local(1), local(2)),
            Copy(local(2), global(0)),
            Ret,
        ]);

        coalesce(&mut func);

        assert_eq!(func.instructions, vec![IConst(global(0), 5), Ret]);
    }
}
