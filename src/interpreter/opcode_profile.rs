//! Per-opcode dispatch counters, compiled in by the `opcode-profile`
//! feature; the eval loop records one count per dispatched instruction.

use std::sync::atomic::{AtomicU64, Ordering::Relaxed};

use lpc_rs_asm::instruction::Instruction;

/// One reading of the counters, indexed by [`Instruction::index`].
pub type Snapshot = [u64; Instruction::COUNT];

static COUNTS: [AtomicU64; Instruction::COUNT] = [const { AtomicU64::new(0) }; Instruction::COUNT];

/// Count one dispatch of `instruction`.
#[inline]
pub fn record(instruction: &Instruction) {
    COUNTS[instruction.index() as usize].fetch_add(1, Relaxed);
}

/// The current totals.
pub fn snapshot() -> Snapshot {
    std::array::from_fn(|i| COUNTS[i].load(Relaxed))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recording_lands_in_the_instructions_slot() {
        let before = snapshot();
        record(&Instruction::Ret);
        record(&Instruction::Ret);
        let after = snapshot();

        let slot = Instruction::Ret.index() as usize;
        assert_eq!(after[slot] - before[slot], 2);
        assert_eq!(Instruction::MNEMONICS[slot], "ret");
    }
}
