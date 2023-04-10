use std::sync::atomic::AtomicUsize;

use lpc_rs_errors::{LpcError, Result};
use lpc_rs_utils::config::Config;

/// A struct to encapsulate counting instructions, and erroring in the case of
/// hitting the eval limit
#[derive(Debug, Default)]
pub struct InstructionCounter {
    count: AtomicUsize,
    max_instructions: usize,
}

impl InstructionCounter {
    /// Create a new [`InstructionCounter`], with limits taken from `config`
    pub fn new_from_config(config: &Config) -> Self {
        Self {
            count: AtomicUsize::new(0),
            max_instructions: config.max_task_instructions.unwrap_or(0),
        }
    }

    /// Increment the counter by `amount`
    pub fn increment(&self, amount: usize) -> Result<usize> {
        let old_val = self
            .count
            .fetch_add(amount, std::sync::atomic::Ordering::Relaxed);
        let new_val = old_val + amount;
        // We check the limit here to avoid running multiple atomic operations
        self.eval_limit(new_val)?;

        self.set(new_val)
    }

    /// Set the counter to `new_val`
    pub fn set(&self, new_val: usize) -> Result<usize> {
        self.eval_limit(new_val)?;

        self.count
            .store(new_val, std::sync::atomic::Ordering::Relaxed);

        Ok(new_val)
    }

    /// Get the current instruction count
    #[inline]
    pub fn count(&self) -> usize {
        self.count.load(std::sync::atomic::Ordering::Relaxed)
    }

    #[inline]
    fn eval_limit(&self, val: usize) -> Result<()> {
        if self.max_instructions > 0 && val > self.max_instructions {
            return Err(LpcError::new(format!(
                "evaluation limit of `{}` instructions has been reached.",
                self.max_instructions
            )));
        }

        Ok(())
    }
}

impl Clone for InstructionCounter {
    fn clone(&self) -> Self {
        Self {
            count: AtomicUsize::new(self.count()),
            max_instructions: self.max_instructions,
        }
    }
}
