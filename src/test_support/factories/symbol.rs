use std::cell::Cell;

use lpc_rs_core::lpc_type::LpcType;
use lpc_rs_function_support::symbol::Symbol;

/// Builds `Symbol`s named `sym_{n}` (int-typed, location-less), `n` counting
/// up per factory; `build` applies one edit to the fresh symbol.
pub struct SymbolFactory {
    next: Cell<u16>,
}

impl SymbolFactory {
    pub fn new() -> Self {
        Self { next: Cell::new(0) }
    }

    pub fn build<F>(&self, f: F) -> Symbol
    where
        F: FnOnce(&mut Symbol),
    {
        let n = self.next.get();
        self.next.set(n + 1);
        let mut symbol = Symbol::new(&format!("sym_{n}"), LpcType::Int(false));
        f(&mut symbol);
        symbol
    }
}

impl Default for SymbolFactory {
    fn default() -> Self {
        Self::new()
    }
}
