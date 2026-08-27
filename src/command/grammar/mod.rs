//! A context-free grammar over a token stream, parsed by Earley with every
//! derivation enumerated lazily. Nothing here knows about LPC values, objects,
//! or transactions: frontends compile their surface syntax into a [`Grammar`]
//! and read captures back out of a parse.

mod builtins;
mod earley;
mod model;
#[cfg(test)]
mod tests;
mod tokenizer;
mod tree;

pub use builtins::Words;
pub use earley::{Parses, parse};
pub use model::{
    DEFAULT_MAX_DEPTH, Element, Grammar, GrammarBuilder, GrammarError, Label, NtId, Options,
    ProdId, Production, Symbol, TokenClass, lit, nt, tok,
};
pub use tokenizer::{Scan, Token};
pub use tree::{Child, Node, Parse};
