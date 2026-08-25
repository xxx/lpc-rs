//! A context-free grammar over a token stream, parsed by Earley with every
//! derivation enumerated lazily. Nothing here knows about LPC values, objects,
//! or transactions: frontends compile their surface syntax into a [`Grammar`]
//! and read captures back out of a parse.

mod earley;
mod model;
mod tokenizer;

pub use model::{
    Element, Grammar, GrammarBuilder, GrammarError, Label, NtId, Options, ProdId, Production,
    Symbol, TokenClass, lit, nt, tok,
};
pub use tokenizer::{Scan, Token};
