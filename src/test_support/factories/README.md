This project uses [`factori-imp`](https://docs.rs/factori-imp/latest/factori_imp/)
to define test factories and [`fake`](https://docs.rs/fake/latest/fake/)
to generate fixture values.

Use `create!` from the factory prelude. It re-exports the upstream procedural
macro directly because the `factori-imp` 0.9.3 facade appends a trailing semicolon
that Rust 1.99 warns about in expression positions.

Factories without custom builders must be in the same crate as the type, so they
can't be used for third-party types, or even types defined in other lpc-rs
crates; `SymbolFactory` provides a handwritten builder for `Symbol`.

`factori-imp` enables the `syn` features its macros need, so no explicit `syn`
build dependency is necessary; it still depends on `syn` 1 transitively.
