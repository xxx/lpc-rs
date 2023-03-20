This project uses both [`factori`](https://docs.rs/factori/latest/factori/)
and [`beaver`](https://docs.rs/beaver/latest/beaver/) to create fixtures, because they
each have frustrating drawbacks that the other doesn't.

Factori requires the factory to be in the same crate as the type, so it
can't be used for third-party types, or even types defined in other lpc-rs
crates.

Beaver requires the type to be both `Serializable` and `Deserializable`, which
is a tall order for some types.

Sigh.
